#!/usr/bin/env ruby
# frozen_string_literal: true

# Deterministic, dependency-free extraction of the query-under-test from the
# pinned openCypher Gherkin corpus. This does not execute TCK semantics.

require "digest"
require "json"
require "open3"

EXPECTED_COMMIT = "30b451d3b7c94ee5a84a0fdc223947a442dd9493"
SCHEMA = "gerbil-parser.opencypher-tck-query-manifest.v1"

def fail_usage
  warn "usage: opencypher-tck-extract.rb OPENCYPHER_CHECKOUT OUTPUT"
  exit 64
end

def git_head(checkout)
  stdout, stderr, status = Open3.capture3("git", "-C", checkout, "rev-parse", "HEAD")
  raise "cannot resolve openCypher HEAD: #{stderr.strip}" unless status.success?

  stdout.strip
end

def scenario_start?(line)
  line.match?(/\A\s*Scenario(?: Outline)?:/)
end

def leading_width(line)
  line[/\A[ \t]*/].length
end

def extract_query(block, path, scenario_line)
  step_index = block.index { |line| line.match?(/\A\s*When executing query:/) }
  raise "missing query step at #{path}:#{scenario_line}" unless step_index

  match = block.fetch(step_index).match(/\A\s*When executing query:\s*(.*)\z/)
  tail = match[1].sub(/\r?\n\z/, "")
  return [tail, scenario_line + step_index] unless tail.empty?

  cursor = step_index + 1
  cursor += 1 while cursor < block.length && block[cursor].strip.empty?
  unless cursor < block.length && block[cursor].strip == '"""'
    raise "missing query docstring at #{path}:#{scenario_line + step_index}"
  end

  body_start = cursor + 1
  cursor = body_start
  cursor += 1 while cursor < block.length && block[cursor].strip != '"""'
  raise "unterminated query docstring at #{path}:#{scenario_line + step_index}" if cursor == block.length

  body = block[body_start...cursor]
  indent = body.reject { |line| line.strip.empty? }.map { |line| leading_width(line) }.min || 0
  query = body.map do |line|
    width = [leading_width(line), indent].min
    line[width..]
  end.join
  [query, scenario_line + body_start]
end

def parse_table_row(line, path, line_number)
  stripped = line.strip
  raise "invalid Examples row at #{path}:#{line_number}" unless stripped.start_with?("|") && stripped.end_with?("|")

  cells = []
  cell = +""
  escaped = false
  stripped[1...-1].each_char do |char|
    if escaped
      cell << case char
              when "n" then "\n"
              when "|" then "|"
              when "\\" then "\\"
              else "\\#{char}"
              end
      escaped = false
    elsif char == "\\"
      escaped = true
    elsif char == "|"
      cells << cell.strip
      cell = +""
    else
      cell << char
    end
  end
  cell << "\\" if escaped
  cells << cell.strip
  cells
end

def example_rows(block, path, scenario_line)
  sets = []
  index = 0
  while index < block.length
    unless block[index].strip == "Examples:"
      index += 1
      next
    end

    set_number = sets.length + 1
    cursor = index + 1
    cursor += 1 while cursor < block.length &&
                         (block[cursor].strip.empty? ||
                          block[cursor].lstrip.start_with?("#", "@"))
    unless cursor < block.length && block[cursor].strip.start_with?("|")
      raise "missing Examples header at #{path}:#{scenario_line + index}"
    end

    header = parse_table_row(block[cursor], path, scenario_line + cursor)
    raise "empty Examples header at #{path}:#{scenario_line + cursor}" if header.empty?
    raise "duplicate Examples column at #{path}:#{scenario_line + cursor}" unless header.uniq.length == header.length
    cursor += 1
    rows = []
    row_number = 0
    while cursor < block.length
      if block[cursor].strip.empty? || block[cursor].lstrip.start_with?("#")
        cursor += 1
        next
      end
      break unless block[cursor].strip.start_with?("|")

      values = parse_table_row(block[cursor], path, scenario_line + cursor)
      unless values.length == header.length
        raise "Examples width mismatch at #{path}:#{scenario_line + cursor}"
      end
      row_number += 1
      rows << ["examples-#{set_number}-row-#{row_number}",
               scenario_line + cursor,
               header.zip(values).to_h]
      cursor += 1
    end
    raise "empty Examples table at #{path}:#{scenario_line + index}" if rows.empty?

    sets.concat(rows)
    index = cursor
  end
  sets
end

def substitute(template, values)
  values.reduce(template.dup) do |result, (name, value)|
    result.gsub("<#{name}>", value)
  end
end

def expected_outcome(block)
  block.each do |line|
    match = line.strip.match(
      /\AThen an? ([A-Za-z]+Error) should be raised at (compile time|runtime)(?::\s*(\S+))?/
    )
    next unless match

    return [match[2] == "compile time" ? "compile-error" : "runtime-error",
            match[2], match[1], match[3]]
  end
  ["expected-success", nil, nil, nil]
end

def source_digest(source)
  "sha256:#{Digest::SHA256.hexdigest(source)}"
end

def scheme(value)
  case value
  when String then JSON.generate(value)
  when Integer then value.to_s
  when true then "#t"
  when false, nil then "#f"
  when Array then "(#{value.map { |item| scheme(item) }.join(' ')})"
  else raise "cannot encode Scheme datum: #{value.inspect}"
  end
end

checkout, output = ARGV
fail_usage unless checkout && output && ARGV.length == 2

checkout = File.expand_path(checkout)
features_root = File.join(checkout, "tck", "features")
raise "openCypher TCK features are unavailable: #{features_root}" unless Dir.exist?(features_root)

head = git_head(checkout)
raise "openCypher checkout drift: expected #{EXPECTED_COMMIT}, got #{head}" unless head == EXPECTED_COMMIT

feature_paths = Dir.glob(File.join(features_root, "**", "*.feature")).sort
tree_digest = Digest::SHA256.new
feature_paths.each do |path|
  relative = path.delete_prefix("#{features_root}/")
  tree_digest.update(relative)
  tree_digest.update("\0")
  tree_digest.update(File.binread(path))
  tree_digest.update("\0")
end

records = []
feature_paths.each do |path|
  relative = path.delete_prefix("#{features_root}/")
  lines = File.readlines(path, encoding: "UTF-8")
  starts = lines.each_index.select { |index| scenario_start?(lines[index]) }
  pending_tags = []
  scenario_tags = {}
  lines.each_with_index do |line, index|
    stripped = line.strip
    if stripped.start_with?("@")
      pending_tags.concat(stripped.split)
    elsif scenario_start?(line)
      scenario_tags[index] = pending_tags.dup
      pending_tags.clear
    elsif !stripped.empty? && !stripped.start_with?("#")
      pending_tags.clear unless stripped.start_with?("Feature:")
    end
  end

  starts.each_with_index do |start, ordinal|
    finish = starts.fetch(ordinal + 1, lines.length)
    block = lines[start...finish]
    header = block.fetch(0).strip
    outline = header.start_with?("Scenario Outline:")
    name = header.split(":", 2).fetch(1).strip
    query, query_line = extract_query(block, relative, start + 1)
    outcome, error_phase, error_class, error_reason = expected_outcome(block)
    skip = scenario_tags.fetch(start, []).include?("@skipGrammarCheck")
    rows = outline ? example_rows(block, relative, start + 1) : [[nil, nil, {}]]
    raise "Scenario Outline has no Examples at #{relative}:#{start + 1}" if outline && rows.empty?

    rows.each do |example_id, example_line, values|
      expanded_query = substitute(query, values)
      expanded_name = substitute(name, values)
      unresolved = expanded_query.match?(/<[A-Za-z_][A-Za-z0-9_]*>/)
      digest = source_digest(expanded_query)
      identity = [relative, start + 1, example_id || "scenario", digest].join("#")
      records << [identity, relative, start + 1, expanded_name,
                  example_id, example_line, query_line, skip, outline,
                  unresolved, outcome, error_phase, error_class, error_reason,
                  digest, expanded_query]
    end
  end
end

manifest = [
  ["schema", SCHEMA],
  ["upstreamCommit", head],
  ["featureTreeDigest", "sha256:#{tree_digest.hexdigest}"],
  ["featureFileCount", feature_paths.length],
  ["recordCount", records.length]
]

File.open(output, "w") do |file|
  file.puts "("
  manifest.each { |key, value| file.puts "  (#{key} . #{scheme(value)})" }
  file.puts "  (records"
  records.each { |record| file.puts "    #{scheme(record)}" }
  file.puts "  ))"
end

warn "schema=#{SCHEMA} commit=#{head} features=#{feature_paths.length} records=#{records.length} unresolved=#{records.count { |record| record[9] }}"
