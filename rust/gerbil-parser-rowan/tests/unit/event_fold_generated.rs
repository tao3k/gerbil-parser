pub const PARSER_DIGEST: &str = "sha256:14dc22249e90e4d6af215b04042dd9fe778d4ad4df5943f95bf33ce149783f51";
pub fn parse_fold_lines(source: &str) -> Vec<TreeEvent> {
    let bytes = source.as_bytes();
    let mut events = Vec::with_capacity(bytes.len() / 16 + 2);
    events.push(TreeEvent::StartNode(0u16));
    let mut paragraph_open = false;
    let mut start = 0usize;
    while start < bytes.len() {
        let mut end = start;
        while end < bytes.len() && bytes[end] != b'\n' && bytes[end] != b'\r' {
            end += 1;
        }
        if end < bytes.len() {
            if bytes[end] == b'\r' && bytes.get(end + 1) == Some(&b'\n') {
                end += 2;
            } else {
                end += 1;
            }
        }
        let line = &source[start..end];
        if line.starts_with("* ") {
            if paragraph_open {
                events.push(TreeEvent::FinishNode);
                paragraph_open = false;
            }
            events.push(TreeEvent::StartNode(1u16));
            events
                .push(TreeEvent::Token {
                    kind: 3u16,
                    start,
                    end,
                });
            events.push(TreeEvent::FinishNode);
        } else {
            if !(paragraph_open) {
                events.push(TreeEvent::StartNode(2u16));
                paragraph_open = true;
            }
            events
                .push(TreeEvent::Token {
                    kind: 3u16,
                    start,
                    end,
                });
        }
        start = end;
    }
    if paragraph_open {
        events.push(TreeEvent::FinishNode);
    }
    events.push(TreeEvent::FinishNode);
    events
}
