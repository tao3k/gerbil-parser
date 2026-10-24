;;; -*- Gerbil -*-
;;; Stable lexical facade; pure scanning owners preserve the original forms.
(import (only-in "./scan/profile.ss" make-bounded-text-profile-scanner make-text-profile-scanner)
        (only-in "./scan/basic.ss" identifier-start? horizontal-whitespace? newline? scan-whitespace scan-horizontal-whitespace scan-newline scan-line scan-character-run scan-until-delimiters scan-identifier scan-emit)
        (only-in "./scan/number.ss" scan-decimal-digits scan-number-literal scan-number-literal/profile)
        (only-in "./scan/regular.ss" make-ranked-regular-scanner)
        (only-in "./scan/quoted.ss" scan-quoted-string scan-quoted-strings scan-escaped-quoted-strings scan-quoted-string/profile scan-balanced-word)
        (only-in "./scan/comment.ss" scan-heredoc scan-line-comment scan-block-comment scan-nested-block-comment)
        (only-in "./scan/literal.ss" make-literal-trie literal-trie-terminal literal-trie-child make-bounded-literal-end-scanner make-literal-end-scanner make-ranked-literal-scanner scan-longest-literal)
        (only-in "./scan/lexical.ss" make-ranked-lexical-scanner-factory))
(export make-literal-trie literal-trie-terminal literal-trie-child
        make-bounded-text-profile-scanner make-bounded-literal-end-scanner
        make-text-profile-scanner
        identifier-start? horizontal-whitespace? newline?
        scan-whitespace
        scan-horizontal-whitespace
        scan-newline
        scan-line
        scan-character-run
        scan-until-delimiters
        scan-decimal-digits
        scan-number-literal
        scan-number-literal/profile
        scan-identifier
        scan-quoted-string
        scan-quoted-strings
        scan-escaped-quoted-strings
        scan-quoted-string/profile
        scan-heredoc
        scan-balanced-word
        scan-line-comment
        scan-block-comment
        scan-nested-block-comment
        make-literal-end-scanner
        make-ranked-literal-scanner make-ranked-lexical-scanner-factory
        make-ranked-regular-scanner
        scan-longest-literal
        scan-emit)
