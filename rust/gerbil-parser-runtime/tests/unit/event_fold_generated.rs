pub const PARSER_DIGEST: &str = "sha256:4c5ec7bcc8c90c1c84c165b05e500c7fc2b4fef38063cabf479dc348069cf4fd";
pub fn parse_fold_lines(source: &str) -> Vec<TreeEvent> {
    #[inline(always)]
    fn __event_push_token(
        events: &mut Vec<TreeEvent>,
        kind: u16,
        start: usize,
        end: usize,
    ) {
        if start != end {
            events
                .push(TreeEvent::Token {
                    kind,
                    start,
                    end,
                });
        }
    }
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
            __event_push_token(&mut events, 3u16, start, end);
            events.push(TreeEvent::FinishNode);
        } else {
            if !(paragraph_open) {
                events.push(TreeEvent::StartNode(2u16));
                paragraph_open = true;
            }
            __event_push_token(&mut events, 3u16, start, end);
        }
        start = end;
    }
    if paragraph_open {
        events.push(TreeEvent::FinishNode);
    }
    events.push(TreeEvent::FinishNode);
    events
}
