//! Inert reader for the bounded `(object ...)` / `(list ...)` native metadata wire.
use crate::{NativeArtifactError, wire};
use serde_json::{Map, Value};
fn invalid() -> NativeArtifactError {
    wire::error("descriptor-datum", None)
}
pub(crate) fn read(bytes: &[u8]) -> Result<Value, NativeArtifactError> {
    let text = std::str::from_utf8(bytes).map_err(|_| invalid())?;
    let mut reader = Reader { text, at: 0 };
    let value = reader.value(0)?;
    reader.space();
    if reader.at != text.len() {
        return Err(invalid());
    }
    Ok(value)
}
struct Reader<'a> {
    text: &'a str,
    at: usize,
}
impl Reader<'_> {
    fn peek(&self) -> Option<char> {
        self.text[self.at..].chars().next()
    }
    fn take(&mut self) -> Result<char, NativeArtifactError> {
        let ch = self.peek().ok_or_else(invalid)?;
        self.at += ch.len_utf8();
        Ok(ch)
    }
    fn space(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.take().expect("peeked character");
        }
    }
    fn expect(&mut self, ch: char) -> Result<(), NativeArtifactError> {
        self.space();
        if self.take()? != ch {
            return Err(invalid());
        }
        Ok(())
    }
    fn atom(&mut self) -> &str {
        let start = self.at;
        while self
            .peek()
            .is_some_and(|ch| !ch.is_whitespace() && ch != '(' && ch != ')')
        {
            self.take().expect("peeked character");
        }
        &self.text[start..self.at]
    }
    fn string(&mut self) -> Result<String, NativeArtifactError> {
        self.expect('"')?;
        let mut value = String::new();
        loop {
            match self.take()? {
                '"' => return Ok(value),
                '\\' => value.push(self.escape()?),
                ch => value.push(ch),
            }
        }
    }
    fn escape(&mut self) -> Result<char, NativeArtifactError> {
        match self.take()? {
            '"' => Ok('"'),
            '\\' => Ok('\\'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            'a' => Ok('\u{7}'),
            'b' => Ok('\u{8}'),
            'v' => Ok('\u{b}'),
            'f' => Ok('\u{c}'),
            'x' => self.hex_character(),
            _ => Err(invalid()),
        }
    }
    fn hex_character(&mut self) -> Result<char, NativeArtifactError> {
        let start = self.at;
        while self.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
            self.take()?;
        }
        let hex = &self.text[start..self.at];
        if hex.is_empty() || hex.len() > 6 || self.take()? != ';' {
            return Err(invalid());
        }
        char::from_u32(u32::from_str_radix(hex, 16).map_err(|_| invalid())?).ok_or_else(invalid)
    }
    fn value(&mut self, depth: usize) -> Result<Value, NativeArtifactError> {
        if depth > 64 {
            return Err(invalid());
        }
        self.space();
        match self.peek().ok_or_else(invalid)? {
            '"' => Ok(Value::String(self.string()?)),
            '(' => {
                self.take()?;
                self.space();
                match self.atom() {
                    "list" => {
                        let mut values = Vec::new();
                        loop {
                            self.space();
                            if self.peek() == Some(')') {
                                self.take()?;
                                break;
                            }
                            values.push(self.value(depth + 1)?);
                        }
                        Ok(Value::Array(values))
                    }
                    "object" => {
                        let mut values = Map::new();
                        loop {
                            self.space();
                            if self.peek() == Some(')') {
                                self.take()?;
                                break;
                            }
                            self.expect('(')?;
                            self.space();
                            let key = self.string()?;
                            let value = self.value(depth + 1)?;
                            self.expect(')')?;
                            if values.insert(key, value).is_some() {
                                return Err(invalid());
                            }
                        }
                        Ok(Value::Object(values))
                    }
                    _ => Err(invalid()),
                }
            }
            _ => match self.atom() {
                "#t" => Ok(Value::Bool(true)),
                "#f" => Ok(Value::Bool(false)),
                number => Ok(Value::Number(number.parse().map_err(|_| invalid())?)),
            },
        }
    }
}

#[cfg(test)]
#[path = "../tests/unit/datum.rs"]
mod tests;
