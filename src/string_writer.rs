use std::{fmt, io, str};

pub struct StringWriter<'a>(pub &'a mut String);

impl fmt::Write for StringWriter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.push_str(s);
        Ok(())
    }
}

impl io::Write for StringWriter<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        str::from_utf8(buf)
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
            .map(|s| {
                self.0.push_str(s);
                buf.len()
            })
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
