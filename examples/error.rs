#![feature(never_type)]
#![feature(exhaustive_patterns)]

#[derive(partial_enum::Enum)]
enum Error {
    Connect(ConnectError),
    Write(WriteError),
}

struct ConnectError;
struct WriteError;

struct Connection;

impl Connection {
    fn connect() -> Result<Connection, partial::Error<ConnectError, !>> {
        Ok(Self)
    }

    fn write(&mut self, msg: &[u8]) -> Result<(), partial::Error<!, WriteError>> {
        match msg {
            b"bar" => Err(partial::Error::Write(WriteError)),
            _ => Ok(()),
        }
    }
}

fn send_messages(msg: &[u8], msg2: &[u8]) -> Result<(), Error> {
    // This can only return a `ConnectError` as an error, and is transparently
    // convertible to the original error enum.
    let mut cnx = Connection::connect()?;
    // Similarily, this can only return a `WriteError`.
    cnx.write(msg)?;
    // Alternatively, we can match and handle the error manually. Note that with
    // `exhaustive_patterns` feature, the connection error does not need to be
    // handled.
    match cnx.write(msg2) {
        Ok(_) => Ok(()),
        Err(partial::Error::Write(_)) => Err(Error::Write(WriteError)),
    }
}

fn main() {
    assert!(matches!(send_messages(b"foo", b"bar"), Err(Error::Write(_))));
}
