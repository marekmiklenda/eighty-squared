pub mod alu;
pub mod instruction;
pub mod io;
pub mod memory;
pub mod processor;
pub mod register;

pub type Bit = bool;
pub type PortAddress = u8;

pub type EightySquaredResult<T> = Result<T, EightySquaredError>;
#[derive(Debug)]
pub enum EightySquaredError {
    IoError(std::io::Error),
    InvalidOpcode(u8),
    InvalidDestSrcAddressing(u8),
    ArgumentNotSpecified,
    InvalidRegPair(u8),
    InvalidCcc(u8),
    Custom(Box<dyn std::error::Error + Send + Sync + 'static>),
}

impl std::error::Error for EightySquaredError {}

impl From<std::io::Error> for EightySquaredError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

impl From<Box<dyn std::error::Error + Send + Sync + 'static>> for EightySquaredError {
    fn from(value: Box<dyn std::error::Error + Send + Sync + 'static>) -> Self {
        Self::Custom(value)
    }
}

impl std::fmt::Display for EightySquaredError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Display;

        match self {
            Self::IoError(e) => Display::fmt(e, f),
            Self::InvalidOpcode(o) => write!(f, "Invalid opcode {:X}", o),
            Self::InvalidDestSrcAddressing(a) => write!(f, "Invalid DestSrc addressing: {}", a),
            Self::ArgumentNotSpecified => {
                write!(f, "An instruction did not receive a required argument.")
            }
            Self::InvalidRegPair(a) => write!(f, "Invalid register pair: {}", a),
            Self::InvalidCcc(c) => write!(f, "Invalid ccc: {}", c),
            Self::Custom(v) => Display::fmt(v, f),
        }
    }
}
