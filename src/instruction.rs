use crate::{
    alu::{Alu, AluResult},
    io::ProcessorIo,
    memory::Address,
    processor::{ProcessorHardware, ProcessorInner, ProcessorState},
    register::{ExtendedRegisterAddressing, FlagRegister, GeneralPurposeAddressing, Writable},
    EightySquaredError, EightySquaredResult, PortAddress,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DestSrcAddressing {
    Register(GeneralPurposeAddressing),
    Memory,
}

impl TryFrom<u8> for DestSrcAddressing {
    type Error = EightySquaredError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let v = match value {
            0b111 => Self::Register(GeneralPurposeAddressing::A),
            0b000 => Self::Register(GeneralPurposeAddressing::B),
            0b001 => Self::Register(GeneralPurposeAddressing::C),
            0b010 => Self::Register(GeneralPurposeAddressing::D),
            0b011 => Self::Register(GeneralPurposeAddressing::E),
            0b100 => Self::Register(GeneralPurposeAddressing::H),
            0b101 => Self::Register(GeneralPurposeAddressing::L),
            0b110 => Self::Memory,
            _ => return Err(EightySquaredError::InvalidDestSrcAddressing(value)),
        };

        Ok(v)
    }
}

impl From<DestSrcAddressing> for u8 {
    fn from(value: DestSrcAddressing) -> Self {
        match value {
            DestSrcAddressing::Register(r) => r as u8,
            DestSrcAddressing::Memory => 0b110,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum ConditionCode {
    /// 'Z'ero flag not set
    NZ = 0b000,
    /// 'Z'ero flag set
    Z = 0b001,
    /// 'C'arry flag not set
    NC = 0b010,
    /// 'C'arry flag set
    C = 0b011,
    /// 'P'arity flag not set - ODD
    PO = 0b100,
    /// 'P'arity flag set - EVEN
    PE = 0b101,
    /// 'S'ign flag not set - POSITIVE
    P = 0b110,
    /// 'S'ign flag set - MINUS
    M = 0b111,
}

impl ConditionCode {
    pub fn evaluate(&self, flags: &FlagRegister) -> bool {
        match self {
            Self::C => flags.get_carry_bit(),
            Self::NC => !flags.get_carry_bit(),
            Self::Z => flags.get_zero_bit(),
            Self::NZ => !flags.get_zero_bit(),
            Self::M => flags.get_sign_bit(),
            Self::P => !flags.get_sign_bit(),
            Self::PE => flags.get_parity_bit(),
            Self::PO => !flags.get_parity_bit(),
        }
    }
}

impl TryFrom<u8> for ConditionCode {
    type Error = EightySquaredError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let v = match value {
            0b000 => Self::NZ,
            0b001 => Self::Z,
            0b010 => Self::NC,
            0b011 => Self::C,
            0b100 => Self::PO,
            0b101 => Self::PE,
            0b110 => Self::P,
            0b111 => Self::M,
            _ => return Err(EightySquaredError::InvalidCcc(value)),
        };

        Ok(v)
    }
}

impl From<ConditionCode> for u8 {
    fn from(value: ConditionCode) -> Self {
        value as u8
    }
}

#[derive(Clone, Copy, Debug)]
pub enum InstructionPrimitive {
    ///Move register to register
    MOV,
    ///Move immediate to register
    MVI,
    ///Load register pair immediate
    LXI,
    ///Load A from memory
    LDA,
    ///Store A to memory
    STA,
    ///Load H:L from memory
    LHLD,
    ///Store H:L to memory
    SHLD,
    ///Load indirect through BC or DE
    LDAX,
    ///Store indirect through BC or DE
    STAX,
    ///Exchange DE and HL content
    XCHG,
    ///Add register to A
    ADD,
    ///Add immediate to A
    ADI,
    ///Add register to A with carry
    ADC,
    ///Add immediate to A with carry
    ACI,
    ///Subtract register from A
    SUB,
    ///Subtract immediate from A
    SUI,
    ///Subtract register from A with borrow
    SBB,
    ///Subtract immediate from A with borrow
    SBI,
    ///Increment register
    INR,
    ///Decrement register
    DCR,
    ///Increment register pair
    INX,
    ///Decrement register pair
    DCX,
    ///Add register pair to HL
    DAD,
    ///Decimal Adjust accumulator
    DAA,
    ///AND register with A
    ANA,
    ///AND immediate with A
    ANI,
    ///OR  register with A
    ORA,
    ///OR  immediate with A
    ORI,
    ///ExclusiveOR register with A
    XRA,
    ///ExclusiveOR immediate with A
    XRI,
    ///Compare register with A
    CMP,
    ///Compare immediate with A
    CPI,
    ///Rotate A left
    RLC,
    ///Rotate A right
    RRC,
    ///Rotate A left through carry
    RAL,
    ///Rotate A right through carry
    RAR,
    ///Compliment A
    CMA,
    ///Compliment Carry flag
    CMC,
    ///Set Carry flag
    STC,
    ///Unconditional jump
    JMP,
    ///Conditional jump
    Jccc,
    ///Unconditional subroutine call
    CALL,
    ///Conditional subroutine call
    Cccc,
    ///Unconditional return from subroutine
    RET,
    ///Conditional return from subroutine
    Rccc,
    ///Restart
    RST,
    ///Jump to address in H:L
    PCHL,
    ///Push register pair on the stack
    PUSH,
    ///Pop  register pair from the stack
    POP,
    ///Swap H:L with top word on stack
    XTHL,
    ///Set SP to content of H:L
    SPHL,
    ///Read input port into A
    IN,
    ///Write A to output port
    OUT,
    ///Enable interrupts
    EI,
    ///Disable interrupts
    DI,
    ///Halt processor
    HLT,
    ///No operation
    NOP,
}

impl InstructionPrimitive {
    /// Returns the number of bytes the whole instruction occupies (incl. opcode)
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u8 {
        match self {
            Self::MOV => 1,
            Self::MVI => 2,
            Self::LXI => 3,
            Self::LDA => 3,
            Self::STA => 3,
            Self::LHLD => 3,
            Self::SHLD => 3,
            Self::LDAX => 1,
            Self::STAX => 1,
            Self::XCHG => 1,
            Self::ADD => 1,
            Self::ADI => 2,
            Self::ADC => 1,
            Self::ACI => 2,
            Self::SUB => 1,
            Self::SUI => 2,
            Self::SBB => 1,
            Self::SBI => 2,
            Self::INR => 1,
            Self::DCR => 1,
            Self::INX => 1,
            Self::DCX => 1,
            Self::DAD => 1,
            Self::DAA => 1,
            Self::ANA => 1,
            Self::ANI => 2,
            Self::ORA => 1,
            Self::ORI => 1,
            Self::XRA => 1,
            Self::XRI => 2,
            Self::CMP => 1,
            Self::CPI => 2,
            Self::RLC => 1,
            Self::RRC => 1,
            Self::RAL => 1,
            Self::RAR => 1,
            Self::CMA => 1,
            Self::CMC => 1,
            Self::STC => 1,
            Self::JMP => 3,
            Self::Jccc => 3,
            Self::CALL => 3,
            Self::Cccc => 3,
            Self::RET => 1,
            Self::Rccc => 1,
            Self::RST => 1,
            Self::PCHL => 1,
            Self::PUSH => 1,
            Self::POP => 1,
            Self::XTHL => 1,
            Self::SPHL => 1,
            Self::IN => 2,
            Self::OUT => 2,
            Self::EI => 1,
            Self::DI => 1,
            Self::HLT => 1,
            Self::NOP => 1,
        }
    }
}

impl TryFrom<u8> for InstructionPrimitive {
    type Error = EightySquaredError;
    fn try_from(opcode: u8) -> Result<Self, Self::Error> {
        let v = match opcode {
            // HLT needs to be before MOV for correct parsing
            0b01110110 => Self::HLT,
            v if v & 0b11000000 == 0b01000000 => Self::MOV,
            v if v & 0b11000111 == 0b00000110 => Self::MVI,
            v if v & 0b11001111 == 0b00000001 => Self::LXI,
            0b00111010 => Self::LDA,
            0b00110010 => Self::STA,
            0b00101010 => Self::LHLD,
            0b00100010 => Self::SHLD,
            v if v & 0b11001111 == 0b00001010 => Self::LDAX,
            v if v & 0b11001111 == 0b00000010 => Self::STAX,
            0b11101011 => Self::XCHG,
            v if v & 0b11111000 == 0b10000000 => Self::ADD,
            0b11000110 => Self::ADI,
            v if v & 0b11111000 == 0b10001000 => Self::ADC,
            0b11001110 => Self::ACI,
            v if v & 0b11111000 == 0b10010000 => Self::SUB,
            0b11010110 => Self::SUI,
            v if v & 0b11111000 == 0b10011000 => Self::SBB,
            0b11011110 => Self::SBI,
            v if v & 0b11000111 == 0b00000100 => Self::INR,
            v if v & 0b11000111 == 0b00000101 => Self::DCR,
            v if v & 0b11001111 == 0b00000011 => Self::INX,
            v if v & 0b11001111 == 0b00001011 => Self::DCX,
            v if v & 0b11001111 == 0b00001001 => Self::DAD,
            0b00100111 => Self::DAA,
            v if v & 0b11111000 == 0b10100000 => Self::ANA,
            0b11100110 => Self::ANI,
            v if v & 0b11111000 == 0b10110000 => Self::ORA,
            0b11110110 => Self::ORI,
            v if v & 0b11111000 == 0b10101000 => Self::XRA,
            0b11101110 => Self::XRI,
            v if v & 0b11111000 == 0b10111000 => Self::CMP,
            0b11111110 => Self::CPI,
            0b00000111 => Self::RLC,
            0b00001111 => Self::RRC,
            0b00010111 => Self::RAL,
            0b00011111 => Self::RAR,
            0b00101111 => Self::CMA,
            0b00111111 => Self::CMC,
            0b00110111 => Self::STC,
            0b11000011 => Self::JMP,
            v if v & 0b11000111 == 0b11000010 => Self::Jccc,
            0b11001101 => Self::CALL,
            v if v & 0b11000111 == 0b11000100 => Self::Cccc,
            0b11001001 => Self::RET,
            v if v & 0b11000111 == 0b11000000 => Self::Rccc,
            v if v & 0b11000111 == 0b11000111 => Self::RST,
            0b11101001 => Self::PCHL,
            v if v & 0b11001111 == 0b11000101 => Self::PUSH,
            v if v & 0b11001111 == 0b11000001 => Self::POP,
            0b11100011 => Self::XTHL,
            0b11111001 => Self::SPHL,
            0b11011011 => Self::IN,
            0b11010011 => Self::OUT,
            0b11111011 => Self::EI,
            0b11110011 => Self::DI,
            0b00000000 => Self::NOP,

            _ => return Err(EightySquaredError::InvalidOpcode(opcode)),
        };

        Ok(v)
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    ///Move register to register
    MOV(DestSrcAddressing, DestSrcAddressing),
    ///Move immediate to register
    MVI(DestSrcAddressing, u8),
    ///Load register pair immediate
    LXI(ExtendedRegisterAddressing, u16),
    ///Load A from memory
    LDA(Address),
    ///Store A to memory
    STA(Address),
    ///Load H:L from memory
    LHLD(Address),
    ///Store H:L to memory
    SHLD(Address),
    ///Load indirect through BC or DE
    LDAX(ExtendedRegisterAddressing),
    ///Store indirect through BC or DE
    STAX(ExtendedRegisterAddressing),
    ///Exchange DE and HL content
    XCHG,
    ///Add register to A
    ADD(DestSrcAddressing),
    ///Add immediate to A
    ADI(u8),
    ///Add register to A with carry
    ADC(DestSrcAddressing),
    ///Add immediate to A with carry
    ACI(u8),
    ///Subtract register from A
    SUB(DestSrcAddressing),
    ///Subtract immediate from A
    SUI(u8),
    ///Subtract register from A with borrow
    SBB(DestSrcAddressing),
    ///Subtract immediate from A with borrow
    SBI(u8),
    ///Increment register
    INR(DestSrcAddressing),
    ///Decrement register
    DCR(DestSrcAddressing),
    ///Increment register pair
    INX(ExtendedRegisterAddressing),
    ///Decrement register pair
    DCX(ExtendedRegisterAddressing),
    ///Add register pair to HL (16 bit add)
    DAD(ExtendedRegisterAddressing),
    ///Decimal Adjust accumulator
    DAA,
    ///AND register with A
    ANA(DestSrcAddressing),
    ///AND immediate with A
    ANI(u8),
    ///OR  register with A
    ORA(DestSrcAddressing),
    ///OR  immediate with A
    ORI(u8),
    ///ExclusiveOR register with A
    XRA(DestSrcAddressing),
    ///ExclusiveOR immediate with A
    XRI(u8),
    ///Compare register with A
    CMP(DestSrcAddressing),
    ///Compare immediate with A
    CPI(u8),
    ///Rotate A left
    RLC,
    ///Rotate A right
    RRC,
    ///Rotate A left through carry
    RAL,
    ///Rotate A right through carry
    RAR,
    ///Compliment A
    CMA,
    ///Compliment Carry flag
    CMC,
    ///Set Carry flag
    STC,
    ///Unconditional jump
    JMP(Address),
    ///Conditional jump
    Jccc(ConditionCode, Address),
    ///Unconditional subroutine call
    CALL(Address),
    ///Conditional subroutine call
    Cccc(ConditionCode, Address),
    ///Unconditional return from subroutine
    RET,
    ///Conditional return from subroutine
    Rccc(ConditionCode),
    ///Restart (Call n*8)
    RST(u8),
    ///Jump to address in H:L
    PCHL,
    ///Push register pair on the stack
    PUSH(ExtendedRegisterAddressing),
    ///Pop  register pair from the stack
    POP(ExtendedRegisterAddressing),
    ///Swap H:L with top word on stack
    XTHL,
    ///Set SP to content of H:L
    SPHL,
    ///Read input port into A
    IN(PortAddress),
    ///Write A to output port
    OUT(PortAddress),
    ///Enable interrupts
    EI,
    ///Disable interrupts
    DI,
    ///Halt processor
    HLT,
    ///No operation
    NOP,
}

impl Instruction {
    pub fn parse(
        primitive: InstructionPrimitive,
        opcode: u8,
        a: Option<u8>,
        b: Option<u8>,
    ) -> EightySquaredResult<Self> {
        macro_rules! get_opt {
            (u8 $v:expr) => {
                match $v {
                    Some(v) => v,
                    None => return Err(EightySquaredError::ArgumentNotSpecified),
                }
            };

            (u16) => {
                match (a, b) {
                    (Some(a), Some(b)) => (b as u16) << 8 | a as u16,
                    _ => return Err(EightySquaredError::ArgumentNotSpecified),
                }
            };

            (ds $v:expr) => {
                DestSrcAddressing::try_from($v & 0b111)?
            };

            (rp $v:expr) => {
                ExtendedRegisterAddressing::try_from($v & 0b11)?
            };

            (ccc $v:expr) => {
                ConditionCode::try_from($v & 0b111)?
            };

            (nnn $v:expr) => {
                $v & 0b111
            };
        }

        macro_rules! primitive {
            ($tgt:ident) => {
                Self::$tgt
            };

            ($tgt:ident, $($a:tt $($aex:expr)?),*) => {
                Self::$tgt($(get_opt!($a $($aex)?)),*)
            };
        }

        let v = match primitive {
            // TODO: Rules for only one may be a memory address etc
            InstructionPrimitive::MOV => primitive!(MOV, ds opcode >> 3, ds opcode),
            InstructionPrimitive::MVI => primitive!(MVI, ds opcode >> 3, u8 a),
            InstructionPrimitive::LXI => primitive!(LXI, rp opcode >> 4, u16),
            InstructionPrimitive::LDA => primitive!(LDA, u16),
            InstructionPrimitive::STA => primitive!(STA, u16),
            InstructionPrimitive::LHLD => primitive!(LHLD, u16),
            InstructionPrimitive::SHLD => primitive!(SHLD, u16),
            InstructionPrimitive::LDAX => primitive!(LDAX, rp opcode >> 4),
            InstructionPrimitive::STAX => primitive!(STAX, rp opcode >> 4),
            InstructionPrimitive::XCHG => primitive!(XCHG),
            InstructionPrimitive::ADD => primitive!(ADD, ds opcode),
            InstructionPrimitive::ADI => primitive!(ADI, u8 a),
            InstructionPrimitive::ADC => primitive!(ADC, ds opcode),
            InstructionPrimitive::ACI => primitive!(ACI, u8 a),
            InstructionPrimitive::SUB => primitive!(SUB, ds opcode),
            InstructionPrimitive::SUI => primitive!(SUI, u8 a),
            InstructionPrimitive::SBB => primitive!(SBB, ds opcode),
            InstructionPrimitive::SBI => primitive!(SBI, u8 a),
            InstructionPrimitive::INR => primitive!(INR, ds opcode >> 3),
            InstructionPrimitive::DCR => primitive!(DCR, ds opcode >> 3),
            InstructionPrimitive::INX => primitive!(INX, rp opcode >> 4),
            InstructionPrimitive::DCX => primitive!(DCX, rp opcode >> 4),
            InstructionPrimitive::DAD => primitive!(DAD, rp opcode >> 4),
            InstructionPrimitive::DAA => primitive!(DAA),
            InstructionPrimitive::ANA => primitive!(ANA, ds opcode),
            InstructionPrimitive::ANI => primitive!(ANI, u8 a),
            InstructionPrimitive::ORA => primitive!(ORA, ds opcode),
            InstructionPrimitive::ORI => primitive!(ORI, u8 a),
            InstructionPrimitive::XRA => primitive!(XRA, ds opcode),
            InstructionPrimitive::XRI => primitive!(XRI, u8 a),
            InstructionPrimitive::CMP => primitive!(CMP, ds opcode),
            InstructionPrimitive::CPI => primitive!(CPI, u8 a),
            InstructionPrimitive::RLC => primitive!(RLC),
            InstructionPrimitive::RRC => primitive!(RRC),
            InstructionPrimitive::RAL => primitive!(RAL),
            InstructionPrimitive::RAR => primitive!(RAR),
            InstructionPrimitive::CMA => primitive!(CMA),
            InstructionPrimitive::CMC => primitive!(CMC),
            InstructionPrimitive::STC => primitive!(STC),
            InstructionPrimitive::JMP => primitive!(JMP, u16),
            InstructionPrimitive::Jccc => primitive!(Jccc, ccc opcode >> 3, u16),
            InstructionPrimitive::CALL => primitive!(CALL, u16),
            InstructionPrimitive::Cccc => primitive!(Cccc, ccc opcode >> 3, u16),
            InstructionPrimitive::RET => primitive!(RET),
            InstructionPrimitive::Rccc => primitive!(Rccc, ccc opcode >> 3),
            InstructionPrimitive::RST => primitive!(RST, nnn opcode >> 3),
            InstructionPrimitive::PCHL => primitive!(PCHL),
            InstructionPrimitive::PUSH => primitive!(PUSH, rp opcode >> 4),
            InstructionPrimitive::POP => primitive!(POP, rp opcode >> 4),
            InstructionPrimitive::XTHL => primitive!(XTHL),
            InstructionPrimitive::SPHL => primitive!(SPHL),
            InstructionPrimitive::IN => primitive!(IN, u8 a),
            InstructionPrimitive::OUT => primitive!(OUT, u8 a),
            InstructionPrimitive::EI => primitive!(EI),
            InstructionPrimitive::DI => primitive!(DI),
            InstructionPrimitive::HLT => primitive!(HLT),
            InstructionPrimitive::NOP => primitive!(NOP),
        };

        Ok(v)
    }

    pub fn into_bytes<W>(self, writer: &mut W) -> EightySquaredResult<()>
    where
        W: std::io::Write,
    {
        use byteorder::{LittleEndian, WriteBytesExt};

        macro_rules! ds {
            ($v:expr $(, $shl:literal)?) => {
                u8::from($v) $(<< $shl)?
            };
        }

        macro_rules! write_inner {
            (u8 $v:expr) => {
                writer.write_u8($v)?
            };

            (u16 $v:expr) => {
                writer.write_u16::<LittleEndian>($v)?
            };
        }

        macro_rules! write {
            ($($designator:tt $value:expr);+) => {{
                $(
                    write_inner!($designator $value)
                );+
            }};
        }

        match self {
            Self::MOV(a, b) => write!(u8 0b01000000 | ds!(a, 3) | ds!(b)),
            Self::MVI(a, b) => write!(u8 0b00000110 | ds!(a, 3); u8 b),
            Self::LXI(a, b) => write!(u8 0b00000001 | ds!(a, 4); u16 b),
            Self::LDA(a) => write!(u8 0b00111010; u16 a),
            Self::STA(a) => write!(u8 0b00110010; u16 a),
            Self::LHLD(a) => write!(u8 0b00101010; u16 a), // LHLD a    00101010 lb hb
            Self::SHLD(a) => write!(u8 0b00100010; u16 a), // SHLD a    00100010 lb hb
            Self::LDAX(a) => write!(u8 0b00001010 | ds!(a, 4)), // LDAX RP   00RP1010 *1
            Self::STAX(a) => write!(u8 0b00000010 | ds!(a, 4)), // STAX RP   00RP0010 *1
            Self::XCHG => write!(u8 0b11101011),           // XCHG      11101011
            Self::ADD(a) => write!(u8 0b10000000 | ds!(a)), // ADD S     10000SSS
            Self::ADI(a) => write!(u8 0b11000110; u8 a),   // ADI #     11000110 db
            Self::ADC(a) => write!(u8 0b10001000 | ds!(a)), // ADC S     10001SSS
            Self::ACI(a) => write!(u8 0b11001110; u8 a),   // ACI #     11001110 db
            Self::SUB(a) => write!(u8 0b10010000 | ds!(a)), // SUB S     10010SSS
            Self::SUI(a) => write!(u8 0b11010110; u8 a),   // SUI #     11010110 db
            Self::SBB(a) => write!(u8 0b10011000 | ds!(a)), // SBB S     10011SSS
            Self::SBI(a) => write!(u8 0b11011110; u8 a),   // SBI #     11011110 db
            Self::INR(a) => write!(u8 0b00000100 | ds!(a, 3)), // INR D     00DDD100
            Self::DCR(a) => write!(u8 0b00000101 | ds!(a, 3)), // DCR D     00DDD101
            Self::INX(a) => write!(u8 0b00000011 | ds!(a, 4)), // INX RP    00RP0011
            Self::DCX(a) => write!(u8 0b00001011 | ds!(a, 4)), // DCX RP    00RP1011
            Self::DAD(a) => write!(u8 0b00001001 | ds!(a, 4)), // DAD RP    00RP1001
            Self::DAA => write!(u8 0b00100111),            // DAA       00100111
            Self::ANA(a) => write!(u8 0b10100000 | ds!(a)), // ANA S     10100SSS
            Self::ANI(a) => write!(u8 0b11100110; u8 a),   // ANI #     11100110 db
            Self::ORA(a) => write!(u8 0b10110000 | ds!(a)), // ORA S     10110SSS
            Self::ORI(a) => write!(u8 0b11110110; u8 a),   // ORI #     11110110
            Self::XRA(a) => write!(u8 0b10101000 | ds!(a)), // XRA S     10101SSS
            Self::XRI(a) => write!(u8 0b11101110; u8 a),   // XRI #     11101110 db
            Self::CMP(a) => write!(u8 0b10111000 | ds!(a)), // CMP S     10111SSS
            Self::CPI(a) => write!(u8 0b11111110; u8 a),   // CPI #     11111110
            Self::RLC => write!(u8 0b00000111),            // RLC       00000111
            Self::RRC => write!(u8 0b00001111),            // RRC       00001111
            Self::RAL => write!(u8 0b00010111),            // RAL       00010111
            Self::RAR => write!(u8 0b00011111),            // RAR       00011111
            Self::CMA => write!(u8 0b00101111),            // CMA       00101111
            Self::CMC => write!(u8 0b00111111),            // CMC       00111111
            Self::STC => write!(u8 0b00110111),            // STC       00110111
            Self::JMP(a) => write!(u8 0b11000011; u16 a),  // JMP a     11000011 lb hb
            Self::Jccc(a, b) => write!(u8 0b11000010 | ds!(a, 3); u16 b), // Jccc a    11CCC010 lb hb
            Self::CALL(a) => write!(u8 0b11001101; u16 a), // CALL a    11001101 lb hb
            Self::Cccc(a, b) => write!(u8 0b11000100 | ds!(a, 3); u16 b), // Cccc a    11CCC100 lb hb
            Self::RET => write!(u8 0b11001001),                           // RET       11001001
            Self::Rccc(a) => write!(u8 0b11000000 | ds!(a, 3)),           // Rccc      11CCC000
            Self::RST(a) => write!(u8 0b11000111 | ds!(a, 3)),            // RST n     11NNN111
            Self::PCHL => write!(u8 0b11101001),                          // PCHL      11101001
            Self::PUSH(a) => write!(u8 0b11000101 | ds!(a, 4)),           // PUSH RP   11RP0101 *2
            Self::POP(a) => write!(u8 0b11000001 | ds!(a, 4)),            // POP RP    11RP0001 *2
            Self::XTHL => write!(u8 0b11100011),                          // XTHL      11100011
            Self::SPHL => write!(u8 0b11111001),                          // SPHL      11111001
            Self::IN(a) => write!(u8 0b11011011; u8 a),                   // IN p      11011011 pa
            Self::OUT(a) => write!(u8 0b11010011; u8 a),                  // OUT p     11010011 pa
            Self::EI => write!(u8 0b11111011),                            // EI        11111011
            Self::DI => write!(u8 0b11110011),                            // DI        11110011
            Self::HLT => write!(u8 0b01110110),                           // HLT       01110110
            Self::NOP => write!(u8 0b00000000),                           // NOP       00000000
        };

        Ok(())
    }

    pub fn execute<I>(self, processor: &mut ProcessorInner<I>) -> EightySquaredResult<()>
    where
        I: ProcessorIo,
    {
        let ProcessorInner { hardware, control } = processor;

        macro_rules! ds {
            (dst $dst:expr) => {
                match $dst {
                    DestSrcAddressing::Memory => {
                        Writable::from((&mut hardware.memory[hardware.registers.pairs.hl.get()]))
                    }
                    DestSrcAddressing::Register(reg) => {
                        Writable::from((&mut hardware.registers[reg]))
                    }
                }
            };

            (src $src:expr) => {
                match $src {
                    DestSrcAddressing::Memory => hardware.memory[hardware.registers.pairs.hl.get()],
                    DestSrcAddressing::Register(reg) => hardware.registers[reg].get(),
                }
            };
        }

        macro_rules! arith_op {
            ($op:ident, $value:expr, $cin:literal) => {{
                let result = Alu::$op(hardware.registers.a.get(), $value, $cin);

                hardware.registers.a.set(result.value);
                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_aux_carry_bit(result.aux_carry);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }};
        }

        macro_rules! logic_op {
            ($op:ident, $value:expr) => {{
                let result = Alu::$op(hardware.registers.a.get(), $value);

                hardware.registers.a.set(result.value);
                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }};
        }

        fn push(hardware: &mut ProcessorHardware, data: u16) {
            let sp = hardware.registers.sp.get();
            hardware.registers.sp.set(sp.wrapping_sub(2));
            hardware.memory.set_u16(sp.wrapping_sub(1), data);
        }

        fn pop(hardware: &mut ProcessorHardware) -> u16 {
            let sp = hardware.registers.sp.get();
            hardware.registers.sp.set(sp.wrapping_add(2));
            hardware.memory.get_u16(sp.wrapping_add(1))
        }

        match self {
            Self::MOV(a, b) => {
                let src = ds!(src b);
                let mut dst = ds!(dst a);

                dst.write(src);
            }
            Self::MVI(a, b) => {
                let mut dst = ds!(dst a);

                dst.write(b);
            }
            Self::LXI(a, b) => {
                hardware.registers[a].set(b);
            }
            Self::LDA(a) => {
                hardware.registers.a.set(hardware.memory[a]);
            }
            Self::STA(a) => {
                hardware.memory[a] = hardware.registers.a.get();
            }
            Self::LHLD(a) => {
                hardware.registers.pairs.hl.set(hardware.memory.get_u16(a));
            }
            Self::SHLD(a) => {
                hardware
                    .memory
                    .set_u16(a, hardware.registers.pairs.hl.get());
            }
            Self::LDAX(a) => hardware
                .registers
                .a
                .set(hardware.memory[hardware.registers[a].get()]),
            Self::STAX(a) => {
                hardware.memory[hardware.registers[a].get()] = hardware.registers.a.get();
            }
            Self::XCHG => {
                let tmp = hardware.registers.pairs.hl.get();
                hardware
                    .registers
                    .pairs
                    .hl
                    .set(hardware.registers.pairs.de.get());
                hardware.registers.pairs.de.set(tmp);
            }
            Self::ADD(a) => {
                let src = ds!(src a);
                arith_op!(add, src, false);
            }
            Self::ADI(a) => {
                arith_op!(add, a, false);
            }
            Self::ADC(a) => {
                let src = ds!(src a);
                arith_op!(add, src, true);
            }
            Self::ACI(a) => {
                arith_op!(add, a, true);
            }
            Self::SUB(a) => {
                let src = ds!(src a);
                arith_op!(sub, src, false);
            }
            Self::SUI(a) => {
                arith_op!(sub, a, false);
            }
            Self::SBB(a) => {
                let src = ds!(src a);
                arith_op!(sub, src, true);
            }
            Self::SBI(a) => {
                arith_op!(sub, a, true);
            }
            Self::INR(a) => {
                let src = ds!(src a);
                let mut dst = ds!(dst a);

                let result = Alu::add(src, 1, false);
                dst.write(result.value);

                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_aux_carry_bit(result.aux_carry);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }
            Self::DCR(a) => {
                let src = ds!(src a);
                let mut dst = ds!(dst a);

                let result = Alu::sub(src, 1, false);
                dst.write(result.value);

                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_aux_carry_bit(result.aux_carry);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }
            Self::INX(a) => {
                let src = hardware.registers[a].get();

                // We can increment directly, because this operation does not manipulate flags
                hardware.registers[a].set(src.wrapping_add(1));
            }
            Self::DCX(a) => {
                let src = hardware.registers[a].get();

                // We can decrement directly, because this operation does not manipulate flags
                hardware.registers[a].set(src.wrapping_sub(1));
            }
            Self::DAD(a) => {
                let b = hardware.registers[a].get();
                let a = hardware.registers.pairs.hl.get();

                let (result, cout) = Alu::add16(a, b, false);

                hardware.registers.pairs.hl.set(result);
                hardware.registers.flags.set_carry_bit(cout);
            }
            Self::DAA => {
                let mut new_a = AluResult {
                    value: hardware.registers.a.get(),
                    carry_out: hardware.registers.flags.get_carry_bit(),
                    aux_carry: hardware.registers.flags.get_aux_carry_bit(),
                };

                if new_a.aux_carry || new_a.value & 0x0F > 9 {
                    let result = Alu::add(new_a.value, 6, false);

                    new_a.value = result.value;
                    new_a.aux_carry = result.aux_carry;
                }

                let msnibble = new_a.value >> 4;
                if new_a.carry_out || msnibble > 9 {
                    let result = Alu::add(msnibble, 6, false);
                    new_a.value |= result.value << 4;
                    new_a.carry_out = result.aux_carry;
                }

                hardware.registers.a.set(new_a.value);
                hardware.registers.flags.set_from_accumulator(new_a.value);
                hardware.registers.flags.set_aux_carry_bit(new_a.aux_carry);
                if new_a.carry_out {
                    hardware.registers.flags.set_carry_bit(true);
                }
            }
            Self::ANA(a) => {
                let src = ds!(src a);
                logic_op!(and, src);
            }
            Self::ANI(a) => {
                logic_op!(and, a);
            }
            Self::ORA(a) => {
                let src = ds!(src a);
                logic_op!(or, src);
            }
            Self::ORI(a) => {
                logic_op!(or, a);
            }
            Self::XRA(a) => {
                let src = ds!(src a);
                logic_op!(xor, src);
            }
            Self::XRI(a) => {
                logic_op!(xor, a);
            }
            Self::CMP(a) => {
                let src = ds!(src a);
                let result = Alu::sub(hardware.registers.a.get(), src, false);
                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_aux_carry_bit(result.aux_carry);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }
            Self::CPI(a) => {
                let result = Alu::sub(hardware.registers.a.get(), a, false);
                hardware.registers.flags.set_from_accumulator(result.value);
                hardware.registers.flags.set_aux_carry_bit(result.aux_carry);
                hardware.registers.flags.set_carry_bit(result.carry_out);
            }
            Self::RLC => {
                let value = hardware.registers.a.get();
                let msb = value >> 7;

                hardware.registers.a.set(value << 1 | msb);
                hardware.registers.flags.set_carry_bit(msb == 1);
            }
            Self::RRC => {
                let value = hardware.registers.a.get();
                let lsb = value & 1;

                hardware.registers.a.set(value >> 1 | lsb << 7);
                hardware.registers.flags.set_carry_bit(lsb == 1);
            }
            Self::RAL => {
                let value = hardware.registers.a.get();
                let msb = value >> 7 == 1;
                let carry = hardware.registers.flags.get_carry_bit() as u8;

                hardware.registers.a.set(value << 1 | carry);
                hardware.registers.flags.set_carry_bit(msb);
            }
            Self::RAR => {
                let value = hardware.registers.a.get();
                let lsb = value & 1 == 1;
                let carry = hardware.registers.flags.get_carry_bit() as u8;

                hardware.registers.a.set(value >> 1 | carry << 7);
                hardware.registers.flags.set_carry_bit(lsb);
            }
            Self::CMA => hardware.registers.a.set(0xFF - hardware.registers.a.get()),
            Self::CMC => {
                hardware
                    .registers
                    .flags
                    .set_carry_bit(!hardware.registers.flags.get_carry_bit());
            }
            Self::STC => {
                hardware.registers.flags.set_carry_bit(true);
            }
            Self::JMP(a) => {
                hardware.registers.pc.set(a);
            }
            Self::Jccc(a, b) => {
                if a.evaluate(&hardware.registers.flags) {
                    hardware.registers.pc.set(b);
                }
            }
            Self::CALL(a) => {
                push(hardware, hardware.registers.pc.get());
                hardware.registers.pc.set(a);
            }
            Self::Cccc(a, b) => {
                if a.evaluate(&hardware.registers.flags) {
                    push(hardware, hardware.registers.pc.get());
                    hardware.registers.pc.set(b);
                }
            }
            Self::RET => {
                let addr = pop(hardware);
                hardware.registers.pc.set(addr);
            }
            Self::Rccc(a) => {
                if a.evaluate(&hardware.registers.flags) {
                    let addr = pop(hardware);
                    hardware.registers.pc.set(addr);
                }
            }
            Self::RST(a) => {
                push(hardware, hardware.registers.pc.get());
                hardware.registers.pc.set((a << 3) as u16);
            }
            Self::PCHL => {
                hardware.registers.pc.set(hardware.registers.pairs.hl.get());
            }
            Self::PUSH(a) => {
                push(hardware, hardware.registers[a.as_stack_op()].get());
            }
            Self::POP(a) => {
                let v = pop(hardware);
                hardware.registers[a.as_stack_op()].set(v);
            }
            Self::XTHL => {
                let sp = hardware.registers.sp.get();
                let in_mem = hardware.memory.get_u16(sp.wrapping_add(1));
                hardware
                    .memory
                    .set_u16(sp, hardware.registers.pairs.hl.get());
                hardware.registers.pairs.hl.set(in_mem);
            }
            Self::SPHL => {
                hardware.registers.sp.set(hardware.registers.pairs.hl.get());
            }
            Self::IN(a) => {
                hardware
                    .registers
                    .a
                    .set(control.io.input(hardware, &control.control_panel, a)?);
            }
            Self::OUT(a) => {
                control.io.output(
                    hardware,
                    &control.control_panel,
                    a,
                    hardware.registers.a.get(),
                )?;
            }
            Self::EI => {
                hardware.enable_interrupts = true;
            }
            Self::DI => {
                hardware.enable_interrupts = false;
            }
            Self::HLT => {
                hardware.state = ProcessorState::Stopped;
            }
            Self::NOP => (),
        }

        Ok(())
    }
}
