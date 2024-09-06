#[derive(Clone, Copy)]
pub struct AluResult {
    pub value: u8,
    pub carry_out: bool,
    pub aux_carry: bool,
}

macro_rules! logic_op {
    ($name:ident, $op:tt) => {
        pub fn $name(a: u8, b: u8) -> AluResult {
            AluResult {
                value: a $op b,
                carry_out: false,
                aux_carry: false,
            }
        }
    };
}

pub struct Alu;

impl Alu {
    pub fn add(a: u8, b: u8, cin: bool) -> AluResult {
        let result = a as u16 + b as u16 + cin as u16;

        AluResult {
            value: result as u8,
            carry_out: result > 0xFF,
            aux_carry: ((a & 0x0F) + (b & 0x0F)) > 0x0F,
        }
    }

    pub fn sub(a: u8, b: u8, cin: bool) -> AluResult {
        let result = a.wrapping_sub(b).wrapping_sub(cin as u8);

        AluResult {
            value: result,
            carry_out: (a as u16) < (b as u16 + cin as u16),
            aux_carry: (a & 0x0F) < ((b & 0x0F) + cin as u8),
        }
    }

    pub fn add16(a: u16, b: u16, cin: bool) -> (u16, bool) {
        let result = a as u32 + b as u32 + cin as u32;

        (result as u16, result > 0xFFFF)
    }

    pub fn sub16(a: u16, b: u16, cin: bool) -> (u16, bool) {
        let result = a.wrapping_sub(b).wrapping_sub(cin as u16);

        (result, (a as u32) < (b as u32 + cin as u32))
    }

    logic_op!(and, &);
    logic_op!(or, |);
    logic_op!(xor, ^);
}
