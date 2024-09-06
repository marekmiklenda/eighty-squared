use crate::{Bit, EightySquaredError};

use std::sync::{
    atomic::{AtomicU16, AtomicU8, Ordering},
    Arc,
};

#[repr(transparent)]
#[derive(Clone, Default)]
pub struct RegisterU8(std::sync::Arc<AtomicU8>);

impl RegisterU8 {
    pub fn get(&self) -> u8 {
        self.0.load(Ordering::Relaxed)
    }

    pub fn set(&mut self, value: u8) {
        self.0.store(value, Ordering::Relaxed);
    }
}

#[derive(Clone)]
pub enum RegisterU16 {
    Single(std::sync::Arc<AtomicU16>),
    Pair(RegisterU8, RegisterU8),
}

impl RegisterU16 {
    pub fn new_single() -> Self {
        Self::Single(Arc::new(AtomicU16::new(0)))
    }

    pub fn new_pair(lower_half: RegisterU8, upper_half: RegisterU8) -> Self {
        Self::Pair(lower_half, upper_half)
    }

    pub fn get(&self) -> u16 {
        match self {
            Self::Pair(lower, upper) => (upper.get() as u16) << 8 | lower.get() as u16,
            Self::Single(a) => a.load(Ordering::Relaxed),
        }
    }

    pub fn set(&mut self, value: u16) {
        match self {
            Self::Pair(lower, upper) => {
                lower.set(value as u8);
                upper.set((value >> 8) as u8);
            }
            Self::Single(a) => a.store(value, Ordering::Relaxed),
        }
    }
}

pub enum Writable<'a> {
    Register(&'a mut RegisterU8),
    Value(&'a mut u8),
}

impl<'a> Writable<'a> {
    pub fn write(&mut self, value: u8) {
        match self {
            Self::Register(reg) => reg.set(value),
            Self::Value(v) => **v = value,
        }
    }
}

impl<'a> From<&'a mut RegisterU8> for Writable<'a> {
    fn from(value: &'a mut RegisterU8) -> Self {
        Self::Register(value)
    }
}

impl<'a> From<&'a mut u8> for Writable<'a> {
    fn from(value: &'a mut u8) -> Self {
        Self::Value(value)
    }
}

#[derive(Default, Clone)]
pub struct FlagRegister(RegisterU8);

impl FlagRegister {
    pub const CARRY_BIT: u8 = 0;
    pub const AUX_CARRY_BIT: u8 = 1;
    pub const SIGN_BIT: u8 = 2;
    pub const ZERO_BIT: u8 = 3;
    pub const PARITY_BIT: u8 = 4;

    #[inline]
    fn get_bit(&self, position: u8) -> Bit {
        self.get() >> position & 1 != 0
    }

    #[inline]
    fn set_bit(&mut self, position: u8, value: Bit) {
        let mask = 1 << position;
        let value = if value {
            self.0.get() | mask
        } else {
            self.0.get() & !mask
        };

        self.0.set(value);
    }

    pub fn get_carry_bit(&self) -> Bit {
        self.get_bit(Self::CARRY_BIT)
    }

    pub fn set_carry_bit(&mut self, value: Bit) {
        self.set_bit(Self::CARRY_BIT, value);
    }

    pub fn get_aux_carry_bit(&self) -> Bit {
        self.get_bit(Self::AUX_CARRY_BIT)
    }

    pub fn set_aux_carry_bit(&mut self, value: Bit) {
        self.set_bit(Self::AUX_CARRY_BIT, value);
    }

    pub fn get_sign_bit(&self) -> Bit {
        self.get_bit(Self::SIGN_BIT)
    }

    pub fn set_sign_bit(&mut self, value: Bit) {
        self.set_bit(Self::SIGN_BIT, value);
    }

    pub fn get_zero_bit(&self) -> Bit {
        self.get_bit(Self::ZERO_BIT)
    }

    pub fn set_zero_bit(&mut self, value: Bit) {
        self.set_bit(Self::ZERO_BIT, value);
    }

    pub fn get_parity_bit(&self) -> Bit {
        self.get_bit(Self::PARITY_BIT)
    }

    pub fn set_parity_bit(&mut self, value: Bit) {
        self.set_bit(Self::PARITY_BIT, value);
    }

    pub fn set_from_accumulator(&mut self, value: u8) {
        self.set_zero_bit(value == 0);
        self.set_sign_bit(value > 0x7F);

        let parity = (0..8u8).fold(0u8, |ones, val| ones + ((value >> val) & 1)) % 2 == 0;
        self.set_parity_bit(parity);
    }
}

impl std::fmt::Debug for FlagRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[0b{:08b} => carry= {:5}, aux_carry= {:5}, sign= {:5}, zero= {:5}, parity= {:5}]",
            self.get(),
            self.get_carry_bit(),
            self.get_aux_carry_bit(),
            self.get_sign_bit(),
            self.get_zero_bit(),
            self.get_parity_bit()
        )
    }
}

impl std::ops::Deref for FlagRegister {
    type Target = RegisterU8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for FlagRegister {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<RegisterU8> for FlagRegister {
    fn as_ref(&self) -> &RegisterU8 {
        &self.0
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GeneralPurposeAddressing {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    A = 7,
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExtendedRegisterAddressing {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

impl TryFrom<u8> for ExtendedRegisterAddressing {
    type Error = EightySquaredError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let v = match value {
            0 => Self::BC,
            1 => Self::DE,
            2 => Self::HL,
            3 => Self::SP,
            _ => return Err(EightySquaredError::InvalidRegPair(value)),
        };

        Ok(v)
    }
}

impl From<ExtendedRegisterAddressing> for u8 {
    fn from(value: ExtendedRegisterAddressing) -> Self {
        value as u8
    }
}

#[derive(Clone, Default)]
pub struct GeneralPurposeRegisters {
    pub b: RegisterU8,
    pub c: RegisterU8,
    pub d: RegisterU8,
    pub e: RegisterU8,
    pub h: RegisterU8,
    pub l: RegisterU8,
}

impl std::fmt::Debug for GeneralPurposeRegisters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[b =   {:02X}, c =   {:02X}, d =   {:02X}, e =   {:02X}, h =   {:02X}, l =   {:02X}]",
            self.b.get(),
            self.c.get(),
            self.d.get(),
            self.e.get(),
            self.h.get(),
            self.l.get(),
        )
    }
}

#[derive(Clone)]
pub struct RegisterPairs {
    pub bc: RegisterU16,
    pub de: RegisterU16,
    pub hl: RegisterU16,
}

impl From<GeneralPurposeRegisters> for RegisterPairs {
    fn from(value: GeneralPurposeRegisters) -> Self {
        Self {
            bc: RegisterU16::new_pair(value.c, value.b),
            de: RegisterU16::new_pair(value.e, value.d),
            hl: RegisterU16::new_pair(value.l, value.h),
        }
    }
}

impl std::fmt::Debug for RegisterPairs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[bc= {:04X}, de= {:04X}, hl= {:04X}]",
            self.bc.get(),
            self.de.get(),
            self.hl.get()
        )
    }
}

pub struct Registers {
    pub general_purpose: GeneralPurposeRegisters,
    pub pairs: RegisterPairs,
    /// Accumulator
    pub a: RegisterU8,
    /// Program counter
    pub pc: RegisterU16,
    /// Stack pointer
    pub sp: RegisterU16,
    /// Flag register
    pub flags: FlagRegister,
}

impl Default for Registers {
    fn default() -> Self {
        let general_purpose = GeneralPurposeRegisters::default();
        let pairs = RegisterPairs::from(general_purpose.clone());

        Self {
            a: RegisterU8::default(),
            pc: RegisterU16::new_single(),
            sp: RegisterU16::new_single(),
            flags: FlagRegister::default(),
            general_purpose,
            pairs,
        }
    }
}

impl std::ops::Index<GeneralPurposeAddressing> for Registers {
    type Output = RegisterU8;
    fn index(&self, index: GeneralPurposeAddressing) -> &Self::Output {
        match index {
            GeneralPurposeAddressing::A => &self.a,
            GeneralPurposeAddressing::B => &self.general_purpose.b,
            GeneralPurposeAddressing::C => &self.general_purpose.c,
            GeneralPurposeAddressing::D => &self.general_purpose.d,
            GeneralPurposeAddressing::E => &self.general_purpose.e,
            GeneralPurposeAddressing::H => &self.general_purpose.h,
            GeneralPurposeAddressing::L => &self.general_purpose.l,
        }
    }
}

impl std::ops::IndexMut<GeneralPurposeAddressing> for Registers {
    fn index_mut(&mut self, index: GeneralPurposeAddressing) -> &mut Self::Output {
        match index {
            GeneralPurposeAddressing::A => &mut self.a,
            GeneralPurposeAddressing::B => &mut self.general_purpose.b,
            GeneralPurposeAddressing::C => &mut self.general_purpose.c,
            GeneralPurposeAddressing::D => &mut self.general_purpose.d,
            GeneralPurposeAddressing::E => &mut self.general_purpose.e,
            GeneralPurposeAddressing::H => &mut self.general_purpose.h,
            GeneralPurposeAddressing::L => &mut self.general_purpose.l,
        }
    }
}

impl std::ops::Index<ExtendedRegisterAddressing> for Registers {
    type Output = RegisterU16;
    fn index(&self, index: ExtendedRegisterAddressing) -> &Self::Output {
        match index {
            ExtendedRegisterAddressing::BC => &self.pairs.bc,
            ExtendedRegisterAddressing::DE => &self.pairs.de,
            ExtendedRegisterAddressing::HL => &self.pairs.hl,
            ExtendedRegisterAddressing::SP => &self.sp,
        }
    }
}

impl std::ops::IndexMut<ExtendedRegisterAddressing> for Registers {
    fn index_mut(&mut self, index: ExtendedRegisterAddressing) -> &mut Self::Output {
        match index {
            ExtendedRegisterAddressing::BC => &mut self.pairs.bc,
            ExtendedRegisterAddressing::DE => &mut self.pairs.de,
            ExtendedRegisterAddressing::HL => &mut self.pairs.hl,
            ExtendedRegisterAddressing::SP => &mut self.sp,
        }
    }
}

impl std::fmt::Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            " a =   {:02X}, pc= {:04X}, sp= {:04X}\n{:?}\n{:?}\n{:?}",
            self.a.get(),
            self.pc.get(),
            self.sp.get(),
            self.general_purpose,
            self.pairs,
            self.flags
        )
    }
}
