use crate::{EightySquaredError, EightySquaredResult};

pub type Address = u16;

pub struct Memory([u8; Memory::MEMORY_SIZE]);

impl Memory {
    pub const MEMORY_SIZE: usize = (u16::MAX as usize) + 1;

    /// Loads bytes from a reader into the memory starting at start address. Excess data that cannot fit into the memory is discarded.
    pub fn load<R>(&mut self, start: Address, read: &mut R) -> EightySquaredResult<()>
    where
        R: std::io::Read,
    {
        let mut cursor = start as usize;
        loop {
            let n = read.read(&mut self.0[cursor..])?;
            cursor += n;

            if n == 0 || cursor > Address::MAX as usize {
                break;
            }
        }

        Ok(())
    }

    /// Loads bytes from a byte slice into the memory starting at start address. Excess data that cannot fit into the memory is discarded.
    pub fn load_slice(&mut self, start: Address, slice: &[u8]) -> EightySquaredResult<()> {
        let len = slice.len().min(Self::MEMORY_SIZE - start as usize);
        let end = len + start as usize;
        self.0[start as usize..end].copy_from_slice(&slice[..len]);
        Ok(())
    }

    pub fn dump<W>(&self, target: &mut W) -> EightySquaredResult<()>
    where
        W: std::io::Write,
    {
        target.write_all(&self.0).map_err(EightySquaredError::from)
    }

    pub fn set_u16(&mut self, address: Address, value: u16) {
        self[address] = value as u8;
        self[address.wrapping_add(1)] = (value >> 8) as u8;
    }

    pub fn get_u16(&self, address: Address) -> u16 {
        self[address] as u16 | ((self[address + 1] as u16) << 8)
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self([0; Self::MEMORY_SIZE])
    }
}

impl std::ops::Index<Address> for Memory {
    type Output = u8;
    fn index(&self, index: Address) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl std::ops::IndexMut<Address> for Memory {
    fn index_mut(&mut self, index: Address) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    #[allow(unused)]
    use super::*;

    #[test]
    fn test_copy() {
        let mut memory = Memory::default();
        let garbage = [0x69u8; Memory::MEMORY_SIZE];
        let mut reader = Cursor::new([0x36u8; 10]);

        memory.load(0x0A, &mut reader).unwrap();
        memory.load_slice(0x0100, &garbage).unwrap();
        memory.load_slice(0x00, &[0xFF; 10]).unwrap();

        assert_eq!(memory[0x0100], 0x69);
        assert_eq!(memory[0x0FFF], 0x69);
        assert_eq!(memory[0x00], 0xFF);
        assert_eq!(memory[0x09], 0xFF);
        assert_eq!(memory[0x0A], 0x36);
        assert_eq!(memory[0x13], 0x36);
    }
}
