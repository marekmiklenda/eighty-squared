use std::{fs::File, path::PathBuf};

use crate::{
    instruction::Instruction,
    processor::{ProcessorControlPanel, ProcessorHardware},
    EightySquaredResult,
};

pub trait ProcessorIo
where
    Self: Sized,
{
    fn input(
        &mut self,
        hardware: &ProcessorHardware,
        cp: &ProcessorControlPanel,
        device: u8,
    ) -> Result<u8, Box<dyn std::error::Error + Send + Sync + 'static>>;

    fn output(
        &mut self,
        hardware: &ProcessorHardware,
        cp: &ProcessorControlPanel,
        device: u8,
        value: u8,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>>;

    #[allow(unused)]
    fn debug_instr(
        &mut self,
        hardware: &ProcessorHardware,
        cp: &ProcessorControlPanel,
        instr: Instruction,
    ) {
    }
}

impl ProcessorIo for () {
    fn input(
        &mut self,
        _hardware: &ProcessorHardware,
        _cp: &ProcessorControlPanel,
        _device: u8,
    ) -> Result<u8, Box<dyn std::error::Error + Send + Sync + 'static>> {
        Ok(0)
    }

    fn output(
        &mut self,
        _hardware: &ProcessorHardware,
        _cp: &ProcessorControlPanel,
        _device: u8,
        _value: u8,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
        Ok(())
    }
}

pub struct StreamIo<W>
where
    W: std::io::Write,
{
    debug_instrs: bool,
    eof_byte: u8,
    stdin: std::vec::IntoIter<u8>,
    stdout: W,
    size_hint: usize,
    debug_memory_folder: PathBuf,
}

impl<W> StreamIo<W>
where
    W: std::io::Write,
{
    const DEV_STOP: u8 = 0xFF;
    const DEV_DBG_REG: u8 = 0xFE;
    const DEV_DBG_MEM: u8 = 0xFD;

    const DEV_STDIN: u8 = 0x00;
    const DEV_STDOUT: u8 = 0x00;

    /// Warning: stream MUST be terminated with EOF byte
    pub fn new<R, A>(
        stdin: &mut R,
        stdout: W,
        eof_byte: u8,
        debug_instrs: bool,
        debug_memory_folder: A,
    ) -> EightySquaredResult<Self>
    where
        R: std::io::BufRead,
        PathBuf: From<A>,
    {
        let mut stdin_vec = Vec::new();
        let size_hint = stdin.read_until(eof_byte, &mut stdin_vec)?;

        Ok(Self {
            debug_instrs,
            size_hint,
            stdin: stdin_vec.into_iter(),
            stdout,
            eof_byte,
            debug_memory_folder: PathBuf::from(debug_memory_folder),
        })
    }

    pub fn size_hint(&self) -> usize {
        self.size_hint
    }
}

impl<W> ProcessorIo for StreamIo<W>
where
    W: std::io::Write,
{
    fn input(
        &mut self,
        _hardware: &ProcessorHardware,
        _cp: &ProcessorControlPanel,
        device: u8,
    ) -> Result<u8, Box<dyn std::error::Error + Send + Sync + 'static>> {
        let v = match device {
            Self::DEV_STDIN => self.stdin.next().unwrap_or(self.eof_byte),
            _ => 0,
        };

        Ok(v)
    }

    fn output(
        &mut self,
        hardware: &ProcessorHardware,
        cp: &ProcessorControlPanel,
        device: u8,
        value: u8,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
        use byteorder::WriteBytesExt;
        match device {
            Self::DEV_STOP => cp.stop(),
            Self::DEV_DBG_REG => println!("{:?}", hardware.registers),
            Self::DEV_DBG_MEM => {
                let mut file = {
                    self.debug_memory_folder.push(format!(
                        "memdump_{}.bin",
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_micros()
                    ));

                    let file = File::create(&self.debug_memory_folder)?;
                    self.debug_memory_folder.pop();
                    file
                };

                hardware.memory.dump(&mut file).unwrap();
            }
            Self::DEV_STDOUT => self.stdout.write_u8(value)?,
            _ => (),
        }

        Ok(())
    }

    fn debug_instr(
        &mut self,
        _hardware: &ProcessorHardware,
        _cp: &ProcessorControlPanel,
        instr: Instruction,
    ) {
        if self.debug_instrs {
            println!("{:?}", instr);
        }
    }
}
