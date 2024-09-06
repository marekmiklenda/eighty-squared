use crate::{
    instruction::{Instruction, InstructionPrimitive},
    io::ProcessorIo,
    memory::Memory,
    register::Registers,
    EightySquaredResult,
};

#[derive(Default, Clone, Copy)]
pub enum ProcessorState {
    #[default]
    Normal,
    /// No further activity takes place until an interrupt occurs
    Stopped,
}

#[derive(Clone)]
pub struct ProcessorControlPanel {
    interrupt_sender: tokio::sync::mpsc::Sender<Instruction>,
    paused_sender: tokio::sync::watch::Sender<bool>,
    cancelled_sender: tokio::sync::watch::Sender<bool>,
}

impl ProcessorControlPanel {
    /// Pauses the processor. No further instructions and interrupts will be processed until resumed.
    pub fn pause(&self) {
        let _ = self.paused_sender.send(true);
    }

    pub fn resume(&self) {
        let _ = self.paused_sender.send(false);
    }

    /// Stops the processor simulation. The simulation will exit and will need to be restarted to use again.
    pub fn stop(&self) {
        let _ = self.cancelled_sender.send(true);
    }

    pub async fn send_interrupt(&self, interrupt: Instruction) {
        let _ = self.interrupt_sender.send(interrupt).await;
    }
}

#[derive(Default)]
pub struct ProcessorHardware {
    pub memory: Memory,
    pub registers: Registers,
    pub enable_interrupts: bool,
    pub state: ProcessorState,
}

pub struct ProcessorControl<I>
where
    I: ProcessorIo,
{
    pub(crate) io: I,
    pub control_panel: ProcessorControlPanel,
    interrupt_carry: Option<Instruction>,

    cancelled_sender: tokio::sync::watch::Sender<bool>,
    cancelled_receiver: tokio::sync::watch::Receiver<bool>,
    paused_receiver: tokio::sync::watch::Receiver<bool>,
    interrupt_receiver: tokio::sync::mpsc::Receiver<Instruction>,
}

pub struct ProcessorInner<I>
where
    I: ProcessorIo,
{
    pub hardware: ProcessorHardware,
    pub control: ProcessorControl<I>,
}

impl<I> ProcessorInner<I>
where
    I: ProcessorIo,
{
    fn decode_instr(&mut self) -> EightySquaredResult<Instruction> {
        let addr = self.hardware.registers.pc.get();

        let opcode = self.hardware.memory[addr];
        let primitive = InstructionPrimitive::try_from(opcode)?;
        let len = primitive.len();

        let next_instr = addr.wrapping_add(primitive.len() as u16);
        self.hardware.registers.pc.set(next_instr);

        let a = if len > 1 {
            Some(self.hardware.memory[addr.wrapping_add(1)])
        } else {
            None
        };

        let b = if len > 2 {
            Some(self.hardware.memory[addr.wrapping_add(2)])
        } else {
            None
        };

        Instruction::parse(primitive, opcode, a, b)
    }

    async fn simulate_inner(&mut self) -> EightySquaredResult<()> {
        let _ = self.control.cancelled_sender.send(false);

        loop {
            if *self.control.cancelled_receiver.borrow() {
                break;
            }

            // If the CPU is paused, wait for either a new enabled value or for cancellation.
            if *self.control.paused_receiver.borrow() {
                tokio::select! {
                    _ = self.control.paused_receiver.changed() => continue,
                    _ = self.control.cancelled_receiver.changed() => continue,
                }
            }

            match self.hardware.state {
                ProcessorState::Normal => {
                    let interrupt_instr = if self.hardware.enable_interrupts {
                        self
                            .control
                            .interrupt_carry
                            .or_else(|| self.control.interrupt_receiver.try_recv().ok())
                    } else {
                        None
                    };

                    let instr = match interrupt_instr {
                        Some(v) => {
                            self.hardware.enable_interrupts = false;
                            self.control.interrupt_carry = None;

                            v
                        }
                        None => self.decode_instr()?,
                    };

                    self.control
                        .io
                        .debug_instr(&self.hardware, &self.control.control_panel, instr);
                    instr.execute(self)?;
                }
                ProcessorState::Stopped => {
                    if self.hardware.enable_interrupts {
                        tokio::select! {
                            _ = self.control.paused_receiver.changed() => continue,
                            _ = self.control.cancelled_receiver.changed() => continue,
                            interrupt = self.control.interrupt_receiver.recv() => {
                                self.control.interrupt_carry = interrupt;
                                self.hardware.state = ProcessorState::Normal;
                            }
                        }
                    } else {
                        tokio::select! {
                            _ = self.control.paused_receiver.changed() => continue,
                            _ = self.control.cancelled_receiver.changed() => continue,
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

#[repr(transparent)]
pub struct Processor<I>(Box<ProcessorInner<I>>)
where
    I: ProcessorIo;

impl<I> Processor<I>
where
    I: ProcessorIo,
{
    pub fn new(io: I, interrupt_queue_capacity: usize) -> Self {
        // Start unpaused
        let (paused_sender, paused_receiver) = tokio::sync::watch::channel(false);
        let (cancelled_sender, cancelled_receiver) = tokio::sync::watch::channel(false);

        let (interrupt_sender, interrupt_receiver) =
            tokio::sync::mpsc::channel(interrupt_queue_capacity);

        let control_panel = ProcessorControlPanel {
            cancelled_sender: cancelled_sender.clone(),
            paused_sender,
            interrupt_sender,
        };

        let hardware = ProcessorHardware::default();
        let control = ProcessorControl {
            io,
            control_panel: control_panel.clone(),
            interrupt_carry: None,

            cancelled_sender,
            cancelled_receiver,
            paused_receiver,
            interrupt_receiver,
        };

        let processor = ProcessorInner { hardware, control };

        Self(Box::new(processor))
    }

    pub async fn simulate(mut self) -> EightySquaredResult<Self> {
        self.simulate_inner().await?;
        Ok(self)
    }
}

impl<I> std::ops::Deref for Processor<I>
where
    I: ProcessorIo,
{
    type Target = ProcessorInner<I>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<I> std::ops::DerefMut for Processor<I>
where
    I: ProcessorIo,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
