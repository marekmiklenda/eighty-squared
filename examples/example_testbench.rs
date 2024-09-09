use std::{fs::File, io::BufReader};

use eighty_squared::{
    instruction::Instruction, io::StreamIo, processor::Processor, EightySquaredResult,
};

// This example loads the processor's memory with the file at env/program.bin; the program is a simple sorting algorithm
// The processor reads the incoming unsorted numbers on port 0 when interrupt RST(0) occurs, simulating signalling that a value is available.
// After the last value is loaded (terminated by 0xFF), the processor sorts the values and outputs them also on port 0, where they get written to env/output.bin
async fn main_wrapped() -> EightySquaredResult<()> {
    let mut stdin = BufReader::new(File::open("env/input.bin")?);
    let stdout = File::create("env/output.bin")?;
    let io = StreamIo::new(&mut stdin, stdout, 0xFF, false, "env/")?;
    let stdin_len = io.size_hint();

    let mut processor = Processor::new(io, 10);

    let mut program_file = File::open("env/program.bin")?;
    processor.hardware.memory.load(0x00, &mut program_file)?;
    processor.hardware.registers.pc.set(0x060);

    let processor_cp = processor.control.control_panel.clone();
    let handle = tokio::spawn(processor.simulate());

    for _ in 0..stdin_len {
        // Simulated delay
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        processor_cp.send_interrupt(Instruction::RST(0)).await;
    }

    // Unwrap the join error
    handle.await.unwrap()?;
    Ok(())
}

#[tokio::main]
async fn main() {
    if let Err(e) = main_wrapped().await {
        eprintln!("Error: {}", e);
    }
}
