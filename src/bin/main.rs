use std::{fs::File, io::BufReader};

use eighty_squared::{
    instruction::Instruction, io::StreamIo, processor::Processor, EightySquaredResult,
};

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
