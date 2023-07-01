mod simulator;
use simulator::decoder::*;
use simulator::instruction::*;
use simulator::runtime::*;
use std::fs::File;
use std::io::{Read, Write};
use std::{env, io};

fn main() -> DecoderResult<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(DecoderError::GenericError(String::from(
            "No filename given",
        )));
    }

    let mut runtime = Runtime::new();

    let filename = &args[1];
    let mut buffer = [0u8; 1024];
    let mut file = File::open(filename)?;
    let mut out = io::BufWriter::new(io::stdout().lock());
    out.write_all(format!("; {filename}\nbits 16\n\n").as_bytes())?;
    let mut decoder = Decoder::new();
    let mut instructions = vec![];
    runtime.print_registers(&mut out).unwrap();
    write!(out, "\n")?;
    loop {
        let n_bytes = file.read(&mut buffer)?;
        if n_bytes == 0 {
            break;
        }
        let data = &buffer[0..n_bytes];
        decoder.decode(data, &mut instructions)?;
        for instruction in instructions.iter() {
            write!(out, "{instruction}\n")?;
            runtime.execute(instruction);
        }
    }
    write!(out, "\n")?;
    runtime.print_registers(&mut out).unwrap();
    Ok(())
}
