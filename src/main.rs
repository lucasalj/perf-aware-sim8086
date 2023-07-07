mod simulator;
use simulator::decoder::*;
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
    let mut file = File::open(filename)?;
    print!("; {filename}\nbits 16\n\n");
    let mut decoder = Decoder::new();
    file.read_to_end(runtime.instruction_memory())?;
    loop {
        let ip = runtime.ip();
        let (inst, ip) = decoder.decode(runtime.instruction_memory(), ip)?;
        if let Some(inst) = inst {
            runtime.set_ip(ip);
            print!("{inst}\n");
            runtime.execute(&inst);
            // runtime.print_registers();
        }
        if runtime.ip() >= runtime.instruction_memory().len() {
            break;
        }
    }
    Ok(())
}
