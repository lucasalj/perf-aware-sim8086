use std::io::{BufWriter, Write};

use super::instruction::*;

#[derive(Debug)]
pub struct Runtime {
    reg: [u16; 8],
}

#[derive(Debug)]
pub enum RegisterPart {
    LOW,
    HIGH,
    FULL,
}

impl Runtime {
    pub fn new() -> Self {
        Self { reg: [0u16; 8] }
    }

    pub fn reg_part(reg: Register) -> RegisterPart {
        match reg {
            Register::AL => RegisterPart::LOW,
            Register::CL => RegisterPart::LOW,
            Register::DL => RegisterPart::LOW,
            Register::BL => RegisterPart::LOW,
            Register::AH => RegisterPart::HIGH,
            Register::CH => RegisterPart::HIGH,
            Register::DH => RegisterPart::HIGH,
            Register::BH => RegisterPart::HIGH,
            Register::AX => RegisterPart::FULL,
            Register::CX => RegisterPart::FULL,
            Register::DX => RegisterPart::FULL,
            Register::BX => RegisterPart::FULL,
            Register::SP => RegisterPart::FULL,
            Register::BP => RegisterPart::FULL,
            Register::SI => RegisterPart::FULL,
            Register::DI => RegisterPart::FULL,
        }
    }

    pub fn reg_index(reg: Register) -> usize {
        match reg {
            Register::AL => 0,
            Register::CL => 2,
            Register::DL => 3,
            Register::BL => 1,
            Register::AH => 0,
            Register::CH => 2,
            Register::DH => 3,
            Register::BH => 1,
            Register::AX => 0,
            Register::CX => 2,
            Register::DX => 3,
            Register::BX => 1,
            Register::SP => 4,
            Register::BP => 5,
            Register::SI => 6,
            Register::DI => 7,
        }
    }
    pub fn index_to_reg(idx: usize) -> Register {
        match idx {
            0 => Register::AX,
            2 => Register::CX,
            3 => Register::DX,
            1 => Register::BX,
            4 => Register::SP,
            5 => Register::BP,
            6 => Register::SI,
            7 => Register::DI,
            _ => panic!("Invalid index to register"),
        }
    }

    pub fn set_reg(&mut self, reg: Register, val: u16) {
        let idx = Runtime::reg_index(reg);
        match Runtime::reg_part(reg) {
            RegisterPart::LOW => self.reg[idx] = (self.reg[idx] & 0xFF00) | (val & 0x00FF),
            RegisterPart::HIGH => self.reg[idx] = (self.reg[idx] & 0x00FF) | (val & 0xFF00),
            RegisterPart::FULL => self.reg[idx] = val,
        }
    }

    pub fn execute_mov(
        &mut self,
        src: SrcOperand,
        dst: Option<DstOperand>,
        size_specifier: Option<SizeSpecifier>,
    ) {
        match src {
            SrcOperand::Register(_) => todo!(),
            SrcOperand::MemoryAddressing(_) => todo!(),
            SrcOperand::Immediate(imm) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(reg) => self.set_reg(reg, imm as u16),
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::RelativePosition(_) => todo!(),
        }
    }

    pub fn execute(&mut self, instruction: &Instruction) {
        match instruction.operation() {
            InstructionOperation::MOV => self.execute_mov(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
            InstructionOperation::ADD => todo!(),
            InstructionOperation::SUB => todo!(),
            InstructionOperation::CMP => todo!(),
            InstructionOperation::JE => todo!(),
            InstructionOperation::JL => todo!(),
            InstructionOperation::JLE => todo!(),
            InstructionOperation::JB => todo!(),
            InstructionOperation::JBE => todo!(),
            InstructionOperation::JP => todo!(),
            InstructionOperation::JO => todo!(),
            InstructionOperation::JS => todo!(),
            InstructionOperation::JNE => todo!(),
            InstructionOperation::JNL => todo!(),
            InstructionOperation::JNLE => todo!(),
            InstructionOperation::JNB => todo!(),
            InstructionOperation::JNBE => todo!(),
            InstructionOperation::JNP => todo!(),
            InstructionOperation::JNO => todo!(),
            InstructionOperation::JNS => todo!(),
            InstructionOperation::LOOP => todo!(),
            InstructionOperation::LOOPZ => todo!(),
            InstructionOperation::LOOPNZ => todo!(),
            InstructionOperation::JCXZ => todo!(),
        }
    }

    pub fn print_registers<W: Write>(&self, out: &mut BufWriter<W>) -> Result<(), std::io::Error> {
        write!(out, "  Registers\n")?;
        write!(out, "-------------\n")?;
        for reg_idx in 0..self.reg.len() {
            let reg = Runtime::index_to_reg(reg_idx);
            write!(out, "{reg}: 0x{:04x}\n", self.reg[reg_idx])?;
        }
        write!(out, "-------------\n")?;
        Ok(())
    }
}
