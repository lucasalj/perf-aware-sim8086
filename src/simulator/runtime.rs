use std::{
    fs::File,
    io::{BufWriter, Write},
};

use super::instruction::*;

#[derive(Debug)]
pub struct Runtime {
    gen_reg: [u16; 8],
    instruction_memory: Vec<u8>,
    main_memory: [u8; 1048576],
    zero_flag: bool,
    signal_flag: bool,
    instruction_pointer: usize,
}

#[derive(Debug)]
pub enum RegisterPart {
    LOW,
    HIGH,
    FULL,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            gen_reg: [0u16; 8],
            instruction_memory: Vec::new(),
            main_memory: [0u8; 1048576],
            zero_flag: false,
            signal_flag: false,
            instruction_pointer: 0,
        }
    }

    pub fn instruction_memory(&mut self) -> &mut Vec<u8> {
        &mut self.instruction_memory
    }

    pub fn set_ip(&mut self, new_val: usize) {
        self.instruction_pointer = new_val
    }

    pub fn ip(&self) -> usize {
        self.instruction_pointer
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

    pub fn store_reg(&mut self, reg: Register, val: u16) {
        let idx = Runtime::reg_index(reg);
        match Runtime::reg_part(reg) {
            RegisterPart::LOW => self.gen_reg[idx] = (self.gen_reg[idx] & 0xFF00) | (val & 0x00FF),
            RegisterPart::HIGH => self.gen_reg[idx] = (self.gen_reg[idx] & 0x00FF) | (val & 0xFF00),
            RegisterPart::FULL => self.gen_reg[idx] = val,
        }
    }

    pub fn load_reg(&mut self, reg: Register) -> u16 {
        let idx = Runtime::reg_index(reg);
        match Runtime::reg_part(reg) {
            RegisterPart::LOW => self.gen_reg[idx] & 0x00FF,
            RegisterPart::HIGH => (self.gen_reg[idx] & 0xFF00) >> 8,
            RegisterPart::FULL => self.gen_reg[idx],
        }
    }

    pub fn set_flags(&mut self, val: u16) {
        self.zero_flag = val == 0;
        self.signal_flag = (val & 0x8000) == 0x8000;
    }

    pub fn execute_mov(
        &mut self,
        src: SrcOperand,
        dst: Option<DstOperand>,
        size_specifier: Option<SizeSpecifier>,
    ) {
        match src {
            SrcOperand::Register(src_reg) => {
                let val = self.load_reg(src_reg);
                match dst {
                    Some(dst) => match dst {
                        DstOperand::Register(reg) => self.store_reg(reg, val),
                        DstOperand::MemoryAddressing(MemoryAddressing {
                            reg_first_operand,
                            reg_sec_operand,
                            disp,
                        }) => {
                            let addr = reg_first_operand.map_or(0, |reg| self.load_reg(reg))
                                + reg_sec_operand.map_or(0, |reg| self.load_reg(reg))
                                + disp;
                            self.main_memory[addr as usize] = (val & 0xFF) as u8;
                            self.main_memory[(addr + 1) as usize] = ((val & 0xFF00) >> 8) as u8;
                        }
                    },
                    None => todo!(),
                }
            }
            SrcOperand::MemoryAddressing(MemoryAddressing {
                reg_first_operand,
                reg_sec_operand,
                disp,
            }) => {
                let addr = reg_first_operand.map_or(0, |reg| self.load_reg(reg))
                    + reg_sec_operand.map_or(0, |reg| self.load_reg(reg))
                    + disp;

                match dst {
                    Some(dst) => match dst {
                        DstOperand::Register(reg) => {
                            if let Some(size) = size_specifier {
                                match size {
                                    SizeSpecifier::BYTE => {
                                        let mut val: u16 = self.main_memory[addr as usize] as u16;
                                        val |= self.load_reg(reg) & 0xFF00;
                                        self.store_reg(reg, val);
                                    }
                                    SizeSpecifier::WORD => {
                                        let mut val: u16 = self.main_memory[addr as usize] as u16;
                                        val |= ((self.main_memory[(addr + 1) as usize] as u16)
                                            << 8)
                                            & 0xFF00;
                                        self.store_reg(reg, val);
                                    }
                                }
                                return;
                            }
                            let mut val: u16 = self.main_memory[addr as usize] as u16;
                            val |= ((self.main_memory[(addr + 1) as usize] as u16) << 8) & 0xFF00;
                            self.store_reg(reg, val);
                        }
                        DstOperand::MemoryAddressing(_) => todo!(),
                    },
                    None => todo!(),
                }
            }
            SrcOperand::Immediate(imm) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(reg) => self.store_reg(reg, imm as u16),
                    DstOperand::MemoryAddressing(MemoryAddressing {
                        reg_first_operand,
                        reg_sec_operand,
                        disp,
                    }) => {
                        let addr = reg_first_operand.map_or(0, |reg| self.load_reg(reg))
                            + reg_sec_operand.map_or(0, |reg| self.load_reg(reg))
                            + disp;
                        if let Some(size) = size_specifier {
                            match size {
                                SizeSpecifier::BYTE => {
                                    self.main_memory[addr as usize] = (imm & 0xFF) as u8;
                                }
                                SizeSpecifier::WORD => {
                                    self.main_memory[addr as usize] = (imm & 0xFF) as u8;
                                    self.main_memory[(addr + 1) as usize] =
                                        ((imm >> 8) & 0xFF) as u8;
                                }
                            }
                            return;
                        }
                        self.main_memory[addr as usize] = (imm & 0xFF) as u8;
                        self.main_memory[(addr + 1) as usize] = ((imm >> 8) & 0xFF) as u8;
                    }
                },
                None => todo!(),
            },
            SrcOperand::RelativePosition(_) => todo!(),
        }
    }

    pub fn execute_add(
        &mut self,
        src: SrcOperand,
        dst: Option<DstOperand>,
        _size_specifier: Option<SizeSpecifier>,
    ) {
        match src {
            SrcOperand::Register(src_reg) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_add(self.load_reg(src_reg));
                        self.store_reg(dst_reg, val);
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::MemoryAddressing(_) => todo!(),
            SrcOperand::Immediate(imm) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_add(imm as u16);
                        self.store_reg(dst_reg, val);
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::RelativePosition(_) => todo!(),
        }
    }

    pub fn execute_sub(
        &mut self,
        src: SrcOperand,
        dst: Option<DstOperand>,
        _size_specifier: Option<SizeSpecifier>,
    ) {
        match src {
            SrcOperand::Register(src_reg) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_sub(self.load_reg(src_reg));
                        self.store_reg(dst_reg, val);
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::MemoryAddressing(_) => todo!(),
            SrcOperand::Immediate(imm) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_sub(imm as u16);
                        self.store_reg(dst_reg, val);
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::RelativePosition(_) => todo!(),
        }
    }

    pub fn execute_cmp(
        &mut self,
        src: SrcOperand,
        dst: Option<DstOperand>,
        _size_specifier: Option<SizeSpecifier>,
    ) {
        match src {
            SrcOperand::Register(src_reg) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_sub(self.load_reg(src_reg));
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::MemoryAddressing(_) => todo!(),
            SrcOperand::Immediate(imm) => match dst {
                Some(dst) => match dst {
                    DstOperand::Register(dst_reg) => {
                        let mut val = self.load_reg(dst_reg);
                        val = val.wrapping_sub(imm as u16);
                        self.set_flags(val);
                    }
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            },
            SrcOperand::RelativePosition(_) => todo!(),
        }
    }
    pub fn execute_je(
        &mut self,
        src: SrcOperand,
        _dst: Option<DstOperand>,
        _size_specifier: Option<SizeSpecifier>,
    ) {
        if self.zero_flag {
            match src {
                SrcOperand::Register(_) => unreachable!(),
                SrcOperand::MemoryAddressing(_) => unreachable!(),
                SrcOperand::Immediate(_) => unreachable!(),
                SrcOperand::RelativePosition(p) => {
                    self.instruction_pointer =
                        (self.instruction_pointer as isize + (p as isize)) as usize;
                }
            }
        }
    }

    pub fn execute_jne(
        &mut self,
        src: SrcOperand,
        _dst: Option<DstOperand>,
        _size_specifier: Option<SizeSpecifier>,
    ) {
        if !self.zero_flag {
            match src {
                SrcOperand::Register(_) => unreachable!(),
                SrcOperand::MemoryAddressing(_) => unreachable!(),
                SrcOperand::Immediate(_) => unreachable!(),
                SrcOperand::RelativePosition(p) => {
                    self.instruction_pointer =
                        (self.instruction_pointer as isize + (p as isize)) as usize;
                }
            }
        }
    }

    pub fn execute(&mut self, instruction: &Instruction) {
        match instruction.operation() {
            InstructionOperation::MOV => self.execute_mov(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
            InstructionOperation::ADD => self.execute_add(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
            InstructionOperation::SUB => self.execute_sub(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
            InstructionOperation::CMP => self.execute_cmp(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),

            InstructionOperation::JE => self.execute_je(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
            InstructionOperation::JL => todo!(),
            InstructionOperation::JLE => todo!(),
            InstructionOperation::JB => todo!(),
            InstructionOperation::JBE => todo!(),
            InstructionOperation::JP => todo!(),
            InstructionOperation::JO => todo!(),
            InstructionOperation::JS => todo!(),
            InstructionOperation::JNE => self.execute_jne(
                instruction.src(),
                instruction.dst(),
                instruction.size_specifier(),
            ),
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

    pub fn print_registers(&self) {
        println!("-------------");
        for reg_idx in 0..self.gen_reg.len() {
            let reg = Runtime::index_to_reg(reg_idx);
            println!("{reg}: 0x{:04x}", self.gen_reg[reg_idx]);
        }
        println!("ZF: {}", self.zero_flag as u32);
        println!("SF: {}", self.signal_flag as u32);
        println!("IP: {}", self.instruction_pointer as u32);
        println!("-------------\n");
    }

    pub fn dump_memory_to_file(&self, fname: &str) -> std::io::Result<()> {
        let mut f = File::create(fname)?;
        f.write_all(&self.main_memory)?;
        Ok(())
    }
}
