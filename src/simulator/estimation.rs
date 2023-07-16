use std::{
    fmt::Display,
    ops::{Add, AddAssign},
};

use super::instruction::*;

#[derive(Debug, Clone, Copy)]
pub struct Estimation {
    pub cycles: u64,
}

impl AddAssign for Estimation {
    fn add_assign(&mut self, rhs: Self) {
        self.cycles += rhs.cycles
    }
}

impl Estimation {
    pub fn new() -> Self {
        Self { cycles: 0 }
    }
}

impl Display for Estimation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} cycles", self.cycles)
    }
}

impl Add for Estimation {
    type Output = Estimation;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            cycles: self.cycles + rhs.cycles,
        }
    }
}

pub fn estimate(inst: &Instruction) -> Estimation {
    match inst.operation() {
        InstructionOperation::MOV => estimate_mov(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::ADD => estimate_add(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::SUB => estimate_sub(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::CMP => estimate_cmp(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JE => estimate_je(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JL => estimate_jl(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JLE => estimate_jle(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JB => estimate_jb(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JBE => estimate_jbe(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JP => estimate_jp(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JO => estimate_jo(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JS => estimate_js(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNE => estimate_jne(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNL => estimate_jnl(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNLE => estimate_jnle(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNB => estimate_jnb(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNBE => estimate_jnbe(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNP => estimate_jnp(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNO => estimate_jno(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::JNS => estimate_jns(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::LOOP => estimate_loop(inst.src(), inst.dst(), inst.size_specifier()),
        InstructionOperation::LOOPZ => {
            estimate_loopz(inst.src(), inst.dst(), inst.size_specifier())
        }
        InstructionOperation::LOOPNZ => {
            estimate_loopnz(inst.src(), inst.dst(), inst.size_specifier())
        }
        InstructionOperation::JCXZ => estimate_jcxz(inst.src(), inst.dst(), inst.size_specifier()),
    }
}

fn estimate_loopnz(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_loopz(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jcxz(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_loop(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jns(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jno(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jnp(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jnbe(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jnb(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jnle(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jnl(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jne(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_js(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jo(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jp(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jbe(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jb(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jle(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_jl(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_je(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_cmp(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_sub(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    todo!()
}

fn estimate_ea(
    reg_first_operand: Option<Register>,
    reg_sec_operand: Option<Register>,
    disp: u16,
) -> Estimation {
    if reg_first_operand.is_none() && reg_sec_operand.is_none() {
        return Estimation { cycles: 6 };
    }
    if reg_first_operand.is_some() && reg_sec_operand.is_none() {
        return if disp == 0 {
            Estimation { cycles: 5 }
        } else {
            Estimation { cycles: 9 }
        };
    }
    if reg_first_operand.is_some() && reg_sec_operand.is_some() && disp == 0 {
        let reg_first_op = reg_first_operand.unwrap();
        let reg_sec_op = reg_sec_operand.unwrap();
        return if (reg_first_op == Register::BP && reg_sec_op == Register::DI)
            || (reg_first_op == Register::BX && reg_sec_op == Register::SI)
        {
            Estimation { cycles: 7 }
        } else {
            Estimation { cycles: 8 }
        };
    }
    if reg_first_operand.is_some() && reg_sec_operand.is_some() && disp != 0 {
        let reg_first_op = reg_first_operand.unwrap();
        let reg_sec_op = reg_sec_operand.unwrap();
        return if (reg_first_op == Register::BP && reg_sec_op == Register::DI)
            || (reg_first_op == Register::BX && reg_sec_op == Register::SI)
        {
            Estimation { cycles: 11 }
        } else {
            Estimation { cycles: 12 }
        };
    }
    unreachable!()
}

fn estimate_mov(
    src: SrcOperand,
    dst: Option<DstOperand>,
    _size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    match src {
        SrcOperand::Register(_) => match dst {
            Some(dst) => match dst {
                DstOperand::Register(_) => Estimation { cycles: 2 },
                DstOperand::MemoryAddressing(MemoryAddressing {
                    reg_first_operand,
                    reg_sec_operand,
                    disp,
                }) => {
                    let dst_ea_estimation = estimate_ea(reg_first_operand, reg_sec_operand, disp);
                    return Estimation { cycles: 9 } + dst_ea_estimation;
                }
            },
            None => todo!(),
        },
        SrcOperand::MemoryAddressing(MemoryAddressing {
            reg_first_operand,
            reg_sec_operand,
            disp,
        }) => {
            let src_ea_estimation = estimate_ea(reg_first_operand, reg_sec_operand, disp);
            match dst {
                Some(dst) => match dst {
                    DstOperand::Register(_) => Estimation { cycles: 8 } + src_ea_estimation,
                    DstOperand::MemoryAddressing(_) => todo!(),
                },
                None => todo!(),
            }
        }
        SrcOperand::Immediate(_) => match dst {
            Some(dst) => match dst {
                DstOperand::Register(_) => Estimation { cycles: 4 },
                DstOperand::MemoryAddressing(_) => todo!(),
            },
            None => todo!(),
        },
        SrcOperand::RelativePosition(_) => todo!(),
    }
}

fn estimate_add(
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
) -> Estimation {
    match src {
        SrcOperand::Register(_) => match dst {
            Some(dst) => match dst {
                DstOperand::Register(_) => Estimation { cycles: 3 },
                DstOperand::MemoryAddressing(MemoryAddressing {
                    reg_first_operand,
                    reg_sec_operand,
                    disp,
                }) => {
                    let dst_ea_estimation = estimate_ea(reg_first_operand, reg_sec_operand, disp);
                    Estimation { cycles: 16 } + dst_ea_estimation
                }
            },
            None => todo!(),
        },
        SrcOperand::MemoryAddressing(_) => todo!(),
        SrcOperand::Immediate(_) => match dst {
            Some(dst) => match dst {
                DstOperand::Register(_) => Estimation { cycles: 4 },
                DstOperand::MemoryAddressing(_) => todo!(),
            },
            None => todo!(),
        },
        SrcOperand::RelativePosition(_) => todo!(),
    }
}
