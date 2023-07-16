use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    op: InstructionOperation,
    src: SrcOperand,
    dst: Option<DstOperand>,
    size_specifier: Option<SizeSpecifier>,
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionOperation {
    MOV,
    ADD,
    SUB,
    CMP,
    JE,
    JL,
    JLE,
    JB,
    JBE,
    JP,
    JO,
    JS,
    JNE,
    JNL,
    JNLE,
    JNB,
    JNBE,
    JNP,
    JNO,
    JNS,
    LOOP,
    LOOPZ,
    LOOPNZ,
    JCXZ,
}

#[derive(Debug, Clone, Copy)]
pub enum DstOperand {
    Register(Register),
    MemoryAddressing(MemoryAddressing),
}

#[derive(Debug, Clone, Copy)]
pub enum SrcOperand {
    Register(Register),
    MemoryAddressing(MemoryAddressing),
    Immediate(i16),
    RelativePosition(i8),
}

#[derive(Debug, Clone, Copy)]
pub struct MemoryAddressing {
    pub reg_first_operand: Option<Register>,
    pub reg_sec_operand: Option<Register>,
    pub disp: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

#[derive(Debug, Clone, Copy)]
pub enum SizeSpecifier {
    BYTE,
    WORD,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::AL => f.write_str("al"),
            Register::CL => f.write_str("cl"),
            Register::DL => f.write_str("dl"),
            Register::BL => f.write_str("bl"),
            Register::AH => f.write_str("ah"),
            Register::CH => f.write_str("ch"),
            Register::DH => f.write_str("dh"),
            Register::BH => f.write_str("bh"),
            Register::AX => f.write_str("ax"),
            Register::CX => f.write_str("cx"),
            Register::DX => f.write_str("dx"),
            Register::BX => f.write_str("bx"),
            Register::SP => f.write_str("sp"),
            Register::BP => f.write_str("bp"),
            Register::SI => f.write_str("si"),
            Register::DI => f.write_str("di"),
        }
    }
}

impl Display for MemoryAddressing {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        if let Some(first_op) = self.reg_first_operand {
            write!(f, "{first_op}")?;
            if let Some(sec_op) = self.reg_sec_operand {
                write!(f, "+{sec_op}")?;
            }
        }
        if self.disp != 0 || (self.reg_first_operand.is_none() && self.reg_sec_operand.is_none()) {
            write!(f, "+0x{:x}", self.disp)?;
        }
        f.write_str("]")
    }
}

impl Display for DstOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DstOperand::Register(reg) => {
                write!(f, "{reg}")
            }
            DstOperand::MemoryAddressing(mem_addr) => {
                write!(f, "{mem_addr}")
            }
        }
    }
}

impl Display for SrcOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcOperand::Register(reg) => {
                write!(f, "{reg}")
            }
            SrcOperand::MemoryAddressing(mem_addr) => {
                write!(f, "{mem_addr}")
            }
            SrcOperand::Immediate(imm) => {
                write!(f, "{imm}")
            }
            SrcOperand::RelativePosition(rel_pos) => {
                write!(f, "${:+}", rel_pos + 2)
            }
        }
    }
}

impl Display for InstructionOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionOperation::MOV => write!(f, "mov"),
            InstructionOperation::ADD => write!(f, "add"),
            InstructionOperation::SUB => write!(f, "sub"),
            InstructionOperation::CMP => write!(f, "cmp"),
            InstructionOperation::JE => write!(f, "je"),
            InstructionOperation::JL => write!(f, "jl"),
            InstructionOperation::JLE => write!(f, "jle"),
            InstructionOperation::JB => write!(f, "jb"),
            InstructionOperation::JBE => write!(f, "jbe"),
            InstructionOperation::JP => write!(f, "jp"),
            InstructionOperation::JO => write!(f, "jo"),
            InstructionOperation::JS => write!(f, "js"),
            InstructionOperation::JNE => write!(f, "jne"),
            InstructionOperation::JNL => write!(f, "jnl"),
            InstructionOperation::JNLE => write!(f, "jnle"),
            InstructionOperation::JNB => write!(f, "jnb"),
            InstructionOperation::JNBE => write!(f, "jnbe"),
            InstructionOperation::JNP => write!(f, "jnp"),
            InstructionOperation::JNO => write!(f, "jno"),
            InstructionOperation::JNS => write!(f, "jns"),
            InstructionOperation::LOOP => write!(f, "loop"),
            InstructionOperation::LOOPZ => write!(f, "loopz"),
            InstructionOperation::LOOPNZ => write!(f, "loopnz"),
            InstructionOperation::JCXZ => write!(f, "jcxz"),
        }
    }
}

impl Display for SizeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SizeSpecifier::BYTE => write!(f, "byte"),
            SizeSpecifier::WORD => write!(f, "word"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let has_two_operands = match self.op {
            InstructionOperation::MOV => true,
            InstructionOperation::ADD => true,
            InstructionOperation::SUB => true,
            InstructionOperation::CMP => true,
            _ => false,
        } && self.dst.is_some();
        write!(f, "{} ", self.op)?;
        if let Some(sz) = self.size_specifier {
            write!(f, "{} ", sz)?;
        }
        if has_two_operands {
            write!(f, "{}, {}", self.dst.unwrap(), self.src)
        } else {
            write!(f, "{}", self.src)
        }
    }
}

impl Instruction {
    pub fn new(
        op: InstructionOperation,
        src: SrcOperand,
        dst: Option<DstOperand>,
        size_specifier: Option<SizeSpecifier>,
    ) -> Self {
        Self {
            op,
            src,
            dst,
            size_specifier,
        }
    }
    pub fn operation(&self) -> InstructionOperation {
        return self.op.clone();
    }
    pub fn src(&self) -> SrcOperand {
        return self.src.clone();
    }
    pub fn dst(&self) -> Option<DstOperand> {
        return self.dst.clone();
    }
    pub fn size_specifier(&self) -> Option<SizeSpecifier> {
        return self.size_specifier.clone();
    }
}

impl MemoryAddressing {
    pub fn new(
        reg_first_operand: Option<Register>,
        reg_sec_operand: Option<Register>,
        disp: u16,
    ) -> Self {
        Self {
            reg_first_operand,
            reg_sec_operand,
            disp,
        }
    }
}
