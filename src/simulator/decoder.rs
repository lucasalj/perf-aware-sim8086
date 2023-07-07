use std::fmt::Display;

use super::instruction::*;

pub type DecoderResult<T> = Result<T, DecoderError>;

#[derive(Debug)]
enum DecoderState {
    Init,
    MovRegMemToFromReg {
        reg_is_dest: bool,
        is_word: bool,
        state: MovRegMemToFromRegState,
    },
    MovImmToRegMem {
        is_word: bool,
        state: MovImmToRegMemState,
    },
    MovAccMemToAccMem {
        acc_to_mem: bool,
        is_word: bool,
        state: MovAccMemToAccMemState,
    },
    #[allow(dead_code)]
    MovRegMemSRToRegMemSR {
        to_sr: bool,
        state: MovRegMemSRToRegMemSRState,
    },
    ArithRegMemWithRegToEither {
        op: ArithOperation,
        reg_is_dest: bool,
        is_word: bool,
        state: ArithRegMemWithRegToEitherState,
    },
    ArithImmWithRegMem {
        signed: bool,
        is_word: bool,
        state: ArithImmWithRegMemState,
    },
    ArithImmToAcc {
        op: ArithOperation,
        is_word: bool,
        state: ArithImmToAccState,
    },
    CondJump {
        jmp: ConditionalJump,
    },
}

#[derive(Debug, Clone, Copy)]
enum MovRegMemToFromRegState {
    Init,
    DirectAddressReadLowDisp {
        reg: Register,
    },
    DirectAddressReadHighDisp {
        reg: Register,
        low_disp: u8,
    },
    EffectAddrReadLowDisp {
        reg: Register,
        effect_addr: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    EffectAddrReadHighDisp {
        reg: Register,
        effect_addr: EffectiveAddressCalculation,
        low_disp: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum MovImmToRegMemState {
    Init,
    ToRegReadLowImm {
        reg: Register,
    },
    ToRegReadHighImm {
        reg: Register,
        low_imm: u8,
    },
    ToDirectAddrReadLowDisp,
    ToDirectAddrReadHighDisp {
        low_disp: u8,
    },
    ToDirectAddrReadLowImm {
        disp: u16,
    },
    ToDirectAddrReadHighImm {
        disp: u16,
        low_imm: u8,
    },
    ToEffectAddrReadLowDisp {
        effect_addr: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    ToEffectAddrReadHighDisp {
        effect_addr: EffectiveAddressCalculation,
        low_disp: u8,
    },
    ToEffectAddrReadLowImm {
        effect_addr: EffectiveAddressCalculation,
        disp: Option<i16>,
    },
    ToEffectAddrReadHighImm {
        effect_addr: EffectiveAddressCalculation,
        disp: Option<i16>,
        low_imm: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum MovAccMemToAccMemState {
    ReadAddrLo,
    ReadAddrHigh { addr_lo: u8 },
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum MovRegMemSRToRegMemSRState {
    Init,
    ToFromDirectAddrReadLowDisp {
        sr: SegmentRegister,
    },
    ToFromDirectAddrReadHighDisp {
        sr: SegmentRegister,
        low_disp: u8,
    },
    ToFromEffectAddrReadLowDisp {
        sr: SegmentRegister,
        effect_addr: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    ToFromEffectAddrReadHighDisp {
        sr: SegmentRegister,
        effect_addr: EffectiveAddressCalculation,
        low_disp: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum ConditionalJump {
    JumpOnEqual,
    JumpOnLess,
    JumpOnLessOrEqual,
    JumpOnBelow,
    JumpOnBelowOrEqual,
    JumpOnParity,
    JumpOnOverflow,
    JumpOnSign,
    JumpOnNotEqual,
    JumpOnNotLess,
    JumpOnNotLessOrEqual,
    JumpOnNotBelow,
    JumpOnNotBelowOrEqual,
    JumpOnNotPar,
    JumpOnNotOverflow,
    JumpOnNotSign,
    LoopCxTimes,
    LoopWhileZero,
    LoopWhileNotZero,
    JumpOnCxZero,
}

#[derive(Debug, Clone, Copy)]
enum ArithOperation {
    ADD,
    SUB,
    CMP,
}

#[derive(Debug, Clone, Copy)]
enum ArithRegMemWithRegToEitherState {
    Init,
    DirectAddressReadLowDisp {
        reg: Register,
    },
    DirectAddressReadHighDisp {
        reg: Register,
        low_disp: u8,
    },
    EffectAddrReadLowDisp {
        reg: Register,
        effect_addr: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    EffectAddrReadHighDisp {
        reg: Register,
        effect_addr: EffectiveAddressCalculation,
        low_disp: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum ArithImmWithRegMemState {
    Init,
    WithRegReadLowImm {
        op: ArithOperation,
        reg: Register,
    },
    WithRegReadHighImm {
        op: ArithOperation,
        reg: Register,
        low_imm: u8,
    },
    WithDirectAddrReadLowDisp {
        op: ArithOperation,
    },
    WithDirectAddrReadHighDisp {
        op: ArithOperation,
        low_disp: u8,
    },
    WithDirectAddrReadLowImm {
        op: ArithOperation,
        disp: u16,
    },
    WithDirectAddrReadHighImm {
        op: ArithOperation,
        disp: u16,
        low_imm: u8,
    },
    WithEffectAddrReadLowDisp {
        op: ArithOperation,
        effect_addr: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    WithEffectAddrReadHighDisp {
        op: ArithOperation,
        effect_addr: EffectiveAddressCalculation,
        low_disp: u8,
    },
    WithEffectAddrReadLowImm {
        op: ArithOperation,
        effect_addr: EffectiveAddressCalculation,
        disp: Option<i16>,
    },
    WithEffectAddrReadHighImm {
        op: ArithOperation,
        effect_addr: EffectiveAddressCalculation,
        disp: Option<i16>,
        low_imm: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum ArithImmToAccState {
    Init,
    ReadHighImm { low_imm: u8 },
}

impl Display for ArithOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithOperation::ADD => f.write_str("add"),
            ArithOperation::SUB => f.write_str("sub"),
            ArithOperation::CMP => f.write_str("cmp"),
        }
    }
}

impl Display for ConditionalJump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionalJump::JumpOnEqual => f.write_str("je"),
            ConditionalJump::JumpOnLess => f.write_str("jl"),
            ConditionalJump::JumpOnLessOrEqual => f.write_str("jle"),
            ConditionalJump::JumpOnBelow => f.write_str("jb"),
            ConditionalJump::JumpOnBelowOrEqual => f.write_str("jbe"),
            ConditionalJump::JumpOnParity => f.write_str("jp"),
            ConditionalJump::JumpOnOverflow => f.write_str("jo"),
            ConditionalJump::JumpOnSign => f.write_str("js"),
            ConditionalJump::JumpOnNotEqual => f.write_str("jne"),
            ConditionalJump::JumpOnNotLess => f.write_str("jnl"),
            ConditionalJump::JumpOnNotLessOrEqual => f.write_str("jnle"),
            ConditionalJump::JumpOnNotBelow => f.write_str("jnb"),
            ConditionalJump::JumpOnNotBelowOrEqual => f.write_str("jnbe"),
            ConditionalJump::JumpOnNotPar => f.write_str("jnp"),
            ConditionalJump::JumpOnNotOverflow => f.write_str("jno"),
            ConditionalJump::JumpOnNotSign => f.write_str("jns"),
            ConditionalJump::LoopCxTimes => f.write_str("loop"),
            ConditionalJump::LoopWhileZero => f.write_str("loopz"),
            ConditionalJump::LoopWhileNotZero => f.write_str("loopnz"),
            ConditionalJump::JumpOnCxZero => f.write_str("jcxz"),
        }
    }
}

impl ConditionalJump {
    fn to_instruction_operation(&self) -> InstructionOperation {
        match self {
            ConditionalJump::JumpOnEqual => InstructionOperation::JE,
            ConditionalJump::JumpOnLess => InstructionOperation::JL,
            ConditionalJump::JumpOnLessOrEqual => InstructionOperation::JLE,
            ConditionalJump::JumpOnBelow => InstructionOperation::JB,
            ConditionalJump::JumpOnBelowOrEqual => InstructionOperation::JBE,
            ConditionalJump::JumpOnParity => InstructionOperation::JP,
            ConditionalJump::JumpOnOverflow => InstructionOperation::JO,
            ConditionalJump::JumpOnSign => InstructionOperation::JS,
            ConditionalJump::JumpOnNotEqual => InstructionOperation::JNE,
            ConditionalJump::JumpOnNotLess => InstructionOperation::JNL,
            ConditionalJump::JumpOnNotLessOrEqual => InstructionOperation::JNLE,
            ConditionalJump::JumpOnNotBelow => InstructionOperation::JNB,
            ConditionalJump::JumpOnNotBelowOrEqual => InstructionOperation::JNBE,
            ConditionalJump::JumpOnNotPar => InstructionOperation::JNP,
            ConditionalJump::JumpOnNotOverflow => InstructionOperation::JNO,
            ConditionalJump::JumpOnNotSign => InstructionOperation::JNS,
            ConditionalJump::LoopCxTimes => InstructionOperation::LOOP,
            ConditionalJump::LoopWhileZero => InstructionOperation::LOOPZ,
            ConditionalJump::LoopWhileNotZero => InstructionOperation::LOOPNZ,
            ConditionalJump::JumpOnCxZero => InstructionOperation::JCXZ,
        }
    }
}

impl Register {
    fn from_reg_w(reg: u8, w: bool) -> DecoderResult<Self> {
        Ok(match (reg, w) {
            (0, false) => Register::AL,
            (1, false) => Register::CL,
            (2, false) => Register::DL,
            (3, false) => Register::BL,
            (4, false) => Register::AH,
            (5, false) => Register::CH,
            (6, false) => Register::DH,
            (7, false) => Register::BH,
            (0, true) => Register::AX,
            (1, true) => Register::CX,
            (2, true) => Register::DX,
            (3, true) => Register::BX,
            (4, true) => Register::SP,
            (5, true) => Register::BP,
            (6, true) => Register::SI,
            (7, true) => Register::DI,
            _ => {
                return Err(DecoderError::GenericError(format!(
                    "Cannot decode reg value: {reg} to register"
                )))
            }
        })
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
enum EffectiveAddressCalculation {
    BX_SI,
    BX_DI,
    BP_SI,
    BP_DI,
    SI,
    DI,
    BP,
    BX,
}

impl EffectiveAddressCalculation {
    fn first_reg(&self) -> Option<Register> {
        Some(match self {
            EffectiveAddressCalculation::BX_SI => Register::BX,
            EffectiveAddressCalculation::BX_DI => Register::BX,
            EffectiveAddressCalculation::BP_SI => Register::BP,
            EffectiveAddressCalculation::BP_DI => Register::BP,
            EffectiveAddressCalculation::SI => Register::SI,
            EffectiveAddressCalculation::DI => Register::DI,
            EffectiveAddressCalculation::BP => Register::BP,
            EffectiveAddressCalculation::BX => Register::BX,
        })
    }

    fn second_reg(&self) -> Option<Register> {
        match self {
            EffectiveAddressCalculation::BX_SI => Some(Register::SI),
            EffectiveAddressCalculation::BX_DI => Some(Register::DI),
            EffectiveAddressCalculation::BP_SI => Some(Register::SI),
            EffectiveAddressCalculation::BP_DI => Some(Register::DI),
            EffectiveAddressCalculation::SI => None,
            EffectiveAddressCalculation::DI => None,
            EffectiveAddressCalculation::BP => None,
            EffectiveAddressCalculation::BX => None,
        }
    }

    fn from_rm(rm: u8) -> DecoderResult<Self> {
        Ok(match rm {
            0 => EffectiveAddressCalculation::BX_SI,
            1 => EffectiveAddressCalculation::BX_DI,
            2 => EffectiveAddressCalculation::BP_SI,
            3 => EffectiveAddressCalculation::BP_DI,
            4 => EffectiveAddressCalculation::SI,
            5 => EffectiveAddressCalculation::DI,
            6 => EffectiveAddressCalculation::BP,
            7 => EffectiveAddressCalculation::BX,
            _ => {
                return Err(DecoderError::GenericError(format!(
                    "Cannot decode r/m value: {rm} to effective address"
                )));
            }
        })
    }
}

#[allow(non_camel_case_types, dead_code)]
#[derive(Debug, Clone, Copy)]
enum SegmentRegister {
    ES,
    CS,
    SS,
    DS,
}

impl SegmentRegister {
    #[allow(dead_code)]
    fn from_sr(sr: u8) -> Self {
        match sr {
            0 => SegmentRegister::ES,
            1 => SegmentRegister::CS,
            2 => SegmentRegister::SS,
            3 => SegmentRegister::DS,
            _ => unreachable!(),
        }
    }
}

impl Display for SegmentRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SegmentRegister::ES => f.write_str("es"),
            SegmentRegister::CS => f.write_str("cs"),
            SegmentRegister::SS => f.write_str("ss"),
            SegmentRegister::DS => f.write_str("ds"),
        }
    }
}

impl ArithOperation {
    fn from_number(number: u8) -> DecoderResult<Self> {
        Ok(match number {
            0 => ArithOperation::ADD,
            5 => ArithOperation::SUB,
            7 => ArithOperation::CMP,
            _ => {
                return Err(DecoderError::GenericError(format!(
                    "Cannot decode arithmetic operation with value: {number}"
                )));
            }
        })
    }

    fn to_instruction_operation(&self) -> InstructionOperation {
        match self {
            ArithOperation::ADD => InstructionOperation::ADD,
            ArithOperation::SUB => InstructionOperation::SUB,
            ArithOperation::CMP => InstructionOperation::CMP,
        }
    }
}

#[derive(Debug)]
pub enum DecoderError {
    IoError(std::io::Error),
    GenericError(String),
}

impl std::fmt::Display for DecoderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecoderError::IoError(io_err) => write!(f, "{}", io_err.to_string()),
            DecoderError::GenericError(err_msg) => write!(f, "{err_msg}"),
        }
    }
}

impl From<std::io::Error> for DecoderError {
    fn from(value: std::io::Error) -> Self {
        DecoderError::IoError(value)
    }
}

const MOV_REGMEM_TO_REGMEM_OPCODE_MASK: u8 = 0xFC;
const MOV_REGMEM_TO_REGMEM_OPCODE: u8 = 0x88;

const MOV_IMMEDIATE_TO_REGMEM_OPCODE_MASK: u8 = 0xFE;
const MOV_IMMEDIATE_TO_REGMEM_OPCODE: u8 = 0xC6;

const MOV_IMMEDIATE_TO_REG_OPCODE_MASK: u8 = 0xF0;
const MOV_IMMEDIATE_TO_REG_OPCODE: u8 = 0xB0;

const MOV_ACCMEM_TO_ACCMEM_OPCODE_MASK: u8 = 0xFC;
const MOV_ACCMEM_TO_ACCMEM_OPCODE: u8 = 0xA0;

const MOV_REGMEMSR_TO_REGMEMSR_OPCODE_MASK: u8 = 0xFD;
const MOV_REGMEMSR_TO_REGMEMSR_OPCODE: u8 = 0x8C;

const ARITH_REGMEM_WITH_REG_TO_EITHER_OPCODE_MASK: u8 = 0xC4;
const ARITH_REGMEM_WITH_REG_TO_EITHER_OPCODE: u8 = 0x00;

const ARITH_IMMEDIATE_WITH_REGMEM_OPCODE_MASK: u8 = 0xFC;
const ARITH_IMMEDIATE_WITH_REGMEM_OPCODE: u8 = 0x80;

const ARITH_IMMEDIATE_TO_ACC_OPCODE_MASK: u8 = 0xC6;
const ARITH_IMMEDIATE_TO_ACC_OPCODE: u8 = 0x04;

const JUMP_ON_EQUAL_OPCODE: u8 = 0x74;
const JUMP_ON_LESS_OPCODE: u8 = 0x7C;
const JUMP_ON_LESS_OR_EQUAL_OPCODE: u8 = 0x7E;
const JUMP_ON_BELOW_OPCODE: u8 = 0x72;
const JUMP_ON_BELOW_OR_EQUAL_OPCODE: u8 = 0x76;
const JUMP_ON_PARITY_OPCODE: u8 = 0x7A;
const JUMP_ON_OVERFLOW_OPCODE: u8 = 0x70;
const JUMP_ON_SIGN_OPCODE: u8 = 0x78;
const JUMP_ON_NOT_EQUAL_OPCODE: u8 = 0x75;
const JUMP_ON_NOT_LESS_OPCODE: u8 = 0x7D;
const JUMP_ON_NOT_LESS_OR_EQUAL_OPCODE: u8 = 0x7F;
const JUMP_ON_NOT_BELOW_OPCODE: u8 = 0x73;
const JUMP_ON_NOT_BELOW_OR_EQUAL_OPCODE: u8 = 0x77;
const JUMP_ON_NOT_PAR_OPCODE: u8 = 0x7B;
const JUMP_ON_NOT_OVERFLOW_OPCODE: u8 = 0x71;
const JUMP_ON_NOT_SIGN_OPCODE: u8 = 0x79;
const LOOP_CX_TIMES_OPCODE: u8 = 0xE2;
const LOOP_WHILE_ZERO_OPCODE: u8 = 0xE1;
const LOOP_WHILE_NOT_ZERO_OPCODE: u8 = 0xE0;
const JUMP_ON_CX_ZERO_OPCODE: u8 = 0xE3;

const D_MASK: u8 = 0x02;
const D_REG_DST: u8 = 0x02;

const W_MASK: u8 = 0x01;
const W_WORD: u8 = 0x01;

const S_MASK: u8 = 0x02;
const S_SIGN: u8 = 0x02;

const MOD_MASK: u8 = 0xC0;
const MOD_REG: u8 = 0xC0;
const MOD_MEM_0: u8 = 0x00;
#[allow(dead_code)]
const MOD_MEM_1: u8 = 0x40;
const MOD_MEM_2: u8 = 0x80;

const REG_MASK: u8 = 0x38;
const REG_BIT_OFFSET: u8 = 3;

const RM_MASK: u8 = 0x07;
const RM_BIT_OFFSET: u8 = 0;
const RM_VALUE_DIRECT_ACCESS: u8 = 0x06;

#[allow(dead_code)]
const SR_MASK: u8 = 0x18;
#[allow(dead_code)]
const SR_OFFSET: u8 = 3;

const ARITHMETIC_OPERATION_MASK: u8 = 0x38;
const ARITHMETIC_OPERATION_OFFSET: u8 = 3;

#[derive(Debug)]
pub struct Decoder {
    state: DecoderState,
}

impl Decoder {
    pub fn new() -> Decoder {
        Decoder {
            state: DecoderState::Init,
        }
    }

    fn decode_init(
        &mut self,
        instruction_byte: u8,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok((
            if instruction_byte & MOV_REGMEM_TO_REGMEM_OPCODE_MASK == MOV_REGMEM_TO_REGMEM_OPCODE {
                let d = instruction_byte & D_MASK;
                let w = instruction_byte & W_MASK;
                DecoderState::MovRegMemToFromReg {
                    reg_is_dest: d == D_REG_DST,
                    is_word: w == W_WORD,
                    state: MovRegMemToFromRegState::Init,
                }
            } else if instruction_byte & MOV_IMMEDIATE_TO_REGMEM_OPCODE_MASK
                == MOV_IMMEDIATE_TO_REGMEM_OPCODE
            {
                let w = instruction_byte & W_MASK;
                DecoderState::MovImmToRegMem {
                    is_word: w == W_WORD,
                    state: MovImmToRegMemState::Init,
                }
            } else if instruction_byte & MOV_IMMEDIATE_TO_REG_OPCODE_MASK
                == MOV_IMMEDIATE_TO_REG_OPCODE
            {
                let is_word = 0x1 == ((instruction_byte.clone() >> 3) & 0x1);

                let reg = Register::from_reg_w(instruction_byte.clone() & 0x7, is_word)?;
                DecoderState::MovImmToRegMem {
                    is_word,
                    state: MovImmToRegMemState::ToRegReadLowImm { reg },
                }
            } else if instruction_byte & MOV_ACCMEM_TO_ACCMEM_OPCODE_MASK
                == MOV_ACCMEM_TO_ACCMEM_OPCODE
            {
                let acc_to_mem = 0x2 == (instruction_byte.clone() & 0x2);
                let is_word = (instruction_byte & W_MASK) == W_WORD;

                DecoderState::MovAccMemToAccMem {
                    acc_to_mem,
                    is_word,
                    state: MovAccMemToAccMemState::ReadAddrLo,
                }
            } else if instruction_byte & MOV_REGMEMSR_TO_REGMEMSR_OPCODE_MASK
                == MOV_REGMEMSR_TO_REGMEMSR_OPCODE
            {
                let to_sr = 0x2 == (instruction_byte.clone() & 0x2);

                DecoderState::MovRegMemSRToRegMemSR {
                    to_sr,
                    state: MovRegMemSRToRegMemSRState::Init,
                }
            } else if instruction_byte & ARITH_REGMEM_WITH_REG_TO_EITHER_OPCODE_MASK
                == ARITH_REGMEM_WITH_REG_TO_EITHER_OPCODE
            {
                let op = ArithOperation::from_number(
                    (instruction_byte.clone() & ARITHMETIC_OPERATION_MASK)
                        >> ARITHMETIC_OPERATION_OFFSET,
                )?;
                let d = instruction_byte & D_MASK;
                let w = instruction_byte & W_MASK;
                let reg_is_dest = d == D_REG_DST;
                let is_word = w == W_WORD;

                DecoderState::ArithRegMemWithRegToEither {
                    op,
                    reg_is_dest,
                    is_word,
                    state: ArithRegMemWithRegToEitherState::Init,
                }
            } else if instruction_byte & ARITH_IMMEDIATE_WITH_REGMEM_OPCODE_MASK
                == ARITH_IMMEDIATE_WITH_REGMEM_OPCODE
            {
                let s = instruction_byte & S_MASK;
                let w = instruction_byte & W_MASK;
                let signed = s == S_SIGN;
                let is_word = w == W_WORD;

                DecoderState::ArithImmWithRegMem {
                    signed,
                    is_word,
                    state: ArithImmWithRegMemState::Init,
                }
            } else if instruction_byte & ARITH_IMMEDIATE_TO_ACC_OPCODE_MASK
                == ARITH_IMMEDIATE_TO_ACC_OPCODE
            {
                let op = ArithOperation::from_number(
                    (instruction_byte.clone() & ARITHMETIC_OPERATION_MASK)
                        >> ARITHMETIC_OPERATION_OFFSET,
                )?;

                let w = instruction_byte & W_MASK;
                let is_word = w == W_WORD;

                DecoderState::ArithImmToAcc {
                    op,
                    is_word,
                    state: ArithImmToAccState::Init,
                }
            } else {
                match instruction_byte {
                    JUMP_ON_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnEqual,
                    },
                    JUMP_ON_LESS_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnLess,
                    },
                    JUMP_ON_LESS_OR_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnLessOrEqual,
                    },
                    JUMP_ON_BELOW_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnBelow,
                    },
                    JUMP_ON_BELOW_OR_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnBelowOrEqual,
                    },
                    JUMP_ON_PARITY_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnParity,
                    },
                    JUMP_ON_OVERFLOW_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnOverflow,
                    },
                    JUMP_ON_SIGN_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnSign,
                    },
                    JUMP_ON_NOT_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotEqual,
                    },
                    JUMP_ON_NOT_LESS_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotLess,
                    },
                    JUMP_ON_NOT_LESS_OR_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotLessOrEqual,
                    },
                    JUMP_ON_NOT_BELOW_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotBelow,
                    },
                    JUMP_ON_NOT_BELOW_OR_EQUAL_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotBelowOrEqual,
                    },
                    JUMP_ON_NOT_PAR_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotPar,
                    },
                    JUMP_ON_NOT_OVERFLOW_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotOverflow,
                    },
                    JUMP_ON_NOT_SIGN_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnNotSign,
                    },
                    LOOP_CX_TIMES_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::LoopCxTimes,
                    },
                    LOOP_WHILE_ZERO_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::LoopWhileZero,
                    },
                    LOOP_WHILE_NOT_ZERO_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::LoopWhileNotZero,
                    },
                    JUMP_ON_CX_ZERO_OPCODE => DecoderState::CondJump {
                        jmp: ConditionalJump::JumpOnCxZero,
                    },
                    _ => return Err(DecoderError::GenericError(String::from("unknown opcode"))),
                }
            },
            None,
        ))
    }

    fn decode_move_mem_to_from_reg(
        &mut self,
        instruction_byte: u8,
        reg_is_dest: bool,
        is_word: bool,
        state: MovRegMemToFromRegState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            MovRegMemToFromRegState::Init => match instruction_byte & MOD_MASK {
                MOD_REG => {
                    let reg = Register::from_reg_w(
                        (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                        is_word,
                    )?;
                    let rm = Register::from_reg_w(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        is_word,
                    )?;
                    let inst;
                    if reg_is_dest {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Register(rm),
                            Some(DstOperand::Register(reg)),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Register(reg),
                            Some(DstOperand::Register(rm)),
                            None,
                        ));
                    }
                    (DecoderState::Init, inst)
                }
                MOD_MEM_0 => {
                    if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS {
                        let reg = Register::from_reg_w(
                            (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                            is_word,
                        )?;
                        (
                            DecoderState::MovRegMemToFromReg {
                                reg_is_dest,
                                is_word,
                                state: MovRegMemToFromRegState::DirectAddressReadLowDisp { reg },
                            },
                            None,
                        )
                    } else {
                        let reg = Register::from_reg_w(
                            (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                            is_word,
                        )?;
                        let rm = EffectiveAddressCalculation::from_rm(
                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        )?;
                        let inst;
                        if reg_is_dest {
                            inst = Some(Instruction::new(
                                InstructionOperation::MOV,
                                SrcOperand::MemoryAddressing(MemoryAddressing::new(
                                    rm.first_reg(),
                                    rm.second_reg(),
                                    0,
                                )),
                                Some(DstOperand::Register(reg)),
                                None,
                            ));
                        } else {
                            inst = Some(Instruction::new(
                                InstructionOperation::MOV,
                                SrcOperand::Register(reg),
                                Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                    rm.first_reg(),
                                    rm.second_reg(),
                                    0,
                                ))),
                                None,
                            ));
                        }
                        (DecoderState::Init, inst)
                    }
                }
                mode => {
                    let reg = Register::from_reg_w(
                        (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                        is_word,
                    )?;
                    let effect_addr = EffectiveAddressCalculation::from_rm(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                    )?;
                    (
                        DecoderState::MovRegMemToFromReg {
                            reg_is_dest,
                            is_word,
                            state: MovRegMemToFromRegState::EffectAddrReadLowDisp {
                                reg,
                                effect_addr,
                                is_disp16: mode == MOD_MEM_2,
                            },
                        },
                        None,
                    )
                }
            },
            MovRegMemToFromRegState::DirectAddressReadLowDisp { reg } => (
                DecoderState::MovRegMemToFromReg {
                    reg_is_dest,
                    is_word,
                    state: MovRegMemToFromRegState::DirectAddressReadHighDisp {
                        reg: reg.clone(),
                        low_disp: instruction_byte.clone(),
                    },
                },
                None,
            ),
            MovRegMemToFromRegState::DirectAddressReadHighDisp { reg, low_disp } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp.to_owned() as u16);
                let inst;
                if reg_is_dest {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::MemoryAddressing(MemoryAddressing::new(None, None, disp)),
                        Some(DstOperand::Register(reg)),
                        None,
                    ));
                } else {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Register(reg),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            None, None, disp,
                        ))),
                        None,
                    ));
                }
                (DecoderState::Init, inst)
            }
            MovRegMemToFromRegState::EffectAddrReadLowDisp {
                reg,
                effect_addr,
                is_disp16,
            } => {
                let low_disp: u8 = instruction_byte.clone();
                if is_disp16.to_owned() {
                    (
                        DecoderState::MovRegMemToFromReg {
                            reg_is_dest,
                            is_word,
                            state: MovRegMemToFromRegState::EffectAddrReadHighDisp {
                                reg: reg.clone(),
                                effect_addr: effect_addr.clone(),
                                low_disp,
                            },
                        },
                        None,
                    )
                } else {
                    let inst;
                    if reg_is_dest {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                low_disp as u16,
                            )),
                            Some(DstOperand::Register(reg)),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Register(reg),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                low_disp as u16,
                            ))),
                            None,
                        ));
                    }
                    (DecoderState::Init, inst)
                }
            }
            MovRegMemToFromRegState::EffectAddrReadHighDisp {
                reg,
                effect_addr,
                low_disp,
            } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp.clone() as u16);
                let inst;
                if reg_is_dest {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            disp,
                        )),
                        Some(DstOperand::Register(reg)),
                        None,
                    ));
                } else {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Register(reg),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            disp,
                        ))),
                        None,
                    ));
                }
                (DecoderState::Init, inst)
            }
        })
    }

    fn decode_mov_imm_to_reg_mem(
        &mut self,
        instruction_byte: u8,
        is_word: bool,
        state: MovImmToRegMemState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            MovImmToRegMemState::Init => match instruction_byte & MOD_MASK {
                MOD_REG => {
                    let rm = Register::from_reg_w(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        is_word,
                    )?;
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToRegReadLowImm { reg: rm },
                        },
                        None,
                    )
                }
                MOD_MEM_0 => {
                    if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS {
                        (
                            DecoderState::MovImmToRegMem {
                                is_word,
                                state: MovImmToRegMemState::ToDirectAddrReadLowDisp,
                            },
                            None,
                        )
                    } else {
                        let effect_addr = EffectiveAddressCalculation::from_rm(
                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        )?;
                        (
                            DecoderState::MovImmToRegMem {
                                is_word,
                                state: MovImmToRegMemState::ToEffectAddrReadLowImm {
                                    effect_addr,
                                    disp: None,
                                },
                            },
                            None,
                        )
                    }
                }
                mode => {
                    let effect_addr = EffectiveAddressCalculation::from_rm(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                    )?;
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToEffectAddrReadLowDisp {
                                effect_addr,
                                is_disp16: mode == MOD_MEM_2,
                            },
                        },
                        None,
                    )
                }
            },
            MovImmToRegMemState::ToRegReadLowImm { reg } => {
                let low_imm = instruction_byte.clone();
                if is_word {
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToRegReadHighImm { reg, low_imm },
                        },
                        None,
                    )
                } else {
                    let imm = if low_imm & 0x80 == 0x80 {
                        (0xFF00 | (low_imm as u16)) as i16
                    } else {
                        low_imm as i16
                    };
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Immediate(imm),
                            Some(DstOperand::Register(reg)),
                            None,
                        )),
                    )
                }
            }
            MovImmToRegMemState::ToRegReadHighImm { reg, low_imm } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                (
                    DecoderState::Init,
                    Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::Register(reg)),
                        None,
                    )),
                )
            }
            MovImmToRegMemState::ToDirectAddrReadLowDisp => {
                let low_disp = instruction_byte.clone();
                (
                    DecoderState::MovImmToRegMem {
                        is_word,
                        state: MovImmToRegMemState::ToDirectAddrReadHighDisp { low_disp },
                    },
                    None,
                )
            }
            MovImmToRegMemState::ToDirectAddrReadHighDisp { low_disp } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                (
                    DecoderState::MovImmToRegMem {
                        is_word,
                        state: MovImmToRegMemState::ToDirectAddrReadLowImm { disp },
                    },
                    None,
                )
            }
            MovImmToRegMemState::ToDirectAddrReadLowImm { disp } => {
                let low_imm = instruction_byte.clone();
                if is_word {
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToDirectAddrReadHighImm { disp, low_imm },
                        },
                        None,
                    )
                } else {
                    let imm8 = if low_imm & 0x80 == 0x80 {
                        ((0xFF00 as u16) | (low_imm as u16)) as i16
                    } else {
                        low_imm as i16
                    };
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Immediate(imm8 as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                None, None, disp,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        )),
                    )
                }
            }
            MovImmToRegMemState::ToDirectAddrReadHighImm { disp, low_imm } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                (
                    DecoderState::Init,
                    Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            None, None, disp,
                        ))),
                        Some(SizeSpecifier::WORD),
                    )),
                )
            }
            MovImmToRegMemState::ToEffectAddrReadLowDisp {
                effect_addr,
                is_disp16,
            } => {
                let low_disp = instruction_byte.clone();
                if is_disp16 {
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToEffectAddrReadHighDisp {
                                effect_addr,
                                low_disp,
                            },
                        },
                        None,
                    )
                } else {
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToEffectAddrReadLowImm {
                                effect_addr,
                                disp: Some(low_disp as i16),
                            },
                        },
                        None,
                    )
                }
            }
            MovImmToRegMemState::ToEffectAddrReadHighDisp {
                effect_addr,
                low_disp,
            } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                (
                    DecoderState::MovImmToRegMem {
                        is_word,
                        state: MovImmToRegMemState::ToEffectAddrReadLowImm {
                            effect_addr,
                            disp: Some(disp as i16),
                        },
                    },
                    None,
                )
            }
            MovImmToRegMemState::ToEffectAddrReadLowImm { effect_addr, disp } => {
                let low_imm = instruction_byte.clone();
                if is_word {
                    (
                        DecoderState::MovImmToRegMem {
                            is_word,
                            state: MovImmToRegMemState::ToEffectAddrReadHighImm {
                                effect_addr,
                                disp,
                                low_imm,
                            },
                        },
                        None,
                    )
                } else {
                    let inst;
                    if let Some(disp) = disp {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                disp as u16,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                0,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        ));
                    }
                    (DecoderState::Init, inst)
                }
            }
            MovImmToRegMemState::ToEffectAddrReadHighImm {
                effect_addr,
                disp,
                low_imm,
            } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                let inst;
                if let Some(disp) = disp {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            disp as u16,
                        ))),
                        Some(SizeSpecifier::BYTE),
                    ));
                } else {
                    inst = Some(Instruction::new(
                        InstructionOperation::MOV,
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            0,
                        ))),
                        Some(SizeSpecifier::BYTE),
                    ));
                }
                (DecoderState::Init, inst)
            }
        })
    }

    fn decode_mov_acc_mem_to_acc_mem(
        &mut self,
        instruction_byte: u8,
        acc_to_mem: bool,
        is_word: bool,
        state: MovAccMemToAccMemState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            MovAccMemToAccMemState::ReadAddrLo => {
                let addr_lo = instruction_byte.clone();
                (
                    DecoderState::MovAccMemToAccMem {
                        acc_to_mem,
                        is_word,
                        state: MovAccMemToAccMemState::ReadAddrHigh { addr_lo },
                    },
                    None,
                )
            }
            MovAccMemToAccMemState::ReadAddrHigh { addr_lo } => {
                let addr = ((instruction_byte.clone() as u16) << 8) | (addr_lo as u16);
                let inst;
                if acc_to_mem {
                    if is_word {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Register(Register::AX),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                None, None, addr,
                            ))),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::Register(Register::AL),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                None, None, addr,
                            ))),
                            None,
                        ));
                    }
                } else {
                    if is_word {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::MemoryAddressing(MemoryAddressing::new(None, None, addr)),
                            Some(DstOperand::Register(Register::AX)),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            InstructionOperation::MOV,
                            SrcOperand::MemoryAddressing(MemoryAddressing::new(None, None, addr)),
                            Some(DstOperand::Register(Register::AL)),
                            None,
                        ));
                    }
                }
                (DecoderState::Init, inst)
            }
        })
    }

    //fn decode_mov_reg_mem_srto_reg_mem_sr(
    //    &mut self,
    //    instruction_byte: u8,
    //    to_sr: bool,
    //    state: MovRegMemSRToRegMemSRState,
    //    out: &mut Vec<Instruction>,
    //) -> DecoderResult<DecoderState> {
    //    Ok(match state {
    //        MovRegMemSRToRegMemSRState::Init => {
    //            let sr = SegmentRegister::from_sr((instruction_byte & SR_MASK) >> SR_OFFSET);
    //            match instruction_byte & MOD_MASK {
    //                MOD_REG => {
    //                    let rm = Register::from_reg_w(
    //                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
    //                        true,
    //                    )?;
    //                    if to_sr {
    //                        out.write_all(format!("mov {sr}, {rm}\n").as_bytes())?;
    //                    } else {
    //                        out.write_all(format!("mov {rm}, {sr}\n").as_bytes())?;
    //                    }
    //                    DecoderState::Init
    //                }
    //                MOD_MEM_0 => {
    //                    if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS
    //                    {
    //                        DecoderState::MovRegMemSRToRegMemSR {
    //                            to_sr,
    //                            state: MovRegMemSRToRegMemSRState::ToFromDirectAddrReadLowDisp {
    //                                sr,
    //                            },
    //                        }
    //                    } else {
    //                        let effective_addr_calc = EffectiveAddressCalculation::from_rm(
    //                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
    //                        )?;
    //                        if to_sr {
    //                            out.write_all(
    //                                format!(
    //                                    "mov {sr}, {}\n",
    //                                    effective_addr_calc.to_string_without_disp()
    //                                )
    //                                .as_bytes(),
    //                            )?;
    //                        } else {
    //                            out.write_all(
    //                                format!(
    //                                    "mov {}, {sr}\n",
    //                                    effective_addr_calc.to_string_without_disp()
    //                                )
    //                                .as_bytes(),
    //                            )?;
    //                        }
    //                        DecoderState::Init
    //                    }
    //                }
    //                mode => {
    //                    let effect_addr = EffectiveAddressCalculation::from_rm(
    //                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
    //                    )?;
    //                    DecoderState::MovRegMemSRToRegMemSR {
    //                        to_sr,
    //                        state: MovRegMemSRToRegMemSRState::ToFromEffectAddrReadLowDisp {
    //                            sr,
    //                            effect_addr,
    //                            is_disp16: mode == MOD_MEM_2,
    //                        },
    //                    }
    //                }
    //            }
    //        }
    //        MovRegMemSRToRegMemSRState::ToFromDirectAddrReadLowDisp { sr } => {
    //            let low_disp = instruction_byte.clone();
    //            DecoderState::MovRegMemSRToRegMemSR {
    //                to_sr,
    //                state: MovRegMemSRToRegMemSRState::ToFromDirectAddrReadHighDisp {
    //                    sr,
    //                    low_disp,
    //                },
    //            }
    //        }
    //        MovRegMemSRToRegMemSRState::ToFromDirectAddrReadHighDisp { sr, low_disp } => {
    //            let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
    //            if to_sr {
    //                out.write_all(format!("mov {sr}, [{disp}]\n").as_bytes())?;
    //            } else {
    //                out.write_all(format!("mov [{disp}], {sr}\n").as_bytes())?;
    //            }
    //            DecoderState::Init
    //        }
    //        MovRegMemSRToRegMemSRState::ToFromEffectAddrReadLowDisp {
    //            sr,
    //            effect_addr,
    //            is_disp16,
    //        } => {
    //            let low_disp = instruction_byte.clone();
    //            if is_disp16 {
    //                DecoderState::MovRegMemSRToRegMemSR {
    //                    to_sr,
    //                    state: MovRegMemSRToRegMemSRState::ToFromEffectAddrReadHighDisp {
    //                        sr,
    //                        low_disp,
    //                        effect_addr,
    //                    },
    //                }
    //            } else {
    //                if to_sr {
    //                    out.write_all(
    //                        format!(
    //                            "mov {sr}, {}\n",
    //                            effect_addr.to_string_with_disp8(low_disp as i8)
    //                        )
    //                        .as_bytes(),
    //                    )?;
    //                } else {
    //                    out.write_all(
    //                        format!(
    //                            "mov {}, {sr}\n",
    //                            effect_addr.to_string_with_disp8(low_disp as i8)
    //                        )
    //                        .as_bytes(),
    //                    )?;
    //                }
    //                DecoderState::Init
    //            }
    //        }
    //        MovRegMemSRToRegMemSRState::ToFromEffectAddrReadHighDisp {
    //            sr,
    //            effect_addr,
    //            low_disp,
    //        } => {
    //            let disp = (((instruction_byte.clone() as u16) << 8) | (low_disp as u16)) as i16;
    //            if to_sr {
    //                out.write_all(
    //                    format!("mov {sr}, {}\n", effect_addr.to_string_with_disp16(disp))
    //                        .as_bytes(),
    //                )?;
    //            } else {
    //                out.write_all(
    //                    format!("mov {}, {sr}\n", effect_addr.to_string_with_disp16(disp))
    //                        .as_bytes(),
    //                )?;
    //            }
    //            DecoderState::Init
    //        }
    //    })
    //}

    fn decode_arith_reg_mem_with_reg_to_either(
        &mut self,
        instruction_byte: u8,
        op: ArithOperation,
        reg_is_dest: bool,
        is_word: bool,
        state: ArithRegMemWithRegToEitherState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            ArithRegMemWithRegToEitherState::Init => match instruction_byte & MOD_MASK {
                MOD_REG => {
                    let reg = Register::from_reg_w(
                        (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                        is_word,
                    )?;
                    let rm = Register::from_reg_w(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        is_word,
                    )?;
                    let inst;
                    if reg_is_dest {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Register(rm),
                            Some(DstOperand::Register(reg)),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Register(reg),
                            Some(DstOperand::Register(rm)),
                            None,
                        ));
                    }
                    (DecoderState::Init, inst)
                }
                MOD_MEM_0 => {
                    if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS {
                        let reg = Register::from_reg_w(
                            (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                            is_word,
                        )?;
                        (
                            DecoderState::ArithRegMemWithRegToEither {
                                op,
                                reg_is_dest,
                                is_word,
                                state: ArithRegMemWithRegToEitherState::DirectAddressReadLowDisp {
                                    reg,
                                },
                            },
                            None,
                        )
                    } else {
                        let reg = Register::from_reg_w(
                            (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                            is_word,
                        )?;
                        let rm = EffectiveAddressCalculation::from_rm(
                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        )?;
                        let inst;
                        if reg_is_dest {
                            inst = Some(Instruction::new(
                                op.to_instruction_operation(),
                                SrcOperand::MemoryAddressing(MemoryAddressing::new(
                                    rm.first_reg(),
                                    rm.second_reg(),
                                    0,
                                )),
                                Some(DstOperand::Register(reg)),
                                None,
                            ));
                        } else {
                            inst = Some(Instruction::new(
                                op.to_instruction_operation(),
                                SrcOperand::Register(reg),
                                Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                    rm.first_reg(),
                                    rm.second_reg(),
                                    0,
                                ))),
                                None,
                            ));
                        }
                        (DecoderState::Init, inst)
                    }
                }
                mode => {
                    let reg = Register::from_reg_w(
                        (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                        is_word,
                    )?;
                    let effect_addr = EffectiveAddressCalculation::from_rm(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                    )?;
                    (
                        DecoderState::ArithRegMemWithRegToEither {
                            op,
                            reg_is_dest,
                            is_word,
                            state: ArithRegMemWithRegToEitherState::EffectAddrReadLowDisp {
                                reg,
                                effect_addr,
                                is_disp16: mode == MOD_MEM_2,
                            },
                        },
                        None,
                    )
                }
            },
            ArithRegMemWithRegToEitherState::DirectAddressReadLowDisp { reg } => (
                DecoderState::ArithRegMemWithRegToEither {
                    op,
                    reg_is_dest,
                    is_word,
                    state: ArithRegMemWithRegToEitherState::DirectAddressReadHighDisp {
                        reg: reg.clone(),
                        low_disp: instruction_byte.clone(),
                    },
                },
                None,
            ),
            ArithRegMemWithRegToEitherState::DirectAddressReadHighDisp { reg, low_disp } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp.to_owned() as u16);
                let inst;
                if reg_is_dest {
                    inst = Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::MemoryAddressing(MemoryAddressing::new(None, None, disp)),
                        Some(DstOperand::Register(reg)),
                        None,
                    ));
                } else {
                    inst = Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Register(reg),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            None, None, disp,
                        ))),
                        None,
                    ));
                }
                (DecoderState::Init, inst)
            }
            ArithRegMemWithRegToEitherState::EffectAddrReadLowDisp {
                reg,
                effect_addr,
                is_disp16,
            } => {
                let low_disp: u8 = instruction_byte.clone();
                if is_disp16.to_owned() {
                    (
                        DecoderState::ArithRegMemWithRegToEither {
                            op,
                            reg_is_dest,
                            is_word,
                            state: ArithRegMemWithRegToEitherState::EffectAddrReadHighDisp {
                                reg: reg.clone(),
                                effect_addr: effect_addr.clone(),
                                low_disp,
                            },
                        },
                        None,
                    )
                } else {
                    let inst;
                    if reg_is_dest {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                low_disp as u16,
                            )),
                            Some(DstOperand::Register(reg)),
                            None,
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Register(reg),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                low_disp as u16,
                            ))),
                            None,
                        ));
                    }
                    (DecoderState::Init, inst)
                }
            }
            ArithRegMemWithRegToEitherState::EffectAddrReadHighDisp {
                reg,
                effect_addr,
                low_disp,
            } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp.clone() as u16);
                if reg_is_dest {
                    Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            low_disp as u16,
                        )),
                        Some(DstOperand::Register(reg)),
                        None,
                    ));
                } else {
                    Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Register(reg),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            disp as u16,
                        ))),
                        None,
                    ));
                }
                (DecoderState::Init, None)
            }
        })
    }

    fn decode_arith_imm_with_reg_mem(
        &mut self,
        instruction_byte: u8,
        signed: bool,
        is_word: bool,
        state: ArithImmWithRegMemState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            ArithImmWithRegMemState::Init => match instruction_byte & MOD_MASK {
                MOD_REG => {
                    let rm = Register::from_reg_w(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        is_word,
                    )?;
                    let op = ArithOperation::from_number(
                        (instruction_byte & ARITHMETIC_OPERATION_MASK)
                            >> ARITHMETIC_OPERATION_OFFSET,
                    )?;
                    (
                        DecoderState::ArithImmWithRegMem {
                            is_word,
                            state: ArithImmWithRegMemState::WithRegReadLowImm { reg: rm, op },
                            signed,
                        },
                        None,
                    )
                }
                MOD_MEM_0 => {
                    if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS {
                        let op = ArithOperation::from_number(
                            (instruction_byte & ARITHMETIC_OPERATION_MASK)
                                >> ARITHMETIC_OPERATION_OFFSET,
                        )?;
                        (
                            DecoderState::ArithImmWithRegMem {
                                is_word,
                                state: ArithImmWithRegMemState::WithDirectAddrReadLowDisp { op },
                                signed,
                            },
                            None,
                        )
                    } else {
                        let effect_addr = EffectiveAddressCalculation::from_rm(
                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        )?;
                        let op = ArithOperation::from_number(
                            (instruction_byte & ARITHMETIC_OPERATION_MASK)
                                >> ARITHMETIC_OPERATION_OFFSET,
                        )?;
                        (
                            DecoderState::ArithImmWithRegMem {
                                is_word,
                                state: ArithImmWithRegMemState::WithEffectAddrReadLowImm {
                                    effect_addr,
                                    disp: None,
                                    op,
                                },
                                signed,
                            },
                            None,
                        )
                    }
                }
                mode => {
                    let effect_addr = EffectiveAddressCalculation::from_rm(
                        (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                    )?;
                    let op = ArithOperation::from_number(
                        (instruction_byte & ARITHMETIC_OPERATION_MASK)
                            >> ARITHMETIC_OPERATION_OFFSET,
                    )?;
                    (
                        DecoderState::ArithImmWithRegMem {
                            is_word,
                            state: ArithImmWithRegMemState::WithEffectAddrReadLowDisp {
                                op,
                                effect_addr,
                                is_disp16: mode == MOD_MEM_2,
                            },
                            signed,
                        },
                        None,
                    )
                }
            },
            ArithImmWithRegMemState::WithRegReadLowImm { op, reg } => {
                let low_imm = instruction_byte.clone();
                if is_word && signed {
                    let imm_sext = if low_imm & 0x80 == 0x80 {
                        (0xFF00 | (low_imm as u16)) as i16
                    } else {
                        low_imm as i16
                    };
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(imm_sext),
                            Some(DstOperand::Register(reg)),
                            None,
                        )),
                    )
                } else if is_word {
                    (
                        DecoderState::ArithImmWithRegMem {
                            is_word,
                            state: ArithImmWithRegMemState::WithRegReadHighImm { reg, low_imm, op },
                            signed,
                        },
                        None,
                    )
                } else {
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::Register(reg)),
                            None,
                        )),
                    )
                }
            }
            ArithImmWithRegMemState::WithRegReadHighImm { op, reg, low_imm } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                (
                    DecoderState::Init,
                    Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::Register(reg)),
                        None,
                    )),
                )
            }
            ArithImmWithRegMemState::WithDirectAddrReadLowDisp { op } => {
                let low_disp = instruction_byte.clone();
                (
                    DecoderState::ArithImmWithRegMem {
                        signed,
                        is_word,
                        state: ArithImmWithRegMemState::WithDirectAddrReadHighDisp { op, low_disp },
                    },
                    None,
                )
            }
            ArithImmWithRegMemState::WithDirectAddrReadHighDisp { op, low_disp } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                (
                    DecoderState::ArithImmWithRegMem {
                        signed,
                        is_word,
                        state: ArithImmWithRegMemState::WithDirectAddrReadLowImm { op, disp },
                    },
                    None,
                )
            }
            ArithImmWithRegMemState::WithDirectAddrReadLowImm { op, disp } => {
                let low_imm = instruction_byte.clone();
                if is_word && signed {
                    let imm_sext = if low_imm & 0x80 == 0x80 {
                        (0xFF00 | (low_imm as u16)) as i16
                    } else {
                        low_imm as i16
                    };
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(imm_sext),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                None, None, disp,
                            ))),
                            Some(SizeSpecifier::WORD),
                        )),
                    )
                } else if is_word {
                    (
                        DecoderState::ArithImmWithRegMem {
                            signed,
                            is_word,
                            state: ArithImmWithRegMemState::WithDirectAddrReadHighImm {
                                op,
                                disp,
                                low_imm,
                            },
                        },
                        None,
                    )
                } else {
                    let imm8 = low_imm as i8;
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(imm8 as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                None, None, disp,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        )),
                    )
                }
            }
            ArithImmWithRegMemState::WithDirectAddrReadHighImm { op, disp, low_imm } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                (
                    DecoderState::Init,
                    Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            None, None, disp,
                        ))),
                        Some(SizeSpecifier::WORD),
                    )),
                )
            }
            ArithImmWithRegMemState::WithEffectAddrReadLowDisp {
                op,
                effect_addr,
                is_disp16,
            } => {
                let low_disp = instruction_byte.clone();
                if is_disp16 {
                    (
                        DecoderState::ArithImmWithRegMem {
                            signed,
                            is_word,
                            state: ArithImmWithRegMemState::WithEffectAddrReadHighDisp {
                                op,
                                effect_addr,
                                low_disp,
                            },
                        },
                        None,
                    )
                } else {
                    (
                        DecoderState::ArithImmWithRegMem {
                            signed,
                            is_word,
                            state: ArithImmWithRegMemState::WithEffectAddrReadLowImm {
                                op,
                                effect_addr,
                                disp: Some(low_disp as i16),
                            },
                        },
                        None,
                    )
                }
            }
            ArithImmWithRegMemState::WithEffectAddrReadHighDisp {
                op,
                effect_addr,
                low_disp,
            } => {
                let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                (
                    DecoderState::ArithImmWithRegMem {
                        signed,
                        is_word,
                        state: ArithImmWithRegMemState::WithEffectAddrReadLowImm {
                            op,
                            effect_addr,
                            disp: Some(disp as i16),
                        },
                    },
                    None,
                )
            }
            ArithImmWithRegMemState::WithEffectAddrReadLowImm {
                op,
                effect_addr,
                disp,
            } => {
                let low_imm = instruction_byte.clone();
                if is_word && signed {
                    let imm_sext = if low_imm & 0x80 == 0x80 {
                        (0xFF00 | (low_imm as u16)) as i16
                    } else {
                        low_imm as i16
                    };
                    let inst;
                    if let Some(disp) = disp {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(imm_sext),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                disp as u16,
                            ))),
                            Some(SizeSpecifier::WORD),
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(imm_sext),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                0,
                            ))),
                            Some(SizeSpecifier::WORD),
                        ));
                    }
                    (DecoderState::Init, inst)
                } else if is_word {
                    (
                        DecoderState::ArithImmWithRegMem {
                            signed,
                            is_word,
                            state: ArithImmWithRegMemState::WithEffectAddrReadHighImm {
                                op,
                                effect_addr,
                                disp,
                                low_imm,
                            },
                        },
                        None,
                    )
                } else {
                    let inst;
                    if let Some(disp) = disp {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                disp as u16,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        ));
                    } else {
                        inst = Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                                effect_addr.first_reg(),
                                effect_addr.second_reg(),
                                0,
                            ))),
                            Some(SizeSpecifier::BYTE),
                        ));
                    }
                    (DecoderState::Init, inst)
                }
            }
            ArithImmWithRegMemState::WithEffectAddrReadHighImm {
                op,
                effect_addr,
                disp,
                low_imm,
            } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                let inst;
                if let Some(disp) = disp {
                    inst = Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            disp as u16,
                        ))),
                        Some(SizeSpecifier::WORD),
                    ));
                } else {
                    inst = Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::MemoryAddressing(MemoryAddressing::new(
                            effect_addr.first_reg(),
                            effect_addr.second_reg(),
                            0,
                        ))),
                        Some(SizeSpecifier::WORD),
                    ));
                }
                (DecoderState::Init, inst)
            }
        })
    }

    fn decode_arith_imm_to_acc(
        &mut self,
        instruction_byte: u8,
        op: ArithOperation,
        is_word: bool,
        state: ArithImmToAccState,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        Ok(match state {
            ArithImmToAccState::Init => {
                let low_imm = instruction_byte;
                if is_word {
                    (
                        DecoderState::ArithImmToAcc {
                            op,
                            is_word,
                            state: ArithImmToAccState::ReadHighImm { low_imm },
                        },
                        None,
                    )
                } else {
                    (
                        DecoderState::Init,
                        Some(Instruction::new(
                            op.to_instruction_operation(),
                            SrcOperand::Immediate(low_imm as i16),
                            Some(DstOperand::Register(Register::AL)),
                            None,
                        )),
                    )
                }
            }
            ArithImmToAccState::ReadHighImm { low_imm } => {
                let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                (
                    DecoderState::Init,
                    Some(Instruction::new(
                        op.to_instruction_operation(),
                        SrcOperand::Immediate(imm16 as i16),
                        Some(DstOperand::Register(Register::AX)),
                        None,
                    )),
                )
            }
        })
    }

    fn decode_conditional_jump(
        &mut self,
        instruction_byte: u8,
        jmp: ConditionalJump,
    ) -> DecoderResult<(DecoderState, Option<Instruction>)> {
        let ip_inc8 = instruction_byte as i8;
        Ok((
            DecoderState::Init,
            Some(Instruction::new(
                jmp.to_instruction_operation(),
                SrcOperand::RelativePosition(ip_inc8),
                None,
                None,
            )),
        ))
    }

    pub fn decode(
        &mut self,
        memory: &[u8],
        instruction_pointer: usize,
    ) -> DecoderResult<(Option<Instruction>, usize)> {
        let mut ip = instruction_pointer;
        loop {
            if ip >= memory.len() {
                break;
            }
            let instruction_byte = memory[ip];
            let inst;
            (self.state, inst) = match self.state {
                DecoderState::Init => self.decode_init(instruction_byte.clone())?,
                DecoderState::MovRegMemToFromReg {
                    reg_is_dest,
                    is_word,
                    state,
                } => self.decode_move_mem_to_from_reg(
                    instruction_byte.clone(),
                    reg_is_dest,
                    is_word,
                    state,
                )?,
                DecoderState::MovImmToRegMem { is_word, state } => {
                    self.decode_mov_imm_to_reg_mem(instruction_byte.clone(), is_word, state)?
                }
                DecoderState::MovAccMemToAccMem {
                    acc_to_mem,
                    is_word,
                    state,
                } => self.decode_mov_acc_mem_to_acc_mem(
                    instruction_byte.clone(),
                    acc_to_mem,
                    is_word,
                    state,
                )?,
                DecoderState::MovRegMemSRToRegMemSR { to_sr: _, state: _ } => {
                    todo!()
                    // self
                    // .decode_mov_reg_mem_srto_reg_mem_sr(
                    //     instruction_byte.clone(),
                    //     to_sr,
                    //     state,
                    //     out,
                    // )?
                }
                DecoderState::ArithRegMemWithRegToEither {
                    op,
                    reg_is_dest,
                    is_word,
                    state,
                } => self.decode_arith_reg_mem_with_reg_to_either(
                    instruction_byte.clone(),
                    op,
                    reg_is_dest,
                    is_word,
                    state,
                )?,
                DecoderState::ArithImmWithRegMem {
                    signed,
                    is_word,
                    state,
                } => self.decode_arith_imm_with_reg_mem(
                    instruction_byte.clone(),
                    signed,
                    is_word,
                    state,
                )?,
                DecoderState::ArithImmToAcc { op, is_word, state } => {
                    self.decode_arith_imm_to_acc(instruction_byte.clone(), op, is_word, state)?
                }
                DecoderState::CondJump { jmp } => {
                    self.decode_conditional_jump(instruction_byte.clone(), jmp)?
                }
            };
            ip += 1;
            if inst.is_some() {
                return Ok((inst, ip));
            }
        }
        Ok((None, ip))
    }
}
