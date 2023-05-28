use std::fmt::Display;
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use std::{env, io};

type DecoderResult<T> = Result<T, DecoderError>;

#[derive(Debug)]
enum DecoderState {
    Init,
    MovFromRegMemToRegMemInit {
        reg_is_dest: bool,
        is_word: bool,
    },
    MovDirectAddressInit {
        reg_is_dest: bool,
        reg: Register,
    },
    MovDirectAddressLowDispRead {
        reg_is_dest: bool,
        reg: Register,
        low_disp: u8,
    },
    MovEffectiveAddressDispInit {
        reg_is_dest: bool,
        reg: Register,
        effective_addr_calc: EffectiveAddressCalculation,
        is_disp16: bool,
    },
    MovEffectiveAddressDisp16 {
        reg_is_dest: bool,
        reg: Register,
        effective_addr_calc: EffectiveAddressCalculation,
        low_disp: u8,
    },
    MovImmediateToRegMemInit {
        is_word: bool,
    },
    MovImmediateToRegInit {
        reg: Register,
        is_word: bool,
    },
    MovImmediateToDirectAddressInit {
        is_word: bool,
    },
    MovImmediateToEffectiveAddrMem0Init {
        effective_addr_calc: EffectiveAddressCalculation,
        is_word: bool,
    },
    MovImmediateToEffectiveAddrDispInit {
        effective_addr_calc: EffectiveAddressCalculation,
        is_disp16: bool,
        is_word: bool,
    },
    MovImmediate16ToRegInit {
        reg: Register,
        low_imm: u8,
    },
    MovImmediateToDirectAddressDisp8Read {
        low_disp: u8,
        is_word: bool,
    },
    MovImmediateToDirectAddressDisp16Read {
        disp16: u16,
        is_word: bool,
    },
    MovImmediate16ToDirectAddressDisp16Init {
        disp16: u16,
        low_imm: u8,
    },
    MovImmediate16ToEffectiveAddrMem0 {
        effective_addr_calc: EffectiveAddressCalculation,
        low_imm: u8,
    },
    MovImmediateToEffectiveAddrDisp8Read {
        effective_addr_calc: EffectiveAddressCalculation,
        is_disp16: bool,
        is_word: bool,
        disp8: u8,
    },
    MovImmediateToEffectiveAddrDisp16Read {
        effective_addr_calc: EffectiveAddressCalculation,
        is_word: bool,
        disp16: u16,
    },
    MovImmediateToEffectiveAddrDisp8Imm8Read {
        effective_addr_calc: EffectiveAddressCalculation,
        disp8: u8,
        imm8: u8,
    },
    MovImmediateToEffectiveAddrDisp16ReadImm8Read {
        effective_addr_calc: EffectiveAddressCalculation,
        disp16: u16,
        imm8: u8,
    },
    MovAccMemToAccMemInit {
        acc_to_mem: bool,
        is_word: bool,
    },
    MovAccMemToAccMemInitAddrLoRead {
        acc_to_mem: bool,
        is_word: bool,
        addr_lo: u8,
    },
    MovRegMemSRToRegMemSR {
        to_sr: bool,
    },
    MovDirAddrSRToDirAddrSR {
        to_sr: bool,
        sr: SegmentRegister,
    },
    MovEffectAddrSRToEffectAddrSRWithDisp {
        effective_addr_calc: EffectiveAddressCalculation,
        to_sr: bool,
        sr: SegmentRegister,
        is_disp16: bool,
    },
    MovDirAddrSRToDirAddrSRLowDispRead {
        to_sr: bool,
        sr: SegmentRegister,
        low_disp: u8,
    },
    MovEffectAddrSRToEffectAddrSRLowDispRead {
        effective_addr_calc: EffectiveAddressCalculation,
        to_sr: bool,
        sr: SegmentRegister,
        low_disp: u8,
    },
}

#[derive(Debug, Clone, Copy)]
enum Register {
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

    fn to_string_without_disp(&self) -> String {
        String::from(match self {
            EffectiveAddressCalculation::BX_SI => "[bx+si]",
            EffectiveAddressCalculation::BX_DI => "[bx+di]",
            EffectiveAddressCalculation::BP_SI => "[bp+si]",
            EffectiveAddressCalculation::BP_DI => "[bp+di]",
            EffectiveAddressCalculation::SI => "[si]",
            EffectiveAddressCalculation::DI => "[di]",
            EffectiveAddressCalculation::BP => "[bp]",
            EffectiveAddressCalculation::BX => "[bx]",
        })
    }

    fn to_string_with_disp8(&self, disp: i8) -> String {
        let sign = if disp < 0 {
            "-"
        } else if disp > 0 {
            "+"
        } else {
            ""
        };
        let disp = if disp != 0 {
            format!("{}", disp.abs())
        } else {
            String::new()
        };
        String::from(match self {
            EffectiveAddressCalculation::BX_SI => format!("[bx+si{sign}{disp}]"),
            EffectiveAddressCalculation::BX_DI => format!("[bx+di{sign}{disp}]"),
            EffectiveAddressCalculation::BP_SI => format!("[bp+si{sign}{disp}]"),
            EffectiveAddressCalculation::BP_DI => format!("[bp+di{sign}{disp}]"),
            EffectiveAddressCalculation::SI => format!("[si{sign}{disp}]"),
            EffectiveAddressCalculation::DI => format!("[di{sign}{disp}]"),
            EffectiveAddressCalculation::BP => format!("[bp{sign}{disp}]"),
            EffectiveAddressCalculation::BX => format!("[bx{sign}{disp}]"),
        })
    }

    fn to_string_with_disp16(&self, disp: i16) -> String {
        let sign = if disp < 0 {
            "-"
        } else if disp > 0 {
            "+"
        } else {
            ""
        };
        let disp = if disp != 0 {
            format!("{}", disp.abs())
        } else {
            String::new()
        };
        String::from(match self {
            EffectiveAddressCalculation::BX_SI => format!("[bx+si{sign}{disp}]"),
            EffectiveAddressCalculation::BX_DI => format!("[bx+di{sign}{disp}]"),
            EffectiveAddressCalculation::BP_SI => format!("[bp+si{sign}{disp}]"),
            EffectiveAddressCalculation::BP_DI => format!("[bp+di{sign}{disp}]"),
            EffectiveAddressCalculation::SI => format!("[si{sign}{disp}]"),
            EffectiveAddressCalculation::DI => format!("[di{sign}{disp}]"),
            EffectiveAddressCalculation::BP => format!("[bp{sign}{disp}]"),
            EffectiveAddressCalculation::BX => format!("[bx{sign}{disp}]"),
        })
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
enum SegmentRegister {
    ES,
    CS,
    SS,
    DS,
}

impl SegmentRegister {
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

#[derive(Debug)]
enum DecoderError {
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

const D_MASK: u8 = 0x02;
const D_REG_DST: u8 = 0x02;

const W_MASK: u8 = 0x01;
const W_WORD: u8 = 0x01;

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

const SR_MASK: u8 = 0x18;
const SR_OFFSET: u8 = 4;

#[derive(Debug)]
struct Decoder {
    state: DecoderState,
}

impl Decoder {
    fn new() -> Decoder {
        Decoder {
            state: DecoderState::Init,
        }
    }

    fn decode<W: Write>(&mut self, input: &[u8], out: &mut BufWriter<W>) -> DecoderResult<()> {
        for instruction_byte in input.into_iter() {
            self.state = match self.state {
                DecoderState::Init => {
                    if instruction_byte & MOV_REGMEM_TO_REGMEM_OPCODE_MASK
                        == MOV_REGMEM_TO_REGMEM_OPCODE
                    {
                        let d = instruction_byte & D_MASK;
                        let w = instruction_byte & W_MASK;
                        DecoderState::MovFromRegMemToRegMemInit {
                            reg_is_dest: d == D_REG_DST,
                            is_word: w == W_WORD,
                        }
                    } else if instruction_byte & MOV_IMMEDIATE_TO_REGMEM_OPCODE_MASK
                        == MOV_IMMEDIATE_TO_REGMEM_OPCODE
                    {
                        let w = instruction_byte & W_MASK;
                        DecoderState::MovImmediateToRegMemInit {
                            is_word: w == W_WORD,
                        }
                    } else if instruction_byte & MOV_IMMEDIATE_TO_REG_OPCODE_MASK
                        == MOV_IMMEDIATE_TO_REG_OPCODE
                    {
                        let is_word = 0x1 == ((instruction_byte.clone() >> 3) & 0x1);

                        let reg = Register::from_reg_w(instruction_byte.clone() & 0x7, is_word)?;
                        DecoderState::MovImmediateToRegInit { reg, is_word }
                    } else if instruction_byte & MOV_ACCMEM_TO_ACCMEM_OPCODE_MASK
                        == MOV_ACCMEM_TO_ACCMEM_OPCODE
                    {
                        let acc_to_mem = 0x2 == (instruction_byte.clone() & 0x2);
                        let is_word = (instruction_byte & W_MASK) == W_WORD;

                        DecoderState::MovAccMemToAccMemInit {
                            acc_to_mem,
                            is_word,
                        }
                    } else if instruction_byte & MOV_REGMEMSR_TO_REGMEMSR_OPCODE_MASK
                        == MOV_REGMEMSR_TO_REGMEMSR_OPCODE
                    {
                        let to_sr = 0x2 == (instruction_byte.clone() & 0x2);

                        DecoderState::MovRegMemSRToRegMemSR { to_sr }
                    } else {
                        return Err(DecoderError::GenericError(String::from("unknown opcode")));
                    }
                }
                DecoderState::MovFromRegMemToRegMemInit {
                    reg_is_dest,
                    is_word,
                } => match instruction_byte & MOD_MASK {
                    MOD_REG => {
                        self.gen_mov_reg_to_reg(
                            instruction_byte.clone(),
                            reg_is_dest,
                            is_word,
                            out,
                        )?;
                        DecoderState::Init
                    }
                    MOD_MEM_0 => {
                        if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET) == RM_VALUE_DIRECT_ACCESS
                        {
                            let reg = Register::from_reg_w(
                                (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                                is_word,
                            )?;
                            DecoderState::MovDirectAddressInit { reg_is_dest, reg }
                        } else {
                            self.gen_mov_between_reg_mem_0(
                                instruction_byte.clone(),
                                reg_is_dest,
                                is_word,
                                out,
                            )?;
                            DecoderState::Init
                        }
                    }
                    mode => {
                        let reg = Register::from_reg_w(
                            (instruction_byte & REG_MASK) >> REG_BIT_OFFSET,
                            is_word,
                        )?;
                        let effective_addr_calc = EffectiveAddressCalculation::from_rm(
                            (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                        )?;
                        DecoderState::MovEffectiveAddressDispInit {
                            reg_is_dest,
                            reg,
                            effective_addr_calc,
                            is_disp16: mode == MOD_MEM_2,
                        }
                    }
                },
                DecoderState::MovDirectAddressInit { reg_is_dest, reg } => {
                    DecoderState::MovDirectAddressLowDispRead {
                        reg_is_dest,
                        reg,
                        low_disp: instruction_byte.clone(),
                    }
                }
                DecoderState::MovDirectAddressLowDispRead {
                    reg_is_dest,
                    reg,
                    low_disp,
                } => {
                    let disp =
                        (((instruction_byte.clone() as u16) << 8) | (low_disp as u16)) as i16;
                    if reg_is_dest {
                        out.write_all(format!("mov {reg}, [{disp}]\n").as_bytes())?;
                    } else {
                        out.write_all(format!("mov [{disp}], {reg}\n").as_bytes())?;
                    }
                    DecoderState::Init
                }
                DecoderState::MovEffectiveAddressDispInit {
                    reg_is_dest,
                    reg,
                    effective_addr_calc,
                    is_disp16,
                } => {
                    let disp8: u8 = instruction_byte.clone();
                    if is_disp16 {
                        DecoderState::MovEffectiveAddressDisp16 {
                            reg_is_dest,
                            reg,
                            effective_addr_calc,
                            low_disp: disp8,
                        }
                    } else {
                        if reg_is_dest {
                            out.write_all(
                                format!(
                                    "mov {reg}, {}\n",
                                    effective_addr_calc.to_string_with_disp8(disp8 as i8)
                                )
                                .as_bytes(),
                            )?;
                        } else {
                            out.write_all(
                                format!(
                                    "mov {}, {reg}\n",
                                    effective_addr_calc.to_string_with_disp8(disp8 as i8)
                                )
                                .as_bytes(),
                            )?;
                        }
                        DecoderState::Init
                    }
                }
                DecoderState::MovEffectiveAddressDisp16 {
                    reg_is_dest,
                    reg,
                    effective_addr_calc,
                    low_disp,
                } => {
                    let disp =
                        (((instruction_byte.clone() as u16) << 8) | (low_disp as u16)) as i16;
                    if reg_is_dest {
                        out.write_all(
                            format!(
                                "mov {reg}, {}\n",
                                effective_addr_calc.to_string_with_disp16(disp)
                            )
                            .as_bytes(),
                        )?;
                    } else {
                        out.write_all(
                            format!(
                                "mov {}, {reg}\n",
                                effective_addr_calc.to_string_with_disp16(disp)
                            )
                            .as_bytes(),
                        )?;
                    }
                    DecoderState::Init
                }
                DecoderState::MovImmediateToRegMemInit { is_word } => {
                    match instruction_byte & MOD_MASK {
                        MOD_REG => {
                            let rm = Register::from_reg_w(
                                (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                                is_word,
                            )?;
                            DecoderState::MovImmediateToRegInit { reg: rm, is_word }
                        }
                        MOD_MEM_0 => {
                            if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET)
                                == RM_VALUE_DIRECT_ACCESS
                            {
                                DecoderState::MovImmediateToDirectAddressInit { is_word }
                            } else {
                                let effective_addr_calc = EffectiveAddressCalculation::from_rm(
                                    (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                                )?;
                                DecoderState::MovImmediateToEffectiveAddrMem0Init {
                                    effective_addr_calc,
                                    is_word,
                                }
                            }
                        }
                        mode => {
                            let effective_addr_calc = EffectiveAddressCalculation::from_rm(
                                (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                            )?;
                            DecoderState::MovImmediateToEffectiveAddrDispInit {
                                effective_addr_calc,
                                is_disp16: mode == MOD_MEM_2,
                                is_word,
                            }
                        }
                    }
                }
                DecoderState::MovImmediateToRegInit { reg, is_word } => {
                    let imm8 = instruction_byte;
                    if is_word {
                        DecoderState::MovImmediate16ToRegInit {
                            reg,
                            low_imm: imm8.clone(),
                        }
                    } else {
                        out.write_all(format!("mov {reg}, {imm8}\n").as_bytes())?;
                        DecoderState::Init
                    }
                }
                DecoderState::MovImmediate16ToRegInit { reg, low_imm } => {
                    let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                    out.write_all(format!("mov {reg}, {imm16}\n").as_bytes())?;
                    DecoderState::Init
                }
                DecoderState::MovImmediateToDirectAddressInit { is_word } => {
                    let low_disp = instruction_byte.clone();
                    DecoderState::MovImmediateToDirectAddressDisp8Read { low_disp, is_word }
                }
                DecoderState::MovImmediateToDirectAddressDisp8Read { low_disp, is_word } => {
                    let disp16 = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                    DecoderState::MovImmediateToDirectAddressDisp16Read { disp16, is_word }
                }
                DecoderState::MovImmediateToDirectAddressDisp16Read { disp16, is_word } => {
                    let imm8 = instruction_byte;
                    if is_word {
                        DecoderState::MovImmediate16ToDirectAddressDisp16Init {
                            disp16,
                            low_imm: imm8.clone(),
                        }
                    } else {
                        out.write_all(format!("mov byte [{disp16}], {imm8}\n").as_bytes())?;
                        DecoderState::Init
                    }
                }
                DecoderState::MovImmediate16ToDirectAddressDisp16Init { disp16, low_imm } => {
                    let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                    out.write_all(format!("mov word [{disp16}], {imm16}\n").as_bytes())?;
                    DecoderState::Init
                }
                DecoderState::MovImmediateToEffectiveAddrMem0Init {
                    effective_addr_calc,
                    is_word,
                } => {
                    let imm8 = instruction_byte;
                    if is_word {
                        DecoderState::MovImmediate16ToEffectiveAddrMem0 {
                            effective_addr_calc,
                            low_imm: imm8.clone(),
                        }
                    } else {
                        out.write_all(
                            format!(
                                "mov byte {}, {imm8}\n",
                                effective_addr_calc.to_string_without_disp()
                            )
                            .as_bytes(),
                        )?;
                        DecoderState::Init
                    }
                }
                DecoderState::MovImmediate16ToEffectiveAddrMem0 {
                    effective_addr_calc,
                    low_imm,
                } => {
                    let imm16 = ((instruction_byte.clone() as u16) << 8) | (low_imm as u16);
                    out.write_all(
                        format!(
                            "mov word {}, {imm16}\n",
                            effective_addr_calc.to_string_without_disp()
                        )
                        .as_bytes(),
                    )?;
                    DecoderState::Init
                }
                DecoderState::MovImmediateToEffectiveAddrDispInit {
                    effective_addr_calc,
                    is_disp16,
                    is_word,
                } => {
                    let disp8 = instruction_byte.clone();
                    DecoderState::MovImmediateToEffectiveAddrDisp8Read {
                        effective_addr_calc,
                        is_disp16,
                        is_word,
                        disp8,
                    }
                }
                DecoderState::MovImmediateToEffectiveAddrDisp8Read {
                    effective_addr_calc,
                    is_disp16,
                    is_word,
                    disp8,
                } => {
                    if is_disp16 {
                        let disp16 = ((instruction_byte.clone() as u16) << 8) | (disp8 as u16);
                        DecoderState::MovImmediateToEffectiveAddrDisp16Read {
                            effective_addr_calc,
                            is_word,
                            disp16,
                        }
                    } else {
                        let imm8 = instruction_byte.clone();
                        if is_word {
                            DecoderState::MovImmediateToEffectiveAddrDisp8Imm8Read {
                                effective_addr_calc,
                                disp8,
                                imm8,
                            }
                        } else {
                            out.write_all(
                                format!(
                                    "mov byte {}, {imm8}\n",
                                    effective_addr_calc.to_string_with_disp8(disp8 as i8)
                                )
                                .as_bytes(),
                            )?;
                            DecoderState::Init
                        }
                    }
                }
                DecoderState::MovImmediateToEffectiveAddrDisp8Imm8Read {
                    effective_addr_calc,
                    disp8,
                    imm8,
                } => {
                    let imm16 = ((instruction_byte.clone() as u16) << 8) | (imm8 as u16);
                    out.write_all(
                        format!(
                            "mov word {}, {imm16}\n",
                            effective_addr_calc.to_string_with_disp8(disp8 as i8)
                        )
                        .as_bytes(),
                    )?;
                    DecoderState::Init
                }
                DecoderState::MovImmediateToEffectiveAddrDisp16Read {
                    effective_addr_calc,
                    is_word,
                    disp16,
                } => {
                    let imm8 = instruction_byte.clone();
                    if is_word {
                        DecoderState::MovImmediateToEffectiveAddrDisp16ReadImm8Read {
                            effective_addr_calc,
                            disp16,
                            imm8,
                        }
                    } else {
                        out.write_all(
                            format!(
                                "mov byte {}, {imm8}\n",
                                effective_addr_calc.to_string_with_disp16(disp16 as i16)
                            )
                            .as_bytes(),
                        )?;
                        DecoderState::Init
                    }
                }
                DecoderState::MovImmediateToEffectiveAddrDisp16ReadImm8Read {
                    effective_addr_calc,
                    disp16,
                    imm8,
                } => {
                    let imm16 = ((instruction_byte.clone() as u16) << 8) | (imm8 as u16);
                    out.write_all(
                        format!(
                            "mov word {}, {imm16}\n",
                            effective_addr_calc.to_string_with_disp16(disp16 as i16)
                        )
                        .as_bytes(),
                    )?;
                    DecoderState::Init
                }
                DecoderState::MovAccMemToAccMemInit {
                    acc_to_mem,
                    is_word,
                } => {
                    let addr_lo = instruction_byte.clone();
                    DecoderState::MovAccMemToAccMemInitAddrLoRead {
                        acc_to_mem,
                        is_word,
                        addr_lo,
                    }
                }
                DecoderState::MovAccMemToAccMemInitAddrLoRead {
                    acc_to_mem,
                    is_word,
                    addr_lo,
                } => {
                    let addr = ((instruction_byte.clone() as u16) << 8) | (addr_lo as u16);
                    if acc_to_mem {
                        if is_word {
                            out.write_all(format!("mov [{addr}], ax\n").as_bytes())?;
                        } else {
                            out.write_all(format!("mov [{addr}], al\n").as_bytes())?;
                        }
                    } else {
                        if is_word {
                            out.write_all(format!("mov ax, [{addr}]\n").as_bytes())?;
                        } else {
                            out.write_all(format!("mov al, [{addr}]\n").as_bytes())?;
                        }
                    }
                    DecoderState::Init
                }
                DecoderState::MovRegMemSRToRegMemSR { to_sr } => {
                    let sr = SegmentRegister::from_sr((instruction_byte & SR_MASK) >> SR_OFFSET);
                    match instruction_byte & MOD_MASK {
                        MOD_REG => {
                            let rm = Register::from_reg_w(
                                (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                                true,
                            )?;
                            if to_sr {
                                out.write_all(format!("mov {sr}, {rm}\n").as_bytes())?;
                            } else {
                                out.write_all(format!("mov {rm}, {sr}\n").as_bytes())?;
                            }
                            DecoderState::Init
                        }
                        MOD_MEM_0 => {
                            if ((instruction_byte & RM_MASK) >> RM_BIT_OFFSET)
                                == RM_VALUE_DIRECT_ACCESS
                            {
                                DecoderState::MovDirAddrSRToDirAddrSR { to_sr, sr }
                            } else {
                                let effective_addr_calc = EffectiveAddressCalculation::from_rm(
                                    (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                                )?;
                                if to_sr {
                                    out.write_all(
                                        format!(
                                            "mov {sr}, {}\n",
                                            effective_addr_calc.to_string_without_disp()
                                        )
                                        .as_bytes(),
                                    )?;
                                } else {
                                    out.write_all(
                                        format!(
                                            "mov {}, {sr}\n",
                                            effective_addr_calc.to_string_without_disp()
                                        )
                                        .as_bytes(),
                                    )?;
                                }
                                DecoderState::Init
                            }
                        }
                        mode => {
                            let effective_addr_calc = EffectiveAddressCalculation::from_rm(
                                (instruction_byte & RM_MASK) >> RM_BIT_OFFSET,
                            )?;
                            DecoderState::MovEffectAddrSRToEffectAddrSRWithDisp {
                                effective_addr_calc,
                                to_sr,
                                sr,
                                is_disp16: mode == MOD_MEM_2,
                            }
                        }
                    }
                }
                DecoderState::MovDirAddrSRToDirAddrSR { to_sr, sr } => {
                    let disp8 = instruction_byte.clone();
                    DecoderState::MovDirAddrSRToDirAddrSRLowDispRead {
                        to_sr,
                        sr,
                        low_disp: disp8,
                    }
                }
                DecoderState::MovEffectAddrSRToEffectAddrSRWithDisp {
                    effective_addr_calc,
                    to_sr,
                    sr,
                    is_disp16,
                } => {
                    let disp8 = instruction_byte.clone();
                    if is_disp16 {
                        DecoderState::MovEffectAddrSRToEffectAddrSRLowDispRead {
                            effective_addr_calc,
                            to_sr,
                            sr,
                            low_disp: disp8,
                        }
                    } else {
                        if to_sr {
                            out.write_all(
                                format!(
                                    "mov {sr}, {}\n",
                                    effective_addr_calc.to_string_with_disp8(disp8 as i8)
                                )
                                .as_bytes(),
                            )?;
                        } else {
                            out.write_all(
                                format!(
                                    "mov {}, {sr}\n",
                                    effective_addr_calc.to_string_with_disp8(disp8 as i8)
                                )
                                .as_bytes(),
                            )?;
                        }
                        DecoderState::Init
                    }
                }
                DecoderState::MovDirAddrSRToDirAddrSRLowDispRead {
                    to_sr,
                    sr,
                    low_disp,
                } => {
                    let disp = ((instruction_byte.clone() as u16) << 8) | (low_disp as u16);
                    if to_sr {
                        out.write_all(format!("mov {sr}, [{disp}]\n").as_bytes())?;
                    } else {
                        out.write_all(format!("mov [{disp}], {sr}\n").as_bytes())?;
                    }
                    DecoderState::Init
                }
                DecoderState::MovEffectAddrSRToEffectAddrSRLowDispRead {
                    effective_addr_calc,
                    to_sr,
                    sr,
                    low_disp,
                } => {
                    let disp =
                        (((instruction_byte.clone() as u16) << 8) | (low_disp as u16)) as i16;
                    if to_sr {
                        out.write_all(
                            format!(
                                "mov {sr}, {}\n",
                                effective_addr_calc.to_string_with_disp16(disp)
                            )
                            .as_bytes(),
                        )?;
                    } else {
                        out.write_all(
                            format!(
                                "mov {}, {sr}\n",
                                effective_addr_calc.to_string_with_disp16(disp)
                            )
                            .as_bytes(),
                        )?;
                    }
                    DecoderState::Init
                }
            }
        }
        Ok(())
    }

    fn gen_mov_reg_to_reg<W: Write>(
        &mut self,
        instruction_byte: u8,
        reg_is_dest: bool,
        is_word: bool,
        out: &mut BufWriter<W>,
    ) -> DecoderResult<()> {
        let reg = Register::from_reg_w((instruction_byte & REG_MASK) >> REG_BIT_OFFSET, is_word)?;
        let rm = Register::from_reg_w((instruction_byte & RM_MASK) >> RM_BIT_OFFSET, is_word)?;
        if reg_is_dest {
            out.write_all(format!("mov {reg}, {rm}\n").as_bytes())?;
        } else {
            out.write_all(format!("mov {rm}, {reg}\n").as_bytes())?;
        }
        Ok(())
    }

    fn gen_mov_between_reg_mem_0<W: Write>(
        &mut self,
        instruction_byte: u8,
        reg_is_dest: bool,
        is_word: bool,
        out: &mut BufWriter<W>,
    ) -> DecoderResult<()> {
        let reg = Register::from_reg_w((instruction_byte & REG_MASK) >> REG_BIT_OFFSET, is_word)?;
        let rm =
            EffectiveAddressCalculation::from_rm((instruction_byte & RM_MASK) >> RM_BIT_OFFSET)?;
        if reg_is_dest {
            out.write_all(format!("mov {reg}, {}\n", rm.to_string_without_disp()).as_bytes())?;
        } else {
            out.write_all(format!("mov {}, {reg}\n", rm.to_string_without_disp()).as_bytes())?;
        }
        Ok(())
    }
}

fn main() -> DecoderResult<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(DecoderError::GenericError(String::from(
            "No filename given",
        )));
    }

    let filename = &args[1];
    let mut buffer = [0u8; 1024];
    let mut file = File::open(filename)?;
    let mut out = io::BufWriter::new(io::stdout().lock());
    out.write_all(format!("; {filename}\nbits 16\n\n").as_bytes())?;
    let mut decoder = Decoder::new();
    loop {
        let n_bytes = file.read(&mut buffer)?;
        if n_bytes == 0 {
            break;
        }
        let data = &buffer[0..n_bytes];
        decoder.decode(data, &mut out)?;
    }
    Ok(())
}
