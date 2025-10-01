use std::fmt;

use crate::{
    mem::Memory,
    registers::{Flag, Registers, REG, REG_WIDE},
};

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Nop,
    Halt,
    Stop,

    Op8(Option<Flag>, U8, Op8),
    Op16(Option<Flag>, U16, Op16),

    JR(Option<Flag>, U8),
    JP(Option<Flag>, U16),
    Push(U16),
    Pop(REG_WIDE),
    Ret(Option<Flag>),
    Call(Option<Flag>, U16),

    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
}

#[derive(Debug, Clone, Copy)]
pub enum Op8 {
    Inc,
    Dec,
    Ld(U8),
    Add(U8),
    Sub(U8),
    And(U8),
    Or(U8),
    Xor(U8),
    Cmp(U8),
}

impl Op8 {
    fn cycles(&self) -> usize {
        match self {
            Op8::Inc | Op8::Dec => 1,
            Op8::Ld(u8)
            | Op8::Add(u8)
            | Op8::Sub(u8)
            | Op8::And(u8)
            | Op8::Or(u8)
            | Op8::Xor(u8)
            | Op8::Cmp(u8) => 1 + u8.cycles(),
        }
    }

    fn opcode(&self, dest: &str) -> String {
        match self {
            Op8::Inc => format!("INC {}", dest),
            Op8::Dec => format!("DEC {}", dest),
            Op8::Ld(u8) => format!("LD {dest}, {}", u8.opcode()),
            Op8::Add(u8) => todo!(),
            Op8::Sub(u8) => todo!(),
            Op8::And(u8) => todo!(),
            Op8::Or(u8) => todo!(),
            Op8::Xor(u8) => todo!(),
            Op8::Cmp(u8) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op16 {
    Ld(U16),
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy)]
pub enum Addr {
    Reg(REG_WIDE),
    Imm,
}

impl Addr {
    pub fn value(&self, regs: &Registers, mem: &Memory) -> u16 {
        match self {
            Addr::Reg(reg_wide) => regs[*reg_wide],
            Addr::Imm => mem.read_16(regs[REG_WIDE::PC]),
        }
    }

    fn opcode(&self) -> String {
        match self {
            Addr::Reg(reg_wide) => format!("{reg_wide:?}"),
            Addr::Imm => format!("a16"),
        }
    }

    fn cycles(&self) -> usize {
        match self {
            Addr::Reg(_) => 0,
            Addr::Imm => 2,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum U8 {
    Mem8(Addr),
    Imm8,
    Reg8(REG),
}

impl U8 {
    pub fn cycles(&self) -> usize {
        match self {
            U8::Mem8(addr) => 1 + addr.cycles(),
            U8::Imm8 => 1,
            U8::Reg8(_) => 0,
        }
    }

    pub fn read(&self, regs: &Registers, mem: &Memory) -> u8 {
        match self {
            U8::Mem8(addr) => mem[addr.value(regs, mem)],
            U8::Imm8 => mem[regs[REG_WIDE::PC]],
            U8::Reg8(reg) => regs[*reg],
        }
    }

    pub fn write(&self, regs: &mut Registers, mem: &mut Memory, data: u8) {
        match self {
            U8::Mem8(addr) => mem.write_u8(addr.value(regs, mem), data),
            U8::Imm8 => panic!("No writing to immediates."),
            U8::Reg8(reg) => regs[*reg] = data,
        }
    }

    pub fn opcode(&self) -> String {
        match self {
            U8::Mem8(addr) => format!("({})", addr.opcode()),
            U8::Imm8 => format!("d8"),
            U8::Reg8(reg) => format!("{reg:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum U16 {
    Mem16(Addr),
    Imm16,
    Reg16(REG_WIDE),
}

impl U16 {
    pub fn cycles(&self) -> usize {
        match self {
            U16::Mem16(addr) => 2 + addr.cycles(),
            U16::Imm16 => 2,
            U16::Reg16(_) => 0,
        }
    }

    pub fn read(&self, regs: &Registers, mem: &Memory) -> u16 {
        match self {
            U16::Mem16(addr) => mem.read_16(addr.value(regs, mem)),
            U16::Imm16 => mem.read_16(regs[REG_WIDE::PC]),
            U16::Reg16(reg) => regs[*reg],
        }
    }

    pub fn write(&self, regs: &mut Registers, mem: &mut Memory, data: u16) {
        match self {
            U16::Mem16(addr) => mem.write_u16(addr.value(regs, mem), data),
            U16::Imm16 => panic!("No writing to immediates."),
            U16::Reg16(reg) => regs[*reg] = data,
        }
    }

    pub fn opcode(&self) -> String {
        match self {
            U16::Mem16(addr) => format!("({})", addr.opcode()),
            U16::Imm16 => format!(""),
            U16::Reg16(reg_wide) => todo!(),
        }
    }
}

impl Op16 {
    fn opcode(&self, src: Option<&str>) -> String {
        match self {
            Op16::Ld(data) => todo!(),
            Op16::Inc => todo!(),
            Op16::Dec => todo!(),
        }
    }
}

impl Instr {
    pub fn opcode(&self) -> String {
        match self {
            Instr::Nop => todo!(),
            Instr::Halt => todo!(),
            Instr::Stop => todo!(),
            Instr::Op8(flag, reg, op8) => todo!(),
            Instr::Op16(flag, reg_wide, op16) => todo!(),
            Instr::JR(flag, u8) => todo!(),
            Instr::JP(flag, u16) => todo!(),
            Instr::Push(u16) => todo!(),
            Instr::Pop(reg_wide) => todo!(),
            Instr::Ret(flag) => todo!(),
            Instr::Call(flag, u16) => todo!(),
            Instr::RLCA => todo!(),
            Instr::RRCA => todo!(),
            Instr::RLA => todo!(),
            Instr::RRA => todo!(),
            Instr::DAA => todo!(),
        }
    }
}
