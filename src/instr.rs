use crate::{
    mem::Memory,
    registers::{Flag, Registers, REG, REG_WIDE},
};

pub enum Instr {
    Nop,
    Halt,
    Stop,

    Op8(Option<Flag>, REG, Op8),
    Op16(Option<Flag>, REG_WIDE, Op16),

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
    LdI(U8),
    Add(U8),
    Sub(U8),
    And(U8),
    Or(U8),
    Xor(U8),
    Cmp(U8),
}

#[derive(Debug, Clone, Copy)]
pub enum Op16 {
    Ld(U16),
    Inc,
    Dec,
}

#[derive(Debug, Clone, Copy)]
pub enum Addr {
    C(u16),
    R(REG_WIDE),
}

impl Addr {
    pub fn value(&self, regs: &Registers) -> u16 {
        match self {
            Addr::C(addr) => *addr,
            Addr::R(reg_wide) => regs[*reg_wide],
        }
    }
}

pub enum U8 {
    Mem8(Addr),
    // PC Addr
    Imm8,
    Reg8(REG),
}

pub enum U16 {
    Mem16(Addr),
    // PC
    Imm16,
    Reg16(REG_WIDE),
}

impl U8 {
    pub fn cycles(&self) -> usize {
        match self {
            U8::Mem8(_) => 1,
            U8::Imm8 => 1,
            U8::Reg8(_) => 0,
        }
    }

    pub fn read(&self, regs: &Registers, mem: &Memory) -> u8 {
        match self {
            U8::Mem8(addr) => mem[addr.value(regs)],
            U8::Imm8 => mem[regs[REG_WIDE::PC]],
            U8::Reg8(reg) => regs[*reg],
        }
    }

    pub fn write(&self, regs: &mut Registers, mem: &mut Memory, data: u8) {
        match self {
            U8::Mem8(addr) => mem.write_u8(addr.value(regs), data),
            U8::Imm8 => panic!("No writing to immediates."),
            U8::Reg8(reg) => regs[*reg] = data,
        }
    }
}

impl U16 {
    pub fn cycles(&self) -> usize {
        match self {
            U16::Mem16(_) => 2,
            U16::Imm16 => 2,
            U16::Reg16(_) => 0,
        }
    }

    pub fn read(&self, regs: &Registers, mem: &Memory) -> u16 {
        match self {
            U16::Mem16(addr) => mem.read_16(addr.value(regs)),
            U16::Imm16 => mem.read_16(regs[REG_WIDE::PC]),
            U16::Reg16(reg) => regs[*reg],
        }
    }

    pub fn write(&self, regs: &mut Registers, mem: &mut Memory, data: u16) {
        match self {
            U16::Mem16(addr) => mem.write_u16(addr.value(regs), data),
            U16::Imm16 => panic!("No writing to immediates."),
            U16::Reg16(reg) => regs[*reg] = data,
        }
    }
}
