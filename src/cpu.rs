#![allow(non_snake_case)]

use crate::{
    mem::Memory,
    registers::{
        Flags, Registers,
        REG::{self, *},
        REG_WIDE::{self, *},
    },
};

pub struct CPU {
    regs: Registers,
    flags: Flags,
    /// Using the 1MHz convention instead of the 4MHz physical clock that takes 4 cycles for everything
    cycles: usize,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            regs: Registers::new(),
            flags: Flags::new(),
            cycles: 0,
        }
    }

    /// Decodes and executes the next instruction regardless of current cycle counts or interrupt
    pub fn next_instruction(&mut self, mem: &mut Memory) {
        let instr: u8 = mem.read_8(self.regs[PC]);

        match instr {
            // NOP
            0x00 => {
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // LD BC, d16
            0x01 => {
                self.LD_RR(mem, BC);
            }
            // LD (BC), A
            0x02 => {
                self.LD_MEM_RR_R(mem, BC, A);
            }
            // INC BC
            0x03 => {
                self.INC_RR(BC);
            }
            // INC B
            0x04 => {
                self.INC_R(B);
            }
            // DEC B
            0x05 => {
                self.DEC_R(B);
            }
            // LD B, d8
            0x06 => {
                self.LD_R(mem, B);
            }
            // RLCA
            0x07 => {
                self.regs[A] = self.regs[A].rotate_left(1);
                self.cycles += 1;
                self.regs[PC] += 1;
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(self.regs[A] & 0b1 == 0b1);
            }
            // LD (a16), SP
            0x08 => {
                let addr = mem.read_16(self.regs[PC] + 1);
                mem.write(addr, self.regs[SP] as u8);
                mem.write(addr + 1, (self.regs[SP] >> 8) as u8);

                self.regs[PC] += 3;
                self.cycles += 5;
            }
            // ADD HL, BC
            0x09 => {
                self.ADD_RR_RR(HL, BC);
            }
            // LD A, (BC)
            0x0A => {
                self.LD_R_MEM_RR(mem, A, BC);
            }
            // DEC BC
            0x0B => {
                self.DEC_RR(BC);
            }
            // INC C
            0x0C => {
                self.INC_R(C);
            }
            // DEC C
            0x0D => {
                self.DEC_R(C);
            }
            // LD C, d8
            0x0E => {
                self.LD_R(mem, C);
            }
            // RRCA
            0x0F => {
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(self.regs[A] & 0b1 == 0b1);
                self.regs[A] = self.regs[A].rotate_right(1);

                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // STOP
            0x10 => {
                self.stop();
            }
            // LD DE, d16
            0x11 => {
                self.LD_RR(mem, DE);
            }
            // LD (DE), A
            0x12 => {
                self.LD_MEM_RR_R(mem, DE, A);
            }
            // INC DE
            0x13 => {
                self.INC_RR(DE);
            }
            // INC D
            0x14 => {
                self.INC_R(D);
            }
            // DEC D
            0x15 => {
                self.DEC_R(D);
            }
            // LD D, d8
            0x16 => {
                self.LD_R(mem, D);
            }
            // RLA
            0x17 => {
                let carry = self.flags.c();
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(self.regs[A] & 0x80 != 0);
                self.regs[A] <<= 1;
                if carry {
                    self.regs[A] |= 1;
                }
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // JR s8
            0x18 => {
                // TODO - Do I need to add 2 to this as well?
                self.regs[PC] =
                    self.regs[PC].wrapping_add_signed((mem.read_8(self.regs[PC] + 1) as i8) as i16);
                self.cycles += 3;
            }
            // ADD HL, DE
            0x19 => {
                self.ADD_RR_RR(HL, DE);
            }
            // LD A, (DE)
            0x1A => {
                self.LD_R_MEM_RR(mem, A, DE);
            }
            // DEC DE
            0x1B => {
                self.DEC_RR(DE);
            }
            // INC E
            0x1C => {
                self.INC_R(E);
            }
            // DEC E
            0x1D => {
                self.DEC_R(E);
            }
            // LD E, d8
            0x1E => {
                self.LD_R(mem, E);
            }
            // RRA
            0x1F => {
                let carry = self.flags.c();
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(self.regs[A] & 0b1 == 0b1);
                self.regs[A] >>= 1;
                if carry {
                    self.regs[A] |= 0x80;
                }
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // JR NZ, s8
            0x20 => {
                self.JR_C(mem, !self.flags.z());
            }
            // LD HL, d16
            0x21 => {
                self.LD_RR(mem, HL);
            }
            // LD (HL+), A
            0x22 => {
                self.LD_MEM_RR_R(mem, HL, A);
                self.regs[HL] = self.regs[HL].wrapping_add(1);
            }
            // INC HL
            0x23 => {
                self.INC_RR(HL);
            }
            // INC H
            0x24 => {
                self.INC_R(H);
            }
            // DEC H
            0x25 => {
                self.DEC_R(H);
            }
            // LD H, d8
            0x26 => {
                self.LD_R(mem, H);
            }
            // DAA
            0x27 => {
                todo!("DAA");
            }
            // JR Z, s8
            0x28 => {
                self.JR_C(mem, self.flags.z());
            }
            // ADD HL, HL
            0x29 => {
                self.ADD_RR_RR(HL, HL);
            }
            // LD A, (HL+)
            0x2A => {
                self.LD_R_MEM_RR(mem, A, HL);
                self.regs[HL] = self.regs[HL].wrapping_add(1);
            }
            // DEC HL
            0x2B => {
                self.DEC_RR(HL);
            }
            // INC L
            0x2C => {
                self.INC_R(L);
            }
            // DEC L
            0x2D => {
                self.DEC_R(L);
            }
            // LD L, d8
            0x2E => {
                self.LD_R(mem, L);
            }
            // CPL
            0x2F => {
                self.flags.set_n(true);
                self.flags.set_h(true);
                self.regs[L] = !self.regs[L];
                self.cycles += 1;
                self.regs[PC] += 1;
            }
            _ => {
                log::error!("Instruction not implemented: {:#02X}", instr);
            }
        }
    }

    fn JR_C(&mut self, mem: &mut Memory, jump: bool) {
        if jump {
            self.regs[PC] =
                self.regs[PC].wrapping_add_signed((mem.read_8(self.regs[PC] + 1) as i8) as i16);
            self.cycles += 3;
        } else {
            self.regs[PC] += 2;
            self.cycles += 2;
        }
    }

    fn ADD_RR_RR(&mut self, r1: REG_WIDE, r2: REG_WIDE) {
        self.flags
            .set_h(Self::half_carry_add(self.regs[r1], self.regs[r2]));
        self.flags.set_n(false);

        self.regs[r1] = self.regs[r1].wrapping_add(self.regs[r2]);
        self.flags.set_z(self.regs[r1] == 0);

        self.cycles += 2;
        self.regs[PC] += 1;
    }

    fn DEC_R(&mut self, reg: REG) {
        self.flags
            .set_h((self.regs[reg] & 0xf).wrapping_sub(1) & 0x10 != 0);
        self.regs[reg] = self.regs[reg].wrapping_sub(1);
        self.flags.set_z(self.regs[reg] == 0);
        self.flags.set_n(true);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn INC_R(&mut self, reg: REG) {
        self.flags
            .set_h((self.regs[reg] & 0xf).wrapping_add(1) & 0x10 != 0);
        self.regs[reg] = self.regs[reg].wrapping_add(1);
        self.flags.set_z(self.regs[reg] == 0);
        self.flags.set_n(false);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn DEC_RR(&mut self, reg: REG_WIDE) {
        self.regs[reg] = self.regs[reg].wrapping_sub(1);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn INC_RR(&mut self, reg: REG_WIDE) {
        self.regs[reg] = self.regs[reg].wrapping_add(1);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn LD_RR(&mut self, mem: &Memory, dst: REG_WIDE) {
        self.regs[dst] = mem.read_16(self.regs[PC] + 1);
        self.regs[PC] += 3;
        self.cycles += 3;
    }

    fn LD_R(&mut self, mem: &Memory, dst: REG) {
        self.regs[dst] = mem.read_8(self.regs[PC] + 1);
        self.regs[PC] += 2;
        self.cycles += 2;
    }

    fn LD_MEM_RR_R(&mut self, mem: &mut Memory, addr: REG_WIDE, data: REG) {
        mem.write(self.regs[addr], self.regs[data]);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn LD_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] = mem.read_8(self.regs[addr]);

        self.cycles += 2;
        self.regs[PC] += 1;
    }

    fn half_carry_add(a: u16, b: u16) -> bool {
        ((a & 0x0FFF) + (b & 0x0FFF)) & 0x1000 == 0x1000
    }

    fn stop(&mut self) {
        todo!("STOP");
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}
