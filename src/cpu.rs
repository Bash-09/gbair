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
        let instr: u8 = mem[self.regs[PC]];

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
                mem.write_u8(addr, self.regs[SP] as u8);
                mem.write_u8(addr + 1, (self.regs[SP] >> 8) as u8);

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
                    self.regs[PC].wrapping_add_signed((mem[self.regs[PC] + 1] as i8) as i16);
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
            // JR NC, s8
            0x30 => {
                self.JR_C(mem, !self.flags.c());
            }
            // LD SP, d16
            0x31 => {
                self.LD_RR(mem, SP);
            }
            // LD (HL-), A
            0x32 => {
                self.LD_MEM_RR_R(mem, HL, A);
                self.regs[HL] -= 1;
            }
            // INC SP
            0x33 => {
                self.INC_RR(SP);
            }
            // INC (HL)
            0x34 => {
                self.INC_MEM_RR(mem, HL);
            }
            // DEC (HL)
            0x35 => {
                self.DEC_MEM_RR(mem, HL);
            }
            // LD (HL) d8
            0x36 => {
                self.LD_MEM_RR_D(mem, HL);
            }
            // SCF
            0x37 => {
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(true);
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // JR C, s8
            0x38 => {
                self.JR_C(mem, self.flags.c());
            }
            // ADD HL, SP
            0x39 => {
                self.ADD_RR_RR(HL, SP);
            }
            // LD A, (HL-)
            0x3A => {
                self.LD_R_MEM_RR(mem, A, HL);
                self.regs[HL] -= 1;
            }
            // DEC SP
            0x3B => {
                self.DEC_RR(SP);
            }
            // INC A
            0x3C => {
                self.INC_R(A);
            }
            // DEC A
            0x3D => {
                self.DEC_R(A);
            }
            // LD A, d8
            0x3E => {
                self.LD_R(mem, A);
            }
            // CCF
            0x3F => {
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(!self.flags.c());
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // LD B, B
            0x40 => {
                self.LD_R_R(B, B);
            }
            // LD, B, C
            0x41 => {
                self.LD_R_R(B, C);
            }
            // LD B, D
            0x42 => {
                self.LD_R_R(B, D);
            }
            // LD B, E
            0x43 => {
                self.LD_R_R(B, E);
            }
            // LD B, H
            0x44 => {
                self.LD_R_R(B, H);
            }
            // LD B, L
            0x45 => {
                self.LD_R_R(B, L);
            }
            // LD B, (HL)
            0x46 => {
                self.LD_R_MEM_RR(mem, B, HL);
            }
            // LD B, A
            0x47 => {
                self.LD_R_R(B, A);
            }
            // LD C, B
            0x48 => {
                self.LD_R_R(C, B);
            }
            // LD C, C
            0x49 => {
                self.LD_R_R(C, C);
            }
            // LD C, D
            0x4A => {
                self.LD_R_R(C, D);
            }
            // LD C, E
            0x4B => {
                self.LD_R_R(C, E);
            }
            // LD C, H
            0x4C => {
                self.LD_R_R(C, H);
            }
            // LD C, L
            0x4D => {
                self.LD_R_R(C, L);
            }
            // LD C, (HL)
            0x4E => {
                self.LD_R_MEM_RR(mem, C, HL);
            }
            // LD C, A
            0x4F => {
                self.LD_R_R(C, A);
            }
            // LD D, B
            0x50 => {
                self.LD_R_R(D, B);
            }
            // LD, D, C
            0x51 => {
                self.LD_R_R(D, C);
            }
            // LD D, D
            0x52 => {
                self.LD_R_R(D, D);
            }
            // LD D, E
            0x53 => {
                self.LD_R_R(D, E);
            }
            // LD D, H
            0x54 => {
                self.LD_R_R(D, H);
            }
            // LD D, L
            0x55 => {
                self.LD_R_R(D, L);
            }
            // LD D, (HL)
            0x56 => {
                self.LD_R_MEM_RR(mem, D, HL);
            }
            // LD D, A
            0x57 => {
                self.LD_R_R(D, A);
            }
            // LD E, B
            0x58 => {
                self.LD_R_R(E, B);
            }
            // LD E, C
            0x59 => {
                self.LD_R_R(E, C);
            }
            // LD E, D
            0x5A => {
                self.LD_R_R(E, D);
            }
            // LD E, E
            0x5B => {
                self.LD_R_R(E, E);
            }
            // LD E, H
            0x5C => {
                self.LD_R_R(E, H);
            }
            // LD E, L
            0x5D => {
                self.LD_R_R(E, L);
            }
            // LD E, (HL)
            0x5E => {
                self.LD_R_MEM_RR(mem, E, HL);
            }
            // LD E, A
            0x5F => {
                self.LD_R_R(E, A);
            }
            // LD H, B
            0x60 => {
                self.LD_R_R(H, B);
            }
            // LD H, C
            0x61 => {
                self.LD_R_R(H, C);
            }
            // LD H, D
            0x62 => {
                self.LD_R_R(H, D);
            }
            // LD H, E
            0x63 => {
                self.LD_R_R(H, E);
            }
            // LD H, H
            0x64 => {
                self.LD_R_R(H, H);
            }
            // LD H, L
            0x65 => {
                self.LD_R_R(H, L);
            }
            // LD H, (HL)
            0x66 => {
                self.LD_R_MEM_RR(mem, H, HL);
            }
            // LD H, A
            0x67 => {
                self.LD_R_R(H, A);
            }
            // LD L, B
            0x68 => {
                self.LD_R_R(L, B);
            }
            // LD L, C
            0x69 => {
                self.LD_R_R(L, C);
            }
            // LD L, D
            0x6A => {
                self.LD_R_R(L, D);
            }
            // LD L, E
            0x6B => {
                self.LD_R_R(L, E);
            }
            // LD L, H
            0x6C => {
                self.LD_R_R(L, H);
            }
            // LD L, L
            0x6D => {
                self.LD_R_R(L, L);
            }
            // LD L, (HL)
            0x6E => {
                self.LD_R_MEM_RR(mem, L, HL);
            }
            // LD L, A
            0x6F => {
                self.LD_R_R(L, A);
            }
            // LD (HL), B
            0x70 => {
                self.LD_MEM_RR_R(mem, HL, B);
            }
            // LD (HL), C
            0x71 => {
                self.LD_MEM_RR_R(mem, HL, C);
            }
            // LD (HL), D
            0x72 => {
                self.LD_MEM_RR_R(mem, HL, D);
            }
            // LD (HL), E
            0x73 => {
                self.LD_MEM_RR_R(mem, HL, E);
            }
            // LD (HL), H
            0x74 => {
                self.LD_MEM_RR_R(mem, HL, H);
            }
            // LD (HL), L
            0x75 => {
                self.LD_MEM_RR_R(mem, HL, L);
            }
            // HALT
            0x76 => {
                todo!("HALT");
            }
            // LD (HL), A
            0x77 => {
                self.LD_MEM_RR_R(mem, HL, A);
            }
            // LD A, B
            0x78 => {
                self.LD_R_R(A, B);
            }
            // LD A, C
            0x79 => {
                self.LD_R_R(A, C);
            }
            // LD A, D
            0x7A => {
                self.LD_R_R(A, D);
            }
            // LD A, E
            0x7B => {
                self.LD_R_R(A, E);
            }
            // LD A, H
            0x7C => {
                self.LD_R_R(A, H);
            }
            // LD A, L
            0x7D => {
                self.LD_R_R(A, L);
            }
            // LD A, (HL)
            0x7E => {
                self.LD_R_MEM_RR(mem, A, HL);
            }
            // LD A, A
            0x7F => {
                self.LD_R_R(A, A);
            }
            // ADD A, B
            0x80 => {
                self.ADD_R_R(A, B);
            }
            // ADD A, C
            0x81 => {
                self.ADD_R_R(A, C);
            }
            // ADD A, D
            0x82 => {
                self.ADD_R_R(A, D);
            }
            // ADD A, E
            0x83 => {
                self.ADD_R_R(A, E);
            }
            // ADD A, H
            0x84 => {
                self.ADD_R_R(A, H);
            }
            // ADD A, L
            0x85 => {
                self.ADD_R_R(A, L);
            }
            // ADD A, (HL)
            0x86 => {
                self.ADD_R_MEM_RR(mem, A, HL);
            }
            // ADD A, A
            0x87 => {
                self.ADD_R_R(A, A);
            }
            // ADC A, B
            0x88 => {
                self.ADC_R_R(A, B);
            }
            // ADC A, C
            0x89 => {
                self.ADC_R_R(A, C);
            }
            // ADC A, D
            0x8A => {
                self.ADC_R_R(A, D);
            }
            // ADC A, E
            0x8B => {
                self.ADC_R_R(A, E);
            }
            // ADC A, H
            0x8C => {
                self.ADC_R_R(A, H);
            }
            // ADC A, L
            0x8D => {
                self.ADC_R_R(A, L);
            }
            // ADC A, (HL)
            0x8E => {
                self.ADC_R_MEM_RR(mem, A, HL);
            }
            // ADC A, A
            0x8F => {
                self.ADC_R_R(A, A);
            }
            // SUB A, B
            0x90 => {
                self.SUB_R_R(A, B);
            }
            // SUB A, C
            0x91 => {
                self.SUB_R_R(A, C);
            }
            // SUB A, D
            0x92 => {
                self.SUB_R_R(A, D);
            }
            // SUB A, E
            0x93 => {
                self.SUB_R_R(A, E);
            }
            // SUB A, H
            0x94 => {
                self.SUB_R_R(A, H);
            }
            // SUB A, L
            0x95 => {
                self.SUB_R_R(A, L);
            }
            // SUB A, (HL)
            0x96 => {
                self.SUB_R_MEM_RR(mem, A, HL);
            }
            // SUB A, A
            0x97 => {
                self.SUB_R_R(A, A);
            }
            // SBC A, B
            0x98 => {
                self.SBC_R_R(A, B);
            }
            // SBC A, C
            0x99 => {
                self.SBC_R_R(A, C);
            }
            // SBC A, D
            0x9A => {
                self.SBC_R_R(A, D);
            }
            // SBC A, E
            0x9B => {
                self.SBC_R_R(A, E);
            }
            // SBC A, H
            0x9C => {
                self.SBC_R_R(A, H);
            }
            // SBC A, L
            0x9D => {
                self.SBC_R_R(A, L);
            }
            // SBC A, (HL)
            0x9E => {
                self.SBC_R_MEM_RR(mem, A, HL);
            }
            // SBC A, A
            0x9F => {
                self.SBC_R_R(A, A);
            }
            // AND A, B
            0xA0 => {
                self.AND_R_R(A, B);
            }
            // AND A, C
            0xA1 => {
                self.AND_R_R(A, C);
            }
            // AND A, D
            0xA2 => {
                self.AND_R_R(A, D);
            }
            // AND A, E
            0xA3 => {
                self.AND_R_R(A, E);
            }
            // AND A, H
            0xA4 => {
                self.AND_R_R(A, H);
            }
            // AND A, L
            0xA5 => {
                self.AND_R_R(A, L);
            }
            // AND A, (HL)
            0xA6 => {
                self.AND_R_MEM_RR(mem, A, HL);
            }
            // AND A, A
            0xA7 => {
                self.AND_R_R(A, A);
            }
            // XOR A, B
            0xA8 => {
                self.XOR_R_R(A, B);
            }
            // XOR A, C
            0xA9 => {
                self.XOR_R_R(A, C);
            }
            // XOR A, D
            0xAA => {
                self.XOR_R_R(A, D);
            }
            // XOR A, E
            0xAB => {
                self.XOR_R_R(A, E);
            }
            // XOR A, H
            0xAC => {
                self.XOR_R_R(A, H);
            }
            // XOR A, L
            0xAD => {
                self.XOR_R_R(A, L);
            }
            // XOR A, (HL)
            0xAE => {
                self.XOR_R_MEM_RR(mem, A, HL);
            }
            // XOR A, A
            0xAF => {
                self.XOR_R_R(A, A);
            }
            // OR A, B
            0xB0 => {
                self.OR_R_R(A, B);
            }
            // OR A, C
            0xB1 => {
                self.OR_R_R(A, C);
            }
            // OR A, D
            0xB2 => {
                self.OR_R_R(A, D);
            }
            // OR A, E
            0xB3 => {
                self.OR_R_R(A, E);
            }
            // OR A, H
            0xB4 => {
                self.OR_R_R(A, H);
            }
            // OR A, L
            0xB5 => {
                self.OR_R_R(A, L);
            }
            // OR A, (HL)
            0xB6 => {
                self.OR_R_MEM_RR(mem, A, HL);
            }
            // OR A, A
            0xB7 => {
                self.OR_R_R(A, A);
            }
            // CP A, B
            0xB8 => {
                self.CP_R_R(A, B);
            }
            // CP A, C
            0xB9 => {
                self.CP_R_R(A, C);
            }
            // CP A, D
            0xBA => {
                self.CP_R_R(A, D);
            }
            // CP A, E
            0xBB => {
                self.CP_R_R(A, E);
            }
            // CP A, H
            0xBC => {
                self.CP_R_R(A, H);
            }
            // CP A, L
            0xBD => {
                self.CP_R_R(A, L);
            }
            // CP A, (HL)
            0xBE => {
                self.CP_R_MEM_RR(mem, A, HL);
            }
            // CP A, A
            0xBF => {
                self.CP_R_R(A, A);
            }
            // RET NZ
            0xC0 => {
                self.RET_C(mem, !self.flags.z());
            }
            // POP BC
            0xC1 => {
                self.POP(mem, BC);
            }
            // JP NZ, a16
            0xC2 => {
                self.JP_C_AA(mem, !self.flags.z());
            }
            // JP a16
            0xC3 => {
                self.JP_C_AA(mem, true);
            }
            // CALL NZ, a16
            0xC4 => {
                self.CALL_C(mem, !self.flags.z());
            }
            // PUSH BC
            0xC5 => {
                self.PUSH(mem, BC);
            }
            // ADD A, d8
            0xC6 => {
                let rhs = mem[self.regs[PC] + 1];
                self.flags.set_h(Self::half_carry_add_u8(self.regs[A], rhs));
                self.flags.set_n(false);

                let original = self.regs[A];
                self.regs[A] = self.regs[A].wrapping_add(rhs);
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_c(self.regs[A] < original);
                self.regs[PC] += 2;
                self.cycles += 2;
            }
            // RST 0
            0xC7 => {
                self.RST(mem, 0);
            }
            // RET Z
            0xC8 => {
                self.RET_C(mem, self.flags.z());
            }
            // RET
            0xC9 => {
                self.RET(mem);
            }
            // JP Z, a16
            0xCA => {
                self.JP_C_AA(mem, self.flags.z());
            }
            // Double Instruction
            0xCB => {
                todo!("2-byte Instructions");
            }
            // CALL Z, a16
            0xCC => {
                self.CALL_C(mem, self.flags.z());
            }
            // CALL a16
            0xCD => {
                self.CALL_C(mem, true);
            }
            // ADC A, d8
            0xCE => {
                let rhs = mem[self.regs[PC] + 1];
                let c = if self.flags.c() { 1 } else { 0 };
                let h = ((self.regs[A] & 0x0F)
                    .wrapping_add(rhs & 0x0F)
                    .wrapping_add(c))
                    & 0x10
                    == 0x10;
                self.flags.set_h(h);
                self.flags.set_n(false);

                let original = self.regs[A];
                self.regs[A] = self.regs[A].wrapping_add(rhs).wrapping_add(c);
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_c(self.regs[A] < original);
                self.regs[PC] += 2;
                self.cycles += 2;
            }
            // RST 1
            0xCF => {
                self.RST(mem, 1);
            }
            // RET NC
            0xD0 => {
                self.RET_C(mem, !self.flags.c());
            }
            // POP DE
            0xD1 => {
                self.POP(mem, DE);
            }
            // JP NC, a16
            0xD2 => {
                self.JP_C_AA(mem, !self.flags.c());
            }
            // CALL NC, a16
            0xD4 => {
                self.CALL_C(mem, !self.flags.c());
            }
            // PUSH DE
            0xD5 => {
                self.PUSH(mem, DE);
            }
            // SUB d8
            0xD6 => {
                let rhs = mem[self.regs[PC] + 1];
                self.flags.set_h(Self::half_carry_sub_u8(self.regs[A], rhs));
                self.flags.set_n(true);

                let original = self.regs[A];
                self.regs[A] = self.regs[A].wrapping_sub(rhs);
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_c(self.regs[A] > original);
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // RST 2
            0xD7 => {
                self.RST(mem, 2);
            }
            // RET C
            0xD8 => {
                self.RET_C(mem, self.flags.c());
            }
            // RETI
            0xD9 => {
                self.POP(mem, PC);
                self.regs[PC] -= 1;
                self.cycles += 1;
                todo!("RETI interrupt stuff");
            }
            // JP C, a16
            0xDA => {
                self.JP_C_AA(mem, self.flags.c());
            }
            // CALL C, a16
            0xDC => {
                self.CALL_C(mem, self.flags.c());
            }
            // SBC A, d8
            0xDE => {
                let rhs = mem[self.regs[PC] + 1];
                let c = if self.flags.c() { 1 } else { 0 };
                let h = ((self.regs[A] & 0x0F)
                    .wrapping_sub(rhs & 0x0F)
                    .wrapping_sub(c))
                    & 0x10
                    == 0x10;
                self.flags.set_h(h);
                self.flags.set_n(false);

                let original = self.regs[A];
                self.regs[A] = self.regs[A].wrapping_sub(rhs).wrapping_sub(c);
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_c(self.regs[A] > original);
                self.regs[PC] += 2;
                self.cycles += 2;
            }
            // RST 3
            0xDF => {
                self.RST(mem, 3);
            }
            // LD (a8), A
            0xE0 => {
                mem.write_u8(0xFF00 + mem[self.regs[PC] + 1] as u16, self.regs[A]);
                self.regs[PC] += 2;
                self.cycles += 3;
            }
            // POP HL
            0xE1 => {
                self.POP(mem, HL);
            }
            // LD (C), A
            0xE2 => {
                mem.write_u8(0xFF00 + self.regs[C] as u16, self.regs[A]);

                self.regs[PC] += 1;
                self.cycles += 2;
            }
            // PUSH HL
            0xE5 => {
                self.PUSH(mem, HL);
            }
            // AND d8
            0xE6 => {
                let rhs = mem[self.regs[PC] + 1];
                self.regs[A] &= rhs;
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_n(false);
                self.flags.set_h(true);
                self.flags.set_c(false);
                self.regs[PC] += 2;
                self.cycles += 2;
            }
            // RST 4
            0xE7 => {
                self.RST(mem, 4);
            }
            // ADD SP, s8
            0xE8 => {
                let rhs = mem[self.regs[PC] + 1] as i8 as i16;
                if rhs > 0 {
                    self.flags
                        .set_h(Self::half_carry_add_u16(self.regs[SP], rhs as u16));
                } else {
                    self.flags
                        .set_h(Self::half_carry_sub_u16(self.regs[SP], (-rhs) as u16));
                }

                let original = self.regs[SP];
                self.regs[SP] = self.regs[SP].wrapping_add_signed(rhs);
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_c(self.regs[SP] < original);
                self.regs[PC] += 2;
                self.cycles += 4;
            }
            // JP HL
            0xE9 => {
                self.regs[PC] = self.regs[HL];
                self.cycles += 1;
            }
            // LD (a16), A
            0xEA => {
                mem.write_u8(mem.read_16(self.regs[PC] + 1), self.regs[A]);

                self.regs[PC] += 3;
                self.cycles += 4;
            }
            // XOR d8
            0xEE => {
                let rhs = mem[self.regs[PC] + 1];
                self.regs[A] ^= rhs;
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(false);
                self.regs[PC] += 2;
                self.cycles += 2;
            }
            // RST 5
            0xEF => {
                self.RST(mem, 5);
            }
            // LD A, (a8)
            0xF0 => {
                self.regs[A] = mem[self.regs[PC] + 1];
                self.regs[PC] += 2;
                self.cycles += 3;
            }
            // POP AF
            0xF1 => {
                self.POP(mem, AF);
            }
            // LD A, (C)
            0xF2 => {
                self.regs[A] = mem[0xFF00 + self.regs[C] as u16];
                self.regs[PC] += 1;
                self.cycles += 2;
            }
            // DI
            0xF3 => {
                todo!("Disable Interrupts");
            }
            // PUSH AF
            0xF5 => {
                self.PUSH(mem, AF);
            }
            // OR d8
            0xF6 => {
                let rhs = mem[self.regs[PC] + 1];
                self.regs[A] |= rhs;
                self.flags.set_z(self.regs[A] == 0);
                self.flags.set_n(false);
                self.flags.set_h(false);
                self.flags.set_c(false);
                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // RST 6
            0xF7 => {
                self.RST(mem, 6);
            }
            // LD HL, SP+s8
            0xF8 => {
                let rhs = mem[self.regs[PC] + 1] as i8 as i16;
                if rhs > 0 {
                    self.flags
                        .set_h(Self::half_carry_add_u16(self.regs[SP], rhs as u16));
                } else {
                    self.flags
                        .set_h(Self::half_carry_sub_u16(self.regs[SP], (-rhs) as u16));
                }

                let original = self.regs[SP];
                self.regs[HL] = self.regs[SP].wrapping_add_signed(rhs);
                self.flags.set_z(false);
                self.flags.set_n(false);
                self.flags.set_c(self.regs[SP] < original);
                self.regs[PC] += 2;
                self.cycles += 3;
            }
            // LD SP, HL
            0xF9 => {
                self.regs[SP] = self.regs[HL];
                self.regs[PC] += 1;
                self.cycles += 2;
            }
            // LD A, (a16)
            0xFA => {
                self.regs[A] = mem[mem.read_16(self.regs[PC] + 1)];
                self.regs[PC] += 3;
                self.cycles += 4;
            }
            // EI
            0xFB => {
                todo!("Enabled Interrupt");
            }
            // CP d8
            0xFE => {
                let rhs = mem[self.regs[PC] + 1];
                self.flags.set_z(self.regs[A].wrapping_sub(rhs) == 0);
                self.flags.set_n(true);
                self.flags.set_h(Self::half_carry_sub_u8(self.regs[A], rhs));
                self.flags.set_c(self.regs[A].checked_sub(rhs).is_none());

                self.regs[PC] += 1;
                self.cycles += 1;
            }
            // RST 7
            0xFF => {
                self.RST(mem, 7);
            }

            // Unimpl
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {}
        }
    }

    fn RST(&mut self, mem: &mut Memory, n: u16) {
        self.PUSH(mem, PC);
        self.regs[PC] = n * 8;
    }

    fn RET_C(&mut self, mem: &Memory, cond: bool) {
        if cond {
            self.regs[PC] = mem.read_16(self.regs[SP]);
            self.regs[SP] += 2;
            self.cycles += 5;
        } else {
            self.regs[PC] += 1;
            self.cycles += 2;
        }
    }

    fn RET(&mut self, mem: &Memory) {
        self.regs[PC] = mem.read_16(self.regs[SP]);
        self.regs[SP] += 2;
        self.cycles += 4;
    }

    fn POP(&mut self, mem: &Memory, reg: REG_WIDE) {
        self.regs[reg] = mem.read_16(self.regs[SP]);
        self.regs[SP] += 2;
        self.cycles += 3;
        self.regs[PC] += 1;
    }

    fn PUSH(&mut self, mem: &mut Memory, reg: REG_WIDE) {
        self.regs[SP] -= 2;
        mem.write_u16(self.regs[SP], self.regs[reg]);
        self.cycles += 4;
        self.regs[PC] += 1;
    }

    fn JP_C_AA(&mut self, mem: &Memory, jump: bool) {
        if jump {
            self.regs[PC] = mem.read_16(self.regs[PC] + 1);
            self.cycles += 4;
        } else {
            self.regs[PC] += 3;
            self.cycles += 3;
        }
    }

    fn CALL_C(&mut self, mem: &mut Memory, call: bool) {
        if call {
            let addr = mem.read_16(self.regs[PC] + 1);
            self.regs[PC] += 3;
            self.PUSH(mem, PC);

            self.regs[PC] = addr;
            self.cycles += 2;
        } else {
            self.regs[PC] += 3;
            self.cycles += 3;
        }
    }

    fn JR_C(&mut self, mem: &mut Memory, jump: bool) {
        if jump {
            self.regs[PC] =
                self.regs[PC].wrapping_add_signed((mem[self.regs[PC] + 1] as i8) as i16);
            self.cycles += 3;
        } else {
            self.regs[PC] += 2;
            self.cycles += 2;
        }
    }

    fn ADD_RR_RR(&mut self, r1: REG_WIDE, r2: REG_WIDE) {
        self.flags
            .set_h(Self::half_carry_add_u16(self.regs[r1], self.regs[r2]));
        self.flags.set_n(false);

        self.regs[r1] = self.regs[r1].wrapping_add(self.regs[r2]);
        self.flags.set_z(self.regs[r1] == 0);

        self.cycles += 2;
        self.regs[PC] += 1;
    }

    fn ADD_R_R(&mut self, r1: REG, r2: REG) {
        self.flags
            .set_h(Self::half_carry_add_u8(self.regs[r1], self.regs[r2]));
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_add(self.regs[r2]);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] < original);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn ADD_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = mem[self.regs[addr]];
        self.flags
            .set_h(Self::half_carry_add_u8(self.regs[r1], rhs));
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_add(rhs);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] < original);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn ADC_R_R(&mut self, r1: REG, r2: REG) {
        let c = if self.flags.c() { 1 } else { 0 };
        let h = ((self.regs[r1] & 0x0F)
            .wrapping_add(self.regs[r2] & 0x0F)
            .wrapping_add(c))
            & 0x10
            == 0x10;
        self.flags.set_h(h);
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_add(self.regs[r2]).wrapping_add(c);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] < original);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn ADC_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = mem[self.regs[addr]];
        let c = if self.flags.c() { 1 } else { 0 };
        let h = ((self.regs[r1] & 0x0F)
            .wrapping_add(rhs & 0x0F)
            .wrapping_add(c))
            & 0x10
            == 0x10;
        self.flags.set_h(h);
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_add(rhs).wrapping_add(c);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] < original);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn SUB_R_R(&mut self, r1: REG, r2: REG) {
        self.flags
            .set_h(Self::half_carry_sub_u8(self.regs[r1], self.regs[r2]));
        self.flags.set_n(true);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_sub(self.regs[r2]);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] > original);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn SUB_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = mem[self.regs[addr]];
        self.flags
            .set_h(Self::half_carry_sub_u8(self.regs[r1], rhs));
        self.flags.set_n(true);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_sub(rhs);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] > original);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn SBC_R_R(&mut self, r1: REG, r2: REG) {
        let c = if self.flags.c() { 1 } else { 0 };
        let h = ((self.regs[r1] & 0x0F)
            .wrapping_sub(self.regs[r2] & 0x0F)
            .wrapping_sub(c))
            & 0x10
            == 0x10;
        self.flags.set_h(h);
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_sub(self.regs[r2]).wrapping_sub(c);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] > original);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn SBC_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = mem[self.regs[addr]];
        let c = if self.flags.c() { 1 } else { 0 };
        let h = ((self.regs[r1] & 0x0F)
            .wrapping_sub(rhs & 0x0F)
            .wrapping_sub(c))
            & 0x10
            == 0x10;
        self.flags.set_h(h);
        self.flags.set_n(false);

        let original = self.regs[r1];
        self.regs[r1] = self.regs[r1].wrapping_sub(rhs).wrapping_sub(c);
        self.flags.set_z(self.regs[r1] == 0);
        self.flags.set_c(self.regs[r1] > original);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn AND_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] &= self.regs[src];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(true);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn AND_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] &= mem[self.regs[addr]];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(true);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn XOR_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] ^= self.regs[src];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(false);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn XOR_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] ^= mem[self.regs[addr]];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(false);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn OR_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] |= self.regs[src];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(false);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn OR_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] |= mem[self.regs[addr]];
        self.flags.set_z(self.regs[dst] == 0);
        self.flags.set_n(false);
        self.flags.set_h(false);
        self.flags.set_c(false);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn CP_R_R(&mut self, lhs: REG, rhs: REG) {
        self.flags
            .set_z(self.regs[lhs].wrapping_sub(self.regs[rhs]) == 0);
        self.flags.set_n(true);
        self.flags
            .set_h(Self::half_carry_sub_u8(self.regs[lhs], self.regs[rhs]));
        self.flags
            .set_c(self.regs[lhs].checked_sub(self.regs[rhs]).is_none());

        self.regs[PC] += 1;
        self.cycles += 1;
    }

    fn CP_R_MEM_RR(&mut self, mem: &Memory, lhs: REG, addr: REG_WIDE) {
        let rhs = mem[self.regs[addr]];
        self.flags.set_z(self.regs[lhs].wrapping_sub(rhs) == 0);
        self.flags.set_n(true);
        self.flags
            .set_h(Self::half_carry_sub_u8(self.regs[lhs], rhs));
        self.flags.set_c(self.regs[lhs].checked_sub(rhs).is_none());

        self.regs[PC] += 1;
        self.cycles += 2;
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

    fn INC_MEM_RR(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        let addr = self.regs[addr];
        self.flags.set_h(Self::half_carry_add_u8(mem[addr], 1));
        mem.write_u8(addr, mem[addr].wrapping_add(1));
        self.flags.set_n(false);
        self.flags.set_z(mem[addr] == 0);
        self.regs[PC] += 1;
        self.cycles += 3;
    }

    fn DEC_MEM_RR(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        let addr = self.regs[addr];
        self.flags.set_h(Self::half_carry_sub_u8(mem[addr], 1));
        mem.write_u8(addr, mem[addr].wrapping_sub(1));
        self.flags.set_n(true);
        self.flags.set_z(mem[addr] == 0);
        self.regs[PC] += 1;
        self.cycles += 3;
    }

    fn LD_RR(&mut self, mem: &Memory, dst: REG_WIDE) {
        self.regs[dst] = mem.read_16(self.regs[PC] + 1);
        self.regs[PC] += 3;
        self.cycles += 3;
    }

    fn LD_R(&mut self, mem: &Memory, dst: REG) {
        self.regs[dst] = mem[self.regs[PC] + 1];
        self.regs[PC] += 2;
        self.cycles += 2;
    }

    fn LD_MEM_RR_R(&mut self, mem: &mut Memory, addr: REG_WIDE, data: REG) {
        mem.write_u8(self.regs[addr], self.regs[data]);
        self.regs[PC] += 1;
        self.cycles += 2;
    }

    fn LD_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] = mem[self.regs[addr]];

        self.cycles += 2;
        self.regs[PC] += 1;
    }
    fn LD_MEM_RR_D(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        mem.write_u8(self.regs[addr], mem[self.regs[PC] + 1]);
        self.regs[PC] += 2;
        self.cycles += 3;
    }

    fn LD_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] = self.regs[src];
        self.cycles += 1;
        self.regs[PC] += 1;
    }

    fn half_carry_add_u16(a: u16, b: u16) -> bool {
        ((a & 0x0FFF).wrapping_add(b & 0x0FFF)) & 0x1000 == 0x1000
    }
    fn half_carry_add_u8(a: u8, b: u8) -> bool {
        ((a & 0x0F).wrapping_add(b & 0x0F)) & 0x10 == 0x10
    }
    fn half_carry_sub_u16(a: u16, b: u16) -> bool {
        ((a & 0x0FFF).wrapping_sub(b & 0x0FFF)) & 0x1000 == 0x1000
    }
    fn half_carry_sub_u8(a: u8, b: u8) -> bool {
        ((a & 0x0F).wrapping_sub(b & 0x0F)) & 0x10 == 0x10
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
