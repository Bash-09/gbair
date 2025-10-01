#![allow(non_snake_case)]

use crate::{
    mem::{self, Memory},
    registers::{
        Flag, Flags, Registers,
        REG::{self, *},
        REG_WIDE::{self, *},
    },
};

pub struct CPU {
    pub regs: Registers,
    pub flags: Flags,
    /// Using the 1MHz convention instead of the 4MHz physical clock that takes 4 cycles for everything
    pub cycles: usize,
    pub interrupt_master_enable: bool,
    pub interrupt_enable_scheduled: bool,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            regs: Registers::new(),
            flags: Flags::new(),
            cycles: 0,
            interrupt_master_enable: false,
            interrupt_enable_scheduled: false,
        }
    }

    pub fn next_cycle(&mut self, mem: &mut Memory) {
        self.cycles = self.cycles.saturating_sub(1);

        if self.cycles == 0 {
            self.handle_interrupts(mem);
        }

        if self.cycles == 0 {
            self.next_instruction(mem);
        }
    }

    /// Checks any pending interrupts and branches to them
    pub fn handle_interrupts(&mut self, mem: &mut Memory) {
        // Don't handle if IME is disabled or no interrupts are pending
        if !self.interrupt_master_enable {
            return;
        }

        let interrupts = self.READ_8(mem, mem::ADDR_IE) & self.READ_8(mem, mem::ADDR_IF);
        if interrupts == 0 {
            return;
        }

        // Disable IME and push PC to stack.
        self.interrupt_master_enable = false;
        self.PUSH(mem, REG_WIDE::PC);

        // Branch to interrupt handler and clear bit.
        if interrupts & 1 << 0 != 0 {
            // VBLANK
            self.regs[PC] = mem::ADDR_INT_VBLANK;
            mem.write_u8(mem::ADDR_IF, mem.read_8(mem::ADDR_IF) & !(1 << 0));
        } else if interrupts & 1 << 1 != 0 {
            // LCD STAT
            self.regs[PC] = mem::ADDR_INT_LCDC;
            mem.write_u8(mem::ADDR_IF, mem.read_8(mem::ADDR_IF) & !(1 << 1));
        } else if interrupts & 1 << 2 != 0 {
            // Timer
            self.regs[PC] = mem::ADDR_INT_TIMER;
            mem.write_u8(mem::ADDR_IF, mem.read_8(mem::ADDR_IF) & !(1 << 2));
        } else if interrupts & 1 << 3 != 0 {
            // Serial
            self.regs[PC] = mem::ADDR_INT_SERIAL;
            mem.write_u8(mem::ADDR_IF, mem.read_8(mem::ADDR_IF) & !(1 << 3));
        } else if interrupts & 1 << 4 != 0 {
            // Joypad
            self.regs[PC] = mem::ADDR_INT_HTL_P0_P13;
            mem.write_u8(mem::ADDR_IF, mem.read_8(mem::ADDR_IF) & !(1 << 4));
        }
    }

    pub fn next_instruction_summary(&self, mem: &Memory) -> String {
        let opcode = self.next_instruction_opcode(mem);
        let mut out = opcode.to_string();

        if out.contains(", (AF)") {
            out = out.replace(
                ", (AF)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[AF],
                    mem.read_16(self.regs[AF])
                ),
            );
        }
        if out.contains(", (BC)") {
            out = out.replace(
                ", (BC)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[BC],
                    mem.read_16(self.regs[BC])
                ),
            );
        }
        if out.contains(", (DE)") {
            out = out.replace(
                ", (DE)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[DE],
                    mem.read_16(self.regs[DE])
                ),
            );
        }
        if out.contains(", (HL)") {
            out = out.replace(
                ", (HL)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[HL],
                    mem.read_16(self.regs[HL])
                ),
            );
        }
        if out.contains(", (HL+)") {
            out = out.replace(
                ", (HL+)",
                &format!(
                    ", ({:#04X}+)={:#04X}",
                    self.regs[HL],
                    mem.read_16(self.regs[HL])
                ),
            );
        }
        if out.contains(", (HL-)") {
            out = out.replace(
                ", (HL-)",
                &format!(
                    ", ({:#04X}-)={:#04X}",
                    self.regs[HL],
                    mem.read_16(self.regs[HL])
                ),
            );
        }
        if out.contains(", (SP)") {
            out = out.replace(
                ", (SP)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[SP],
                    mem.read_16(self.regs[SP])
                ),
            );
        }
        if out.contains(", (PC)") {
            out = out.replace(
                ", (PC)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    self.regs[PC],
                    mem.read_16(self.regs[PC])
                ),
            );
        }
        if out.contains(", (a16)") {
            out = out.replace(
                ", (a16)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    mem.read_16(self.regs[PC].wrapping_add(1)),
                    mem.read_16(mem.read_16(self.regs[PC].wrapping_add(1)))
                ),
            );
        }
        if out.contains(", (a8)") {
            out = out.replace(
                ", (a8)",
                &format!(
                    ", ({:#04X})={:#04X}",
                    (mem.read_8(self.regs[PC].wrapping_add(1)) as u16).wrapping_add(0xFF00),
                    mem.read_16(
                        (mem.read_8(self.regs[PC].wrapping_add(1)) as u16).wrapping_add(0xFF00)
                    )
                ),
            );
        }

        if out.contains(", AF") {
            out = out.replace(", AF", &format!(", {:#04X}", self.regs[AF]));
        }
        if out.contains(", BC") {
            out = out.replace(", BC", &format!(", {:#04X}", self.regs[BC]));
        }
        if out.contains(", DE") {
            out = out.replace(", DE", &format!(", {:#04X}", self.regs[DE]));
        }
        if out.contains(", HL") {
            out = out.replace(", HL", &format!(", {:#04X}", self.regs[HL]));
        }
        if out.contains(", SP") {
            out = out.replace(", SP", &format!(", {:#04X}", self.regs[SP]));
        }
        if out.contains(", PC") {
            out = out.replace(", PC", &format!(", {:#04X}", self.regs[PC]));
        }

        if out.contains("(AF)") {
            out = out.replace("(AF)", &format!("({:#04X})", self.regs[AF]));
        }
        if out.contains("(BC)") {
            out = out.replace("(BC)", &format!("({:#04X})", self.regs[BC]));
        }
        if out.contains("(DE)") {
            out = out.replace("(DE)", &format!("({:#04X})", self.regs[DE]));
        }
        if out.contains("(HL)") {
            out = out.replace("(HL)", &format!("({:#04X})", self.regs[HL]));
        }
        if out.contains("(HL+)") {
            out = out.replace("(HL+)", &format!("({:#04X})", self.regs[HL]));
        }
        if out.contains("(HL-)") {
            out = out.replace("(HL-)", &format!("({:#04X})", self.regs[HL]));
        }
        if out.contains("(SP)") {
            out = out.replace("(SP)", &format!("({:#04X})", self.regs[SP]));
        }
        if out.contains("(PC)") {
            out = out.replace("(PC)", &format!("({:#04X})", self.regs[PC]));
        }

        if out.contains("a16") {
            out = out.replace(
                "a16",
                &format!("{:#04X}", mem.read_16(self.regs[PC].wrapping_add(1))),
            );
        }
        if out.contains("a8") {
            out = out.replace(
                "a8",
                &format!(
                    "{:#04X}",
                    (mem.read_8(self.regs[PC].wrapping_add(1)) as u16).wrapping_add(0xFF00)
                ),
            );
        }
        if out.contains("d16") {
            out = out.replace(
                "d16",
                &format!("{:#04X}", mem.read_16(self.regs[PC].wrapping_add(1))),
            );
        }
        if out.contains("d8") {
            out = out.replace(
                "d8",
                &format!("{:#04X}", mem.read_8(self.regs[PC].wrapping_add(1))),
            );
        }
        if out.contains("s8") {
            out = out.replace(
                "s8",
                &format!("{}", mem.read_8(self.regs[PC].wrapping_add(1)) as i8),
            );
        }

        return out;
    }

    pub fn next_instruction_opcode(&self, mem: &Memory) -> &'static str {
        match mem[self.regs[PC]] {
            // NOP
            0x00 => "NOP",
            // LD BC, d16
            0x01 => "LD BC, d16",
            // LD (BC), A
            0x02 => "LD (BC), A",
            // INC BC
            0x03 => "INC BC",
            // INC B
            0x04 => "INC B",
            // DEC B
            0x05 => "DEC B",
            // LD B, d8
            0x06 => "LD B, d8",
            // RLCA
            0x07 => "RLCA",
            // LD (a16), SP
            0x08 => "LD (a16), SP",
            // ADD HL, BC
            0x09 => "ADD HL, BC",
            // LD A, (BC)
            0x0A => "LD A, (BC)",
            // DEC BC
            0x0B => "DEC BC",
            // INC C
            0x0C => "INC C",
            // DEC C
            0x0D => "DEC C",
            // LD C, d8
            0x0E => "LD C, d8",
            // RRCA
            0x0F => "RRCA",
            // STOP
            0x10 => "STOP",
            // LD DE, d16
            0x11 => "LD DE, d16",
            // LD (DE), A
            0x12 => "LD (DE), A",
            // INC DE
            0x13 => "INC DE",
            // INC D
            0x14 => "INC D",
            // DEC D
            0x15 => "DEC D",
            // LD D, d8
            0x16 => "LD D, d8",
            // RLA
            0x17 => "RLA",
            // JR s8
            0x18 => "JR s8",
            // ADD HL, DE
            0x19 => "ADD HL, DE",
            // LD A, (DE)
            0x1A => "LD A, (DE)",
            // DEC DE
            0x1B => "DEC DE",
            // INC E
            0x1C => "INC E",
            // DEC E
            0x1D => "DEC E",
            // LD E, d8
            0x1E => "LD E, d8",
            // RRA
            0x1F => "RRA",
            // JR NZ, s8
            0x20 => "JR NZ, s8",
            // LD HL, d16
            0x21 => "LD HL, d16",
            // LD (HL+), A
            0x22 => "LD (HL+), A",
            // INC HL
            0x23 => "INC HL",
            // INC H
            0x24 => "INC H",
            // DEC H
            0x25 => "DEC H",
            // LD H, d8
            0x26 => "LD H, d8",
            // DAA
            0x27 => "DAA",
            // JR Z, s8
            0x28 => "JR Z, s8",
            // ADD HL, HL
            0x29 => "ADD HL, HL",
            // LD A, (HL+)
            0x2A => "LD A, (HL+)",
            // DEC HL
            0x2B => "DEC HL",
            // INC L
            0x2C => "INC L",
            // DEC L
            0x2D => "DEC L",
            // LD L, d8
            0x2E => "LD L, d8",
            // CPL
            0x2F => "CPL",
            // JR NC, s8
            0x30 => "JR NC, s8",
            // LD SP, d16
            0x31 => "LD SP, d16",
            // LD (HL-), A
            0x32 => "LD (HL-), A",
            // INC SP
            0x33 => "INC SP",
            // INC (HL)
            0x34 => "INC (HL)",
            // DEC (HL)
            0x35 => "DEC (HL)",
            // LD (HL) d8
            0x36 => "LD (HL) d8",
            // SCF
            0x37 => "SCF",
            // JR C, s8
            0x38 => "JR C, s8",
            // ADD HL, SP
            0x39 => "ADD HL, SP",
            // LD A, (HL-)
            0x3A => "LD A, (HL-)",
            // DEC SP
            0x3B => "DEC SP",
            // INC A
            0x3C => "INC A",
            // DEC A
            0x3D => "DEC A",
            // LD A, d8
            0x3E => "LD A, d8",
            // CCF
            0x3F => "CCF",
            // LD B, B
            0x40 => "LD B, B",
            // LD, B, C
            0x41 => "LD, B, C",
            // LD B, D
            0x42 => "LD B, D",
            // LD B, E
            0x43 => "LD B, E",
            // LD B, H
            0x44 => "LD B, H",
            // LD B, L
            0x45 => "LD B, L",
            // LD B, (HL)
            0x46 => "LD B, (HL)",
            // LD B, A
            0x47 => "LD B, A",
            // LD C, B
            0x48 => "LD C, B",
            // LD C, C
            0x49 => "LD C, C",
            // LD C, D
            0x4A => "LD C, D",
            // LD C, E
            0x4B => "LD C, E",
            // LD C, H
            0x4C => "LD C, H",
            // LD C, L
            0x4D => "LD C, L",
            // LD C, (HL)
            0x4E => "LD C, (HL)",
            // LD C, A
            0x4F => "LD C, A",
            // LD D, B
            0x50 => "LD D, B",
            // LD, D, C
            0x51 => "LD, D, C",
            // LD D, D
            0x52 => "LD D, D",
            // LD D, E
            0x53 => "LD D, E",
            // LD D, H
            0x54 => "LD D, H",
            // LD D, L
            0x55 => "LD D, L",
            // LD D, (HL)
            0x56 => "LD D, (HL)",
            // LD D, A
            0x57 => "LD D, A",
            // LD E, B
            0x58 => "LD E, B",
            // LD E, C
            0x59 => "LD E, C",
            // LD E, D
            0x5A => "LD E, D",
            // LD E, E
            0x5B => "LD E, E",
            // LD E, H
            0x5C => "LD E, H",
            // LD E, L
            0x5D => "LD E, L",
            // LD E, (HL)
            0x5E => "LD E, (HL)",
            // LD E, A
            0x5F => "LD E, A",
            // LD H, B
            0x60 => "LD H, B",
            // LD H, C
            0x61 => "LD H, C",
            // LD H, D
            0x62 => "LD H, D",
            // LD H, E
            0x63 => "LD H, E",
            // LD H, H
            0x64 => "LD H, H",
            // LD H, L
            0x65 => "LD H, L",
            // LD H, (HL)
            0x66 => "LD H, (HL)",
            // LD H, A
            0x67 => "LD H, A",
            // LD L, B
            0x68 => "LD L, B",
            // LD L, C
            0x69 => "LD L, C",
            // LD L, D
            0x6A => "LD L, D",
            // LD L, E
            0x6B => "LD L, E",
            // LD L, H
            0x6C => "LD L, H",
            // LD L, L
            0x6D => "LD L, L",
            // LD L, (HL)
            0x6E => "LD L, (HL)",
            // LD L, A
            0x6F => "LD L, A",
            // LD (HL), B
            0x70 => "LD (HL), B",
            // LD (HL), C
            0x71 => "LD (HL), C",
            // LD (HL), D
            0x72 => "LD (HL), D",
            // LD (HL), E
            0x73 => "LD (HL), E",
            // LD (HL), H
            0x74 => "LD (HL), H",
            // LD (HL), L
            0x75 => "LD (HL), L",
            // HALT
            0x76 => "HALT",
            // LD (HL), A
            0x77 => "LD (HL), A",
            // LD A, B
            0x78 => "LD A, B",
            // LD A, C
            0x79 => "LD A, C",
            // LD A, D
            0x7A => "LD A, D",
            // LD A, E
            0x7B => "LD A, E",
            // LD A, H
            0x7C => "LD A, H",
            // LD A, L
            0x7D => "LD A, L",
            // LD A, (HL)
            0x7E => "LD A, (HL)",
            // LD A, A
            0x7F => "LD A, A",
            // ADD A, B
            0x80 => "ADD A, B",
            // ADD A, C
            0x81 => "ADD A, C",
            // ADD A, D
            0x82 => "ADD A, D",
            // ADD A, E
            0x83 => "ADD A, E",
            // ADD A, H
            0x84 => "ADD A, H",
            // ADD A, L
            0x85 => "ADD A, L",
            // ADD A, (HL)
            0x86 => "ADD A, (HL)",
            // ADD A, A
            0x87 => "ADD A, A",
            // ADC A, B
            0x88 => "ADC A, B",
            // ADC A, C
            0x89 => "ADC A, C",
            // ADC A, D
            0x8A => "ADC A, D",
            // ADC A, E
            0x8B => "ADC A, E",
            // ADC A, H
            0x8C => "ADC A, H",
            // ADC A, L
            0x8D => "ADC A, L",
            // ADC A, (HL)
            0x8E => "ADC A, (HL)",
            // ADC A, A
            0x8F => "ADC A, A",
            // SUB A, B
            0x90 => "SUB A, B",
            // SUB A, C
            0x91 => "SUB A, C",
            // SUB A, D
            0x92 => "SUB A, D",
            // SUB A, E
            0x93 => "SUB A, E",
            // SUB A, H
            0x94 => "SUB A, H",
            // SUB A, L
            0x95 => "SUB A, L",
            // SUB A, (HL)
            0x96 => "SUB A, (HL)",
            // SUB A, A
            0x97 => "SUB A, A",
            // SBC A, B
            0x98 => "SBC A, B",
            // SBC A, C
            0x99 => "SBC A, C",
            // SBC A, D
            0x9A => "SBC A, D",
            // SBC A, E
            0x9B => "SBC A, E",
            // SBC A, H
            0x9C => "SBC A, H",
            // SBC A, L
            0x9D => "SBC A, L",
            // SBC A, (HL)
            0x9E => "SBC A, (HL)",
            // SBC A, A
            0x9F => "SBC A, A",
            // AND A, B
            0xA0 => "AND A, B",
            // AND A, C
            0xA1 => "AND A, C",
            // AND A, D
            0xA2 => "AND A, D",
            // AND A, E
            0xA3 => "AND A, E",
            // AND A, H
            0xA4 => "AND A, H",
            // AND A, L
            0xA5 => "AND A, L",
            // AND A, (HL)
            0xA6 => "AND A, (HL)",
            // AND A, A
            0xA7 => "AND A, A",
            // XOR A, B
            0xA8 => "XOR A, B",
            // XOR A, C
            0xA9 => "XOR A, C",
            // XOR A, D
            0xAA => "XOR A, D",
            // XOR A, E
            0xAB => "XOR A, E",
            // XOR A, H
            0xAC => "XOR A, H",
            // XOR A, L
            0xAD => "XOR A, L",
            // XOR A, (HL)
            0xAE => "XOR A, (HL)",
            // XOR A, A
            0xAF => "XOR A, A",
            // OR A, B
            0xB0 => "OR A, B",
            // OR A, C
            0xB1 => "OR A, C",
            // OR A, D
            0xB2 => "OR A, D",
            // OR A, E
            0xB3 => "OR A, E",
            // OR A, H
            0xB4 => "OR A, H",
            // OR A, L
            0xB5 => "OR A, L",
            // OR A, (HL)
            0xB6 => "OR A, (HL)",
            // OR A, A
            0xB7 => "OR A, A",
            // CP A, B
            0xB8 => "CP A, B",
            // CP A, C
            0xB9 => "CP A, C",
            // CP A, D
            0xBA => "CP A, D",
            // CP A, E
            0xBB => "CP A, E",
            // CP A, H
            0xBC => "CP A, H",
            // CP A, L
            0xBD => "CP A, L",
            // CP A, (HL)
            0xBE => "CP A, (HL)",
            // CP A, A
            0xBF => "CP A, A",
            // RET NZ
            0xC0 => "RET NZ",
            // POP BC
            0xC1 => "POP BC",
            // JP NZ, a16
            0xC2 => "JP NZ, a16",
            // JP a16
            0xC3 => "JP a16",
            // CALL NZ, a16
            0xC4 => "CALL NZ, a16",
            // PUSH BC
            0xC5 => "PUSH BC",
            // ADD A, d8
            0xC6 => "ADD A, d8",
            // RST 0
            0xC7 => "RST 0",
            // RET Z
            0xC8 => "RET Z",
            // RET
            0xC9 => "RET",
            // JP Z, a16
            0xCA => "JP Z, a16",
            // Double Instruction
            0xCB => "Double Instruction",
            // CALL Z, a16
            0xCC => "CALL Z, a16",
            // CALL a16
            0xCD => "CALL a16",
            // ADC A, d8
            0xCE => "ADC A, d8",
            // RST 1
            0xCF => "RST 1",
            // RET NC
            0xD0 => "RET NC",
            // POP DE
            0xD1 => "POP DE",
            // JP NC, a16
            0xD2 => "JP NC, a16",
            // CALL NC, a16
            0xD4 => "CALL NC, a16",
            // PUSH DE
            0xD5 => "PUSH DE",
            // SUB d8
            0xD6 => "SUB d8",
            // RST 2
            0xD7 => "RST 2",
            // RET C
            0xD8 => "RET C",
            // RETI
            0xD9 => "RETI",
            // JP C, a16
            0xDA => "JP C, a16",
            // CALL C, a16
            0xDC => "CALL C, a16",
            // SBC A, d8
            0xDE => "SBC A, d8",
            // RST 3
            0xDF => "RST 3",
            // LD (a8), A
            0xE0 => "LD (a8), A",
            // POP HL
            0xE1 => "POP HL",
            // LD (C), A
            0xE2 => "LD (C), A",
            // PUSH HL
            0xE5 => "PUSH HL",
            // AND d8
            0xE6 => "AND d8",
            // RST 4
            0xE7 => "RST 4",
            // ADD SP, s8
            0xE8 => "ADD SP, s8",
            // JP HL
            0xE9 => "JP HL",
            // LD (a16), A
            0xEA => "LD (a16), A",
            // XOR d8
            0xEE => "XOR d8",
            // RST 5
            0xEF => "RST 5",
            // LD A, (a8)
            0xF0 => "LD A, (a8)",
            // POP AF
            0xF1 => "POP AF",
            // LD A, (C)
            0xF2 => "LD A, (C)",
            // DI
            0xF3 => "DI",
            // PUSH AF
            0xF5 => "PUSH AF",
            // OR d8
            0xF6 => "OR d8",
            // RST 6
            0xF7 => "RST 6",
            // LD HL, SP+s8
            0xF8 => "LD HL, SP+s8",
            // LD SP, HL
            0xF9 => "LD SP, HL",
            // LD A, (a16)
            0xFA => "LD A, (a16)",
            // EI
            0xFB => "EI",
            // CP d8
            0xFE => "CP d8",
            // RST 7
            0xFF => "RST 7",
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => "Undef",
        }
    }

    /// Decodes and executes the next instruction regardless of current cycle counts or interrupt
    pub fn next_instruction(&mut self, mem: &mut Memory) {
        let instr: u8 = mem[self.regs[PC]];
        self.regs[PC] += 1;
        self.cycles += 1;

        let mut scheduled_this_instruction = false;

        match instr {
            // NOP
            0x00 => {}
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
                self.flags.set(Flag::Z, false);
                self.flags.set(Flag::N, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, self.regs[A] & 0b1 == 0b1);
            }
            // LD (a16), SP
            0x08 => {
                let addr = self.IMM16(mem);
                self.WRITE_16(mem, addr, self.regs[SP]);
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
                self.flags.set(Flag::Z, false);
                self.flags.set(Flag::C, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, self.regs[A] & 0b1 == 0b1);
                self.regs[A] = self.regs[A].rotate_right(1);
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
                let carry = self.flags.read(Flag::C);
                self.flags.set(Flag::Z, false);
                self.flags.set(Flag::C, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, self.regs[A] & 0x80 != 0);
                self.regs[A] <<= 1;
                if carry {
                    self.regs[A] |= 1;
                }
            }
            // JR s8
            0x18 => {
                let offset = self.IMM8(mem) as i8 as i16;
                self.JR(offset);
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
                let carry = self.flags.read(Flag::C);
                self.flags.set(Flag::Z, false);
                self.flags.set(Flag::C, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, self.regs[A] & 0b1 == 0b1);
                self.regs[A] >>= 1;
                if carry {
                    self.regs[A] |= 0x80;
                }
            }
            // JR NZ, s8
            0x20 => {
                self.JR_C(mem, !self.flags.read(Flag::Z));
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
                self.JR_C(mem, self.flags.read(Flag::Z));
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
                self.flags.set(Flag::C, true);
                self.flags.set(Flag::H, true);
                self.regs[L] = !self.regs[L];
            }
            // JR NC, s8
            0x30 => {
                self.JR_C(mem, !self.flags.read(Flag::C));
            }
            // LD SP, d16
            0x31 => {
                self.LD_RR(mem, SP);
            }
            // LD (HL-), A
            0x32 => {
                self.LD_MEM_RR_R(mem, HL, A);
                self.regs[HL] = self.regs[HL].wrapping_sub(1);
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
                self.LD_MEM_RR(mem, HL);
            }
            // SCF
            0x37 => {
                self.flags.set(Flag::C, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, true);
            }
            // JR C, s8
            0x38 => {
                self.JR_C(mem, self.flags.read(Flag::C));
            }
            // ADD HL, SP
            0x39 => {
                self.ADD_RR_RR(HL, SP);
            }
            // LD A, (HL-)
            0x3A => {
                self.LD_R_MEM_RR(mem, A, HL);
                self.regs[HL] = self.regs[HL].wrapping_sub(1);
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
                self.flags.set(Flag::C, false);
                self.flags.set(Flag::H, false);
                self.flags.set(Flag::C, !self.flags.read(Flag::C));
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
                self.RET_C(mem, !self.flags.read(Flag::Z));
            }
            // POP BC
            0xC1 => {
                self.POP(mem, BC);
            }
            // JP NZ, a16
            0xC2 => {
                self.JP_C_AA(mem, !self.flags.read(Flag::Z));
            }
            // JP a16
            0xC3 => {
                self.JP_C_AA(mem, true);
            }
            // CALL NZ, a16
            0xC4 => {
                self.CALL_C(mem, !self.flags.read(Flag::Z));
            }
            // PUSH BC
            0xC5 => {
                self.PUSH(mem, BC);
            }
            // ADD A, d8
            0xC6 => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.ADD(self.regs[A], rhs);
            }
            // RST 0
            0xC7 => {
                self.RST(mem, 0);
            }
            // RET Z
            0xC8 => {
                self.RET_C(mem, self.flags.read(Flag::Z));
            }
            // RET
            0xC9 => {
                self.RET(mem);
            }
            // JP Z, a16
            0xCA => {
                self.JP_C_AA(mem, self.flags.read(Flag::Z));
            }
            // Double Instruction
            0xCB => {
                todo!("2-byte Instructions");
            }
            // CALL Z, a16
            0xCC => {
                self.CALL_C(mem, self.flags.read(Flag::Z));
            }
            // CALL a16
            0xCD => {
                self.CALL_C(mem, true);
            }
            // ADC A, d8
            0xCE => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.ADC(self.regs[A], rhs);
            }
            // RST 1
            0xCF => {
                self.RST(mem, 1);
            }
            // RET NC
            0xD0 => {
                self.RET_C(mem, !self.flags.read(Flag::C));
            }
            // POP DE
            0xD1 => {
                self.POP(mem, DE);
            }
            // JP NC, a16
            0xD2 => {
                self.JP_C_AA(mem, !self.flags.read(Flag::C));
            }
            // CALL NC, a16
            0xD4 => {
                self.CALL_C(mem, !self.flags.read(Flag::C));
            }
            // PUSH DE
            0xD5 => {
                self.PUSH(mem, DE);
            }
            // SUB d8
            0xD6 => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.SUB(self.regs[A], rhs);
            }
            // RST 2
            0xD7 => {
                self.RST(mem, 2);
            }
            // RET C
            0xD8 => {
                self.RET_C(mem, self.flags.read(Flag::C));
            }
            // RETI
            0xD9 => {
                self.POP(mem, PC);
                self.cycles += 1;
                // Should this be restoring a previous state or just setting straight to true?
                // I'd imagine it could only have been true if an interrupt was triggered at all.
                self.interrupt_master_enable = true;
            }
            // JP C, a16
            0xDA => {
                self.JP_C_AA(mem, self.flags.read(Flag::C));
            }
            // CALL C, a16
            0xDC => {
                self.CALL_C(mem, self.flags.read(Flag::C));
            }
            // SBC A, d8
            0xDE => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.SBC(self.regs[A], rhs);
            }
            // RST 3
            0xDF => {
                self.RST(mem, 3);
            }
            // LD (a8), A
            0xE0 => {
                let offset = self.IMM8(mem) as u16;
                mem.write_u8(0xFF00 + offset, self.regs[A]);
            }
            // POP HL
            0xE1 => {
                self.POP(mem, HL);
            }
            // LD (C), A
            0xE2 => {
                self.WRITE_8(mem, 0xFF00 + self.regs[C] as u16, self.regs[A]);
            }
            // PUSH HL
            0xE5 => {
                self.PUSH(mem, HL);
            }
            // AND d8
            0xE6 => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.AND(self.regs[A], rhs);
            }
            // RST 4
            0xE7 => {
                self.RST(mem, 4);
            }
            // ADD SP, s8
            0xE8 => {
                let rhs = self.IMM8(mem) as i8 as i16 as u16;
                self.regs[SP] = self.ADD_WIDE(self.regs[SP], rhs);
            }
            // JP HL
            0xE9 => {
                self.regs[PC] = self.regs[HL];
            }
            // LD (a16), A
            0xEA => {
                let addr = self.IMM16(mem);
                self.WRITE_8(mem, addr, self.regs[A]);
                self.cycles += 1;
            }
            // XOR d8
            0xEE => {
                let data = self.IMM8(mem);
                self.regs[A] = self.XOR(self.regs[A], data);
            }
            // RST 5
            0xEF => {
                self.RST(mem, 5);
            }
            // LD A, (a8)
            0xF0 => {
                self.regs[A] = self.IMM8(mem);
                self.cycles += 1;
            }
            // POP AF
            0xF1 => {
                self.POP(mem, AF);
            }
            // LD A, (C)
            0xF2 => {
                self.regs[A] = mem[0xFF00 + self.regs[C] as u16];
                self.cycles += 1;
            }
            // DI
            0xF3 => {
                self.interrupt_master_enable = false;
                self.interrupt_enable_scheduled = false;
            }
            // PUSH AF
            0xF5 => {
                self.PUSH(mem, AF);
            }
            // OR d8
            0xF6 => {
                let rhs = self.IMM8(mem);
                self.regs[A] = self.OR(self.regs[A], rhs);
            }
            // RST 6
            0xF7 => {
                self.RST(mem, 6);
            }
            // LD HL, SP+s8
            0xF8 => {
                let data = self.IMM8(mem) as i8 as i16 as u16;
                self.regs[HL] = self.ADD_WIDE(self.regs[HL], data);
            }
            // LD SP, HL
            0xF9 => {
                self.regs[SP] = self.regs[HL];
                self.cycles += 1;
            }
            // LD A, (a16)
            0xFA => {
                let addr = self.IMM16(mem);
                self.regs[A] = self.READ_8(mem, addr);
            }
            // EI
            0xFB => {
                self.interrupt_enable_scheduled = true;
                scheduled_this_instruction = true;
            }
            // CP d8
            0xFE => {
                let data = self.IMM8(mem);
                self.CP(self.regs[A], data);
            }
            // RST 7
            0xFF => {
                self.RST(mem, 7);
            }

            // Unimpl
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {
                log::error!("Undefined instruction: {:02X}", instr);
            }
        }

        if self.interrupt_enable_scheduled && !scheduled_this_instruction {
            self.interrupt_master_enable = true;
        }
    }

    fn READ_8(&mut self, mem: &Memory, addr: u16) -> u8 {
        self.cycles += 1;
        mem.read_8(addr)
    }

    fn READ_16(&mut self, mem: &Memory, addr: u16) -> u16 {
        self.cycles += 2;
        mem.read_16(addr)
    }

    fn WRITE_8(&mut self, mem: &mut Memory, addr: u16, data: u8) {
        self.cycles += 1;
        mem.write_u8(addr, data);
    }

    fn WRITE_16(&mut self, mem: &mut Memory, addr: u16, data: u16) {
        self.cycles += 2;
        mem.write_u16(addr, data);
    }

    fn IMM8(&mut self, mem: &Memory) -> u8 {
        let out = self.READ_8(mem, self.regs[PC]);
        self.regs[PC] += 1;
        out
    }

    fn IMM16(&mut self, mem: &Memory) -> u16 {
        let out = self.READ_16(mem, self.regs[PC]);
        self.regs[PC] += 2;
        out
    }

    fn RST(&mut self, mem: &mut Memory, n: u16) {
        self.PUSH(mem, PC);
        self.regs[PC] = n * 8;
    }

    fn RET_C(&mut self, mem: &Memory, cond: bool) {
        if cond {
            self.RET(mem);
        }
        self.cycles += 1;
    }

    fn RET(&mut self, mem: &Memory) {
        self.regs[PC] = self.READ_16(mem, self.regs[SP]);
        self.regs[SP] += 2;
    }

    fn POP(&mut self, mem: &Memory, reg: REG_WIDE) {
        self.regs[reg] = self.READ_16(mem, self.regs[SP]);
        self.regs[SP] += 2;
    }

    fn PUSH(&mut self, mem: &mut Memory, reg: REG_WIDE) {
        self.regs[SP] -= 2;
        self.WRITE_16(mem, self.regs[SP], self.regs[reg]);
        self.cycles += 1;
    }

    fn JR(&mut self, offset: i16) {
        self.regs[PC] = self.regs[PC].wrapping_add_signed(offset);
        self.cycles += 1;
    }

    fn JP_C_AA(&mut self, mem: &Memory, jump: bool) {
        let target = self.IMM16(mem);
        if jump {
            self.regs[PC] = target;
            self.cycles += 1;
        }
    }

    fn CALL_C(&mut self, mem: &mut Memory, call: bool) {
        let addr = self.IMM16(mem);
        if call {
            self.PUSH(mem, PC);
            self.regs[PC] = addr;
        }
    }

    fn JR_C(&mut self, mem: &mut Memory, jump: bool) {
        let addr = self.IMM8(mem) as i8 as i16;
        if jump {
            self.JR(addr);
        }
        self.cycles += 1;
    }

    fn ADD(&mut self, lhs: u8, rhs: u8) -> u8 {
        self.flags.set(Flag::H, Self::half_carry_add_u8(lhs, rhs));
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::C, lhs.checked_add(rhs).is_none());
        self.flags.set(Flag::Z, lhs.wrapping_add(rhs) == 0);
        lhs.wrapping_add(rhs)
    }

    fn ADD_WIDE(&mut self, lhs: u16, rhs: u16) -> u16 {
        self.flags.set(Flag::H, Self::half_carry_add_u16(lhs, rhs));
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::C, lhs.checked_add(rhs).is_none());
        self.flags.set(Flag::Z, lhs.wrapping_add(rhs) == 0);
        self.cycles += 2;
        lhs.wrapping_add(rhs)
    }

    fn SUB(&mut self, lhs: u8, rhs: u8) -> u8 {
        self.flags.set(Flag::Z, lhs.wrapping_sub(rhs) == 0);
        self.flags.set(Flag::C, true);
        self.flags.set(Flag::H, Self::half_carry_sub_u8(lhs, rhs));
        self.flags.set(Flag::C, lhs.checked_sub(rhs).is_none());
        lhs.wrapping_sub(rhs)
    }

    fn ADD_RR_RR(&mut self, r1: REG_WIDE, r2: REG_WIDE) {
        self.regs[r1] = self.ADD_WIDE(self.regs[r1], self.regs[r2]);
    }

    fn ADD_R_R(&mut self, r1: REG, r2: REG) {
        self.regs[r1] = self.ADD(self.regs[r1], self.regs[r2]);
    }

    fn ADD_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[r1] = self.ADD(self.regs[r1], rhs);
    }

    fn ADC(&mut self, lhs: u8, rhs: u8) -> u8 {
        let c = if self.flags.read(Flag::C) { 1 } else { 0 };
        let h = (lhs & 0x0F).wrapping_add(rhs & 0x0F).wrapping_add(c) & 0x10 == 0x10;
        let ans = lhs.wrapping_add(rhs).wrapping_add(c);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, h);
        self.flags.set(Flag::C, ans < lhs);
        ans
    }

    fn ADC_R_R(&mut self, r1: REG, r2: REG) {
        self.regs[r1] = self.ADC(self.regs[r1], self.regs[r2]);
    }

    fn ADC_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[r1] = self.ADC(self.regs[r1], rhs);
    }

    fn SUB_R_R(&mut self, r1: REG, r2: REG) {
        self.regs[r1] = self.SUB(self.regs[r1], self.regs[r2]);
    }

    fn SUB_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[r1] = self.SUB(self.regs[r1], rhs);
    }

    fn SBC(&mut self, lhs: u8, rhs: u8) -> u8 {
        let c = if self.flags.read(Flag::C) { 1 } else { 0 };
        let h = (lhs & 0x0F).wrapping_sub(rhs & 0x0F).wrapping_sub(c) & 0x10 == 0x10;
        let ans = lhs.wrapping_sub(rhs).wrapping_sub(c);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, true);
        self.flags.set(Flag::H, h);
        self.flags.set(Flag::C, ans > lhs);
        ans
    }

    fn SBC_R_R(&mut self, r1: REG, r2: REG) {
        self.regs[r1] = self.SBC(self.regs[r1], self.regs[r2]);
    }

    fn SBC_R_MEM_RR(&mut self, mem: &Memory, r1: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[r1] = self.SBC(self.regs[r1], rhs);
    }

    fn AND(&mut self, lhs: u8, rhs: u8) -> u8 {
        let ans = lhs & rhs;
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, true);
        self.flags.set(Flag::C, false);
        ans
    }

    fn AND_R_R(&mut self, r1: REG, r2: REG) {
        self.regs[r1] = self.AND(self.regs[r1], self.regs[r2]);
    }

    fn AND_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[dst] = self.AND(self.regs[dst], rhs);
    }

    fn XOR(&mut self, lhs: u8, rhs: u8) -> u8 {
        let ans = lhs ^ rhs;
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, false);
        self.flags.set(Flag::C, false);
        ans
    }

    fn XOR_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] = self.XOR(self.regs[dst], self.regs[src]);
    }

    fn XOR_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[dst] = self.XOR(self.regs[dst], rhs);
    }

    fn OR(&mut self, lhs: u8, rhs: u8) -> u8 {
        let ans = lhs | rhs;
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, false);
        self.flags.set(Flag::C, false);
        ans
    }

    fn OR_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] = self.OR(self.regs[dst], self.regs[src]);
    }

    fn OR_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.regs[dst] = self.OR(self.regs[dst], rhs);
    }

    fn CP(&mut self, lhs: u8, rhs: u8) {
        self.flags.set(Flag::Z, lhs.wrapping_sub(rhs) == 0);
        self.flags.set(Flag::C, true);
        self.flags.set(Flag::H, Self::half_carry_sub_u8(lhs, rhs));
        self.flags.set(Flag::C, lhs.checked_sub(rhs).is_none());
    }

    fn CP_R_R(&mut self, lhs: REG, rhs: REG) {
        self.CP(self.regs[lhs], self.regs[rhs]);
    }

    fn CP_R_MEM_RR(&mut self, mem: &Memory, lhs: REG, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        self.CP(self.regs[lhs], rhs);
    }

    fn INC(&mut self, lhs: u8) -> u8 {
        let ans = lhs.wrapping_add(1);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, Self::half_carry_add_u8(lhs, 1));
        self.cycles += 1;
        ans
    }

    fn DEC(&mut self, lhs: u8) -> u8 {
        let ans = lhs.wrapping_sub(1);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, Self::half_carry_sub_u8(lhs, 1));
        self.cycles += 1;
        ans
    }

    fn INC_WIDE(&mut self, lhs: u16) -> u16 {
        let ans = lhs.wrapping_add(1);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, Self::half_carry_add_u16(lhs, 1));
        self.cycles += 2;
        ans
    }

    fn DEC_WIDE(&mut self, lhs: u16) -> u16 {
        let ans = lhs.wrapping_sub(1);
        self.flags.set(Flag::Z, ans == 0);
        self.flags.set(Flag::C, false);
        self.flags.set(Flag::H, Self::half_carry_sub_u16(lhs, 1));
        self.cycles += 2;
        ans
    }

    fn DEC_R(&mut self, reg: REG) {
        self.regs[reg] = self.DEC(self.regs[reg]);
    }

    fn INC_R(&mut self, reg: REG) {
        self.regs[reg] = self.INC(self.regs[reg]);
    }

    fn DEC_RR(&mut self, reg: REG_WIDE) {
        self.regs[reg] = self.DEC_WIDE(self.regs[reg]);
    }

    fn INC_RR(&mut self, reg: REG_WIDE) {
        self.regs[reg] = self.INC_WIDE(self.regs[reg]);
    }

    fn INC_MEM_RR(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        let data = self.INC(rhs);
        self.WRITE_8(mem, self.regs[addr], data);
    }

    fn DEC_MEM_RR(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        let rhs = self.READ_8(mem, self.regs[addr]);
        let data = self.DEC(rhs);
        self.WRITE_8(mem, self.regs[addr], data);
    }

    fn LD_RR(&mut self, mem: &Memory, dst: REG_WIDE) {
        self.regs[dst] = self.IMM16(mem);
    }

    fn LD_R(&mut self, mem: &Memory, dst: REG) {
        self.regs[dst] = self.IMM8(mem);
    }

    fn LD_MEM_RR_R(&mut self, mem: &mut Memory, addr: REG_WIDE, data: REG) {
        self.WRITE_8(mem, self.regs[addr], self.regs[data]);
    }

    fn LD_R_MEM_RR(&mut self, mem: &mut Memory, dst: REG, addr: REG_WIDE) {
        self.regs[dst] = self.READ_8(mem, self.regs[addr]);
    }
    fn LD_MEM_RR(&mut self, mem: &mut Memory, addr: REG_WIDE) {
        let data = self.IMM8(mem);
        self.WRITE_8(mem, self.regs[addr], data);
    }

    fn LD_R_R(&mut self, dst: REG, src: REG) {
        self.regs[dst] = self.regs[src];
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
