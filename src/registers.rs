use std::ops::{Index, IndexMut};

#[derive(Clone, Copy, Debug)]
pub enum REG {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Clone, Copy, Debug)]
pub enum REG_WIDE {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

pub struct Registers {
    af: RegisterUnion,
    bc: RegisterUnion,
    de: RegisterUnion,
    hl: RegisterUnion,
    sp: u16,
    pc: u16,
}

pub struct Flags {
    flags: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct RegisterPair {
    r1: u8,
    r2: u8,
}

#[repr(C)]
union RegisterUnion {
    pair: RegisterPair,
    combined: u16,
}

impl RegisterUnion {
    pub fn default() -> RegisterUnion {
        RegisterUnion { combined: 0 }
    }
}

impl Index<REG> for Registers {
    type Output = u8;

    fn index(&self, index: REG) -> &Self::Output {
        unsafe {
            match index {
                REG::A => &self.af.pair.r1,
                REG::F => &self.af.pair.r2,
                REG::B => &self.bc.pair.r1,
                REG::C => &self.bc.pair.r2,
                REG::D => &self.de.pair.r1,
                REG::E => &self.de.pair.r2,
                REG::H => &self.hl.pair.r1,
                REG::L => &self.hl.pair.r2,
            }
        }
    }
}
impl IndexMut<REG> for Registers {
    fn index_mut(&mut self, index: REG) -> &mut Self::Output {
        unsafe {
            match index {
                REG::A => &mut self.af.pair.r1,
                REG::F => &mut self.af.pair.r2,
                REG::B => &mut self.bc.pair.r1,
                REG::C => &mut self.bc.pair.r2,
                REG::D => &mut self.de.pair.r1,
                REG::E => &mut self.de.pair.r2,
                REG::H => &mut self.hl.pair.r1,
                REG::L => &mut self.hl.pair.r2,
            }
        }
    }
}
impl Index<REG_WIDE> for Registers {
    type Output = u16;

    fn index(&self, index: REG_WIDE) -> &Self::Output {
        unsafe {
            match index {
                REG_WIDE::AF => &self.af.combined,
                REG_WIDE::BC => &self.bc.combined,
                REG_WIDE::DE => &self.de.combined,
                REG_WIDE::HL => &self.hl.combined,
                REG_WIDE::SP => &self.sp,
                REG_WIDE::PC => &self.pc,
            }
        }
    }
}
impl IndexMut<REG_WIDE> for Registers {
    fn index_mut(&mut self, index: REG_WIDE) -> &mut Self::Output {
        unsafe {
            match index {
                REG_WIDE::AF => &mut self.af.combined,
                REG_WIDE::BC => &mut self.bc.combined,
                REG_WIDE::DE => &mut self.de.combined,
                REG_WIDE::HL => &mut self.hl.combined,
                REG_WIDE::SP => &mut self.sp,
                REG_WIDE::PC => &mut self.pc,
            }
        }
    }
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            af: RegisterUnion::default(),
            bc: RegisterUnion::default(),
            de: RegisterUnion::default(),
            hl: RegisterUnion::default(),
            sp: 0,
            pc: 0,
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}

impl Flags {
    pub fn new() -> Flags {
        Flags { flags: 0 }
    }
    pub fn get(&self) -> u8 {
        self.flags
    }
    /// Read the Zero flag
    pub fn z(&self) -> bool {
        (self.flags & 0b10000000) > 0
    }
    /// Read the Negative flag
    pub fn n(&self) -> bool {
        (self.flags & 0b01000000) > 0
    }
    /// Read the Half-Carry flag
    pub fn h(&self) -> bool {
        (self.flags & 0b00100000) > 0
    }
    /// Read the Carry flag
    pub fn c(&self) -> bool {
        (self.flags & 0b00010000) > 0
    }
    /// Set the Zero flag
    pub fn set_z(&mut self, set: bool) {
        if set {
            self.flags |= 0b10000000;
        } else {
            self.flags &= 0b01111111
        }
    }
    /// Set the Negative flag
    pub fn set_n(&mut self, set: bool) {
        if set {
            self.flags |= 0b01000000;
        } else {
            self.flags &= 0b10111111
        }
    }
    /// Set the Half-carry flag
    pub fn set_h(&mut self, set: bool) {
        if set {
            self.flags |= 0b00100000;
        } else {
            self.flags &= 0b11011111
        }
    }
    /// Set the carry flag
    pub fn set_c(&mut self, set: bool) {
        if set {
            self.flags |= 0b00010000;
        } else {
            self.flags &= 0b11101111
        }
    }
}

impl Default for Flags {
    fn default() -> Self {
        Self::new()
    }
}
