pub struct Registers {
    af: RegisterUnion,
    bc: RegisterUnion,
    de: RegisterUnion,
    hl: RegisterUnion,
    sp: u16,
    pc: u16,
    fl: Flags,
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

impl Registers {
    pub fn new() -> Registers {
        Registers {
            af: RegisterUnion::default(),
            bc: RegisterUnion::default(),
            de: RegisterUnion::default(),
            hl: RegisterUnion::default(),
            sp: 0,
            pc: 0,
            fl: Flags { flags: 0 },
        }
    }

    pub fn a(&self) -> u8 {
        unsafe { self.af.pair.r1 }
    }
    pub fn f(&self) -> u8 {
        unsafe { self.af.pair.r2 }
    }
    pub fn b(&self) -> u8 {
        unsafe { self.bc.pair.r1 }
    }
    pub fn c(&self) -> u8 {
        unsafe { self.bc.pair.r2 }
    }
    pub fn d(&self) -> u8 {
        unsafe { self.de.pair.r1 }
    }
    pub fn e(&self) -> u8 {
        unsafe { self.de.pair.r2 }
    }
    pub fn h(&self) -> u8 {
        unsafe { self.hl.pair.r1 }
    }
    pub fn l(&self) -> u8 {
        unsafe { self.hl.pair.r2 }
    }
    pub fn af(&self) -> u16 {
        unsafe { self.af.combined }
    }
    pub fn bc(&self) -> u16 {
        unsafe { self.bc.combined }
    }
    pub fn de(&self) -> u16 {
        unsafe { self.de.combined }
    }
    pub fn hl(&self) -> u16 {
        unsafe { self.hl.combined }
    }
    pub fn sp(&self) -> u16 {
        self.sp
    }
    pub fn pc(&self) -> u16 {
        self.pc
    }
    pub fn a_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.af.pair.r1 }
    }
    pub fn f_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.af.pair.r2 }
    }
    pub fn b_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.bc.pair.r1 }
    }
    pub fn c_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.bc.pair.r2 }
    }
    pub fn d_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.de.pair.r1 }
    }
    pub fn e_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.de.pair.r2 }
    }
    pub fn h_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.hl.pair.r1 }
    }
    pub fn l_mut(&mut self) -> &mut u8 {
        unsafe { &mut self.hl.pair.r2 }
    }
    pub fn af_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.af.combined }
    }
    pub fn bc_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.bc.combined }
    }
    pub fn de_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.de.combined }
    }
    pub fn hl_mut(&mut self) -> &mut u16 {
        unsafe { &mut self.hl.combined }
    }
    pub fn sp_mut(&mut self) -> &mut u16 {
        &mut self.sp
    }
    pub fn pc_mut(&mut self) -> &mut u16 {
        &mut self.pc
    }
    pub fn write_a(&mut self, val: u8) {
        self.af.pair.r1 = val;
    }
    pub fn write_f(&mut self, val: u8) {
        self.af.pair.r2 = val;
    }
    pub fn write_b(&mut self, val: u8) {
        self.bc.pair.r1 = val;
    }
    pub fn write_c(&mut self, val: u8) {
        self.bc.pair.r2 = val;
    }
    pub fn write_d(&mut self, val: u8) {
        self.de.pair.r1 = val;
    }
    pub fn write_e(&mut self, val: u8) {
        self.de.pair.r2 = val;
    }
    pub fn write_h(&mut self, val: u8) {
        self.hl.pair.r1 = val;
    }
    pub fn write_l(&mut self, val: u8) {
        self.hl.pair.r2 = val;
    }
    pub fn write_af(&mut self, val: u16) {
        self.af.combined = val;
    }
    pub fn write_bc(&mut self, val: u16) {
        self.bc.combined = val;
    }
    pub fn write_de(&mut self, val: u16) {
        self.de.combined = val;
    }
    pub fn write_hl(&mut self, val: u16) {
        self.hl.combined = val;
    }
    pub fn write_sp(&mut self, val: u16) {
        self.sp = val;
    }
    pub fn write_pc(&mut self, val: u16) {
        self.pc = val;
    }
    pub fn fl(&self) -> &Flags {
        &self.fl
    }
    pub fn fl_mut(&mut self) -> &mut Flags {
        &mut self.fl
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}

impl Flags {
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

#[test]
pub fn test_union() {
    let mut regs = Registers::new();

    regs.write_a(0xb);
    assert_eq!(regs.a() as u16, regs.af());
    regs.write_f(0xe);
    assert_eq!(regs.af(), 0x0e0b); // endianess be like
}
