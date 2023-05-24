use crate::registers::Registers;

pub struct CPU {
    regs: Registers,
    /// Using the 1MHz convention instead of the 4MHz physical clock that takes 4 cycles for everything
    cycles: usize,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            regs: Registers::new(),
            cycles: 0,
        }
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}
