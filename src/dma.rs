use crate::mem::Memory;

enum DMAState {
    Transferring(u8),
    Idle,
}

pub struct DMA {
    last_request: u8,
    state: DMAState,
}

const ADDR_DMA_SRC: u16 = 0xFF46;

impl DMA {
    pub fn new() -> DMA {
        DMA {
            last_request: 0,
            state: DMAState::Idle,
        }
    }

    pub fn next_cycle(&mut self, mem: &mut Memory) {
        match &mut self.state {
            DMAState::Transferring(index) => {
                if *index < 160 {
                    let offset = (self.last_request + *index) as u16;
                    let data: u8 = mem.read_8(0xFF00 + offset);
                    mem.write_u8(0xFFE0 + offset, data);
                    *index += 1;
                } else {
                    self.state = DMAState::Idle;
                }
            }
            DMAState::Idle => {
                let addr = mem.read_8(ADDR_DMA_SRC);
                if addr != self.last_request {
                    self.last_request = addr;
                    self.state = DMAState::Transferring(0);
                }
            }
        }
    }

    pub fn stop_transfer(&mut self) {
        self.state = DMAState::Idle;
    }
}

impl Default for DMA {
    fn default() -> Self {
        Self::new()
    }
}
