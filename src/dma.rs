use crate::mem::Memory;

enum DMAState {
    Transferring(u16),
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
                    todo!("DMA transfer");
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
}

impl Default for DMA {
    fn default() -> Self {
        Self::new()
    }
}
