/* http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
Interrupt Enable Register
--------------------------- FFFF
Internal RAM
--------------------------- FF80
Empty but unusable for I/O
--------------------------- FF4C
I/O ports
--------------------------- FF00
Empty but unusable for I/O
--------------------------- FEA0
Sprite Attrib Memory (OAM)
--------------------------- FE00
Echo of 8kB Internal RAM
--------------------------- E000
8kB Internal RAM
--------------------------- C000
8kB switchable RAM bank
--------------------------- A000
8kB Video RAM
--------------------------- 8000 --
16kB switchable ROM bank         |
--------------------------- 4000 |= 32kB Cartrigbe
16kB ROM bank #0                 |
--------------------------- 0000 --
*/

/*
0000 Restart $00 Address
(RST $00 calls this address.)
0008 Restart $08 Address
(RST $08 calls this address.)
0010 Restart $10 Address
(RST $10 calls this address.)
0018 Restart $18 Address
(RST $18 calls this address.)
0020 Restart $20 Address
(RST $20 calls this address.)
0028 Restart $28 Address
(RST $28 calls this address.)
0030 Restart $30 Address
(RST $30 calls this address.)
0038 Restart $38 Address
(RST $38 calls this address.)
0040 Vertical Blank Interrupt Start Address
0048 LCDC Status Interrupt Start Address
0050 Timer Overflow Interrupt Start Address
0058 Serial Transfer Completion Interrupt
Start Address
0060 High-to-Low of P10-P13 Interrupt
Start Address
*/

use std::ops::{Index, IndexMut};

pub const ADDR_INT_VBLANK: u16 = 0x0040;
pub const ADDR_INT_LCDC: u16 = 0x0048;
pub const ADDR_INT_TIMER: u16 = 0x0050;
pub const ADDR_INT_SERIAL: u16 = 0x0058;
pub const ADDR_INT_HTL_P0_P13: u16 = 0x0060;

pub struct Memory {
    map: [u8; 0x10000],
}

impl Index<u16> for Memory {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.map[index as usize]
    }
}

impl Memory {
    pub fn new() -> Memory {
        Memory { map: [0; 0x10000] }
    }

    pub fn get(&self) -> &[u8] {
        &self.map
    }

    pub fn read_8(&self, addr: u16) -> u8 {
        self.map[addr as usize]
    }

    pub fn read_16(&self, addr: u16) -> u16 {
        self.map[addr as usize] as u16 | ((self.map[addr as usize + 1] as u16) << 8)
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        self.map[addr as usize] = data;

        // Write to echo as well
        if (0xE000..0xFE00).contains(&addr) {
            self.map[addr as usize - 0x1000] = data;
        } else if (0xC000..0xDE00).contains(&addr) {
            self.map[addr as usize + 0x1000] = data;
        }
    }

    pub fn write_u16(&mut self, addr: u16, data: u16) {
        self.map[addr as usize] = data as u8;
        self.map[addr as usize + 1] = (data >> 8) as u8;

        // Write to echo
        if (0xE000..0xFE00).contains(&addr) {
            self.map[addr as usize - 0x1000] = data as u8;
            self.map[addr as usize + 1 - 0x1000] = (data >> 8) as u8;
        } else if (0xC000..0xDE00).contains(&addr) {
            self.map[addr as usize + 0x1000] = data as u8;
            self.map[addr as usize + 1 + 0x1000] = (data >> 8) as u8;
        }
    }

    pub fn write_bytes(&mut self, start_addr: u16, data: &[u8]) -> Option<()> {
        for (i, d) in data.iter().enumerate() {
            *self.map.get_mut(start_addr as usize + i)? = *d;
        }
        Some(())
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}
