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

pub struct Memory {
    map: [u8; 0xFFFF],
}

impl Memory {
    pub fn new() -> Memory {
        Memory { map: [0; 0xFFFF] }
    }

    pub fn read(&self) -> &[u8] {
        &self.map
    }

    pub fn write_u8(&mut self, address: u16, data: u8) {
        self.map[address as usize] = data;
    }

    pub fn write_u16(&mut self, address: u16, data: u8) {
        todo!("Double check endianess");
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}
