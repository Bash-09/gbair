use std::ops::Range;

use crate::mem::Memory;

// Hz
pub const HORIZONTAL_SYNC: f64 = 9_198_000.0;
pub const VERTICAL_SYNC: f64 = 59.73;

/*
 Bit 7 - LCD Display Enable             (0=Off, 1=On)
 Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
 Bit 5 - Window Display Enable          (0=Off, 1=On)
 Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
 Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
 Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
 Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
 Bit 0 - BG/Window Display/Priority     (0=Off, 1=On)
*/
const ADDR_LCDC: u16 = 0xFF40;
/*
 Bit 6 - LYC=LY Coincidence Interrupt (1=Enable) (Read/Write)
 Bit 5 - Mode 2 OAM Interrupt         (1=Enable) (Read/Write)
 Bit 4 - Mode 1 V-Blank Interrupt     (1=Enable) (Read/Write)
 Bit 3 - Mode 0 H-Blank Interrupt     (1=Enable) (Read/Write)
 Bit 2 - Coincidence Flag  (0:LYC<>LY, 1:LYC=LY) (Read Only)
 Bit 1-0 - Mode Flag       (Mode 0-3, see below) (Read Only)
           0: During H-Blank
           1: During V-Blank
           2: During Searching OAM
           3: During Transferring Data to LCD Driver
*/
const ADDR_STAT: u16 = 0xFF41;
const ADDR_SCY: u16 = 0xFF42;
const ADDR_SCX: u16 = 0xFF43;
/// LCD Y-coordinate
const ADDR_LY: u16 = 0xFF44;
/// When LY == LYC a stat interrupt is triggered
const ADDR_LYC: u16 = 0xFF45;
/// Background Palette
/// Bit 7-6 - Shade for Color Number 3
/// Bit 5-4 - Shade for Color Number 2
/// Bit 3-2 - Shade for Color Number 1
/// Bit 1-0 - Shade for Color Number 0
/// 0  White
/// 1  Light gray
/// 2  Dark gray
/// 3  Black
const ADDR_BGP: u16 = 0xFF47;
/// Window Y position
const ADDR_WY: u16 = 0xFF4A;

/// Object Palette 0 Data
/// Same as BGP but the lower 2 bits aren't used because sprite data 00 is transparent
const ADD_OBP0: u16 = 0xFF48;
/// Object Palette 1 Data
/// Same as BGP but the lower 2 bits aren't used because sprite data 00 is transparent
const ADD_OBP1: u16 = 0xFF49;

enum LCDMODE {
    HBlank,
    VBlank,
    OAMSearch,
    /// During transferring data to LCD driver
    Transfer,
}

pub struct PPU {}

pub struct Tile {
    pub pixels: [u8; 8 * 8],
}

impl Tile {
    pub fn from_bytes(bytes: &[u8; 16]) -> Tile {
        let mut tile = Tile { pixels: [0; 8 * 8] };

        for i in 0..8 {
            for j in 0..8 {
                let lsb = (bytes[2 * i] & (1 << j)) >> j;
                let msb = (bytes[2 * i + 1] & (1 << j)) >> j;
                tile.pixels[i * 8 + (7 - j)] = (msb << 1) | lsb;
            }
        }

        tile
    }
}

impl PPU {
    pub fn new() -> PPU {
        PPU {}
    }

    fn bg_en(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 0) != 0
    }
    fn obj_en(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 1) != 0
    }
    fn obj_size(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 2) != 0
    }
    fn bg_map(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 3) != 0
    }
    fn tile_sel(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 4) != 0
    }
    fn win_en(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 5) != 0
    }
    fn win_map(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 6) != 0
    }
    fn lcd_en(mem: &Memory) -> bool {
        mem[ADDR_LCDC] & (1 << 7) != 0
    }
    fn lcd_mode(mem: &Memory) -> LCDMODE {
        let mode = mem[ADDR_STAT] & 0b11;
        match mode {
            0 => LCDMODE::HBlank,
            1 => LCDMODE::VBlank,
            2 => LCDMODE::OAMSearch,
            3 => LCDMODE::Transfer,
            _ => panic!("Invalid LCD Mode {}", mode),
        }
    }
    fn lyc_stat(mem: &Memory) -> bool {
        mem[ADDR_STAT] & (1 << 2) != 0
    }
    fn intr_m0(mem: &Memory) -> bool {
        mem[ADDR_STAT] & (1 << 3) != 0
    }
    fn intr_m1(mem: &Memory) -> bool {
        mem[ADDR_STAT] & (1 << 4) != 0
    }
    fn intr_m2(mem: &Memory) -> bool {
        mem[ADDR_STAT] & (1 << 5) != 0
    }
    fn int_lyc(mem: &Memory) -> bool {
        mem[ADDR_STAT] & (1 << 6) != 0
    }
}

impl Default for PPU {
    fn default() -> Self {
        Self::new()
    }
}
