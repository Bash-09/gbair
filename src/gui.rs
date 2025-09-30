use egui::{Color32, Pos2, Rect, RichText, Rounding, ScrollArea, Ui, Vec2};

use crate::{cpu::CPU, mem::Memory, registers::Flag};

pub fn gui_cpu(ui: &mut Ui, cpu: &mut CPU) {
    use crate::registers::REG::*;
    use crate::registers::REG_WIDE::*;

    ui.heading("Registers");
    ui.horizontal(|ui| {
        ui.label("A");
        ui.add(egui::DragValue::new(&mut cpu.regs[A]).hexadecimal(2, true, false));
        ui.label("F");
        ui.add(egui::DragValue::new(&mut cpu.regs[F]).hexadecimal(2, true, false));
        ui.label("AF");
        ui.add(egui::DragValue::new(&mut cpu.regs[AF]).hexadecimal(4, true, false));
    });
    ui.horizontal(|ui| {
        ui.label("B");
        ui.add(egui::DragValue::new(&mut cpu.regs[B]).hexadecimal(2, true, false));
        ui.label("C");
        ui.add(egui::DragValue::new(&mut cpu.regs[C]).hexadecimal(2, true, false));
        ui.label("BC");
        ui.add(egui::DragValue::new(&mut cpu.regs[BC]).hexadecimal(4, true, false));
    });
    ui.horizontal(|ui| {
        ui.label("D");
        ui.add(egui::DragValue::new(&mut cpu.regs[D]).hexadecimal(2, true, false));
        ui.label("E");
        ui.add(egui::DragValue::new(&mut cpu.regs[E]).hexadecimal(2, true, false));
        ui.label("DE");
        ui.add(egui::DragValue::new(&mut cpu.regs[DE]).hexadecimal(4, true, false));
    });
    ui.horizontal(|ui| {
        ui.label("H");
        ui.add(egui::DragValue::new(&mut cpu.regs[H]).hexadecimal(2, true, false));
        ui.label("L");
        ui.add(egui::DragValue::new(&mut cpu.regs[L]).hexadecimal(2, true, false));
        ui.label("HL");
        ui.add(egui::DragValue::new(&mut cpu.regs[HL]).hexadecimal(4, true, false));
    });
    ui.horizontal(|ui| {
        ui.label("SP");
        ui.add(egui::DragValue::new(&mut cpu.regs[SP]).hexadecimal(4, true, false));
        ui.label("PC");
        ui.add(egui::DragValue::new(&mut cpu.regs[PC]).hexadecimal(4, true, false));
    });

    ui.heading("Flags");
    ui.horizontal(|ui| {
        let mut z = cpu.flags.read(Flag::Z);
        ui.checkbox(&mut z, "Z");
        cpu.flags.set(Flag::Z, z);
        let mut n = cpu.flags.read(Flag::N);
        ui.checkbox(&mut n, "N");
        cpu.flags.set(Flag::N, n);
        let mut h = cpu.flags.read(Flag::H);
        ui.checkbox(&mut h, "H");
        cpu.flags.set(Flag::H, h);
        let mut c = cpu.flags.read(Flag::C);
        ui.checkbox(&mut c, "C");
        cpu.flags.set(Flag::C, c);
    });

    ui.label(format!("Cycles: {}", cpu.cycles));
}

pub fn gui_mem(ui: &mut Ui, mem: &Memory, pc: u16) {
    let mid_spacing = 15.0;
    let left_width = 60.0;
    let size = 17.0;
    let height = ui.text_style_height(&egui::TextStyle::Body);
    let colour_bold = Color32::LIGHT_GRAY;
    let colour_boring = Color32::DARK_GRAY;
    let colour_pc = Color32::GREEN;

    ui.horizontal(|ui| {
        ui.add_space(left_width);
        ui.add_space(ui.spacing().item_spacing.x);

        for i in 0..16 {
            if i == 8 {
                ui.add_space(mid_spacing);
            }

            ui.add_sized(
                [size, height],
                egui::Label::new(RichText::new(format!("{:02X}", i)).color(colour_bold)),
            );
        }
    });

    ui.separator();

    ScrollArea::new([false, true]).show_rows(
        ui,
        ui.text_style_height(&egui::TextStyle::Body),
        4096,
        |ui, r| {
            for r in r {
                ui.horizontal(|ui| {
                    ui.add_sized(
                        [left_width, height],
                        egui::Label::new(
                            RichText::new(format!("{:#06X}", r * 16)).color(colour_bold),
                        ),
                    );

                    for i in 0..16 {
                        if i == 8 {
                            ui.add_space(mid_spacing);
                        }

                        let addr = 16 * r as u16 + i;
                        let val = mem[addr];
                        let mut text = RichText::new(format!("{:02X}", val));
                        if val == 0 {
                            text = text.color(colour_boring);
                        }
                        if addr == pc {
                            text = text.color(colour_pc);
                        }

                        ui.add_sized([size, height], egui::Label::new(text));
                    }
                });
            }
        },
    );
}

pub fn gui_tiles(ui: &mut Ui, mem: &Memory) {
    let tiles = mem.get_tiles(255);
    let pixel_size = 3;
    let size = pixel_size * 8 * 16 + 15;
    let (_, space) = ui.allocate_space(Vec2::new(size as f32, size as f32));
    let painter = ui.painter_at(space);

    for y in 0..16 {
        for x in 0..16 {
            let tile = &tiles[y * 16 + x];
            let tile_x = space.left() + (x * (pixel_size * 8) + x) as f32;
            let tile_y = space.top() + (y * (pixel_size * 8) + y) as f32;
            for py in 0..8 {
                for px in 0..8 {
                    let col_id = tile.pixels[py * 8 + px];
                    if col_id == 0 {
                        continue;
                    }
                    painter.rect_filled(
                        Rect::from_min_size(
                            Pos2::new(
                                tile_x + (px * pixel_size) as f32,
                                tile_y + (py * pixel_size) as f32,
                            ),
                            Vec2::new(pixel_size as f32, pixel_size as f32),
                        ),
                        Rounding::none(),
                        Color32::from_gray(col_id * 64),
                    );
                }
            }
        }
    }
}
