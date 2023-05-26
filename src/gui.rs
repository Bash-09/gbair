use egui::{Color32, RichText, ScrollArea, Ui};

use crate::{cpu::CPU, mem::Memory};

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
        let mut z = cpu.flags.z();
        ui.checkbox(&mut z, "Z");
        cpu.flags.set_z(z);
        let mut n = cpu.flags.n();
        ui.checkbox(&mut n, "N");
        cpu.flags.set_n(n);
        let mut h = cpu.flags.h();
        ui.checkbox(&mut h, "H");
        cpu.flags.set_h(h);
        let mut c = cpu.flags.c();
        ui.checkbox(&mut c, "C");
        cpu.flags.set_c(c);
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
