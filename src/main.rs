use cpu::CPU;
use mem::Memory;
use winit::window::WindowBuilder;

use crate::registers::REG_WIDE;

pub mod cpu;
pub mod gui;
pub mod mem;
pub mod registers;

struct App {
    cpu: CPU,
    memory: Memory,

    step: bool,
}

impl App {
    pub fn new() -> App {
        App {
            cpu: CPU::new(),
            memory: Memory::new(),

            step: false,
        }
    }
}

impl wgpu_app::Application for App {
    fn init(&mut self, ctx: &mut wgpu_app::context::Context) {
        log::info!("Initializing");

        let rom = include_bytes!("../roms/Tetris.gb");
        self.memory.write_bytes(0, rom);
        self.cpu.regs[REG_WIDE::PC] = 0x100;
    }

    fn update(
        &mut self,
        t: &wgpu_app::Timer,
        ctx: &mut wgpu_app::context::Context,
    ) -> Result<(), egui_wgpu::wgpu::SurfaceError> {
        if self.step {
            self.cpu.next_instruction(&mut self.memory);
            self.step = false;
        }

        // ****************** RENDER
        let output = ctx.wgpu_state.surface.get_current_texture()?;
        ctx.egui.render(&mut ctx.wgpu_state, &output, |gui_ctx| {
            egui::Window::new("Memory").show(gui_ctx, |ui| {
                gui::gui_mem(ui, &self.memory, self.cpu.regs[registers::REG_WIDE::PC]);
            });
            egui::Window::new("CPU").show(gui_ctx, |ui| {
                gui::gui_cpu(ui, &mut self.cpu);
            });
            egui::Window::new("Controls").show(gui_ctx, |ui| {
                if ui.button("Step").clicked() {
                    self.step = true;
                }
            });
        });
        output.present();

        Ok(())
    }

    fn close(&mut self, ctx: &wgpu_app::context::Context) {
        log::info!("Closing");
    }

    fn handle_event(
        &mut self,
        ctx: &mut wgpu_app::context::Context,
        event: &winit::event::Event<()>,
    ) {
    }
}

fn main() {
    env_logger::init();

    let app = App::new();

    let wb = WindowBuilder::new().with_title("gbair");
    wgpu_app::run(app, wb);
}
