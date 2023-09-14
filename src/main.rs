use std::time::Instant;

use cpu::CPU;
use dma::DMA;
use mem::Memory;
use ppu::PPU;
use winit::{dpi::LogicalSize, window::WindowBuilder};

use crate::{ppu::VERTICAL_SYNC, registers::REG_WIDE};

pub mod cpu;
pub mod dma;
pub mod gui;
pub mod mem;
pub mod ppu;
pub mod registers;

struct App {
    cpu: CPU,
    memory: Memory,
    dma: DMA,
    ppu: PPU,

    step: bool,
    running: bool,
    last_cycle: Instant,

    speed: f64,
    last_frame: Instant,
}

impl App {
    pub fn new() -> App {
        App {
            cpu: CPU::new(),
            memory: Memory::new(),
            dma: DMA::new(),
            ppu: PPU::new(),

            step: false,
            running: false,
            last_cycle: Instant::now(),

            speed: 1.0,
            last_frame: Instant::now(),
        }
    }
}

impl wgpu_app::Application for App {
    fn init(&mut self, ctx: &mut wgpu_app::context::Context) {
        log::info!("Initializing");

        // let rom = include_bytes!("../roms/Tetris.gb");
        let rom = include_bytes!("../roms/Tetris.dump");
        self.memory.write_bytes(0, rom);
        self.cpu.regs[REG_WIDE::PC] = 0x100;

        self.dma.next_cycle(&mut self.memory);
        self.dma.stop_transfer();

        // let tile = [
        //     0x3C, 0x7E, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x5E, 0x7E, 0x0A, 0x7C, 0x56,
        //     0x38, 0x7C,
        // ];
        // self.memory.write_bytes(0x8000, &tile).unwrap();
    }

    fn update(
        &mut self,
        t: &wgpu_app::Timer,
        ctx: &mut wgpu_app::context::Context,
    ) -> Result<(), egui_wgpu::wgpu::SurfaceError> {
        // ******************* RUN THE GAME BOY

        loop {
            let elapsed_cycle = self.last_cycle.elapsed().as_secs_f64();
            const CYCLE_TIME: f64 = 1.0 / 1_050_000.0;
            if self.step || (self.running && elapsed_cycle >= CYCLE_TIME) {
                self.speed = CYCLE_TIME / elapsed_cycle;

                if self.step {
                    self.cpu.cycles = 0;
                }

                self.last_cycle = Instant::now();
                self.cpu.next_cycle(&mut self.memory);
                self.dma.next_cycle(&mut self.memory);

                if self.step {
                    self.step = false;
                    break;
                }
            }

            // Refresh rate
            let elapsed_frame = self.last_frame.elapsed().as_secs_f64();
            if elapsed_frame >= 1.0 / VERTICAL_SYNC {
                break;
            }
        }

        self.last_frame = Instant::now();

        // ****************** RENDER
        let output = ctx.wgpu_state.surface.get_current_texture()?;
        ctx.egui.render(&mut ctx.wgpu_state, &output, |gui_ctx| {
            egui::Window::new("Memory").show(gui_ctx, |ui| {
                gui::gui_mem(ui, &self.memory, self.cpu.regs[registers::REG_WIDE::PC]);
            });
            egui::Window::new("CPU").show(gui_ctx, |ui| {
                gui::gui_cpu(ui, &mut self.cpu);
            });
            egui::Window::new("Tiles").show(gui_ctx, |ui| {
                gui::gui_tiles(ui, &self.memory);
            });
            egui::Window::new("Controls").show(gui_ctx, |ui| {
                ui.label(format!("Speed: {:.2}%", self.speed * 100.0));

                if ui
                    .button(if self.running { "Pause" } else { "Start" })
                    .clicked()
                {
                    self.running = !self.running;
                }
                if !self.running && ui.button("Step").clicked() {
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

    let wb = WindowBuilder::new()
        .with_title("gbair")
        .with_inner_size(LogicalSize::new(1000, 800));
    wgpu_app::run(app, wb);
}
