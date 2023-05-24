use winit::window::WindowBuilder;

pub mod cpu;
pub mod mem;
pub mod registers;

struct App {}

impl App {
    pub fn new() -> App {
        App {}
    }
}

impl wgpu_app::Application for App {
    fn init(&mut self, ctx: &mut wgpu_app::context::Context) {
        log::info!("Initializing");
    }

    fn update(
        &mut self,
        t: &wgpu_app::Timer,
        ctx: &mut wgpu_app::context::Context,
    ) -> Result<(), egui_wgpu::wgpu::SurfaceError> {
        // ****************** RENDER
        let output = ctx.wgpu_state.surface.get_current_texture()?;
        ctx.egui.render(&mut ctx.wgpu_state, &output, |gui_ctx| {
            egui::Window::new("Hello World").show(gui_ctx, |ui| {
                ui.heading("Cum");
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
