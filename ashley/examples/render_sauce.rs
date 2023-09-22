use ashley::{
    ir::{
        transform::{ShaderInterfaceTransform, ShaderInterfaceTransformError},
        write_spirv,
    },
    utils::{MemoryLayout, Std140Int, Std140Vec4},
    Compiler, CompilerDb, ModuleName, QueryError,
};
use spirv_tools::{
    assembler::{Assembler, DisassembleOptions},
    val::{Validator, ValidatorOptions},
};
use std::{
    borrow::Cow,
    fs,
    fs::File,
    io::Write,
    mem,
    path::Path,
    time::{Duration, Instant},
};
use tracing::warn;
use wgpu::{util::DeviceExt, BindingResource, ShaderSource, TextureViewDescriptor};
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

const VERTEX_LAYOUT: wgpu::VertexBufferLayout = wgpu::VertexBufferLayout {
    array_stride: 8 as wgpu::BufferAddress, // f32x2
    step_mode: wgpu::VertexStepMode::Vertex,
    attributes: &[wgpu::VertexAttribute {
        offset: 0,
        shader_location: 0,
        format: wgpu::VertexFormat::Float32x2,
    }],
};

const QUAD: &[f32] = &[-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0];

const VERTEX_SHADER: &str = r#"
struct VertexInput {
    @location(0) position: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

@vertex
fn vs_main(
    input: VertexInput,
) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4<f32>(input.position, 0.0, 1.0);
    out.uv = 0.5 * (input.position + vec2<f32>(1.0));
    return out;
}
"#;

const DEFAULT_FRAGMENT_SHADER: &str = r#"
struct Uniforms {
    time: f32,
    width: u32,
    height: u32,
    frame: u32
};

@group(0) @binding(0) 
var<uniform> uniforms: Uniforms;

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let color = 0.5 + 0.5 * cos(uniforms.time + in.uv.xyx + vec3<f32>(0.0,2.0,4.0));
    return vec4<f32>(color, 1.0);
}
"#;

struct WgpuState {
    window: winit::window::Window,
    surface: wgpu::Surface,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    render_pipeline: wgpu::RenderPipeline,
    pipeline_layout: wgpu::PipelineLayout,
    quad_vertex_buffer: wgpu::Buffer,
    vertex_shader: wgpu::ShaderModule,
    uniforms_buffer: wgpu::Buffer,
    uniforms_bind_group: wgpu::BindGroup,
    texture: wgpu::Texture,
    start_instant: Instant,
    size: winit::dpi::PhysicalSize<u32>,
}

#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable, MemoryLayout)]
#[repr(C)]
struct Uniforms {
    time: f32,
    width: u32,
    height: u32,
    frame: u32,
}

const PRIMITIVE_STATE: wgpu::PrimitiveState = wgpu::PrimitiveState {
    topology: wgpu::PrimitiveTopology::TriangleList, // 1.
    strip_index_format: None,
    front_face: wgpu::FrontFace::Ccw, // 2.
    cull_mode: Some(wgpu::Face::Back),
    polygon_mode: wgpu::PolygonMode::Fill,
    unclipped_depth: false,
    conservative: false,
};

impl WgpuState {
    // ...
    fn new(window: Window) -> Self {
        let size = window.inner_size();

        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            dx12_shader_compiler: Default::default(),
        });

        let surface = unsafe { instance.create_surface(&window) }.unwrap();

        // thank the web backend for the async bullshit
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .unwrap();

        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                features: wgpu::Features::SPIRV_SHADER_PASSTHROUGH,
                limits: wgpu::Limits::default(),
                label: None,
            },
            None, // Trace path
        ))
        .unwrap();

        let surface_caps = surface.get_capabilities(&adapter);
        // Shader code in this tutorial assumes an sRGB surface texture. Using a different
        // one will result all the colors coming out darker. If you want to support non
        // sRGB surfaces, you'll need to account for that when drawing to the frame.
        let surface_format = surface_caps
            .formats
            .iter()
            .copied()
            .find(|f| f.is_srgb())
            .unwrap_or(surface_caps.formats[0]);
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: surface_caps.present_modes[0],
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
        };
        surface.configure(&device, &config);

        let quad_vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(QUAD),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let uniforms_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniforms Buffer"),
            contents: bytemuck::cast_slice(&[Uniforms {
                width: config.width,
                height: config.height,
                time: 0.0,
                frame: 0,
            }]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let img = image::io::Reader::open("data/images/haniyasushin_keiki.jpg")
            .unwrap()
            .decode()
            .unwrap()
            .into_rgba8();

        let texture = device.create_texture_with_data(
            &queue,
            &wgpu::TextureDescriptor {
                label: Some("the only texture"),
                size: wgpu::Extent3d {
                    width: img.width(),
                    height: img.height(),
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                view_formats: &[wgpu::TextureFormat::Rgba8UnormSrgb],
            },
            img.as_raw(),
        );

        let texture_view = texture.create_view(&wgpu::TextureViewDescriptor {
            label: Some("the only texture view"),
            format: Some(wgpu::TextureFormat::Rgba8UnormSrgb),
            dimension: Some(wgpu::TextureViewDimension::D2),
            aspect: wgpu::TextureAspect::All,
            base_mip_level: 0,
            mip_level_count: None,
            base_array_layer: 0,
            array_layer_count: None,
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
            ],
        });

        let uniforms_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: uniforms_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::TextureView(&texture_view),
                },
            ],
            label: Some("uniforms_bind_group"),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let vertex_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: ShaderSource::Wgsl(VERTEX_SHADER.into()),
        });
        let default_fragment_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: ShaderSource::Wgsl(DEFAULT_FRAGMENT_SHADER.into()),
        });

        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &vertex_shader,
                entry_point: "vs_main",
                buffers: &[VERTEX_LAYOUT],
            },
            primitive: PRIMITIVE_STATE,
            depth_stencil: None,
            multisample: Default::default(),
            fragment: Some(wgpu::FragmentState {
                module: &default_fragment_shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::REPLACE),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview: None,
        });

        WgpuState {
            window,
            surface,
            device,
            queue,
            config,
            render_pipeline,
            pipeline_layout,
            quad_vertex_buffer,
            vertex_shader,
            uniforms_buffer,
            uniforms_bind_group,
            texture,
            start_instant: Instant::now(),
            size,
        }
    }

    fn load_shader<P: AsRef<Path>>(&mut self, file_path: P) {
        // load shader source
        let file_path = file_path.as_ref();
        let src = match fs::read_to_string(file_path) {
            Ok(src) => src,
            Err(e) => {
                warn!("failed to load shader: {}", e);
                return;
            }
        };

        let stem = file_path.file_stem().unwrap().to_str().unwrap();

        // create session
        let mut sess = Compiler::new();
        let (pkg, _) = sess.create_source_module(ModuleName::from(stem), file_path.to_str().unwrap(), &src);

        let mut hir = match sess.compile_to_hir(pkg) {
            Ok(spv) => spv,
            Err(err) => {
                warn!("failed to compile shader {file_path:?} ({err}), see standard output for errors");
                return;
            }
        };

        let mut sit = ShaderInterfaceTransform::new(&mut hir, &sess);
        if let Err(err) = sit.provide_uniform_buffer_as_type::<Uniforms>(0, 0) {
            warn!("failed to remap uniforms for {file_path:?} ({err}), see standard output for errors");
            return;
        }
        // provide a texture named "tex"
        sit.provide_texture(0, 1, "tex");
        // will rewrite the bytecode with the new interface
        sit.finish();

        // write SPIR-V bytecode
        let spv = write_spirv(&hir);

        fn validate_spirv(module_name: &str, code: &[u32]) {
            let validator = spirv_tools::val::create(None);
            let result = validator.validate(
                code,
                Some(ValidatorOptions {
                    relax_struct_store: false,
                    relax_logical_pointer: false,
                    before_legalization: false,
                    relax_block_layout: None,
                    uniform_buffer_standard_layout: false,
                    scalar_block_layout: false,
                    skip_block_layout: false,
                    max_limits: vec![],
                }),
            );
            //match result {
            //    Ok(_) => {}
            //    Err(err) => {
            let assembler = spirv_tools::assembler::create(None);
            let dis = assembler
                .disassemble(&code, DisassembleOptions::default())
                .unwrap()
                .unwrap();
            eprintln!("{dis}");
            //eprintln!("[{}] SPIR-V validation failed: {}", module_name, err)
            //    }
            //}
        }
        validate_spirv(stem, &spv);

        let mut f = File::create("shader.spv").unwrap();
        let spv_u8: &[u8] = bytemuck::cast_slice(&spv[..]);
        f.write(spv_u8).unwrap();

        // recreate pipeline
        let shader_module_desc = wgpu::ShaderModuleDescriptorSpirV {
            label: None,
            source: Cow::Borrowed(&spv[..]),
        };
        let fragment_shader = unsafe { self.device.create_shader_module_spirv(&shader_module_desc) };

        self.render_pipeline = self.device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&self.pipeline_layout),
            vertex: wgpu::VertexState {
                module: &self.vertex_shader,
                entry_point: "vs_main",
                buffers: &[VERTEX_LAYOUT],
            },
            primitive: PRIMITIVE_STATE,
            depth_stencil: None,
            multisample: Default::default(),
            fragment: Some(wgpu::FragmentState {
                module: &fragment_shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: self.config.format,
                    blend: Some(wgpu::BlendState::REPLACE),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview: None,
        });
    }

    fn window(&self) -> &Window {
        &self.window
    }

    fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    fn input(&mut self, event: &WindowEvent) -> bool {
        false
    }

    fn update(&mut self) {
        let uniforms = Uniforms {
            time: Instant::now().duration_since(self.start_instant).as_secs_f32(),
            width: self.config.width,
            height: self.config.height,
            frame: 0,
        };
        self.queue
            .write_buffer(&self.uniforms_buffer, 0, bytemuck::cast_slice(&[uniforms]));
    }

    fn render(&mut self) -> Result<(), wgpu::SurfaceError> {
        let output = self.surface.get_current_texture()?;
        let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Render Encoder"),
        });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.1,
                            g: 0.2,
                            b: 0.3,
                            a: 1.0,
                        }),
                        store: true,
                    },
                })],
                depth_stencil_attachment: None,
            });

            render_pass.set_pipeline(&self.render_pipeline);
            render_pass.set_bind_group(0, &self.uniforms_bind_group, &[]);
            render_pass.set_vertex_buffer(0, self.quad_vertex_buffer.slice(..));
            render_pass.draw(0..6, 0..1);
        }

        self.queue.submit([encoder.finish()]);
        output.present();
        Ok(())
    }
}

pub fn main() {
    env_logger::init();
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new().build(&event_loop).unwrap();
    let mut state = WgpuState::new(window);

    state.load_shader("data/shaders/texture_sample.glsl");

    event_loop.run(move |event, _, control_flow| match event {
        Event::WindowEvent { ref event, window_id } if window_id == state.window.id() => {
            if !state.input(event) {
                match event {
                    WindowEvent::CloseRequested
                    | WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                state: ElementState::Pressed,
                                virtual_keycode: Some(VirtualKeyCode::Escape),
                                ..
                            },
                        ..
                    } => *control_flow = ControlFlow::Exit,

                    WindowEvent::Resized(physical_size) => {
                        state.resize(*physical_size);
                    }
                    WindowEvent::ScaleFactorChanged { new_inner_size, .. } => {
                        state.resize(**new_inner_size);
                    }
                    _ => {}
                }
            }
        }
        Event::RedrawRequested(window_id) if window_id == state.window().id() => {
            state.update();
            match state.render() {
                Ok(_) => {}
                Err(wgpu::SurfaceError::Lost) => state.resize(state.size),
                Err(wgpu::SurfaceError::OutOfMemory) => *control_flow = ControlFlow::Exit,
                Err(e) => eprintln!("{:?}", e),
            }
        }
        Event::MainEventsCleared => {
            state.window().request_redraw();
        }
        _ => {}
    });
}
