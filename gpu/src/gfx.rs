// SPDX-License-Identifier: MPL-2.0

use noctane_util::BitStack as _;
// This is usually a bad idea, but we use *so many* WGPU imports that it would be inconvenient
// otherwise.
use wgpu::{*, util::DeviceExt as _};

#[derive(Debug)]
pub enum Error {
    NoCompatibleAdapterFound,
    NoCompatibleDeviceFound,
}

impl Vertex {
    pub fn decode(mut code: u32) -> Self {
        let x = code.pop_bits(11) as u16;
        code.pop_bits(5);
        let y = code.pop_bits(11) as u16;

        Self { x, y }
    }
}

#[repr(C, align(2))]
#[derive(Clone, Copy, Debug, Default)]
pub struct Vertex {
    pub x: u16,
    pub y: u16,
}

unsafe impl bytemuck::Pod for Vertex {}
unsafe impl bytemuck::Zeroable for Vertex {}

/// A WebGPU-backed 3D renderer.
#[derive(Debug)]
pub struct Renderer {
    bind_group: BindGroup,
    device: Device,
    pipeline: RenderPipeline,
    queue: Queue,
    surface: Surface,
    vertices: Vec<Vertex>,
}

impl Renderer {
    /// Creates a new `Renderer`.
    ///
    /// # Safety
    ///
    /// `window` must live for as long as the returned renderer.
    pub async unsafe fn new(
        window: &minifb::Window,
        backends: Backends,
    ) -> Result<Self, Error> {
        let (adapter, surface) = Self::create_adapter_and_surface(window, backends).await?;
        let (device, queue) = Self::create_device_and_queue(&adapter).await?;

        let surface_format = surface
            .get_preferred_format(&adapter)
            .expect("surface is incompatible with the adapter");
        Self::configure_surface(window, &device, &surface, surface_format);
        let bind_group_layout = Self::create_bind_group_layout(&device);
        let bind_group = Self::create_bind_group(&device, &bind_group_layout);
        let pipeline = Self::create_pipeline(
            &device,
            &[],
            surface_format,
        );

        Ok(Self {
            bind_group,
            device,
            pipeline,
            queue,
            surface,
            vertices: Vec::new(),
        })
    }

    /// Creates handles to the graphics backend as well as the surface upon which rendering will
    /// take place.
    async fn create_adapter_and_surface(
        window: &minifb::Window,
        backends: Backends,
    ) -> Result<(Adapter, Surface), Error> {
        let instance = Instance::new(backends);

        // SAFETY: [`Instance::create_surface`] requires that the window is valid and will live for
        // the lifetime of the returned surface. It would be a bug in `minifb` for the first
        // invariant not to hold, and the second holds because both the window and surface live
        // until the end of [`Ui::run`].
        let surface = unsafe { instance.create_surface(window) };

        instance.request_adapter(&RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        })
        .await
        .ok_or_else(|| Error::NoCompatibleAdapterFound)
        .map(|adapter| (adapter, surface))
    }

    /// Creates handles to the logical graphics device as well as the command buffer queue.
    async fn create_device_and_queue(adapter: &Adapter) -> Result<(Device, Queue), Error> {
        adapter.request_device(
            &DeviceDescriptor {
                limits: adapter.limits(),
                features: Features::POLYGON_MODE_LINE,
                ..Default::default()
            },
            None,
        )
        .await
        .map_err(|_| Error::NoCompatibleDeviceFound)
    }

    fn configure_surface(
        window: &minifb::Window,
        device: &Device,
        surface: &Surface,
        format: TextureFormat,
    ) {
        let (width, height) = window.get_size();
        surface.configure(
            device,
            &SurfaceConfiguration {
                usage: {
                    TextureUsages::TEXTURE_BINDING
                        | TextureUsages::COPY_DST
                        | TextureUsages::RENDER_ATTACHMENT
                },
                format,
                width: width as u32,
                height: height as u32,
                present_mode: PresentMode::Fifo,
            },
        );
    }

    fn create_pipeline(
        device: &Device,
        bind_group_layouts: &[&BindGroupLayout],
        texture_format: TextureFormat,
    ) -> RenderPipeline {
        device.create_render_pipeline(&RenderPipelineDescriptor {
            label: None,
            layout: Some(&Self::create_pipeline_layout(device, bind_group_layouts)),
            vertex: VertexState {
                module: &Self::create_vertex_shader(device),
                entry_point: "main",
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<Vertex>() as BufferAddress,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &vertex_attr_array![0 => Uint16x2],
                }],
            },
            fragment: Some(FragmentState {
                module: &Self::create_fragment_shader(device),
                entry_point: "main",
                targets: &[wgpu::ColorTargetState {
                    format: texture_format,
                    blend: None,
                    write_mask: ColorWrites::ALL,
                }],
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                polygon_mode: PolygonMode::Line,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview: None,
        })
    }

    fn create_pipeline_layout(
        device: &Device,
        bind_group_layouts: &[&BindGroupLayout],
    ) -> PipelineLayout {
        device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts,
            push_constant_ranges: &[],
        })
    }

    fn create_vertex_shader(device: &Device) -> ShaderModule {
        device.create_shader_module(&include_wgsl!("gfx/vertex.wgsl"))
    }

    fn create_fragment_shader(device: &Device) -> ShaderModule {
        device.create_shader_module(&include_wgsl!("gfx/fragment.wgsl"))
    }

    fn create_bind_group_layout(device: &Device) -> BindGroupLayout {
        device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: None,
            entries: &[],
        })
    }

    fn create_bind_group(
        device: &Device,
        layout: &BindGroupLayout,
    ) -> BindGroup {
        device.create_bind_group(&BindGroupDescriptor {
            label: None,
            entries: &[],
            layout,
        })
    }

    pub fn draw_quad(&mut self, vertices: [Vertex; 4]) {
        self.draw_triangle([
            vertices[0],
            vertices[1],
            vertices[2],
        ]);
        self.draw_triangle([
            vertices[1],
            vertices[2],
            vertices[3],
        ]);
    }

    pub fn draw_triangle(&mut self, vertices: [Vertex; 3]) {
        self.vertices.extend(vertices);
    }

    pub fn render(&mut self) {
        let frame = self.surface.get_current_texture().unwrap();

        let mut encoder = self.create_command_encoder();
        let vertex_buffer = self.create_vertex_buffer();
        self.do_render_pass(&mut encoder, &vertex_buffer, &Self::create_texture_view(&frame.texture));
        self.queue.submit(Some(encoder.finish()));

        frame.present();

        self.vertices.clear();
    }

    fn create_command_encoder(&self) -> CommandEncoder {
        self.device.create_command_encoder(&CommandEncoderDescriptor::default())
    }

    fn create_texture_view(texture: &Texture) -> TextureView {
        texture.create_view(&TextureViewDescriptor::default())
    }

    fn do_render_pass(
        &self,
        encoder: &mut CommandEncoder,
        vertex_buffer: &Buffer,
        view: &TextureView,
    ) {
        let mut pass = Self::create_render_pass(encoder, view);
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.set_vertex_buffer(0, vertex_buffer.slice(..));
        pass.draw(0..(self.vertices.len() as u32), 0..1);
    }

    fn create_render_pass<'a>(
        encoder: &'a mut CommandEncoder,
        view: &'a TextureView,
    ) -> RenderPass<'a> {
        encoder.begin_render_pass(&RenderPassDescriptor {
            color_attachments: &[RenderPassColorAttachment {
                view,
                resolve_target: None,
                ops: Operations {
                    load: LoadOp::Clear(Color::BLACK),
                    store: true,
                },
            }],
            ..Default::default()
        })
    }

    fn create_vertex_buffer(&self) -> Buffer {
        self.device.create_buffer_init(&util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&self.vertices),
            usage: BufferUsages::VERTEX,
        })
    }
}
