use anyhow::anyhow;
use futures::executor::block_on;
use std::cell::Cell;
// This is usually a bad idea, but we use *so many* WGPU imports that it would be inconvenient
// otherwise.
use wgpu::*;

/// A WebGPU-backed renderer for a single 2D texture.
#[derive(Debug)]
pub struct Renderer {
    bind_group: BindGroup,
    cached_image_data_layout: Cell<Option<ImageDataLayout>>,
    device: Device,
    pipeline: RenderPipeline,
    queue: Queue,
    surface: Surface,
    surface_format: TextureFormat,
    texture: Texture,
    texture_size: Extent3d,
}

impl Renderer {
    /// Creates a new renderer.
    pub fn new(
        window: &minifb::Window,
        backends: Backends,
        texture_size: (u32, u32),
    ) -> anyhow::Result<Self> {
        let (adapter, surface) = Self::new_adapter_and_surface(window, backends)?;
        let (device, queue) = Self::new_device_and_queue(&adapter)?;

        let texture_size = Self::new_texture_size(texture_size);
        let texture = Self::new_texture(&device, texture_size);
        let texture_view = Self::new_texture_view(&texture);

        let bind_group_layout = Self::new_bind_group_layout(&device);
        let bind_group = Self::new_bind_group(
            &device,
            &bind_group_layout,
            &texture_view,
        );

        let surface_format = surface
            .get_preferred_format(&adapter)
            .expect("surface is incompatible with the adapter");

        let pipeline = Self::new_pipeline(
            &device,
            &[&bind_group_layout],
            surface_format,
        );

        Ok(Self {
            bind_group,
            cached_image_data_layout: Cell::new(None),
            device,
            pipeline,
            queue,
            surface,
            surface_format,
            texture,
            texture_size,
        })
    }

    fn new_adapter_and_surface(
        window: &minifb::Window,
        backends: Backends,
    ) -> anyhow::Result<(Adapter, Surface)> {
        let instance = Instance::new(backends);

        // SAFETY: [`Instance::create_surface`] requires that the window is valid and will live for
        // the lifetime of the returned surface. It would be a bug in `minifb` for the first
        // invariant not to hold, and the second holds because both the window and surface live
        // until the end of [`Ui::run`].
        let surface = unsafe { instance.create_surface(window) };

        block_on(instance.request_adapter(&RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }))
        .ok_or_else(|| anyhow!("Failed to find a compatible adapter"))
        .map(|adapter| (adapter, surface))
    }

    fn new_device_and_queue(adapter: &Adapter) -> anyhow::Result<(Device, Queue)> {
        block_on(adapter.request_device(
            &DeviceDescriptor {
                limits: adapter.limits(),
                ..Default::default()
            },
            None
        ))
        .map_err(|e| {
            anyhow::Error::from(e).context("Failed to find a compatible device")
        })
    }

    fn new_texture_size(size: (u32, u32)) -> Extent3d {
        Extent3d {
            width: size.0,
            height: size.1,
            depth_or_array_layers: 1,
        }
    }

    fn new_pipeline(
        device: &Device,
        bind_group_layouts: &[&BindGroupLayout],
        texture_format: TextureFormat,
    ) -> RenderPipeline {
        device.create_render_pipeline(&RenderPipelineDescriptor {
            label: None,
            layout: Some(&Self::new_pipeline_layout(device, bind_group_layouts)),
            vertex: VertexState {
                module: &Self::new_vertex_shader(device),
                entry_point: "main",
                buffers: &[],
            },
            fragment: None,
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview: None,
        })
    }

    fn new_pipeline_layout(
        device: &Device,
        bind_group_layouts: &[&BindGroupLayout],
    ) -> PipelineLayout {
        device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts,
            push_constant_ranges: &[],
        })
    }

    fn new_vertex_shader(device: &Device) -> ShaderModule {
        device.create_shader_module(&include_wgsl!("shader/vertex.wgsl"))
    }

    fn new_texture(device: &Device, size: Extent3d) -> Texture {
        device.create_texture(&TextureDescriptor {
            label: None,
            size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: TextureFormat::Rgba8Unorm,
            usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
        })
    }

    fn new_texture_view(texture: &Texture) -> TextureView {
        texture.create_view(&TextureViewDescriptor::default())
    }

    fn new_bind_group_layout(device: &Device) -> BindGroupLayout {
        device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: None,
            entries: &[],
        })
    }

    fn new_bind_group(
        device: &Device,
        layout: &BindGroupLayout,
        texture_view: &TextureView,
    ) -> BindGroup {
        device.create_bind_group(&BindGroupDescriptor {
            label: None,
            entries: &[],
            layout,
        })
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        self.surface.configure(
            &self.device,
            &SurfaceConfiguration {
                usage: {
                    TextureUsages::TEXTURE_BINDING |
                    TextureUsages::COPY_DST |
                    TextureUsages::RENDER_ATTACHMENT
                },
                format: self.surface_format,
                width,
                height,
                present_mode: PresentMode::Immediate,
            }
        )
    }

    pub fn update_texture(&mut self, data: &[u8]) {
        self.queue.write_texture(
            self.texture.as_image_copy(),
            data,
            self.image_data_layout(),
            self.texture_size,
        );
    }

    fn image_data_layout(&self) -> ImageDataLayout {
        use std::num::NonZeroU32;

        // This cached value is a [`Cell`] because it would be unintuitive for this method to
        // require `&mut self`.
        self.cached_image_data_layout.get().unwrap_or_else(|| {
            let layout = ImageDataLayout {
                offset: 0,
                bytes_per_row: NonZeroU32::new(4 * self.texture_size.width),
                rows_per_image: NonZeroU32::new(self.texture_size.height),
            };

            // Caching the data layout only makes sense if `self.texture_size`, which creating the
            // layout depends on, is immutable.
            self.cached_image_data_layout.set(Some(layout));

            layout
        })
    }

    pub fn render(&self) {
        let mut encoder = self.new_command_encoder();

        let frame = self.surface.get_current_texture().unwrap();

        self.do_render_pass(&mut encoder, &Self::new_texture_view(&frame.texture));
        self.queue.submit(Some(encoder.finish()));
        frame.present();
    }

    fn new_command_encoder(&self) -> CommandEncoder {
        self.device.create_command_encoder(&CommandEncoderDescriptor::default())
    }

    fn do_render_pass(
        &self,
        encoder: &mut CommandEncoder,
        view: &TextureView,
    ) {
        let mut pass = Self::new_render_pass(encoder, view);

        // Look ma! No vertex buffer!
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.draw(0..3, 0..1);
    }

    fn new_render_pass<'a>(
        encoder: &'a mut CommandEncoder,
        view: &'a TextureView,
    ) -> RenderPass<'a> {
        const CLEAR_COLOR: Color = Color {
            r: 0.2,
            g: 0.1,
            b: 0.5,
            a: 1.0,
        };

        encoder.begin_render_pass(&RenderPassDescriptor {
            color_attachments: &[
                RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Clear(CLEAR_COLOR),
                        store: true,
                    },
                },
            ],
            ..Default::default()
        })
    }
}
