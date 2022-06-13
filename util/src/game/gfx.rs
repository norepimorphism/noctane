// SPDX-License-Identifier: MPL-2.0

use anyhow::anyhow;
use futures::executor::block_on;
// This is usually a bad idea, but we use *so many* WGPU imports that it would be inconvenient
// otherwise.
use wgpu::*;

/// A WebGPU-backed 3D renderer.
#[derive(Debug)]
pub struct Renderer {
    device: Device,
    pipeline: RenderPipeline,
    queue: Queue,
    surface: Surface,
}

impl Renderer {
    /// Creates a new `Renderer`.
    pub fn new(
        window: &minifb::Window,
        backends: Backends,
    ) -> anyhow::Result<Self> {
        let (adapter, surface) = Self::create_adapter_and_surface(window, backends)?;
        let (device, queue) = Self::create_device_and_queue(&adapter)?;

        let bind_group_layout = Self::create_bind_group_layout(&device);
        let surface_format = surface
            .get_preferred_format(&adapter)
            .expect("surface is incompatible with the adapter");
        Self::configure_surface(window, &device, &surface, surface_format);
        let pipeline = Self::create_pipeline(&device, &[&bind_group_layout], surface_format);

        Ok(Self {
            device,
            pipeline,
            queue,
            surface,
        })
    }

    /// Creates handles to the graphics backend as well as the surface upon which rendering will
    /// take place.
    fn create_adapter_and_surface(
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

    /// Creates handles to the logical graphics device as well as the command buffer queue.
    fn create_device_and_queue(adapter: &Adapter) -> anyhow::Result<(Device, Queue)> {
        block_on(adapter.request_device(
            &DeviceDescriptor {
                limits: adapter.limits(),
                ..Default::default()
            },
            None,
        ))
        .map_err(|e| anyhow::Error::from(e).context("Failed to find a compatible device"))
    }

    fn create_texture_size(size: (u32, u32)) -> Extent3d {
        Extent3d {
            width: size.0,
            height: size.1,
            depth_or_array_layers: 1,
        }
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
                buffers: &[],
            },
            fragment: None,
            primitive: PrimitiveState::default(),
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

    fn create_texture(device: &Device, size: Extent3d) -> Texture {
        device.create_texture(&TextureDescriptor {
            label: None,
            size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D3,
            format: TextureFormat::Rgba8Unorm,
            usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
        })
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
        texture_view: &TextureView,
    ) -> BindGroup {
        device.create_bind_group(&BindGroupDescriptor {
            label: None,
            entries: &[],
            layout,
        })
    }

    // pub fn update_texture(&mut self, data: &[u8]) {
    //     self.queue.write_texture(
    //         self.texture.as_image_copy(),
    //         data,
    //         self.image_data_layout(),
    //         self.texture_size,
    //     );
    // }

    fn image_data_layout_for_tex_size(size: Extent3d) -> ImageDataLayout {
        use std::num::NonZeroU32;

        const WORD_SIZE: u32 = std::mem::size_of::<u32>() as u32;

        ImageDataLayout {
            offset: 0,
            bytes_per_row: NonZeroU32::new(WORD_SIZE * size.width),
            rows_per_image: NonZeroU32::new(size.height),
        }
    }

    pub fn render(&self) {
        let frame = self.surface.get_current_texture().unwrap();

        let mut encoder = self.create_command_encoder();
        self.do_render_pass(&mut encoder, &Self::create_texture_view(&frame.texture));
        self.queue.submit(Some(encoder.finish()));

        frame.present();
    }

    fn create_command_encoder(&self) -> CommandEncoder {
        self.device
            .create_command_encoder(&CommandEncoderDescriptor::default())
    }

    fn create_texture_view(texture: &Texture) -> TextureView {
        texture.create_view(&TextureViewDescriptor::default())
    }

    fn do_render_pass(&self, encoder: &mut CommandEncoder, view: &TextureView) {
        let mut pass = Self::create_render_pass(encoder, view);

        // pass.set_pipeline(&self.pipeline);
        // pass.set_bind_group(0, &self.bind_group, &[]);
        // pass.draw(0..3, 0..1);
    }

    fn create_render_pass<'a>(
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
            color_attachments: &[RenderPassColorAttachment {
                view,
                resolve_target: None,
                ops: Operations {
                    load: LoadOp::Clear(CLEAR_COLOR),
                    store: true,
                },
            }],
            ..Default::default()
        })
    }
}
