// SPDX-License-Identifier: MPL-2.0

//! Noctane's WebGPU-backed 2D renderer.
//!
//! Due to the PSX's ability to allow DMA transfers from VRAM, it was necessary to split rendering
//! into two stages. First, polygons are drawn to an internal texture, and then the texture is drawn
//! to the underlying surface; if a DMA transfer from VRAM is requested, pending polygons are drawn
//! to the internal texture prematurely and it is consulted.

use noctane_util::BitStack as _;
use raw_window_handle::HasRawWindowHandle;
// This is usually a bad idea, but we use *so many* WGPU imports that it would be inconvenient
// otherwise.
use wgpu::{*, util::DeviceExt as _};

const VRAM_TEXTURE_FORMAT: TextureFormat = TextureFormat::Rgba8Uint;

#[derive(Debug)]
pub enum Error {
    NoCompatibleAdapterFound,
    NoCompatibleDeviceFound,
}

#[derive(Clone, Copy, Debug)]
pub struct VertexBufferEntry {
    pub vert: Vertex,
    pub strat: SampleStrategy,
}

#[derive(Clone, Copy, Debug)]
pub enum SampleStrategy {
    Constant {
        color: [u8; 4],
    },
    Texture(SampleTextureStrategy),
}

#[derive(Clone, Copy, Debug)]
pub struct SampleTextureStrategy {
    /// The top-left vertex of the texture to sample.
    pub vert: Vertex,
    pub page_x: u16,
    pub pal_strat: Option<SamplePaletteStrategy>,
    pub blend_color: Option<[u8; 4]>,
}

#[derive(Clone, Copy, Debug)]
pub struct SamplePaletteStrategy {
    pub kind: SamplePaletteStrategyKind,
    pub vert: Vertex,
}

#[derive(Clone, Copy, Debug)]
pub enum SamplePaletteStrategyKind {
    /// Sample a 4-bit color palette.
    Bpp4,
    /// Sample a 8-bit color palette.
    Bpp8,
}

impl VertexBufferEntry {
    fn generate_raw(self) -> RawVertexBufferEntry {
        match self.strat {
            SampleStrategy::Constant { color } => {
                RawVertexBufferEntry {
                    vert: self.vert,
                    flags: SampleStrategyFlags::STRAT_CONST.bits(),
                    // Don't care.
                    tex_page_x: 0,
                    color,
                    // Don't care.
                    tex_vert: Vertex::default(),
                    // Don't care.
                    pal_vert: Vertex::default(),
                }
            }
            SampleStrategy::Texture(strat) => {
                let mut flags = SampleStrategyFlags::STRAT_TEX;

                let blend_color = strat.blend_color.map(|color| {
                    flags |= SampleStrategyFlags::TEX_BLEND;

                    color
                })
                .unwrap_or_default();
                let pal_vert = strat.pal_strat.map(|strat| {
                    flags |= SampleStrategyFlags::TEX_PALETTE;
                    flags |= match strat.kind {
                        SamplePaletteStrategyKind::Bpp4 => SampleStrategyFlags::PAL_BPP4,
                        SamplePaletteStrategyKind::Bpp8 => SampleStrategyFlags::PAL_BPP8,
                    };

                    strat.vert
                })
                .unwrap_or_default();

                RawVertexBufferEntry {
                    vert: self.vert,
                    flags: flags.bits(),
                    tex_page_x: strat.page_x,
                    color: blend_color,
                    tex_vert: strat.vert,
                    pal_vert,
                }
            }
        }
    }
}

bitflags::bitflags! {
    struct SampleStrategyFlags: u16 {
        const STRAT_CONST   = 0 << 0;
        const STRAT_TEX     = 1 << 0;
        const TEX_PALETTE   = 1 << 1;
        const TEX_BLEND     = 1 << 2;
        const PAL_BPP4      = 0 << 3;
        const PAL_BPP8      = 1 << 3;
    }
}

#[repr(C, align(4))]
#[derive(Clone, Copy, Debug, Default)]
struct RawVertexBufferEntry {
    vert: Vertex,
    flags: u16,
    tex_page_x: u16,
    color: [u8; 4],
    tex_vert: Vertex,
    pal_vert: Vertex,
}

unsafe impl bytemuck::Pod for RawVertexBufferEntry {}
unsafe impl bytemuck::Zeroable for RawVertexBufferEntry {}

impl Vertex {
    pub fn decode(mut code: u32) -> Self {
        let x = code.pop_bits(11) as u16;
        code.pop_bits(5);
        let y = code.pop_bits(11) as u16;

        Self { x, y }
    }

    pub fn new(x: u16, y: u16) -> Self {
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

/// Noctane's WebGPU-backed 2D renderer.
#[derive(Debug)]
pub struct Renderer {
    device: Device,
    just_rendered: bool,
    queue: Queue,
    surface: Surface,
    surface_bind_group: BindGroup,
    surface_format: TextureFormat,
    surface_pipeline: RenderPipeline,
    tex_vram: Texture,
    vertex_buffer_entries: Vec<RawVertexBufferEntry>,
    vram: Texture,
    vram_bind_group: BindGroup,
    vram_pipeline: RenderPipeline,
}

impl Renderer {
    /// Creates a new `Renderer`.
    ///
    /// # Safety
    ///
    /// `window` must live for as long as the returned renderer.
    pub async unsafe fn new(
        window: &impl HasRawWindowHandle,
        backends: Backends,
    ) -> Result<Self, Error> {
        let (adapter, surface) = Self::create_adapter_and_surface(window, backends).await?;
        let (device, queue) = Self::create_device_and_queue(&adapter).await?;
        let vram = Self::create_vram(&device);
        let tex_vram = Self::create_vram(&device);

        let vram_bind_group_layout = Self::create_vram_bind_group_layout(&device);
        let vram_pipeline = Self::create_vram_pipeline(&device, &vram_bind_group_layout);
        let surface_format = surface
            .get_preferred_format(&adapter)
            .expect("surface is incompatible with the adapter");
        let surface_bind_group_layout = Self::create_surface_bind_group_layout(&device);
        let surface_pipeline = Self::create_surface_pipeline(
            &device,
            surface_format,
            &surface_bind_group_layout,
        );

        let vram_bind_group = Self::create_vram_bind_group(
            &device,
            &vram_bind_group_layout,
            &Self::create_texture_view(&tex_vram),
        );
        let surface_bind_group = Self::create_surface_bind_group(
            &device,
            &surface_bind_group_layout,
            &Self::create_texture_view(&vram),
        );

        Ok(Self {
            device,
            just_rendered: false,
            queue,
            surface,
            surface_bind_group,
            surface_format,
            surface_pipeline,
            tex_vram,
            vertex_buffer_entries: Vec::new(),
            vram,
            vram_bind_group,
            vram_pipeline,
        })
    }

    /// Creates handles to the graphics backend as well as the surface upon which rendering will
    /// take place.
    async fn create_adapter_and_surface(
        window: &impl HasRawWindowHandle,
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

    fn create_vram(device: &Device) -> Texture {
        device.create_texture(&TextureDescriptor {
            label: Some("VRAM"),
            size: Extent3d {
                width: 1024,
                height: 512,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: VRAM_TEXTURE_FORMAT,
            usage: {
                TextureUsages::TEXTURE_BINDING
                    | TextureUsages::RENDER_ATTACHMENT
                    | TextureUsages::COPY_SRC
                    | TextureUsages::COPY_DST
            },
        })
    }
}

macro_rules! create_shader_module {
    ($device:expr, $path:literal $(,)?) => {
        $device.create_shader_module(&include_wgsl!($path))
    };
}

impl Renderer {
    fn create_vram_bind_group_layout(device: &Device) -> BindGroupLayout {
        device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("VRAM bind group layout"),
            entries: &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Texture {
                    sample_type: TextureSampleType::Uint,
                    view_dimension: TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            }],
        })
    }

    fn create_vram_pipeline(
        device: &Device,
        bind_group_layout: &BindGroupLayout,
    ) -> RenderPipeline {
        Self::create_pipeline(
            &device,
            VRAM_TEXTURE_FORMAT,
            PolygonMode::Fill,
            &[bind_group_layout],
            &[VertexBufferLayout {
                array_stride: std::mem::size_of::<RawVertexBufferEntry>() as BufferAddress,
                step_mode: VertexStepMode::Vertex,
                attributes: &vertex_attr_array![
                    0 => Uint16x2,
                    1 => Uint16x2,
                    2 => Uint32,
                    3 => Uint16x2,
                    4 => Uint16x2,
                ],
            }],
            &create_shader_module!(device, "gfx/vram/vertex.wgsl"),
            &create_shader_module!(device, "gfx/vram/fragment.wgsl"),
        )
    }

    fn create_surface_bind_group_layout(device: &Device) -> BindGroupLayout {
        device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Surface bind group layout"),
            entries: &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Texture {
                    sample_type: TextureSampleType::Uint,
                    view_dimension: TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            }],
        })
    }

    fn create_surface_pipeline(
        device: &Device,
        format: TextureFormat,
        bind_group_layout: &BindGroupLayout,
    ) -> RenderPipeline {
        Self::create_pipeline(
            &device,
            format,
            PolygonMode::Fill,
            &[&bind_group_layout],
            &[],
            &create_shader_module!(device, "gfx/surface/vertex.wgsl"),
            &create_shader_module!(device, "gfx/surface/fragment.wgsl"),
        )
    }

    fn create_pipeline(
        device: &Device,
        texture_format: TextureFormat,
        polygon_mode: PolygonMode,
        bind_group_layouts: &[&BindGroupLayout],
        vertex_buffers: &[VertexBufferLayout],
        vertex_shader: &ShaderModule,
        fragment_shader: &ShaderModule,
    ) -> RenderPipeline {
        device.create_render_pipeline(&RenderPipelineDescriptor {
            label: None,
            layout: Some(&Self::create_pipeline_layout(device, bind_group_layouts)),
            vertex: VertexState {
                module: vertex_shader,
                entry_point: "main",
                buffers: vertex_buffers,
            },
            fragment: Some(FragmentState {
                module: fragment_shader,
                entry_point: "main",
                targets: &[wgpu::ColorTargetState {
                    format: texture_format,
                    blend: None,
                    write_mask: ColorWrites::ALL,
                }],
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                polygon_mode,
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

    fn create_vram_bind_group(
        device: &Device,
        layout: &BindGroupLayout,
        tex_vram_view: &TextureView,
    ) -> BindGroup {
        device.create_bind_group(&BindGroupDescriptor {
            label: Some("VRAM bind group"),
            layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::TextureView(tex_vram_view),
            }],
        })
    }

    fn create_texture_view(texture: &Texture) -> TextureView {
        texture.create_view(&TextureViewDescriptor::default())
    }

    fn create_surface_bind_group(
        device: &Device,
        layout: &BindGroupLayout,
        vram_view: &TextureView,
    ) -> BindGroup {
        device.create_bind_group(&BindGroupDescriptor {
            label: Some("Surface bind group"),
            layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::TextureView(vram_view),
            }],
        })
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        self.surface.configure(
            &self.device,
            &SurfaceConfiguration {
                usage: {
                    TextureUsages::TEXTURE_BINDING
                        | TextureUsages::RENDER_ATTACHMENT
                        | TextureUsages::COPY_DST
                },
                format: self.surface_format,
                width,
                height,
                present_mode: PresentMode::Fifo,
            },
        );
    }

    pub fn draw_quad(&mut self, entries: [VertexBufferEntry; 4]) {
        self.draw_triangle([
            entries[0],
            entries[1],
            entries[2],
        ]);
        self.draw_triangle([
            entries[1],
            entries[2],
            entries[3],
        ]);
    }

    pub fn draw_triangle(&mut self, entries: [VertexBufferEntry; 3]) {
        if self.just_rendered {
            self.vertex_buffer_entries.clear();
            self.just_rendered = false;
        }
        self.vertex_buffer_entries.extend(entries.map(VertexBufferEntry::generate_raw));
    }

    pub fn blit(&mut self, top_left: Vertex, size: [u16; 2], data: &[u8]) {
        if !self.vertex_buffer_entries.is_empty() {
            self.render();
        }

        let bytes_per_row = std::num::NonZeroU32::new(
            (std::mem::size_of::<u32>() as u32) * u32::from(size[0])
        );
        assert!(bytes_per_row.is_some_and(|value| ((data.len() as u32) % value.get()) == 0));

        self.queue.write_texture(
            ImageCopyTexture {
                texture: &self.vram,
                mip_level: 0,
                origin: Origin3d {
                    x: u32::from(top_left.x),
                    y: u32::from(top_left.y),
                    z: 0,
                },
                aspect: TextureAspect::All,
            },
            data,
            ImageDataLayout {
                offset: 0,
                bytes_per_row,
                rows_per_image: None,
            },
            Extent3d {
                width: u32::from(size[0]),
                height: u32::from(size[1]),
                depth_or_array_layers: 1,
            },
        )
    }

    pub fn render(&mut self) {
        let mut encoder = self.create_command_encoder();
        encoder.copy_texture_to_texture(
            self.vram.as_image_copy(),
            self.tex_vram.as_image_copy(),
            Extent3d { width: 1024, height: 512, depth_or_array_layers: 1 },
        );

        let vram_view = Self::create_texture_view(&self.vram);
        let frame = self.surface.get_current_texture().unwrap();
        let vertex_buffer = self.create_vertex_buffer();
        // Draw to VRAM.
        self.do_vram_render_pass(&mut encoder, &vertex_buffer, &vram_view);
        self.do_surface_render_pass(&mut encoder, &Self::create_texture_view(&frame.texture));
        self.queue.submit(Some(encoder.finish()));

        frame.present();

        self.just_rendered = true;
    }

    fn create_command_encoder(&self) -> CommandEncoder {
        self.device.create_command_encoder(&CommandEncoderDescriptor::default())
    }

    fn create_vertex_buffer(&self) -> Buffer {
        self.device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Vertex buffer"),
            contents: bytemuck::cast_slice(&self.vertex_buffer_entries),
            usage: BufferUsages::VERTEX,
        })
    }

    fn do_vram_render_pass(
        &self,
        encoder: &mut CommandEncoder,
        vertex_buffer: &Buffer,
        view: &TextureView,
    ) {
        let mut pass = Self::create_render_pass(
            encoder,
            view,
            Operations {
                load: LoadOp::Load,
                store: true,
            },
        );
        pass.set_pipeline(&self.vram_pipeline);
        pass.set_bind_group(0, &self.vram_bind_group, &[]);
        pass.set_vertex_buffer(0, vertex_buffer.slice(..));
        pass.draw(0..(self.vertex_buffer_entries.len() as u32), 0..1);
    }

    fn do_surface_render_pass(
        &self,
        encoder: &mut CommandEncoder,
        view: &TextureView,
    ) {
        let mut pass = Self::create_render_pass(
            encoder,
            view,
            Operations {
                load: LoadOp::Clear(Color::BLACK),
                store: true,
            },
        );
        pass.set_pipeline(&self.surface_pipeline);
        pass.set_bind_group(0, &self.surface_bind_group, &[]);
        pass.draw(0..3, 0..1);
    }

    fn create_render_pass<'a>(
        encoder: &'a mut CommandEncoder,
        view: &'a TextureView,
        ops: Operations<Color>,
    ) -> RenderPass<'a> {
        encoder.begin_render_pass(&RenderPassDescriptor {
            color_attachments: &[RenderPassColorAttachment {
                view,
                resolve_target: None,
                ops,
            }],
            ..Default::default()
        })
    }
}
