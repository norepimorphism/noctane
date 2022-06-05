// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_cpu::reg;

pub struct Call {
    pub name: &'static str,
    pub args: &'static [Argument],
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(","),
        )
    }
}

pub struct Argument {
    pub name: &'static str,
    pub value: (),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Call {
    pub fn new(name: &'static str, args: &'static [Argument]) -> Self {
        Self { name, args }
    }

    pub fn in_a0_table(reg: &reg::File, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::file_open()),
            0x01 => Some(Self::file_seek()),
            0x02 => Some(Self::file_read()),
            0x03 => Some(Self::file_write()),
            0x04 => Some(Self::file_close()),
            0x05 => Some(Self::file_ioctl()),
            0x06 => Some(Self::exit()),
            0x07 => Some(Self::file_get_device_flag()),
            0x08 => Some(Self::file_getc()),
            0x09 => Some(Self::file_putc()),
            0x0a => Some(Self::todigit()),
            0x0b => Some(Self::atof()),
            0x0c => Some(Self::strtoul()),
            0x0d => Some(Self::strtol()),
            0x0e => Some(Self::abs()),
            0x0f => Some(Self::labs()),
            0x10 => Some(Self::atoi()),
            0x11 => Some(Self::atol()),
            0x12 => Some(Self::atob()),
            0x13 => Some(Self::setjmp()),
            0x14 => Some(Self::longjmp()),
            0x15 => Some(Self::strcat()),
            0x16 => Some(Self::strncat()),
            0x17 => Some(Self::strcmp()),
            0x18 => Some(Self::strncmp()),
            0x19 => Some(Self::strcpy()),
            0x1a => Some(Self::strncpy()),
            0x1b => Some(Self::strlen()),
            0x1c => Some(Self::index()),
            0x1d => Some(Self::rindex()),
            0x1e => Some(Self::strchr()),
            0x1f => Some(Self::strrchr()),
            0x20 => Some(Self::strpbrk()),
            0x21 => Some(Self::strspn()),
            0x22 => Some(Self::strcspn()),
            0x23 => Some(Self::strtok()),
            0x24 => Some(Self::strstr()),
            0x25 => Some(Self::toupper()),
            0x26 => Some(Self::tolower()),
            0x27 => Some(Self::bcopy()),
            0x28 => Some(Self::bzero()),
            0x29 => Some(Self::bcmp()),
            0x2a => Some(Self::memcpy()),
            0x2b => Some(Self::memset()),
            0x2c => Some(Self::memmove()),
            0x2d => Some(Self::memcmp()),
            0x2e => Some(Self::memchr()),
            0x2f => Some(Self::rand()),
            0x30 => Some(Self::srand()),
            0x31 => Some(Self::qsort()),
            0x32 => Some(Self::strtod()),
            0x33 => Some(Self::malloc()),
            0x34 => Some(Self::free()),
            0x35 => Some(Self::lsearch()),
            0x36 => Some(Self::bsearch()),
            0x37 => Some(Self::calloc()),
            0x38 => Some(Self::realloc()),
            0x39 => Some(Self::init_heap()),
            0x3a => Some(Self::system_error_exit()),
            0x3b => Some(Self::std_in_getchar()),
            0x3c => Some(Self::std_out_putchar()),
            0x3d => Some(Self::std_in_gets()),
            0x3e => Some(Self::std_out_puts()),
            0x3f => Some(Self::printf()),
            0x40 => Some(Self::system_error_unresolved_exception()),
            0x41 => Some(Self::load_exe_header()),
            0x42 => Some(Self::load_exe_file()),
            0x43 => Some(Self::do_execute()),
            0x44 => Some(Self::flush_cache()),
            0x45 => Some(Self::init_a0_b0_c0_vectors()),
            0x46 => Some(Self::gpu_dw()),
            0x47 => Some(Self::gpu_send_dma()),
            0x48 => Some(Self::send_gp1_command()),
            0x49 => Some(Self::gpu_cw()),
            0x4a => Some(Self::gpu_cwp()),
            0x4b => Some(Self::send_gpu_linked_list()),
            0x4c => Some(Self::gpu_abort_dma()),
            0x4d => Some(Self::get_gpu_status()),
            0x4e => Some(Self::gpu_sync()),
            0x4f => Some(Self::system_error()),
            0x50 => Some(Self::system_error()),
            0x51 => Some(Self::load_and_execute()),
            0x52 => Some(Self::system_error()),
            0x53 => Some(Self::system_error()),
            0x54 => Some(Self::cd_init()),
            0x55 => Some(Self::_bu_init()),
            0x56 => Some(Self::cd_remove()),
            0x57..=0x5a => Some(Self::nullsub()),
            0x5b => Some(Self::dev_tty_init()),
            0x5c => Some(Self::dev_tty_open()),
            0x5d => Some(Self::dev_tty_in_out()),
            0x5e => Some(Self::dev_tty_ioctl()),
            0x5f => Some(Self::dev_cd_open()),
            0x60 => Some(Self::dev_cd_read()),
            0x61 => Some(Self::dev_cd_close()),
            0x62 => Some(Self::dev_cd_firstfile()),
            0x63 => Some(Self::dev_cd_nextfile()),
            0x64 => Some(Self::dev_cd_chdir()),
            0x65 => Some(Self::dev_card_open()),
            0x66 => Some(Self::dev_card_read()),
            0x67 => Some(Self::dev_card_write()),
            0x68 => Some(Self::dev_card_close()),
            0x69 => Some(Self::dev_card_firstfile()),
            0x6a => Some(Self::dev_card_nextfile()),
            0x6b => Some(Self::dev_card_erase()),
            0x6c => Some(Self::dev_card_undelete()),
            0x6d => Some(Self::dev_card_format()),
            0x6e => Some(Self::dev_card_rename()),
            0x6f => Some(Self::unk()),
            0x70 => Some(Self::_bu_init()),
            0x71 => Some(Self::cd_init()),
            0x72 => Some(Self::cd_remove()),
            0x73..=0x77 => Some(Self::nullsub()),
            0x78 => Some(Self::cd_async_seek_l()),
            0x79..=0x7b => Some(Self::nullsub()),
            0x7c => Some(Self::cd_async_get_status()),
            0x7d => Some(Self::nullsub()),
            0x7e => Some(Self::cd_async_read_sector()),
            0x7f..=0x80 => Some(Self::nullsub()),
            0x81 => Some(Self::cd_async_set_mode()),
            0x82..=0x8f => Some(Self::nullsub()),
            0x90 => Some(Self::cdrom_io_irq_func_1()),
            0x91 => Some(Self::cdrom_dma_irq_func_1()),
            0x92 => Some(Self::cdrom_io_irq_func_2()),
            0x93 => Some(Self::cdrom_dma_irq_func_2()),
            0x94 => Some(Self::cdrom_get_int_5_err_code()),
            0x95 => Some(Self::cd_init_sub_func()),
            0x96 => Some(Self::add_cdrom_device()),
            0x97 => Some(Self::add_mem_card_device()),
            0x98 => Some(Self::add_duart_tty_device()),
            0x99 => Some(Self::add_dummy_tty_device()),
            0x9a => Some(Self::system_error()),
            0x9b => Some(Self::system_error()),
            0x9c => Some(Self::set_conf()),
            0x9d => Some(Self::get_conf()),
            0x9e => Some(Self::set_cdrom_irq_auto_abort()),
            0x9f => Some(Self::set_mem_size()),
            _ => None,
        }
    }

    pub fn in_b0_table(reg: &reg::File, offset: u8) -> Option<Self> {
        None
    }

    pub fn in_c0_table(reg: &reg::File, offset: u8) -> Option<Self> {
        None
    }

    pub fn try_from_break(reg: &reg::File, code: u32) -> Option<Self> {
        match code {
            0x101 => Some(Self::pc_init()),
            0x102 => Some(Self::pc_create()),
            0x103 => Some(Self::pc_open()),
            0x104 => Some(Self::pc_close()),
            0x105 => Some(Self::pc_read()),
            0x106 => Some(Self::pc_write()),
            0x107 => Some(Self::pc_l_seek()),
            _ => None,
        }
    }

    pub fn from_syscall(reg: &reg::File, code: u32) -> Self {
        match code {
            0 => Self::no_function(),
            1 => Self::enter_critical_section(),
            2 => Self::exit_critical_section(),
            3 => Self::change_thread_sub_function(),
            _ => Self::deliver_event(),
        }
    }
}

macro_rules! def_fn {
    ($fn_name:ident => $name:ident ($($arg_name:ident : $arg_kind:tt),* $(,)?)) => {
        pub fn $fn_name($($arg_name: ArgumentKind::$arg_kind)*) -> Self {
            Self::new(stringify!($name), &[$(Argument::new($arg_name, ArgumentKind::$arg_kind),)*])
        }
    };
}

impl Call {
    def_fn!(file_open                           => FileOpen());
    def_fn!(file_seek                           => FileSeek());
    def_fn!(file_read                           => FileRead());
    def_fn!(file_write                          => FileWrite());
    def_fn!(file_close                          => FileClose());
    def_fn!(file_ioctl                          => FileIoctl());
    def_fn!(exit                                => exit());
    def_fn!(file_get_device_flag                => FileGetDeviceFlag());
    def_fn!(file_getc                           => FileGetc());
    def_fn!(file_putc                           => FilePutc());
    def_fn!(todigit                             => todigit());
    def_fn!(atof                                => atof());
    def_fn!(strtoul                             => strtoul());
    def_fn!(strtol                              => strtol());
    def_fn!(abs                                 => abs());
    def_fn!(labs                                => labs());
    def_fn!(atoi                                => atoi());
    def_fn!(atol                                => atol());
    def_fn!(atob                                => atob());
    def_fn!(setjmp                              => setjmp());
    def_fn!(longjmp                             => longjmp());
    def_fn!(strcat                              => strcat());
    def_fn!(strncat                             => strncat());
    def_fn!(strcmp                              => strcmp());
    def_fn!(strncmp                             => strncmp());
    def_fn!(strcpy                              => strcpy());
    def_fn!(strncpy                             => strncpy());
    def_fn!(strlen                              => strlen());
    def_fn!(index                               => index());
    def_fn!(rindex                              => rindex());
    def_fn!(strchr                              => strchr());
    def_fn!(strrchr                             => strrchr());
    def_fn!(strpbrk                             => strpbrk());
    def_fn!(strspn                              => strspn());
    def_fn!(strcspn                             => strcspn());
    def_fn!(strtok                              => strtok());
    def_fn!(strstr                              => strstr());
    def_fn!(toupper                             => toupper());
    def_fn!(tolower                             => tolower());
    def_fn!(bcopy                               => bcopy());
    def_fn!(bzero                               => bzero());
    def_fn!(bcmp                                => bcmp());
    def_fn!(memcpy                              => memcpy());
    def_fn!(memset                              => memset());
    def_fn!(memmove                             => memmove());
    def_fn!(memcmp                              => memcmp());
    def_fn!(memchr                              => memchr());
    def_fn!(rand                                => rand());
    def_fn!(srand                               => srand());
    def_fn!(qsort                               => qsort());
    def_fn!(strtod                              => strtod());
    def_fn!(malloc                              => malloc());
    def_fn!(free                                => free());
    def_fn!(lsearch                             => lsearch());
    def_fn!(bsearch                             => bsearch());
    def_fn!(calloc                              => calloc());
    def_fn!(realloc                             => realloc());
    def_fn!(init_heap                           => InitHeap());
    def_fn!(system_error_exit                   => SystemErrorExit());
    def_fn!(std_in_getchar                      => std_in_getchar());
    def_fn!(std_out_putchar                     => std_out_putchar());
    def_fn!(std_in_gets                         => std_in_gets());
    def_fn!(std_out_puts                        => std_out_puts());
    def_fn!(printf                              => printf());
    def_fn!(system_error_unresolved_exception   => SystemErrorUnresolvedException());
    def_fn!(load_exe_header                     => LoadExeHeader());
    def_fn!(load_exe_file                       => LoadExeFile());
    def_fn!(do_execute                          => DoExecute());
    def_fn!(flush_cache                         => FlushCache());
    def_fn!(init_a0_b0_c0_vectors               => init_a0_b0_c0_vectors());
    def_fn!(gpu_dw                              => GPU_dw());
    def_fn!(gpu_send_dma                        => gpu_send_dma());
    def_fn!(send_gp1_command                    => SendGP1Command());
    def_fn!(gpu_cw                              => GPU_cw());
    def_fn!(gpu_cwp                             => GPU_cwp());
    def_fn!(send_gpu_linked_list                => send_gpu_linked_list());
    def_fn!(gpu_abort_dma                       => gpu_abort_dma());
    def_fn!(get_gpu_status                      => GetGPUStatus());
    def_fn!(gpu_sync                            => gpu_sync());
    def_fn!(system_error                        => SystemError());
    def_fn!(load_and_execute                    => LoadAndExecute());
    def_fn!(cd_init                             => CdInit());
    def_fn!(_bu_init                            => _bu_init());
    def_fn!(cd_remove                           => CdRemove());
    def_fn!(nullsub                             => nullsub());
    def_fn!(dev_tty_init                        => dev_tty_init());
    def_fn!(dev_tty_open                        => dev_tty_open());
    def_fn!(dev_tty_in_out                      => dev_tty_in_out());
    def_fn!(dev_tty_ioctl                       => dev_tty_ioctl());
    def_fn!(dev_cd_open                         => dev_cd_open());
    def_fn!(dev_cd_read                         => dev_cd_read());
    def_fn!(dev_cd_close                        => dev_cd_close());
    def_fn!(dev_cd_firstfile                    => dev_cd_firstfile());
    def_fn!(dev_cd_nextfile                     => dev_cd_nextfile());
    def_fn!(dev_cd_chdir                        => dev_cd_chdir());
    def_fn!(dev_card_open                       => dev_card_open());
    def_fn!(dev_card_read                       => dev_card_read());
    def_fn!(dev_card_write                      => dev_card_write());
    def_fn!(dev_card_close                      => dev_card_close());
    def_fn!(dev_card_firstfile                  => dev_card_firstfile());
    def_fn!(dev_card_nextfile                   => dev_card_nextfile());
    def_fn!(dev_card_erase                      => dev_card_erase());
    def_fn!(dev_card_undelete                   => dev_card_undelete());
    def_fn!(dev_card_format                     => dev_card_format());
    def_fn!(dev_card_rename                     => dev_card_rename());
    def_fn!(unk                                 => unk());
    def_fn!(cd_async_seek_l                     => CdAsyncSeekL());
    def_fn!(cd_async_get_status                 => CdAsyncGetStatus());
    def_fn!(cd_async_read_sector                => CdAsyncReadSector());
    def_fn!(cd_async_set_mode                   => CdAsyncSetMode());
    def_fn!(cdrom_io_irq_func_1                 => CdromIoIrqFunc1());
    def_fn!(cdrom_dma_irq_func_1                => CdromDmaIrqFunc1());
    def_fn!(cdrom_io_irq_func_2                 => CdromIoIrqFunc2());
    def_fn!(cdrom_dma_irq_func_2                => CdromDmaIrqFunc2());
    def_fn!(cdrom_get_int_5_err_code            => CdromGetInt5errCode());
    def_fn!(cd_init_sub_func                    => CdInitSubFunc());
    def_fn!(add_cdrom_device                    => AddCDROMDevice());
    def_fn!(add_mem_card_device                 => AddMemCardDevice());
    def_fn!(add_duart_tty_device                => AddDuartTtyDevice());
    def_fn!(add_dummy_tty_device                => AddDummyTtyDevice());
    def_fn!(set_conf                            => SetConf());
    def_fn!(get_conf                            => GetConf());
    def_fn!(set_cdrom_irq_auto_abort            => SetCdromIrqAutoAbort());
    def_fn!(set_mem_size                        => SetMemSize());

    def_fn!(pc_init                             => PCInit());
    def_fn!(pc_create                           => PCCreate());
    def_fn!(pc_open                             => PCOpen());
    def_fn!(pc_close                            => PCClose());
    def_fn!(pc_read                             => PCRead());
    def_fn!(pc_write                            => PCWrite());
    def_fn!(pc_l_seek                           => PClSeek());

    def_fn!(no_function                         => NoFunction());
    def_fn!(enter_critical_section              => EnterCriticalSection());
    def_fn!(exit_critical_section               => ExitCriticalSection());
    def_fn!(change_thread_sub_function          => ChangeThreadSubFunction());
    def_fn!(deliver_event                       => DeliverEvent());
}
