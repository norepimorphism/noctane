// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_cpu::Cpu;

pub struct Call {
    pub name: &'static str,
    pub args: Vec<Argument>,
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

impl Argument {
    pub fn new(name: &'static str, value: ArgumentValue) -> Self {
        Self { name, value }
    }
}

pub struct Argument {
    pub name: &'static str,
    pub value: ArgumentValue,
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}
pub enum ArgumentValue {
    Char(char),
    String(String),
    UInt(i32),
    SInt(i32),
    Address(u32),
}

impl fmt::Display for ArgumentValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Char(it)     => {
                write!(f, "'")?;
                for c in std::ascii::escape_default(*it as u8) {
                    write!(f, "{}", c as char)?;
                }

                write!(f, "'")
            }
            Self::String(it) => {
                write!(f, "\"")?;
                for byte in it.as_bytes() {
                    for c in std::ascii::escape_default(*byte) {
                        write!(f, "{}", c as char)?;
                    }
                }

                write!(f, "\"")
            }
            Self::UInt(it)      => write!(f, "{}", format_int!(*it, *it)),
            Self::SInt(it)      => write!(f, "{}", format_int!(*it, it.abs())),
            Self::Address(it)   => write!(f, "{:#08x}", it),
        }
    }
}

impl Call {
    pub fn new(name: &'static str, args: Vec<Argument>) -> Self {
        Self { name, args }
    }

    fn read_string(cpu: &mut Cpu) -> String {
        let mut s = String::new();
        for ptr in cpu.reg().gpr(4).. {
            let c = cpu.mem_mut().read_8(ptr);
            if c == b'\0' {
                break;
            }

            s.push(c as char);
        }

        s
    }

    pub fn in_a0_table(cpu: &mut Cpu, offset: u8) -> Option<Self> {
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
            0x34 => Some(Self::free(cpu.reg().gpr(4))),
            0x35 => Some(Self::lsearch()),
            0x36 => Some(Self::bsearch()),
            0x37 => Some(Self::calloc()),
            0x38 => Some(Self::realloc()),
            0x39 => Some(Self::init_heap()),
            0x3a => Some(Self::system_error_exit()),
            0x3b => Some(Self::std_in_getchar()),
            0x3c => Some(Self::std_out_putchar(cpu.reg().gpr(4) as u8 as char)),
            0x3d => Some(Self::std_in_gets()),
            0x3e => Some(Self::std_out_puts(Self::read_string(cpu))),
            0x3f => Some(Self::printf(Self::read_string(cpu))),
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
            0xa0 => Some(Self::warm_boot()),
            0xa1 => Some(Self::system_error_boot_or_disk_failure()),
            0xa2 => Some(Self::enqueue_cd_intr()),
            0xa3 => Some(Self::dequeue_cd_intr()),
            0xa4 => Some(Self::cd_get_lbn()),
            0xa5 => Some(Self::cd_read_sector()),
            0xa6 => Some(Self::cd_get_status()),
            0xa7 => Some(Self::bu_callback_okay()),
            0xa8 => Some(Self::bu_callback_err_write()),
            0xa9 => Some(Self::bu_callback_err_busy()),
            0xaa => Some(Self::bu_callback_err_eject()),
            0xab => Some(Self::_card_info()),
            0xac => Some(Self::_card_async_load_directory()),
            0xad => Some(Self::set_card_auto_format()),
            0xae => Some(Self::bu_callback_err_prev_write()),
            0xaf => Some(Self::card_write_test()),
            0xb0..=0xb1 => Some(Self::nullsub()),
            0xb2 => Some(Self::ioabort_raw()),
            0xb3 => Some(Self::nullsub()),
            0xb4 => Some(Self::get_system_info()),
            _ => None,
        }
    }

    pub fn in_b0_table(cpu: &mut Cpu, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::alloc_kernel_memory()),
            0x01 => Some(Self::free_kernel_memory()),
            0x02 => Some(Self::init_timer()),
            0x03 => Some(Self::get_timer()),
            0x04 => Some(Self::enable_timer_irq()),
            0x05 => Some(Self::disable_timer_irq()),
            0x06 => Some(Self::restart_timer()),
            0x07 => Some(Self::deliver_event()),
            0x08 => Some(Self::open_event()),
            0x09 => Some(Self::close_event()),
            0x0a => Some(Self::wait_event()),
            0x0b => Some(Self::test_event()),
            0x0c => Some(Self::enable_event()),
            0x0d => Some(Self::disable_event()),
            0x0e => Some(Self::open_thread()),
            0x0f => Some(Self::close_thread()),
            0x10 => Some(Self::change_thread()),
            0x11 => Some(Self::jump_to_0000000x00()),
            0x12 => Some(Self::init_pad()),
            0x13 => Some(Self::start_pad()),
            0x14 => Some(Self::stop_pad()),
            0x15 => Some(Self::outdated_pad_init_and_start()),
            0x16 => Some(Self::outdated_pad_get_buttons()),
            0x17 => Some(Self::return_from_exception()),
            0x18 => Some(Self::set_default_exit_from_exception()),
            0x19 => Some(Self::set_custom_exit_from_exception()),
            0x1a..=0x1f => Some(Self::system_error()),
            0x20 => Some(Self::undeliver_event()),
            0x21..=0x23 => Some(Self::system_error()),
            0x24..=0x29 => Some(Self::jump_to_0000000x00()),
            0x2a..=0x2b => Some(Self::system_error()),
            0x2c..=0x31 => Some(Self::jump_to_0000000x00()),
            0x32 => Some(Self::file_open()),
            0x33 => Some(Self::file_seek()),
            0x34 => Some(Self::file_read()),
            0x35 => Some(Self::file_write()),
            0x36 => Some(Self::file_close()),
            0x37 => Some(Self::file_ioctl()),
            0x38 => Some(Self::exit()),
            0x39 => Some(Self::file_get_device_flag()),
            0x3a => Some(Self::file_getc()),
            0x3b => Some(Self::file_putc()),
            0x3c => Some(Self::std_in_getchar()),
            0x3d => Some(Self::std_out_putchar(cpu.reg().gpr(4) as u8 as char)),
            0x3e => Some(Self::std_in_gets()),
            0x3f => Some(Self::std_out_puts(Self::read_string(cpu))),
            0x40 => Some(Self::chdir()),
            0x41 => Some(Self::format_device()),
            0x42 => Some(Self::firstfile()),
            0x43 => Some(Self::nextfile()),
            0x44 => Some(Self::file_rename()),
            0x45 => Some(Self::file_delete()),
            0x46 => Some(Self::file_undelete()),
            0x47 => Some(Self::add_device()),
            0x48 => Some(Self::remove_device()),
            0x49 => Some(Self::print_installed_devices()),
            0x4a => Some(Self::init_card()),
            0x4b => Some(Self::start_card()),
            0x4c => Some(Self::stop_card()),
            0x4d => Some(Self::_card_info_subfunc()),
            0x4e => Some(Self::write_card_sector()),
            0x4f => Some(Self::read_card_sector()),
            0x50 => Some(Self::allow_new_card()),
            0x51 => Some(Self::krom2_raw_add()),
            0x52 => Some(Self::system_error()),
            0x53 => Some(Self::krom2_offset()),
            0x54 => Some(Self::get_last_error()),
            0x55 => Some(Self::get_last_file_error()),
            0x56 => Some(Self::get_c0_table()),
            0x57 => Some(Self::get_b0_table()),
            0x58 => Some(Self::get_bu_callback_port()),
            0x59 => Some(Self::testdevice()),
            0x5a => Some(Self::system_error()),
            0x5b => Some(Self::change_clear_pad()),
            0x5c => Some(Self::get_card_status()),
            0x5d => Some(Self::wait_card_status()),
            _ => None,
        }
    }

    pub fn in_c0_table(cpu: &Cpu, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::enqueue_timer_and_vblank_irqs()),
            0x01 => Some(Self::enqueue_syscall_handler()),
            0x02 => Some(Self::sys_enq_int_rp()),
            0x03 => Some(Self::sys_deq_int_rp()),
            0x04 => Some(Self::get_free_ev_cb_slot()),
            0x05 => Some(Self::get_free_tcb_slot()),
            0x06 => Some(Self::exception_handler()),
            0x07 => Some(Self::install_exception_handlers()),
            0x08 => Some(Self::sys_init_memory()),
            0x09 => Some(Self::sys_init_kernel_variables()),
            0x0a => Some(Self::change_clear_r_cnt()),
            0x0b => Some(Self::system_error()),
            0x0c => Some(Self::init_def_int()),
            0x0d => Some(Self::set_irq_auto_ack()),
            0x0e..=0x11 => Some(Self::nullsub()),
            0x12 => Some(Self::install_devices()),
            0x13 => Some(Self::flush_std_in_out_put()),
            0x14 => Some(Self::nullsub()),
            0x15 => Some(Self::tty_cdevinput()),
            0x16 => Some(Self::tty_cdevscan()),
            0x17 => Some(Self::tty_circgetc()),
            0x18 => Some(Self::tty_circputc()),
            0x19 => Some(Self::ioabort()),
            0x1a => Some(Self::set_card_find_mode()),
            0x1b => Some(Self::kernel_redirect()),
            0x1c => Some(Self::adjust_a0_table()),
            0x1d => Some(Self::get_card_find_mode()),
            _ => None,
        }
    }

    pub fn try_from_break(cpu: &Cpu, code: u32) -> Option<Self> {
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

    pub fn from_syscall(cpu: &Cpu, code: u32) -> Self {
        match code {
            0 => Self::no_function(),
            1 => Self::enter_critical_section(),
            2 => Self::exit_critical_section(),
            3 => Self::change_thread_sub_function(),
            _ => Self::deliver_event(),
        }
    }
}

macro_rules! get_arg_inner_ty {
    (Char) => { char };
    (String) => { String };
    (UInt) => { u32 };
    (SInt) => { i32 };
    (Address) => { u32 };
}

macro_rules! def_fns {
    (
        $(
            $fn_name:ident => $name:ident ($($arg_name:ident : $arg_ty:ident),* $(,)?)
        ),* $(,)?
    ) => {
        $(
            pub fn $fn_name($($arg_name: get_arg_inner_ty!($arg_ty))*) -> Self {
                Self::new(
                    stringify!($name),
                    vec![
                        $(
                            Argument::new(stringify!($arg_name), ArgumentValue::$arg_ty($arg_name)),
                        )*
                    ],
                )
            }
        )*
    };
}

impl Call {
    def_fns!(
        file_open                           => FileOpen(),
        file_seek                           => FileSeek(),
        file_read                           => FileRead(),
        file_write                          => FileWrite(),
        file_close                          => FileClose(),
        file_ioctl                          => FileIoctl(),
        exit                                => exit(),
        file_get_device_flag                => FileGetDeviceFlag(),
        file_getc                           => FileGetc(),
        file_putc                           => FilePutc(),
        todigit                             => todigit(),
        atof                                => atof(),
        strtoul                             => strtoul(),
        strtol                              => strtol(),
        abs                                 => abs(),
        labs                                => labs(),
        atoi                                => atoi(),
        atol                                => atol(),
        atob                                => atob(),
        setjmp                              => setjmp(),
        longjmp                             => longjmp(),
        strcat                              => strcat(),
        strncat                             => strncat(),
        strcmp                              => strcmp(),
        strncmp                             => strncmp(),
        strcpy                              => strcpy(),
        strncpy                             => strncpy(),
        strlen                              => strlen(),
        index                               => index(),
        rindex                              => rindex(),
        strchr                              => strchr(),
        strrchr                             => strrchr(),
        strpbrk                             => strpbrk(),
        strspn                              => strspn(),
        strcspn                             => strcspn(),
        strtok                              => strtok(),
        strstr                              => strstr(),
        toupper                             => toupper(),
        tolower                             => tolower(),
        bcopy                               => bcopy(),
        bzero                               => bzero(),
        bcmp                                => bcmp(),
        memcpy                              => memcpy(),
        memset                              => memset(),
        memmove                             => memmove(),
        memcmp                              => memcmp(),
        memchr                              => memchr(),
        rand                                => rand(),
        srand                               => srand(),
        qsort                               => qsort(),
        strtod                              => strtod(),
        malloc                              => malloc(),
        free                                => free(obj: Address),
        lsearch                             => lsearch(),
        bsearch                             => bsearch(),
        calloc                              => calloc(),
        realloc                             => realloc(),
        init_heap                           => InitHeap(),
        system_error_exit                   => SystemErrorExit(),
        std_in_getchar                      => std_in_getchar(),
        std_out_putchar                     => std_out_putchar(c: Char),
        std_in_gets                         => std_in_gets(),
        std_out_puts                        => std_out_puts(s: String),
        printf                              => printf(fmt: String),
        system_error_unresolved_exception   => SystemErrorUnresolvedException(),
        load_exe_header                     => LoadExeHeader(),
        load_exe_file                       => LoadExeFile(),
        do_execute                          => DoExecute(),
        flush_cache                         => FlushCache(),
        init_a0_b0_c0_vectors               => init_a0_b0_c0_vectors(),
        gpu_dw                              => GPU_dw(),
        gpu_send_dma                        => gpu_send_dma(),
        send_gp1_command                    => SendGP1Command(),
        gpu_cw                              => GPU_cw(),
        gpu_cwp                             => GPU_cwp(),
        send_gpu_linked_list                => send_gpu_linked_list(),
        gpu_abort_dma                       => gpu_abort_dma(),
        get_gpu_status                      => GetGPUStatus(),
        gpu_sync                            => gpu_sync(),
        system_error                        => SystemError(),
        load_and_execute                    => LoadAndExecute(),
        cd_init                             => CdInit(),
        _bu_init                            => _bu_init(),
        cd_remove                           => CdRemove(),
        nullsub                             => nullsub(),
        dev_tty_init                        => dev_tty_init(),
        dev_tty_open                        => dev_tty_open(),
        dev_tty_in_out                      => dev_tty_in_out(),
        dev_tty_ioctl                       => dev_tty_ioctl(),
        dev_cd_open                         => dev_cd_open(),
        dev_cd_read                         => dev_cd_read(),
        dev_cd_close                        => dev_cd_close(),
        dev_cd_firstfile                    => dev_cd_firstfile(),
        dev_cd_nextfile                     => dev_cd_nextfile(),
        dev_cd_chdir                        => dev_cd_chdir(),
        dev_card_open                       => dev_card_open(),
        dev_card_read                       => dev_card_read(),
        dev_card_write                      => dev_card_write(),
        dev_card_close                      => dev_card_close(),
        dev_card_firstfile                  => dev_card_firstfile(),
        dev_card_nextfile                   => dev_card_nextfile(),
        dev_card_erase                      => dev_card_erase(),
        dev_card_undelete                   => dev_card_undelete(),
        dev_card_format                     => dev_card_format(),
        dev_card_rename                     => dev_card_rename(),
        unk                                 => unk(),
        cd_async_seek_l                     => CdAsyncSeekL(),
        cd_async_get_status                 => CdAsyncGetStatus(),
        cd_async_read_sector                => CdAsyncReadSector(),
        cd_async_set_mode                   => CdAsyncSetMode(),
        cdrom_io_irq_func_1                 => CdromIoIrqFunc1(),
        cdrom_dma_irq_func_1                => CdromDmaIrqFunc1(),
        cdrom_io_irq_func_2                 => CdromIoIrqFunc2(),
        cdrom_dma_irq_func_2                => CdromDmaIrqFunc2(),
        cdrom_get_int_5_err_code            => CdromGetInt5errCode(),
        cd_init_sub_func                    => CdInitSubFunc(),
        add_cdrom_device                    => AddCDROMDevice(),
        add_mem_card_device                 => AddMemCardDevice(),
        add_duart_tty_device                => AddDuartTtyDevice(),
        add_dummy_tty_device                => AddDummyTtyDevice(),
        set_conf                            => SetConf(),
        get_conf                            => GetConf(),
        set_cdrom_irq_auto_abort            => SetCdromIrqAutoAbort(),
        set_mem_size                        => SetMemSize(),
        warm_boot                           => WarmBoot(),
        system_error_boot_or_disk_failure   => SystemErrorBootOrDiskFailure(),
        enqueue_cd_intr                     => EnqueueCdIntr(),
        dequeue_cd_intr                     => DequeueCdIntr(),
        cd_get_lbn                          => CdGetLbn(),
        cd_read_sector                      => CdReadSector(),
        cd_get_status                       => CdGetStatus(),
        bu_callback_okay                    => bu_callback_okay(),
        bu_callback_err_write               => bu_callback_err_write(),
        bu_callback_err_busy                => bu_callback_err_busy(),
        bu_callback_err_eject               => bu_callback_err_eject(),
        _card_info                          => _card_info(),
        _card_async_load_directory          => _card_async_load_directory(),
        set_card_auto_format                => set_card_auto_format(),
        bu_callback_err_prev_write          => bu_callback_err_prev_write(),
        card_write_test                     => card_write_test(),
        ioabort_raw                         => ioabort_raw(),
        get_system_info                     => GetSystemInfo(),

        alloc_kernel_memory                 => alloc_kernel_memory(),
        free_kernel_memory                  => free_kernel_memory(),
        init_timer                          => init_timer(),
        get_timer                           => get_timer(),
        enable_timer_irq                    => enable_timer_irq(),
        disable_timer_irq                   => disable_timer_irq(),
        restart_timer                       => restart_timer(),
        deliver_event                       => DeliverEvent(),
        open_event                          => OpenEvent(),
        close_event                         => CloseEvent(),
        wait_event                          => WaitEvent(),
        test_event                          => TestEvent(),
        enable_event                        => EnableEvent(),
        disable_event                       => DisableEvent(),
        open_thread                         => OpenThread(),
        close_thread                        => CloseThread(),
        change_thread                       => ChangeThread(),
        jump_to_0000000x00                  => jump_to_0000000x00(),
        init_pad                            => InitPad(),
        start_pad                           => StartPad(),
        stop_pad                            => StopPad(),
        outdated_pad_init_and_start         => OutdatedPadInitAndStart(),
        outdated_pad_get_buttons            => OutdatedPadGetButtons(),
        return_from_exception               => ReturnFromException(),
        set_default_exit_from_exception     => SetDefaultExitFromException(),
        set_custom_exit_from_exception      => SetCustomExitFromException(),
        undeliver_event                     => UnDeliverEvent(),
        chdir                               => chdir(),
        format_device                       => FormatDevice(),
        firstfile                           => firstfile(),
        nextfile                            => nextfile(),
        file_rename                         => FileRename(),
        file_delete                         => FileDelete(),
        file_undelete                       => FileUndelete(),
        add_device                          => AddDevice(),
        remove_device                       => RemoveDevice(),
        print_installed_devices             => PrintInstalledDevices(),
        init_card                           => InitCard(),
        start_card                          => StartCard(),
        stop_card                           => StopCard(),
        _card_info_subfunc                  => _card_info_subfunc(),
        write_card_sector                   => write_card_sector(),
        read_card_sector                    => read_card_sector(),
        allow_new_card                      => allow_new_card(),
        krom2_raw_add                       => Krom2RawAdd(),
        krom2_offset                        => Krom2Offset(),
        get_last_error                      => GetLastError(),
        get_last_file_error                 => GetLastFileError(),
        get_c0_table                        => GetC0Table(),
        get_b0_table                        => GetB0Table(),
        get_bu_callback_port                => get_bu_callback_port(),
        testdevice                          => testdevice(),
        change_clear_pad                    => ChangeClearPad(),
        get_card_status                     => get_card_status(),
        wait_card_status                    => wait_card_status(),

        enqueue_timer_and_vblank_irqs       => EnqueueTimerAndVblankIrqs(),
        enqueue_syscall_handler             => EnqueueSyscallHandler(),
        sys_enq_int_rp                      => SysEnqIntRP(),
        sys_deq_int_rp                      => SysDeqIntRP(),
        get_free_ev_cb_slot                 => get_free_EvCB_slot(),
        get_free_tcb_slot                   => get_free_TCB_slot(),
        exception_handler                   => ExceptionHandler(),
        install_exception_handlers          => InstallExceptionHandlers(),
        sys_init_memory                     => SysInitMemory(),
        sys_init_kernel_variables           => SysInitKernelVariables(),
        change_clear_r_cnt                  => ChangeClearRCnt(),
        init_def_int                        => InitDefInt(),
        set_irq_auto_ack                    => SetIrqAutoAck(),
        install_devices                     => InstallDevices(),
        flush_std_in_out_put                => FlushStdInOutPut(),
        tty_cdevinput                       => tty_cdevinput(),
        tty_cdevscan                        => tty_cdevscan(),
        tty_circgetc                        => tty_circgetc(),
        tty_circputc                        => tty_circputc(),
        ioabort                             => ioabort(),
        set_card_find_mode                  => set_card_find_mode(),
        kernel_redirect                     => KernelRedirect(),
        adjust_a0_table                     => AdjustA0Table(),
        get_card_find_mode                  => get_card_find_mode(),

        pc_init                             => PCInit(),
        pc_create                           => PCCreate(),
        pc_open                             => PCOpen(),
        pc_close                            => PCClose(),
        pc_read                             => PCRead(),
        pc_write                            => PCWrite(),
        pc_l_seek                           => PClSeek(),

        no_function                         => NoFunction(),
        enter_critical_section              => EnterCriticalSection(),
        exit_critical_section               => ExitCriticalSection(),
        change_thread_sub_function          => ChangeThreadSubFunction(),
    );
}
