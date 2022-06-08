// SPDX-License-Identifier: MPL-2.0

use std::fmt;

impl Call {
    pub fn new(name: &'static str, args: Vec<Argument>) -> Self {
        Self { name, args }
    }
}

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
            self.args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", "),
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
    UInt(u32),
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
            Self::Address(it)   => write!(f, "{:#010x}", it),
        }
    }
}

pub trait CallSource {
    fn read_raw_arg(&mut self, index: usize) -> u32;

    fn read_8(&mut self, addr: u32) -> u8;

    fn read_16(&mut self, addr: u32) -> u16;

    fn read_32(&mut self, addr: u32) -> u32;
}

impl Call {
    pub fn in_a0_table(src: &mut impl CallSource, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::file_open(src)),
            0x01 => Some(Self::file_seek(src)),
            0x02 => Some(Self::file_read(src)),
            0x03 => Some(Self::file_write(src)),
            0x04 => Some(Self::file_close(src)),
            0x05 => Some(Self::file_ioctl(src)),
            0x06 => Some(Self::exit(src)),
            0x07 => Some(Self::file_get_device_flag(src)),
            0x08 => Some(Self::file_getc(src)),
            0x09 => Some(Self::file_putc(src)),
            0x0a => Some(Self::todigit(src)),
            0x0b => Some(Self::atof(src)),
            0x0c => Some(Self::strtoul(src)),
            0x0d => Some(Self::strtol(src)),
            0x0e => Some(Self::abs(src)),
            0x0f => Some(Self::labs(src)),
            0x10 => Some(Self::atoi(src)),
            0x11 => Some(Self::atol(src)),
            0x12 => Some(Self::atob(src)),
            0x13 => Some(Self::setjmp(src)),
            0x14 => Some(Self::longjmp(src)),
            0x15 => Some(Self::strcat(src)),
            0x16 => Some(Self::strncat(src)),
            0x17 => Some(Self::strcmp(src)),
            0x18 => Some(Self::strncmp(src)),
            0x19 => Some(Self::strcpy(src)),
            0x1a => Some(Self::strncpy(src)),
            0x1b => Some(Self::strlen(src)),
            0x1c => Some(Self::index(src)),
            0x1d => Some(Self::rindex(src)),
            0x1e => Some(Self::strchr(src)),
            0x1f => Some(Self::strrchr(src)),
            0x20 => Some(Self::strpbrk(src)),
            0x21 => Some(Self::strspn(src)),
            0x22 => Some(Self::strcspn(src)),
            0x23 => Some(Self::strtok(src)),
            0x24 => Some(Self::strstr(src)),
            0x25 => Some(Self::toupper(src)),
            0x26 => Some(Self::tolower(src)),
            0x27 => Some(Self::bcopy(src)),
            0x28 => Some(Self::bzero(src)),
            0x29 => Some(Self::bcmp(src)),
            0x2a => Some(Self::memcpy(src)),
            0x2b => Some(Self::memset(src)),
            0x2c => Some(Self::memmove(src)),
            0x2d => Some(Self::memcmp(src)),
            0x2e => Some(Self::memchr(src)),
            0x2f => Some(Self::rand(src)),
            0x30 => Some(Self::srand(src)),
            0x31 => Some(Self::qsort(src)),
            0x32 => Some(Self::strtod(src)),
            0x33 => Some(Self::malloc(src)),
            0x34 => Some(Self::free(src)),
            0x35 => Some(Self::lsearch(src)),
            0x36 => Some(Self::bsearch(src)),
            0x37 => Some(Self::calloc(src)),
            0x38 => Some(Self::realloc(src)),
            0x39 => Some(Self::init_heap(src)),
            0x3a => Some(Self::system_error_exit(src)),
            0x3b => Some(Self::std_in_getchar(src)),
            0x3c => Some(Self::std_out_putchar(src)),
            0x3d => Some(Self::std_in_gets(src)),
            0x3e => Some(Self::std_out_puts(src)),
            0x3f => Some(Self::printf(src)),
            0x40 => Some(Self::system_error_unresolved_exception(src)),
            0x41 => Some(Self::load_exe_header(src)),
            0x42 => Some(Self::load_exe_file(src)),
            0x43 => Some(Self::do_execute(src)),
            0x44 => Some(Self::flush_cache(src)),
            0x45 => Some(Self::init_a0_b0_c0_vectors(src)),
            0x46 => Some(Self::gpu_dw(src)),
            0x47 => Some(Self::gpu_send_dma(src)),
            0x48 => Some(Self::send_gp1_command(src)),
            0x49 => Some(Self::gpu_cw(src)),
            0x4a => Some(Self::gpu_cwp(src)),
            0x4b => Some(Self::send_gpu_linked_list(src)),
            0x4c => Some(Self::gpu_abort_dma(src)),
            0x4d => Some(Self::get_gpu_status(src)),
            0x4e => Some(Self::gpu_sync(src)),
            0x4f => Some(Self::system_error(src)),
            0x50 => Some(Self::system_error(src)),
            0x51 => Some(Self::load_and_execute(src)),
            0x52 => Some(Self::system_error(src)),
            0x53 => Some(Self::system_error(src)),
            0x54 => Some(Self::cd_init(src)),
            0x55 => Some(Self::_bu_init(src)),
            0x56 => Some(Self::cd_remove(src)),
            0x57..=0x5a => Some(Self::nullsub(src)),
            0x5b => Some(Self::dev_tty_init(src)),
            0x5c => Some(Self::dev_tty_open(src)),
            0x5d => Some(Self::dev_tty_in_out(src)),
            0x5e => Some(Self::dev_tty_ioctl(src)),
            0x5f => Some(Self::dev_cd_open(src)),
            0x60 => Some(Self::dev_cd_read(src)),
            0x61 => Some(Self::dev_cd_close(src)),
            0x62 => Some(Self::dev_cd_firstfile(src)),
            0x63 => Some(Self::dev_cd_nextfile(src)),
            0x64 => Some(Self::dev_cd_chdir(src)),
            0x65 => Some(Self::dev_card_open(src)),
            0x66 => Some(Self::dev_card_read(src)),
            0x67 => Some(Self::dev_card_write(src)),
            0x68 => Some(Self::dev_card_close(src)),
            0x69 => Some(Self::dev_card_firstfile(src)),
            0x6a => Some(Self::dev_card_nextfile(src)),
            0x6b => Some(Self::dev_card_erase(src)),
            0x6c => Some(Self::dev_card_undelete(src)),
            0x6d => Some(Self::dev_card_format(src)),
            0x6e => Some(Self::dev_card_rename(src)),
            0x6f => Some(Self::unk(src)),
            0x70 => Some(Self::_bu_init(src)),
            0x71 => Some(Self::cd_init(src)),
            0x72 => Some(Self::cd_remove(src)),
            0x73..=0x77 => Some(Self::nullsub(src)),
            0x78 => Some(Self::cd_async_seek_l(src)),
            0x79..=0x7b => Some(Self::nullsub(src)),
            0x7c => Some(Self::cd_async_get_status(src)),
            0x7d => Some(Self::nullsub(src)),
            0x7e => Some(Self::cd_async_read_sector(src)),
            0x7f..=0x80 => Some(Self::nullsub(src)),
            0x81 => Some(Self::cd_async_set_mode(src)),
            0x82..=0x8f => Some(Self::nullsub(src)),
            0x90 => Some(Self::cdrom_io_irq_func_1(src)),
            0x91 => Some(Self::cdrom_dma_irq_func_1(src)),
            0x92 => Some(Self::cdrom_io_irq_func_2(src)),
            0x93 => Some(Self::cdrom_dma_irq_func_2(src)),
            0x94 => Some(Self::cdrom_get_int_5_err_code(src)),
            0x95 => Some(Self::cd_init_sub_func(src)),
            0x96 => Some(Self::add_cdrom_device(src)),
            0x97 => Some(Self::add_mem_card_device(src)),
            0x98 => Some(Self::add_duart_tty_device(src)),
            0x99 => Some(Self::add_dummy_tty_device(src)),
            0x9a => Some(Self::system_error(src)),
            0x9b => Some(Self::system_error(src)),
            0x9c => Some(Self::set_conf(src)),
            0x9d => Some(Self::get_conf(src)),
            0x9e => Some(Self::set_cdrom_irq_auto_abort(src)),
            0x9f => Some(Self::set_mem_size(src)),
            0xa0 => Some(Self::warm_boot(src)),
            0xa1 => Some(Self::system_error_boot_or_disk_failure(src)),
            0xa2 => Some(Self::enqueue_cd_intr(src)),
            0xa3 => Some(Self::dequeue_cd_intr(src)),
            0xa4 => Some(Self::cd_get_lbn(src)),
            0xa5 => Some(Self::cd_read_sector(src)),
            0xa6 => Some(Self::cd_get_status(src)),
            0xa7 => Some(Self::bu_callback_okay(src)),
            0xa8 => Some(Self::bu_callback_err_write(src)),
            0xa9 => Some(Self::bu_callback_err_busy(src)),
            0xaa => Some(Self::bu_callback_err_eject(src)),
            0xab => Some(Self::_card_info(src)),
            0xac => Some(Self::_card_async_load_directory(src)),
            0xad => Some(Self::set_card_auto_format(src)),
            0xae => Some(Self::bu_callback_err_prev_write(src)),
            0xaf => Some(Self::card_write_test(src)),
            0xb0..=0xb1 => Some(Self::nullsub(src)),
            0xb2 => Some(Self::ioabort_raw(src)),
            0xb3 => Some(Self::nullsub(src)),
            0xb4 => Some(Self::get_system_info(src)),
            _ => None,
        }
    }

    pub fn in_b0_table(src: &mut impl CallSource, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::alloc_kernel_memory(src)),
            0x01 => Some(Self::free_kernel_memory(src)),
            0x02 => Some(Self::init_timer(src)),
            0x03 => Some(Self::get_timer(src)),
            0x04 => Some(Self::enable_timer_irq(src)),
            0x05 => Some(Self::disable_timer_irq(src)),
            0x06 => Some(Self::restart_timer(src)),
            0x07 => Some(Self::deliver_event(src)),
            0x08 => Some(Self::open_event(src)),
            0x09 => Some(Self::close_event(src)),
            0x0a => Some(Self::wait_event(src)),
            0x0b => Some(Self::test_event(src)),
            0x0c => Some(Self::enable_event(src)),
            0x0d => Some(Self::disable_event(src)),
            0x0e => Some(Self::open_thread(src)),
            0x0f => Some(Self::close_thread(src)),
            0x10 => Some(Self::change_thread(src)),
            0x11 => Some(Self::jump_to_0000000x00(src)),
            0x12 => Some(Self::init_pad(src)),
            0x13 => Some(Self::start_pad(src)),
            0x14 => Some(Self::stop_pad(src)),
            0x15 => Some(Self::outdated_pad_init_and_start(src)),
            0x16 => Some(Self::outdated_pad_get_buttons(src)),
            0x17 => Some(Self::return_from_exception(src)),
            0x18 => Some(Self::set_default_exit_from_exception(src)),
            0x19 => Some(Self::set_custom_exit_from_exception(src)),
            0x1a..=0x1f => Some(Self::system_error(src)),
            0x20 => Some(Self::undeliver_event(src)),
            0x21..=0x23 => Some(Self::system_error(src)),
            0x24..=0x29 => Some(Self::jump_to_0000000x00(src)),
            0x2a..=0x2b => Some(Self::system_error(src)),
            0x2c..=0x31 => Some(Self::jump_to_0000000x00(src)),
            0x32 => Some(Self::file_open(src)),
            0x33 => Some(Self::file_seek(src)),
            0x34 => Some(Self::file_read(src)),
            0x35 => Some(Self::file_write(src)),
            0x36 => Some(Self::file_close(src)),
            0x37 => Some(Self::file_ioctl(src)),
            0x38 => Some(Self::exit(src)),
            0x39 => Some(Self::file_get_device_flag(src)),
            0x3a => Some(Self::file_getc(src)),
            0x3b => Some(Self::file_putc(src)),
            0x3c => Some(Self::std_in_getchar(src)),
            0x3d => Some(Self::std_out_putchar(src)),
            0x3e => Some(Self::std_in_gets(src)),
            0x3f => Some(Self::std_out_puts(src)),
            0x40 => Some(Self::chdir(src)),
            0x41 => Some(Self::format_device(src)),
            0x42 => Some(Self::firstfile(src)),
            0x43 => Some(Self::nextfile(src)),
            0x44 => Some(Self::file_rename(src)),
            0x45 => Some(Self::file_delete(src)),
            0x46 => Some(Self::file_undelete(src)),
            0x47 => Some(Self::add_device(src)),
            0x48 => Some(Self::remove_device(src)),
            0x49 => Some(Self::print_installed_devices(src)),
            0x4a => Some(Self::init_card(src)),
            0x4b => Some(Self::start_card(src)),
            0x4c => Some(Self::stop_card(src)),
            0x4d => Some(Self::_card_info_subfunc(src)),
            0x4e => Some(Self::write_card_sector(src)),
            0x4f => Some(Self::read_card_sector(src)),
            0x50 => Some(Self::allow_new_card(src)),
            0x51 => Some(Self::krom2_raw_add(src)),
            0x52 => Some(Self::system_error(src)),
            0x53 => Some(Self::krom2_offset(src)),
            0x54 => Some(Self::get_last_error(src)),
            0x55 => Some(Self::get_last_file_error(src)),
            0x56 => Some(Self::get_c0_table(src)),
            0x57 => Some(Self::get_b0_table(src)),
            0x58 => Some(Self::get_bu_callback_port(src)),
            0x59 => Some(Self::testdevice(src)),
            0x5a => Some(Self::system_error(src)),
            0x5b => Some(Self::change_clear_pad(src)),
            0x5c => Some(Self::get_card_status(src)),
            0x5d => Some(Self::wait_card_status(src)),
            _ => None,
        }
    }

    pub fn in_c0_table(src: &mut impl CallSource, offset: u8) -> Option<Self> {
        match offset {
            0x00 => Some(Self::enqueue_timer_and_vblank_irqs(src)),
            0x01 => Some(Self::enqueue_syscall_handler(src)),
            0x02 => Some(Self::sys_enq_int_rp(src)),
            0x03 => Some(Self::sys_deq_int_rp(src)),
            0x04 => Some(Self::get_free_ev_cb_slot(src)),
            0x05 => Some(Self::get_free_tcb_slot(src)),
            0x06 => Some(Self::exception_handler(src)),
            0x07 => Some(Self::install_exception_handlers(src)),
            0x08 => Some(Self::sys_init_memory(src)),
            0x09 => Some(Self::sys_init_kernel_variables(src)),
            0x0a => Some(Self::change_clear_r_cnt(src)),
            0x0b => Some(Self::system_error(src)),
            0x0c => Some(Self::init_def_int(src)),
            0x0d => Some(Self::set_irq_auto_ack(src)),
            0x0e..=0x11 => Some(Self::nullsub(src)),
            0x12 => Some(Self::install_devices(src)),
            0x13 => Some(Self::flush_std_in_out_put(src)),
            0x14 => Some(Self::nullsub(src)),
            0x15 => Some(Self::tty_cdevinput(src)),
            0x16 => Some(Self::tty_cdevscan(src)),
            0x17 => Some(Self::tty_circgetc(src)),
            0x18 => Some(Self::tty_circputc(src)),
            0x19 => Some(Self::ioabort(src)),
            0x1a => Some(Self::set_card_find_mode(src)),
            0x1b => Some(Self::kernel_redirect(src)),
            0x1c => Some(Self::adjust_a0_table(src)),
            0x1d => Some(Self::get_card_find_mode(src)),
            _ => None,
        }
    }

    pub fn try_from_break(src: &mut impl CallSource, code: u32) -> Option<Self> {
        match code {
            0x101 => Some(Self::pc_init(src)),
            0x102 => Some(Self::pc_create(src)),
            0x103 => Some(Self::pc_open(src)),
            0x104 => Some(Self::pc_close(src)),
            0x105 => Some(Self::pc_read(src)),
            0x106 => Some(Self::pc_write(src)),
            0x107 => Some(Self::pc_l_seek(src)),
            _ => None,
        }
    }

    pub fn from_syscall(src: &mut impl CallSource, code: u32) -> Self {
        match code {
            0 => Self::no_function(src),
            1 => Self::enter_critical_section(src),
            2 => Self::exit_critical_section(src),
            3 => Self::change_thread_sub_function(src),
            _ => Self::deliver_event(src),
        }
    }
}

macro_rules! def_fns {
    (
        $(
            $fn_name:ident => $name:ident ($($arg_name:tt : $arg_ty:ident),* $(,)?)
        ),* $(,)?
    ) => {
        $(
            #[allow(unused_assignments, unused_mut, unused_variables)]
            pub fn $fn_name(src: &mut impl CallSource) -> Self {
                #[allow(unused_macros)]
                macro_rules! convert_arg {
                    (Char $it:expr)     => { $it as u8 as char };
                    (String $it:expr)   => { read_string(src, $it) };
                    (UInt $it:expr)     => { $it };
                    (SInt $it:expr)     => { $it as i32 };
                    (Address $it:expr)  => { $it };
                }

                let mut arg_idx = 0;

                Self::new(
                    stringify!($name),
                    vec![
                        $(
                            {
                                let raw_arg = src.read_raw_arg(arg_idx);
                                arg_idx += 1;

                                Argument::new(
                                    stringify!($arg_name),
                                    ArgumentValue::$arg_ty(convert_arg!($arg_ty raw_arg)),
                                )
                            },
                        )*
                    ],
                )
            }
        )*
    };
}

fn read_string(src: &mut impl CallSource, addr: u32) -> String {
    let mut s = String::new();
    for ptr in addr.. {
        let c = src.read_8(ptr);
        if c == b'\0' {
            break;
        }

        s.push(c as char);
    }

    s
}

impl Call {
    def_fns!(
        file_open                           => FileOpen(path: String, mode: UInt),
        file_seek                           => FileSeek(fd: UInt, offset: SInt, mode: UInt),
        file_read                           => FileRead(fd: UInt, to: Address, len: UInt),
        file_write                          => FileWrite(fd: UInt, from: Address, len: UInt),
        file_close                          => FileClose(fd: UInt),
        file_ioctl                          => FileIoctl(fd: UInt, cmd: UInt, arg: UInt),
        exit                                => exit(code: SInt),
        file_get_device_flag                => FileGetDeviceFlag(fd: UInt),
        file_getc                           => FileGetc(fd: UInt),
        file_putc                           => FilePutc(c: Char, fd: UInt),
        todigit                             => todigit(c: Char),
        atof                                => atof(s: String),
        strtoul                             => strtoul(start: Address, end: Address, base: UInt),
        strtol                              => strtol(start: Address, end: Address, base: UInt),
        abs                                 => abs(val: SInt),
        labs                                => labs(val: SInt),
        atoi                                => atoi(s: String),
        atol                                => atol(s: String),
        atob                                => atob(s: String, to: Address),
        setjmp                              => setjmp(buf: Address),
        longjmp                             => longjmp(buf: Address),
        strcat                              => strcat(to: String, from: String),
        strncat                             => strncat(to: String, from: String, cap: UInt),
        strcmp                              => strcmp(a: String, b: String),
        strncmp                             => strncmp(a: String, b: String, cap: UInt),
        strcpy                              => strcpy(to: Address, from: String),
        strncpy                             => strncpy(to: Address, from: String, cap: UInt),
        strlen                              => strlen(s: String),
        index                               => index(s: String, c: Char),
        rindex                              => rindex(s: String, c: Char),
        strchr                              => strchr(s: String, c: Char),
        strrchr                             => strrchr(s: String, c: Char),
        strpbrk                             => strpbrk(),
        strspn                              => strspn(),
        strcspn                             => strcspn(),
        strtok                              => strtok(),
        strstr                              => strstr(),
        toupper                             => toupper(c: Char),
        tolower                             => tolower(c: Char),
        bcopy                               => bcopy(from: Address, to: Address, len: UInt),
        bzero                               => bzero(start: Address, len: UInt),
        bcmp                                => bcmp(a: Address, b: Address, len: UInt),
        memcpy                              => memcpy(from: Address, to: Address, len: UInt),
        memset                              => memset(start: Address, fill: Char, len: UInt),
        memmove                             => memmove(from: Address, to: Address, len: UInt),
        memcmp                              => memcmp(a: Address, b: Address, len: UInt),
        memchr                              => memchr(start: Address, c: Char, len: UInt),
        rand                                => rand(),
        srand                               => srand(seed: UInt),
        qsort                               => qsort(),
        strtod                              => strtod(start: Address, end: Address),
        malloc                              => malloc(size: UInt),
        free                                => free(obj: Address),
        lsearch                             => lsearch(),
        bsearch                             => bsearch(),
        calloc                              => calloc(width: UInt, len: UInt),
        realloc                             => realloc(obj: Address, size: UInt),
        init_heap                           => InitHeap(at: Address, size: UInt),
        system_error_exit                   => SystemErrorExit(code: UInt),
        std_in_getchar                      => std_in_getchar(),
        std_out_putchar                     => std_out_putchar(c: Char),
        std_in_gets                         => std_in_gets(),
        std_out_puts                        => std_out_puts(s: String),
        printf                              => printf(fmt: String),
        system_error_unresolved_exception   => SystemErrorUnresolvedException(),
        load_exe_header                     => LoadExeHeader(path: String, header: Address),
        load_exe_file                       => LoadExeFile(path: String, header: Address),
        do_execute                          => DoExecute(header: Address, arg1: UInt, arg2: UInt),
        flush_cache                         => FlushCache(),
        init_a0_b0_c0_vectors               => init_a0_b0_c0_vectors(),
        gpu_dw                              => GPU_dw(
            x_to: UInt,
            y_to: UInt,
            x_size: UInt,
            y_size: UInt,
            from: Address,
        ),
        gpu_send_dma                        => gpu_send_dma(),
        send_gp1_command                    => SendGP1Command(cmd: UInt),
        gpu_cw                              => GPU_cw(cmd: UInt),
        gpu_cwp                             => GPU_cwp(from: Address, len: UInt),
        send_gpu_linked_list                => send_gpu_linked_list(from: Address),
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

        alloc_kernel_memory                 => alloc_kernel_memory(size: UInt),
        free_kernel_memory                  => free_kernel_memory(obj: Address),
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
        chdir                               => chdir(path: String),
        format_device                       => FormatDevice(),
        firstfile                           => firstfile(pat: String, dirent: Address),
        nextfile                            => nextfile(),
        file_rename                         => FileRename(),
        file_delete                         => FileDelete(path: String),
        file_undelete                       => FileUndelete(),
        add_device                          => AddDevice(info: Address),
        remove_device                       => RemoveDevice(name: String),
        print_installed_devices             => PrintInstalledDevices(),
        init_card                           => InitCard(),
        start_card                          => StartCard(),
        stop_card                           => StopCard(),
        _card_info_subfunc                  => _card_info_subfunc(port: UInt),
        write_card_sector                   => write_card_sector(
            port: UInt,
            sector: UInt,
            from: Address,
        ),
        read_card_sector                    => read_card_sector(
            port: UInt,
            sector: UInt,
            to: Address,
        ),
        allow_new_card                      => allow_new_card(),
        krom2_raw_add                       => Krom2RawAdd(shiftjis_code: UInt),
        krom2_offset                        => Krom2Offset(shiftjis_code: UInt),
        get_last_error                      => GetLastError(),
        get_last_file_error                 => GetLastFileError(fd: UInt),
        get_c0_table                        => GetC0Table(),
        get_b0_table                        => GetB0Table(),
        get_bu_callback_port                => get_bu_callback_port(),
        testdevice                          => testdevice(name: String),
        change_clear_pad                    => ChangeClearPad(int: UInt),
        get_card_status                     => get_card_status(slot: UInt),
        wait_card_status                    => wait_card_status(slot: UInt),

        enqueue_timer_and_vblank_irqs       => EnqueueTimerAndVblankIrqs(prio: UInt),
        enqueue_syscall_handler             => EnqueueSyscallHandler(prio: UInt),
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
        install_devices                     => InstallDevices(tty_flag: UInt),
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
        pc_create                           => PCCreate(_: UInt, path: String, attr: UInt),
        pc_open                             => PCOpen(_: UInt, path: String, mode: UInt),
        pc_close                            => PCClose(_: UInt, fd: UInt),
        pc_read                             => PCRead(_: UInt, fd: UInt, len: UInt, to: Address),
        pc_write                            => PCWrite(_: UInt, fd: UInt, len: UInt, from: Address),
        pc_l_seek                           => PClSeek(_: UInt, fd: UInt, offset: UInt, mode: UInt),

        no_function                         => NoFunction(),
        enter_critical_section              => EnterCriticalSection(),
        exit_critical_section               => ExitCriticalSection(),
        change_thread_sub_function          => ChangeThreadSubFunction(_: UInt, new: Address),
    );
}
