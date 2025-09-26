use std::{ffi::CStr, mem::transmute, sync::LazyLock};

#[repr(transparent)]
#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct Version(u32);

impl Version {
    pub const fn new(version: u32) -> Self {
        Self(VERSION_MAGIC_FLAG | version)
    }

    pub const fn get(self) -> u32 {
        self.0 & !VERSION_MAGIC_FLAG
    }
}

pub const VERSION_MAGIC_FLAG: u32 = 0xF0CF0000;
pub const CURRENT_VERSION: Version = Version::new(1);

/// data is a pointer to either raw file contents or the file name, depending on
/// the bool
pub type RawUpdateCallback =
    unsafe extern "C-unwind" fn(data: *const u8, byte_size: u32, is_filename: bool);

/// needs to be exposed as "GW2Load_GetAddonAPIVersion"
pub type RawGetAddonApiVersion = unsafe extern "C-unwind" fn() -> Version;
/// needs to be exposed as "GW2Load_OnLoad"
/// return true to indicate success
pub type RawOnLoad = unsafe extern "C-unwind" fn(
    gw2load_handle: *const (),
    swap_chain: *const (),
    d3d11_device: *const (),
    d3d11_device_context: *const (),
) -> bool;
/// needs to be exposed as "GW2Load_OnLoadLauncher"
/// return true to indicate success
pub type RawOnLoadLauncher = unsafe extern "C-unwind" fn(gw2load_handle: *const ()) -> bool;
/// needs to be exposed as "GW2Load_OnClose"
pub type RawOnClose = unsafe extern "C-unwind" fn();
/// needs to be exposed as "GW2Load_OnAddonAPIVersionOutdated"
pub type RawOnAddonVersionOutdated =
    unsafe extern "C-unwind" fn(loader_version: Version) -> Version;
/// needs to be exposed as "GW2Load_UpdateCheck"
pub type RawUpdateCheck =
    unsafe extern "C-unwind" fn(gw2load_handle: *const (), update_callback: RawUpdateCallback);

#[unsafe(no_mangle)]
unsafe extern "C-unwind" fn GW2Load_GetAddonAPIVersion() -> Version {
    CURRENT_VERSION
}

#[macro_export]
macro_rules! on_load {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        unsafe extern "C-unwind" fn GW2Load_OnLoad(
            gw2load_handle: *const (),
            swap_chain: *const (),
            d3d11_device: *const (),
            d3d11_device_context: *const (),
        ) -> bool {
            unsafe {
                $crate::init(gw2load_handle);
                ($func as $crate::RawOnLoad)(
                    gw2load_handle,
                    swap_chain,
                    d3d11_device,
                    d3d11_device_context,
                )
            }
        }
    };
}

#[macro_export]
macro_rules! on_load_launcher {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        unsafe extern "C-unwind" fn GW2Load_OnLoadLauncher(gw2load_handle: *const ()) -> bool {
            unsafe {
                $crate::init(gw2load_handle);
                ($func as $crate::RawOnLoadLauncher)(gw2load_handle)
            }
        }
    };
}

#[macro_export]
macro_rules! on_close {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        unsafe extern "C-unwind" fn GW2Load_OnClose() {
            unsafe { ($func as $crate::RawOnClose)() }
        }
    };
}

#[macro_export]
macro_rules! on_addon_version_outdated {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        unsafe extern "C-unwind" fn GW2Load_OnAddonAPIVersionOutdated(
            loader_version: $crate::Version,
        ) -> $crate::Version {
            unsafe { ($func as $crate::RawOnAddonVersionOutdated)(loader_version) }
        }
    };
}

#[macro_export]
macro_rules! update_check {
    ($func:ident) => {
        #[unsafe(no_mangle)]
        unsafe extern "C-unwind" fn GW2Load_UpdateCheck(
            gw2load_handle: *const (),
            update_callback: $crate::RawUpdateCallback,
        ) {
            unsafe {
                $crate::init(gw2load_handle);
                ($func as $crate::RawUpdateCheck)(gw2load_handle, update_callback)
            }
        }
    };
}

#[repr(i32)]
pub enum LogLevel {
    Trace    = 0,
    Debug    = 1,
    Info     = 2,
    Warn     = 3,
    Err      = 4,
    Critical = 5,
}

#[repr(u32)]
pub enum HookedFunction {
    Present       = 1,
    ResizeBuffers = 2,
}

#[repr(u32)]
pub enum CallbackPoint {
    BeforeCall = 1,
    AfterCall  = 2,
}

pub type RawGenericCallback = unsafe extern "C-unwind" fn();
pub type RawPresentCallback = unsafe extern "C-unwind" fn(swap_chain: *const ());
pub type RawResizeBuffersCallback =
    unsafe extern "C-unwind" fn(swap_chain: *const (), width: u32, height: u32, dxgi_format: u32);

pub const PROC_NAME_REGISTER_CALLBACK: &CStr = c"GW2Load_RegisterCallback";
pub type RawRegisterCallbackFunc = unsafe extern "C-unwind" fn(
    func: HookedFunction,
    priority: i32,
    callback_point: CallbackPoint,
    callback: RawGenericCallback,
);
pub const PROC_NAME_DEREGISTER_CALLBACK: &CStr = c"GW2Load_DeregisterCallback";
pub type RawDeregisterCallbackFunc = unsafe extern "C-unwind" fn(
    func: HookedFunction,
    callback_point: CallbackPoint,
    callback: RawGenericCallback,
);
pub const PROC_NAME_LOG: &CStr = c"GW2Load_Log";
pub type RawLogFunc =
    unsafe extern "C-unwind" fn(level: LogLevel, message: *const u8, byte_size: usize);

static mut HANDLE: usize = 0;

pub fn init(handle: *const ()) {
    unsafe {
        HANDLE = handle as usize;
    }
    #[cfg(feature = "log")]
    log::set_logger(&logger::LOGGER).ok();
}

#[cfg(feature = "log")]
mod logger {
    pub(super) static LOGGER: Logger = Logger;

    pub(super) struct Logger;
    impl log::Log for Logger {
        fn enabled(&self, _metadata: &log::Metadata) -> bool {
            true
        }

        fn log(&self, record: &log::Record) {
            super::log(record.level().into(), &format!("{}", record.args()));
        }

        fn flush(&self) {}
    }

    impl From<log::Level> for super::LogLevel {
        fn from(value: log::Level) -> Self {
            match value {
                log::Level::Error => Self::Err,
                log::Level::Warn => Self::Warn,
                log::Level::Info => Self::Info,
                log::Level::Debug => Self::Debug,
                log::Level::Trace => Self::Trace,
            }
        }
    }
}

trait Callback: Sized {
    const KIND: HookedFunction;
}
impl Callback for RawPresentCallback {
    const KIND: HookedFunction = HookedFunction::Present;
}
impl Callback for RawResizeBuffersCallback {
    const KIND: HookedFunction = HookedFunction::ResizeBuffers;
}

static mut REGISTER_CB: LazyLock<usize> = LazyLock::new(|| unsafe {
    let address = GetProcAddress(HANDLE, PROC_NAME_REGISTER_CALLBACK.as_ptr());
    if address == 0 {
        panic!(
            "couldn't find {}",
            PROC_NAME_REGISTER_CALLBACK.to_str().unwrap()
        );
    }
    address
});

/// panics if [`init`] wasn't called with a valid handle
#[allow(private_bounds)]
pub fn register_callback<T: Callback>(callback: T, callback_point: CallbackPoint, priority: i32) {
    unsafe {
        transmute::<usize, RawRegisterCallbackFunc>(*REGISTER_CB)(
            T::KIND,
            priority,
            callback_point,
            *(&raw const callback as *const _),
        );
    }
}

static mut DEREGISTER_CB: LazyLock<usize> = LazyLock::new(|| unsafe {
    let address = GetProcAddress(HANDLE, PROC_NAME_DEREGISTER_CALLBACK.as_ptr());
    if address == 0 {
        panic!(
            "couldn't find {}",
            PROC_NAME_DEREGISTER_CALLBACK.to_str().unwrap()
        );
    }
    address
});

/// panics if [`init`] wasn't called with a valid handle
#[allow(private_bounds)]
pub fn deregister_callback<T: Callback>(callback: T, callback_point: CallbackPoint) {
    unsafe {
        transmute::<usize, RawDeregisterCallbackFunc>(*DEREGISTER_CB)(
            T::KIND,
            callback_point,
            *(&raw const callback as *const _),
        );
    }
}

static mut LOG: LazyLock<usize> = LazyLock::new(|| unsafe {
    let address = GetProcAddress(HANDLE, PROC_NAME_LOG.as_ptr());
    if address == 0 {
        panic!("couldn't find {}", PROC_NAME_LOG.to_str().unwrap());
    }
    address
});

/// panics if [`init`] wasn't called with a valid handle
pub fn log(level: LogLevel, msg: &str) {
    unsafe {
        transmute::<usize, RawLogFunc>(*LOG)(level, msg.as_ptr(), msg.len());
    }
}

unsafe extern "system" {
    fn GetProcAddress(handle: usize, name: *const i8) -> usize;
}
