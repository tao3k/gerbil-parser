//! Exact public C representations and the only raw payload access.
use crate::NativeError;

/// Layout of `gerbil_parser_result_v2` from `language-v2.h`.
/// Fields are public for foreign declarations; safe consumers use `NativePayload`.
#[repr(C)]
#[derive(Debug)]
pub struct RawResult {
    pub status: i32,
    pub payload: *mut u8,
    pub length: usize,
}

/// Functions from one loaded ABI v2 runtime and an initialized language pack.
/// The generated pack contributes only `create`; every other function is generic.
#[derive(Clone, Copy)]
pub struct LanguageApi {
    pub version: unsafe extern "C" fn() -> u32,
    pub is_owner_thread: unsafe extern "C" fn() -> i32,
    pub create: unsafe extern "C" fn() -> u64,
    pub descriptor: unsafe extern "C" fn(u64, *mut RawResult) -> i32,
    pub parse: unsafe extern "C" fn(u64, *const u8, usize, *mut RawResult) -> i32,
    pub release: unsafe extern "C" fn(u64) -> i32,
    pub result_init: unsafe extern "C" fn(*mut RawResult),
    pub result_release: unsafe extern "C" fn(*mut RawResult),
}

impl RawResult {
    pub(crate) fn empty() -> Self {
        Self {
            status: 0,
            payload: std::ptr::null_mut(),
            length: 0,
        }
    }

    // The caller owns this result and keeps its language/session alive. The API
    // contract supplies a live C allocation, never an arbitrary foreign pointer.
    pub(crate) fn bytes(&self) -> Result<&[u8], NativeError> {
        if self.length == 0 {
            return Ok(&[]);
        }
        if self.payload.is_null() || self.length > isize::MAX as usize {
            return Err(NativeError::InvalidBuffer);
        }
        // SAFETY: attach requires ABI-conforming allocations. The result owns
        // this allocation; &self prevents its release while the slice is live.
        Ok(unsafe { std::slice::from_raw_parts(self.payload, self.length) })
    }
}

unsafe extern "C" {
    #[cfg(feature = "standalone")]
    fn gerbil_parser_runtime_init() -> i32;
    #[cfg(feature = "standalone")]
    fn gerbil_parser_runtime_shutdown() -> i32;
    fn gerbil_parser_language_abi_version() -> u32;
    fn gerbil_parser_language_is_owner_thread() -> i32;
    fn gerbil_parser_language_descriptor(handle: u64, result: *mut RawResult) -> i32;
    fn gerbil_parser_language_parse(
        handle: u64,
        source: *const u8,
        length: usize,
        result: *mut RawResult,
    ) -> i32;
    fn gerbil_parser_language_release(handle: u64) -> i32;
    fn gerbil_parser_result_v2_init(result: *mut RawResult);
    fn gerbil_parser_result_v2_release(result: *mut RawResult);
}

impl LanguageApi {
    /// Bind the generic linked C ABI to one generated pack's factory.
    /// This only constructs the function table; `NativeSession::attach` admits it.
    #[must_use]
    pub const fn linked(create: unsafe extern "C" fn() -> u64) -> Self {
        Self {
            version: gerbil_parser_language_abi_version,
            is_owner_thread: gerbil_parser_language_is_owner_thread,
            create,
            descriptor: gerbil_parser_language_descriptor,
            parse: gerbil_parser_language_parse,
            release: gerbil_parser_language_release,
            result_init: gerbil_parser_result_v2_init,
            result_release: gerbil_parser_result_v2_release,
        }
    }
}

/// Functions from one standalone runtime bundle and its selected language pack.
#[cfg(feature = "standalone")]
#[derive(Clone, Copy)]
pub struct RuntimeApi {
    pub init: unsafe extern "C" fn() -> i32,
    pub shutdown: unsafe extern "C" fn() -> i32,
    pub language: LanguageApi,
}
#[cfg(feature = "standalone")]
impl RuntimeApi {
    /// Bind a compiled standalone bundle to its selected generated factory.
    #[must_use]
    pub const fn linked(create: unsafe extern "C" fn() -> u64) -> Self {
        Self {
            init: gerbil_parser_runtime_init,
            shutdown: gerbil_parser_runtime_shutdown,
            language: LanguageApi::linked(create),
        }
    }
}
