//! Unique ownership of a standalone VM, extending the session borrow chain.
use crate::{NativeError, NativeLanguage, NativeSession, RuntimeApi};

/// Owner of the bundle's process-wide, one-shot SDK lifecycle.
/// Language/result borrows prevent closing or dropping their runtime.
/// ```compile_fail
/// fn close(runtime: &mut gerbil_parser_native::NativeRuntime) {
///     let language = runtime.language().unwrap();
///     let parsed = language.parse("a=1\n").unwrap();
///     runtime.close().unwrap();
///     parsed.bytes().unwrap();
/// }
/// ```
pub struct NativeRuntime {
    session: NativeSession,
    shutdown: unsafe extern "C" fn() -> i32,
    closed: bool,
}
impl NativeRuntime {
    /// Initialize and own a standalone bundle on this OS thread.
    /// # Safety
    /// `api` must come from the bundle built by the native library builder (or
    /// implement the same `runtime.h` and `language.h` contracts). No other
    /// Gambit VM may be initialized in this process. The bundle and its functions
    /// must remain loaded until this owner is dropped. The caller must not close
    /// the VM or release resources borrowed from this owner independently.
    /// Independently created C resources must be released before dropping it.
    /// # Errors
    /// Returns a lifecycle failure, incompatible ABI, or owner admission error.
    pub unsafe fn start(api: RuntimeApi) -> Result<Self, NativeError> {
        // SAFETY: the caller supplies the unique live bundle and SDK lifecycle.
        let status = unsafe { (api.init)() };
        if status != 0 {
            return Err(NativeError::Runtime(status));
        }
        // SAFETY: successful setup initialized the same linked language API.
        let session = match unsafe { NativeSession::attach(api.language) } {
            Ok(session) => session,
            Err(error) => {
                // SAFETY: initialization succeeded, but no handles were created.
                unsafe { (api.shutdown)() };
                return Err(error);
            }
        };
        Ok(Self {
            session,
            shutdown: api.shutdown,
            closed: false,
        })
    }

    /// Register a language in this live runtime.
    /// # Errors
    /// Rejects a closed runtime or language registration failure.
    pub fn language(&self) -> Result<NativeLanguage<'_>, NativeError> {
        if self.closed {
            return Err(NativeError::RuntimeClosed);
        }
        self.session.language()
    }

    /// Disable VM admission and perform normal SDK cleanup once.
    /// Mutable admission excludes all live language/result/view borrows.
    /// # Errors
    /// Returns the bundle's status if independently retained C resources prevent
    /// shutdown. The runtime remains live, and this operation can be retried.
    pub fn close(&mut self) -> Result<(), NativeError> {
        if self.closed {
            return Ok(());
        }
        // SAFETY: borrowers have ended, this owner cannot cross OS threads, and
        // the bundle enforces the remaining C resource barrier before cleanup.
        let status = unsafe { (self.shutdown)() };
        if status != 0 {
            return Err(NativeError::Runtime(status));
        }
        self.closed = true;
        Ok(())
    }
}
impl Drop for NativeRuntime {
    fn drop(&mut self) {
        // C shutdown retains the VM on error rather than invalidating live data.
        let _ = self.close();
    }
}
