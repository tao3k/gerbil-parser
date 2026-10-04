//! Owner-thread admission and the host's VM lifetime boundary.
use crate::{LanguageApi, NativeLanguage};
use std::{marker::PhantomData, rc::Rc};

/// Transport/admission failure. Syntax rejection is represented in the artifact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeError {
    AbiVersion,
    OwnerThread,
    Create,
    Transport(i32),
    InvalidBuffer,
    SourceTooLarge,
}

/// Borrowed host runtime admission, deliberately neither `Send` nor `Sync`.
/// Handles borrow this session, and results borrow their handle.
///
/// ```compile_fail
/// fn send(session: gerbil_parser_native::NativeSession) {
///     std::thread::spawn(move || drop(session));
/// }
/// ```
pub struct NativeSession {
    pub(crate) api: LanguageApi,
    _owner: PhantomData<Rc<()>>,
}

impl NativeSession {
    /// Admit an initialized runtime on its original OS thread.
    /// # Safety
    /// Every function must implement `language-v2.h` and belong to the same live
    /// initialized runtime. `create` must belong to an initialized pack in that
    /// runtime. Functions must not unwind across C. The embedding host must keep
    /// the VM, modules and function pointers alive until this session and all its
    /// borrowers are dropped, and must not release their handles/results itself.
    /// # Errors
    /// Returns an incompatible ABI or owner-thread error before creating handles.
    pub unsafe fn attach(api: LanguageApi) -> Result<Self, NativeError> {
        // SAFETY: the caller guarantees initialized, live ABI function pointers.
        if unsafe { (api.version)() } != 2 {
            return Err(NativeError::AbiVersion);
        }
        // SAFETY: this C guard does not enter Scheme on a non-owner OS thread.
        if unsafe { (api.is_owner_thread)() } == 0 {
            return Err(NativeError::OwnerThread);
        }
        Ok(Self {
            api,
            _owner: PhantomData,
        })
    }

    /// Create a distinct owned handle using the attached pack's factory.
    /// # Errors
    /// Returns `Create` if the pack cannot register a language.
    pub fn language(&self) -> Result<NativeLanguage<'_>, NativeError> {
        // SAFETY: attach admitted this thread; the session cannot cross threads.
        let handle = unsafe { (self.api.create)() };
        if handle == 0 {
            return Err(NativeError::Create);
        }
        Ok(NativeLanguage {
            session: self,
            handle,
        })
    }
}

impl std::fmt::Display for NativeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AbiVersion => f.write_str("native language ABI version must be 2"),
            Self::OwnerThread => f.write_str("native runtime requires its owner OS thread"),
            Self::Create => f.write_str("native language registration failed"),
            Self::Transport(status) => write!(f, "native language transport failed: {status}"),
            Self::InvalidBuffer => f.write_str("native result has an invalid allocation"),
            Self::SourceTooLarge => f.write_str("native source exceeds 64 MiB"),
        }
    }
}
impl std::error::Error for NativeError {}
