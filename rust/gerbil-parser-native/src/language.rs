//! Handle/result ownership with borrowed, validated GPA1 views.
use crate::{NativeError, NativeSession, RawResult};
use gerbil_parser_rowan::{
    ContextualParserSpec, LanguageSpec,
    native_artifact::{NativeArtifactError, NativeArtifactView},
};

/// A unique registered language, released once when its last borrow ends.
pub struct NativeLanguage<'runtime> {
    pub(crate) session: &'runtime NativeSession,
    pub(crate) handle: u64,
}

/// A C-owned result. Its bytes cannot outlive the result or replace its allocation.
pub struct NativePayload<'language, 'runtime> {
    language: &'language NativeLanguage<'runtime>,
    raw: RawResult,
}

/// A result bound to the exact source used by the native parser.
///
/// A live result prevents releasing its handle:
/// ```compile_fail
/// fn release(language: gerbil_parser_native::NativeLanguage<'_>) {
///     let parsed = language.parse("a=1\\n").unwrap();
///     drop(language);
///     parsed.bytes().unwrap();
/// }
/// ```
/// A live view prevents freeing its C payload:
/// ```compile_fail
/// fn free(language: &gerbil_parser_native::NativeLanguage<'_>,
///         spec: &'static gerbil_parser_rowan::ContextualParserSpec) {
///     let parsed = language.parse("a=1\\n").unwrap();
///     let view = parsed.contextual_view(spec, 3).unwrap();
///     drop(parsed);
///     view.accepted();
/// }
/// ```
pub struct NativeParsed<'language, 'runtime, 'source> {
    payload: NativePayload<'language, 'runtime>,
    source: &'source str,
}

impl<'runtime> NativeLanguage<'runtime> {
    /// Read the length-bearing descriptor JSON bytes without copying them.
    /// # Errors
    /// Returns a native transport or invalid-buffer error.
    pub fn descriptor(&self) -> Result<NativePayload<'_, 'runtime>, NativeError> {
        let mut result = self.result();
        // SAFETY: result is initialized, exclusive, and belongs to this session.
        let status = unsafe { (self.session.api.descriptor)(self.handle, &raw mut result.raw) };
        result.admit(status)?;
        Ok(result)
    }

    /// Parse UTF-8 source, including embedded NUL, with no Rust payload copy.
    /// # Errors
    /// Rejects source beyond the C ABI limit or native transport failure.
    pub fn parse<'language, 'source>(
        &'language self,
        source: &'source str,
    ) -> Result<NativeParsed<'language, 'runtime, 'source>, NativeError> {
        if source.len() > 64 * 1024 * 1024 {
            return Err(NativeError::SourceTooLarge);
        }
        let mut payload = self.result();
        // SAFETY: source is readable for len bytes, the C ABI copies it during
        // the call, and payload is an initialized exclusive result.
        let status = unsafe {
            (self.session.api.parse)(
                self.handle,
                source.as_ptr(),
                source.len(),
                &raw mut payload.raw,
            )
        };
        payload.admit(status)?;
        Ok(NativeParsed { payload, source })
    }

    fn result(&self) -> NativePayload<'_, 'runtime> {
        let mut raw = RawResult::empty();
        // SAFETY: a fresh result is initialized exactly once before use.
        unsafe { (self.session.api.result_init)(&raw mut raw) };
        NativePayload {
            language: self,
            raw,
        }
    }
}

impl Drop for NativeLanguage<'_> {
    fn drop(&mut self) {
        // SAFETY: all result borrows ended, the runtime is alive, and the owned
        // handle has never been exposed for an external release.
        unsafe { (self.session.api.release)(self.handle) };
    }
}

impl NativePayload<'_, '_> {
    fn admit(&self, status: i32) -> Result<(), NativeError> {
        if status != 0 {
            return Err(NativeError::Transport(status));
        }
        if self.raw.status != 0 {
            return Err(NativeError::Transport(self.raw.status));
        }
        self.bytes().map(|_| ())
    }

    /// Borrow exactly the C allocation. Zero length never dereferences NULL.
    /// # Errors
    /// Rejects a NULL nonempty or unrepresentably large allocation.
    pub fn bytes(&self) -> Result<&[u8], NativeError> {
        self.raw.bytes()
    }
}

impl Drop for NativePayload<'_, '_> {
    fn drop(&mut self) {
        // SAFETY: this result is owned, initialized, and no byte borrow survives
        // its drop. Its language and runtime are still alive on the owner thread.
        unsafe { (self.language.session.api.result_release)(&raw mut self.raw) };
    }
}

impl NativeParsed<'_, '_, '_> {
    /// Borrow GPA1 bytes for external consumers; views should use `view`.
    /// # Errors
    /// Rejects an invalid C allocation.
    pub fn bytes(&self) -> Result<&[u8], NativeError> {
        self.payload.bytes()
    }

    /// Bind a canonical artifact to its generated catalog and original source.
    /// # Errors
    /// Rejects incompatible product/source identities or malformed records.
    pub fn view(
        &self,
        language: &'static LanguageSpec,
        field_count: usize,
    ) -> Result<NativeArtifactView<'_>, NativeArtifactError> {
        NativeArtifactView::decode(
            self.payload.raw.bytes().map_err(|_| NativeArtifactError {
                reason: "native-buffer",
                event: None,
            })?,
            self.source,
            language,
            field_count,
        )
    }

    /// Bind a contextual artifact to its complete generated product digest.
    /// # Errors
    /// Rejects incompatible product/source identities or malformed records.
    pub fn contextual_view(
        &self,
        product: &'static ContextualParserSpec,
        field_count: usize,
    ) -> Result<NativeArtifactView<'_>, NativeArtifactError> {
        NativeArtifactView::decode_contextual(
            self.payload.raw.bytes().map_err(|_| NativeArtifactError {
                reason: "native-buffer",
                event: None,
            })?,
            self.source,
            product,
            field_count,
        )
    }
}
