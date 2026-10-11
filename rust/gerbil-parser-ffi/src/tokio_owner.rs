//! Bounded async admission to a single native owner OS thread.
use crate::{NativeArtifactView, NativeCatalog, NativeError, NativeRuntime, RuntimeApi};
use gerbil_parser_artifact::NativeArtifactError;
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot};

/// Independent Rust snapshot; no Scheme handle or C allocation crosses threads.
pub struct OwnedParsed {
    source: String,
    payload: Vec<u8>,
    catalog: Arc<NativeCatalog>,
}
impl OwnedParsed {
    /// The exact source submitted to this request.
    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }
    /// Owned GPA1 bytes, still usable after the Scheme runtime closes.
    #[must_use]
    pub fn bytes(&self) -> &[u8] {
        &self.payload
    }
    /// Admit the snapshot against its source and native language catalog.
    /// # Errors
    /// Returns an artifact validation error for incompatible or malformed bytes.
    pub fn view(&self) -> Result<NativeArtifactView<'_>, NativeArtifactError> {
        NativeArtifactView::decode(&self.payload, &self.source, &self.catalog)
    }
}

/// Owner startup, transport, or shutdown failure.
#[derive(Debug)]
pub enum TokioParserError {
    Native(NativeError),
    Catalog(NativeArtifactError),
    OwnerStopped,
    InvalidCapacity,
    Thread(std::io::Error),
}
impl std::fmt::Display for TokioParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl std::error::Error for TokioParserError {}

enum Request {
    Parse(
        String,
        oneshot::Sender<Result<OwnedParsed, TokioParserError>>,
    ),
    Shutdown(oneshot::Sender<Result<(), TokioParserError>>),
}

/// Cloneable async client; all Scheme execution stays on one dedicated thread.
/// The queue bounds waiting requests, not the caller's source allocation.
#[derive(Clone)]
pub struct TokioParser {
    requests: mpsc::Sender<Request>,
}
impl TokioParser {
    /// Initialize the standalone VM and language on their permanent owner thread.
    /// # Safety
    /// `api` must satisfy `NativeRuntime::start` and remain loaded until shutdown.
    /// No other VM may be initialized in this process, including after shutdown.
    /// # Errors
    /// Rejects invalid capacity, thread creation, VM startup, or catalog admission.
    pub async unsafe fn start(api: RuntimeApi, capacity: usize) -> Result<Self, TokioParserError> {
        if capacity == 0 || capacity > tokio::sync::Semaphore::MAX_PERMITS {
            return Err(TokioParserError::InvalidCapacity);
        }
        let (requests, incoming) = mpsc::channel(capacity);
        let (ready, started) = oneshot::channel();
        std::thread::Builder::new()
            .name("gerbil-parser-owner".into())
            .spawn(move || {
                // SAFETY: the caller admits this unique VM and live function table.
                let mut runtime = match unsafe { NativeRuntime::start(api) } {
                    Ok(runtime) => runtime,
                    Err(error) => {
                        let _ = ready.send(Err(TokioParserError::Native(error)));
                        return;
                    }
                };
                let language = match runtime.language() {
                    Ok(language) => language,
                    Err(error) => {
                        let _ = ready.send(Err(TokioParserError::Native(error)));
                        return;
                    }
                };
                let catalog = match language.catalog() {
                    Ok(catalog) => Arc::new(catalog.clone()),
                    Err(error) => {
                        let _ = ready.send(Err(TokioParserError::Catalog(error)));
                        return;
                    }
                };
                if ready.send(Ok(())).is_err() {
                    return;
                }
                let shutdown = serve(incoming, |source| {
                    let payload = {
                        let parsed = language.parse(&source).map_err(TokioParserError::Native)?;
                        parsed.bytes().map_err(TokioParserError::Native)?.to_vec()
                    }; // Release the C allocation on the owner before publishing.
                    Ok(OwnedParsed {
                        source,
                        payload,
                        catalog: Arc::clone(&catalog),
                    })
                });
                drop(language);
                let result = runtime.close().map_err(TokioParserError::Native);
                if let Some(reply) = shutdown {
                    let _ = reply.send(result);
                }
            })
            .map_err(TokioParserError::Thread)?;
        started
            .await
            .map_err(|_| TokioParserError::OwnerStopped)??;
        Ok(Self { requests })
    }

    /// Parse through bounded admission; cancelling a queued request skips it.
    /// Once execution starts, its C resources are released even if cancelled.
    /// # Errors
    /// Reports native transport failure or a stopped owner.
    pub async fn parse(&self, source: String) -> Result<OwnedParsed, TokioParserError> {
        if source.len() > 64 * 1024 * 1024 {
            return Err(TokioParserError::Native(NativeError::SourceTooLarge));
        }
        let permit = self
            .requests
            .reserve()
            .await
            .map_err(|_| TokioParserError::OwnerStopped)?;
        let (reply, result) = oneshot::channel();
        permit.send(Request::Parse(source, reply));
        result.await.map_err(|_| TokioParserError::OwnerStopped)?
    }

    /// Close admission for every client, release the language, and clean up the VM.
    /// Requests ahead of this message finish; requests behind it are cancelled.
    /// Dropping every client also drains pending requests and closes the VM.
    /// # Errors
    /// Reports cleanup failure or an owner that has already stopped.
    pub async fn shutdown(&self) -> Result<(), TokioParserError> {
        let (reply, result) = oneshot::channel();
        self.requests
            .send(Request::Shutdown(reply))
            .await
            .map_err(|_| TokioParserError::OwnerStopped)?;
        result.await.map_err(|_| TokioParserError::OwnerStopped)?
    }
}

fn serve(
    mut incoming: mpsc::Receiver<Request>,
    mut parse: impl FnMut(String) -> Result<OwnedParsed, TokioParserError>,
) -> Option<oneshot::Sender<Result<(), TokioParserError>>> {
    while let Some(request) = incoming.blocking_recv() {
        match request {
            Request::Parse(source, reply) => {
                if !reply.is_closed() {
                    let _ = reply.send(parse(source));
                }
            }
            Request::Shutdown(reply) => {
                incoming.close();
                return Some(reply);
            }
        }
    }
    None
}

#[cfg(test)]
#[path = "../tests/unit/tokio_owner.rs"]
mod tests;
