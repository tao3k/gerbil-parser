//! An immutable payload owner admits and indexes its result once.
use crate::view::Navigation;
use crate::{NativeArtifactError, NativeArtifactView, NativeCatalog};
use std::cell::OnceCell;

/// Length-bearing bytes owned for the lifetime of a parse result.
///
/// Implementations must return the same immutable bytes throughout their lifetime.
/// Ownership may refer to a foreign allocation; reads must preserve its bounds
/// and release must wait until all borrows end. This contract has no parser API.
pub trait NativeArtifactBytes {
    /// Borrow the original immutable allocation.
    /// # Errors
    /// Rejects an invalid allocation or unavailable bytes.
    fn bytes(&self) -> Result<&[u8], NativeArtifactError>;
}

struct Admitted<'catalog> {
    catalog: &'catalog NativeCatalog,
    navigation: Navigation,
    payload_identity: (usize, usize),
}

/// Own a result while borrowing its exact source and handle catalog.
///
/// The cached state contains only external catalog references and owned indexes;
/// it contains no references into `owner`, so moving this storage is safe.
pub struct NativeArtifactStorage<'catalog, 'source, P> {
    owner: P,
    source: &'source str,
    admitted: OnceCell<Admitted<'catalog>>,
}
impl<'catalog, 'source, P: NativeArtifactBytes> NativeArtifactStorage<'catalog, 'source, P> {
    /// Keep the payload alive; admission occurs on the first view request.
    #[must_use]
    pub const fn new(owner: P, source: &'source str) -> Self {
        Self {
            owner,
            source,
            admitted: OnceCell::new(),
        }
    }

    /// Borrow the immutable payload owner without exposing mutable access.
    #[must_use]
    pub const fn owner(&self) -> &P {
        &self.owner
    }

    /// Admit once and reuse the same navigation indexes for this handle catalog.
    /// # Errors
    /// Rejects invalid bytes/source/catalog or a different catalog after admission.
    pub fn view(
        &self,
        catalog: &'catalog NativeCatalog,
    ) -> Result<NativeArtifactView<'_>, NativeArtifactError> {
        let payload = self.owner.bytes()?;
        if self.admitted.get().is_none() {
            let navigation = Navigation::decode(payload, self.source, catalog)?;
            self.admitted
                .set(Admitted {
                    catalog,
                    navigation,
                    payload_identity: (payload.as_ptr() as usize, payload.len()),
                })
                .map_err(|_| NativeArtifactError {
                    reason: "native-view-cache",
                    event: None,
                })?;
        }
        let admitted = self.admitted.get().ok_or(NativeArtifactError {
            reason: "native-view-cache",
            event: None,
        })?;
        if admitted.payload_identity != (payload.as_ptr() as usize, payload.len()) {
            return Err(NativeArtifactError {
                reason: "native-view-payload",
                event: None,
            });
        }
        if !std::ptr::eq(catalog, admitted.catalog) {
            return Err(NativeArtifactError {
                reason: "native-view-catalog",
                event: None,
            });
        }
        Ok(NativeArtifactView::from_navigation(
            payload,
            self.source,
            admitted.catalog,
            &admitted.navigation,
        ))
    }
}
