# Package-private cache --------------------------------------------------------

.pkg_cache <- new.env(parent = emptyenv())

# Helper function
internal_getProviderCache <- function(provider) {
  if (is.null(.pkg_cache[[provider]])) {
    .pkg_cache[[provider]] <- new.env(parent = emptyenv())
  }
  .pkg_cache[[provider]]
}
