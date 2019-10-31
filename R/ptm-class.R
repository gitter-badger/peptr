#' Internal constructor to create `peptr_ptm` type
#'
#' Asserts that `position` is a vector of integers, and that `name` is a string.
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_ptm <- function(position = integer(),
                    name = "unknown") {

  vec_assert(position, ptype = integer())
  vec_assert(name, ptype = character(), size = 1)

  new_rcrd(list(position = position),
           name = name,
           class = "peptr_ptm")
}


#' A class for post-translational modifications that occurs on one residue
#'
#' @param position An integer vector of the modified residues positions
#' @param name A string to identify the PTM (e.g. Phosphorylation, Default: "unknown")
#' @export
#' @importFrom vctrs vec_cast
#' @examples
#' peptr_ptm(c(10L, 22L, 58L, 125L), "phosphorylation")
#' peptr_ptm(c(58L, 132L, 24L), "O-glycosylation")
peptr_ptm <- function(position = integer(),
                      name = "unknown") {

  ptm_check(position, name)

  position <- vec_cast(position, to = integer())
  name <- vec_cast(name, to = character())

  new_ptm(position = position, name = name)
}

setOldClass(c("peptr_ptm", "vctrs_vctr"))

peptr_ptm_name <- function(x) {
  attr(x, "name")
}

peptr_is_ptm <- function(x) {
  inherits(x, "peptr_ptm")
}

vec_ptype_full.peptr_ptm <- function(x, ...) {
  paste0("ptm<", peptr_ptm_name(x), ">")
}

vec_ptype_abbr.peptr_ptm <- function(x, ...) {
  "ptm"
}

#' @export
format.peptr_ptm <- function(x, ...) {
  position <- vctrs::field(x, "position")

  out <- paste0(position)
  out[is.na(position)] <- NA
  out
}
