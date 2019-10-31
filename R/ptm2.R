#' Internal constructor to create `peptr_ptm` type
#'
#' Asserts that `position_1` and `position_2` integer vectors, and that `name` is a string.
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_ptm2 <- function(position_1 = integer(),
                    position_2 = integer(),
                    name = "unknown") {

  vec_assert(position_1, ptype = integer())
  vec_assert(position_2, ptype = integer())
  vec_assert(name, ptype = character(), size = 1)

  new_rcrd(list(position_1 = position_1, position_2 = position_2),
           name = name,
           class = "peptr_ptm2")
}


#' A class for post-translational modifications that occurs between 2 residues
#'
#' @param position_1 An integer vector of the modified residues start positions
#' @param position_2 An integer vector of the modified residues end positions
#' @param name A string to identify the PTM (e.g. disulphide bond, Default: "unknown")
#' @export
#' @importFrom vctrs vec_cast
#' @examples
#' peptr_ptm2(c(10L, 22L, 58L, 125L), c(15L, 137L, 58L, 60L), "disulphide bond")
peptr_ptm2 <- function(position_1 = integer(),
                      position_2 = integer(),
                      name = "unknown") {

  check_position(position_1)
  check_position(position_2)

  position_1 <- vec_cast(position_1, to = integer())
  position_2 <- vec_cast(position_2, to = integer())
  name <- vec_cast(name, to = character())

  new_ptm2(position_1 = position_1, position_2 = position_2, name = name)
}

setOldClass(c("peptr_ptm2", "vctrs_vctr"))

peptr_is_ptm2 <- function(x) {
  inherits(x, "peptr_ptm2")
}

vec_ptype_full.peptr_ptm2 <- function(x, ...) {
  paste0("ptm2<", peptr_ptm_name(x), ">")
}

vec_ptype_abbr.peptr_ptm2 <- function(x, ...) {
  "ptm2"
}

#' @export
format.peptr_ptm2 <- function(x, ...) {
  position_1 <- vctrs::field(x, "position_1")
  position_2 <- vctrs::field(x, "position_2")

  out <- paste0(position_1, "--", position_2)
  out[is.na(position_1) | is.na(position_2)] <- NA
  out
}
