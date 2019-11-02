#' Internal constructor to create `peptr_ptm2` type
#'
#' Asserts that `position_1` and `position_2` integer vectors,
#' and that `name` is a string.
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_ptm2 <- function(position_1 = peptr_position(),
                     position_2 = peptr_position(),
                     name = "unknown") {
  vec_assert(position_1, ptype = peptr_position())
  vec_assert(position_2, ptype = peptr_position())
  vec_assert(name, ptype = character(), size = 1)

  new_rcrd(
    fields = list(position_1 = position_1, position_2 = position_2),
    name = name,
    class = "peptr_ptm2"
  )
}


#' A class for post-translational modifications that occurs between 2 residues
#'
#' @param position_1 An integer vector of the modified residues start positions
#' @param position_2 An integer vector of the modified residues end positions
#' @param name A string to identify the PTM (e.g. disulphide bond, Default: "unknown")
#' @export
#' @importFrom vctrs vec_cast vec_cast_common
#' @examples
#' peptr_ptm2(c(10L, 22L, 58L, 125L), c(15L, 137L, 62L, 60L), "disulphide bond")
peptr_ptm2 <- function(position_1 = peptr_position(),
                       position_2 = peptr_position(),
                       name = "unknown") {
  ptm2_check(position_1, position_2)

  c(position_1, position_2) %<-% vec_cast_common(position_1, position_2, .to = peptr_position())
  name <- vec_cast(name, to = character())

  new_ptm2(position_1 = position_1, position_2 = position_2, name = name)
}

setOldClass(c("peptr_ptm2", "vctrs_vctr"))

peptr_ptm2_name <- function(x) {
  attr(x, "name")
}

peptr_is_ptm2 <- function(x) {
  inherits(x, "peptr_ptm2")
}

vec_ptype_full.peptr_ptm2 <- function(x, ...) {
  paste0("ptm2<", peptr_ptm2_name(x), ">")
}

vec_ptype_abbr.peptr_ptm2 <- function(x, ...) {
  "ptm2"
}

#' @export
#' @importFrom vctrs field
format.peptr_ptm2 <- function(x, ...) {
  position_1 <- format(field(x, "position_1"))
  position_2 <- format(field(x, "position_2"))

  link <- intToUtf8(8596)
  out <- paste0(position_1, "--", position_2)
  out[is.na(position_1) | is.na(position_2)] <- NA
  out
}

obj_print_data.peptr_ptm2 <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  cat(format(x), sep = " ")
  invisible(x)
}
