# Constructor ----

#' @title Internal constructor to create `peptr_ptm` type
#'
#' @description Asserts that `position` is a vector of integers, and that `name` is a string.
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_ptm <- function(position = peptr_position(),
                    name = "unknown") {
  vec_assert(position, ptype = peptr_position())
  vec_assert(name, ptype = character(), size = 1)

  new_rcrd(
    fields = list(position = position),
    name = name,
    class = "peptr_ptm"
  )
}

# Helper ----

#' @title A class for post-translational modifications that occurs on one residue
#'
#' @param position An integer vector of the modified residues positions
#' @param name A string to identify the PTM (e.g. Phosphorylation, Default: "unknown")
#' @export
#' @importFrom vctrs vec_cast
#' @examples
#' peptr_ptm(c(10L, 22L, 58L, 125L), "phosphorylation")
#' peptr_ptm(c(58L, 132L, 24L), "O-glycosylation")
peptr_ptm <- function(position = peptr_position(),
                      name = "unknown") {
  ptm_check(position, name)

  position <- vec_cast(position, to = peptr_position())
  name <- vec_cast(name, to = character())

  new_ptm(position = position, name = name)
}

setOldClass(c("peptr_ptm", "vctrs_vctr"))

# Getters ----

#' @title Access the name attribute of a `peptr_ptm` object.
#'
#' @keywords internal
peptr_get_ptm_name <- function(x) {
  attr(x, "name")
}

# Class check ----


#' @title Test if an object is of class `peptr_ptm`
#'
#' @param x An object.
#'
#' @return `TRUE` if object is of class `peptr_ptm` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- peptr_ptm(c(5, 3, 8))
#' y <- c(5, 3, 8)
#'
#' peptr_is_ptm(x)
#' peptr_is_ptm(y)
peptr_is_ptm <- function(x) {
  inherits(x, "peptr_ptm")
}

# Formatting ----

vec_ptype_full.peptr_ptm <- function(x, ...) {
  paste0("ptm<", peptr_get_ptm_name(x), ">")
}

vec_ptype_abbr.peptr_ptm <- function(x, ...) {
  "ptm"
}

#' @export
#' @importFrom vctrs field
format.peptr_ptm <- function(x, ...) {
  position <- field(x, "position")
  out <- format(position)
  out[is.na(position)] <- NA
  out
}

obj_print_data.peptr_ptm <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  cat(format(x), sep = " ")
  invisible(x)
}

# Casting ----


vec_ptype2.peptr_ptm <- function(x, y, ...) {
  UseMethod("vec_ptype2.peptr_ptm", y)
}

#' @importFrom vctrs vec_default_ptype2
vec_ptype2.peptr_ptm.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

vec_cast.peptr_ptm <- function(x, to, ...) {
  UseMethod("vec_cast.peptr_ptm")
}

#' @importFrom vctrs vec_default_cast
vec_cast.peptr_ptm.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

vec_ptype2.peptr_position.peptr_ptm <- function(x, y, ...) {
  new_ptm()
}

vec_cast.peptr_position.peptr_ptm <- function(x, to, ...) {
  x
}


# Integer

vec_ptype2.peptr_ptm.integer <- function(x, y, ...) integer()
vec_ptype2.integer.peptr_ptm <- function(x, y, ...) integer()

vec_cast.peptr_ptm.integer <- function(x, to, ...) {
  peptr_ptm(x)
}

#' @importFrom vctrs vec_data
vec_cast.integer.peptr_ptm <- function(x, to, ...) {
  as.integer(vec_data(x)$position)
}


# Double

vec_ptype2.peptr_ptm.double <- function(x, y, ...) double()
vec_ptype2.double.peptr_ptm <- function(x, y, ...) double()

vec_cast.peptr_ptm.double <- function(x, to, ...) {
  peptr_ptm(peptr_position(x))
}

#' @importFrom vctrs vec_data
vec_cast.double.peptr_ptm <- function(x, to, ...) {
  as.double(vec_data(x)$position)
}

# peptr_position

#' vec_ptype2.peptr_ptm.peptr_position <- function(x, y, ...) peptr_position()
#' vec_ptype2.peptr_position.peptr_ptm <- function(x, y, ...) peptr_position()
#'
#' vec_cast.peptr_ptm.peptr_position <- function(x, to, ...) {
#'   peptr_ptm(x)
#' }
#'
#' #' @importFrom vctrs vec_data
#' vec_cast.peptr_position.peptr_ptm <- function(x, to, ...) {
#'   peptr_position(vec_data(x)$position)
#' }
