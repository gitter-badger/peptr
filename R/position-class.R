# Constructor ----

#' @title Internal constructor to create `peptr_position` type
#'
#' Asserts that `x` is a integer(-ish) vector.
#'
#' @keywords internal
#' @importFrom vctrs new_vctr vec_assert
new_position <- function(x = integer()) {
  vec_assert(x, ptype = integer())

  new_vctr(x, class = "peptr_position")
}

# Helper ----

#' @title A for positions on peptidic chains
#'
#' @param x An integer vector representing positions
#' @export
#' @importFrom vctrs vec_cast
#' @examples
#' peptr_position(1:10)
peptr_position <- function(x = integer()) {

  position_check(x)

  x <- vec_cast(x, to = integer())

  new_position(x = x)
}

setOldClass(c("peptr_position", "vctrs_vctr"))

# Class check ----

#' @title Test if an object is of class `deb_decimal`
#'
#' @description Test if an object is of class `peptr_position`.
#'
#' @param x An object.
#'
#' @return `TRUE` if object is of class `peptr_position` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- peptr_position(1:30)
#' y <- 1:30
#'
#' peptr_is_position(x)
#' peptr_is_position(y)
peptr_is_position <- function(x) {
  inherits(x, "peptr_position")
}

# Formatting ----
vec_ptype_abbr.peptr_position <- function(x, ...) {
  "pos"
}

#' @importFrom vctrs vec_data
format.peptr_position <- function(x, ...) {
  out <- format_position(vec_data(x))
  out[is.na(x)] <- NA
  out
}


#' @export
obj_print_data.peptr_position <- function(x, ...) {
  if (length(x) == 0)
    return()
  cat(format(x), sep = " ")
  invisible(x)
}

# Casting ----

# coerce integer to peptr_position, vice versa
# coerce double to peptr_position, vice versa
