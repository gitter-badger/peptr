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
#' @family position
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

#' @title Test if an object is of class `peptr_position`
#' @family position
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


obj_print_data.peptr_position <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  cat(format(x), sep = " ")
  invisible(x)
}

# Casting ----

vec_ptype2.peptr_position <- function(x, y, ...) {
  UseMethod("vec_ptype2.peptr_position", y)
}

#' @importFrom vctrs vec_default_ptype2
vec_ptype2.peptr_position.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @rdname vctrs-compat
#' @method vec_cast peptr_position
#' @export
#' @export vec_cast.peptr_position
vec_cast.peptr_position <- function(x, to, ...) {
  UseMethod("vec_cast.peptr_position")
}

#' @importFrom vctrs vec_default_cast
#' @export
vec_cast.peptr_position.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_ptype2.peptr_position peptr_position
vec_ptype2.peptr_position.peptr_position <- function(x, y, ...) {
  new_position()
}

#' @method vec_cast.peptr_position peptr_position
#' @export
vec_cast.peptr_position.peptr_position <- function(x, to, ...) {
  x
}


# Integer

vec_ptype2.peptr_position.integer <- function(x, y, ...) integer()
vec_ptype2.integer.peptr_position <- function(x, y, ...) integer()

vec_cast.peptr_position.integer <- function(x, to, ...) {
  peptr_position(x)
}

#' @importFrom vctrs vec_data
vec_cast.integer.peptr_position <- function(x, to, ...) {
  vec_data(x)
}


# Double

vec_ptype2.peptr_position.double <- function(x, y, ...) double()
vec_ptype2.double.peptr_position <- function(x, y, ...) double()

vec_cast.peptr_position.double <- function(x, to, ...) {
  message("HERE")
  peptr_position(as.integer(x))
}

#' @importFrom vctrs vec_data
vec_cast.double.peptr_position <- function(x, to, ...) {
  as.double(vec_data(x))
}
