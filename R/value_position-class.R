#' Internal constructor to create `peptr_value_position` type
#'
#' Asserts that `value` is a double value, `position` is an integer vector,
#' and that `name` is a string.
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_value_position <- function(value = double(),
                               position = integer(),
                               range = double(),
                               name = "unknown") {
  vec_assert(value, ptype = double())
  vec_assert(position, ptype = integer())
  vec_assert(range, ptype = double(), size = 2)
  vec_assert(name, ptype = character(), size = 1)

  new_rcrd(list(value = value, position = position),
    range = range,
    name = name,
    class = "peptr_value_position"
  )
}

#' A class for position specific variable
#'
#' @param value A double vector of the residue value
#' @param position An integer vector of the residue position
#' @param name A string to identify the variable (e.g. disorder, Default: "unknown")
#' @export
#' @importFrom vctrs vec_cast vec_cast_common
#' @examples
#' peptr_value_position(0.5, 1, c(0, 1))
peptr_value_position <- function(value = double(),
                                 position = integer(),
                                 range = double(),
                                 name = "unknown") {

  value_position_check(value, position, range)

  value <- vec_cast(value, to = double())
  position <- vec_cast(position, to = integer())
  range <- vec_cast(range, to = double())
  name <- vec_cast(name, to = character())

  new_value_position(value = value, position = position, range = range, name = name)
}

setOldClass(c("peptr_value_position", "vctrs_vctr"))


peptr_value_position_name <- function(x) {
  attr(x, "name")
}

peptr_value_position_range <- function(x) {
  attr(x, "range")
}

peptr_is_value_position <- function(x) {
  inherits(x, "peptr_value_position")
}

vec_ptype_full.peptr_value_position <- function(x, ...) {
  paste0("value_position<", peptr_value_position_name(x), ">")
}

vec_ptype_abbr.peptr_value_position <- function(x, ...) {
  "vp"
}

#' @export
#' @importFrom vctrs field
#' @importFrom crayon italic silver
format.peptr_value_position <- function(x, ...) {
  value <- field(x, "value")
  position <- field(x, "position")
  out <- paste0(silver(italic(paste0(position, ":"))), value)
  out[is.na(value) | is.na(position)] <- NA
  out
}

#' @export
obj_print_data.peptr_value_position <- function(x, ...) {
  cat(format(x), sep = " ")
  invisible(x)
}
