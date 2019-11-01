#' Internal constructor to create `peptr_position` type
#'
#' Asserts that
#'
#' @keywords internal
new_position <- function(x = integer()) {
  vec_assert(x, ptype = integer())

  vctrs::new_vctr(x,
                  class = "peptr_position")
}

#' A decimalized class for pounds, shillings, and pence values
#'
#' @param x An integer vectore representing positions
#' @export
peptr_position <- function(x = integer()) {

  position_check(x)

  x <- vec_cast(x, to = integer())

  new_position(x = x)
}

setOldClass(c("peptr_position", "vctrs_vctr"))

peptr_is_position <- function(x) inherits(x, "peptr_position")

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



# coerce integer to peptr_position, vice versa
# coerce double to peptr_position, vice versa
