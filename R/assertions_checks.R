#' @importFrom rlang abort
check_position <- function(position) {
  if (!is.integer(position)) {
    abort("`position` should be an integer vector.", "peptr_wrong_type")
  }
  if (any(position < 1)) {
    abort("`position` should only contain positive integers", "peptr_wrong_value")
  }
}
