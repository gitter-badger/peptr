#' @importFrom rlang abort is_integerish are_na
position_check <- function(position) {
  if (!all(are_na(position))) {
    if (!is_integerish(position)) {
      abort("`position` must be an integer vector.", "peptr_wrong_type")
    }
  }
  if (any(!is.na(position) & position < 1L)) {
    abort("`position` should only contain positive integers", "peptr_wrong_value")
  }

}

#' @importFrom vctrs vec_size
ptm_check <- function(position, name) {
  position_check(position)
  if (!is.character(name) & vec_size(name) != 1L) {
    abort("`name` must be a single string.", "peptr_wrong_type")
  }
}

#' @importFrom rlang are_na abort is_integerish are_na
#' @importFrom vctrs vec_size
ptm2_check <- function(position_1, position_2) {
  if (!all(are_na(position_1))) {
    if (!is_integerish(position_1)) {
      abort("`position_1` must be an integer vector.", "peptr_wrong_type")
    }
  }
  if (any(position_1 < 1)) {
    abort("`position_1` must only contain positive integers.", "peptr_wrong_value")
  }

  if (!all(are_na(position_2))) {
    if (!is_integerish(position_2)) {
      abort("`position_2` must be an integer vector.", "peptr_wrong_type")
    }
  }
  if (any(position_2 < 1)) {
    abort("`position_2` must only contain positive integers.", "peptr_wrong_value")
  }

  if (vec_size(position_1) != vec_size(position_2)) {
    abort(
      paste0(
        "`position_1` and `position_2` must have the same size (",
        vec_size(position_1), "!=", vec_size(position_2),
        ")."
      ),
      "peptr_wrong_size"
    )
  }

  if (any(mapply(function(x, y) {
    x == y
  }, position_1, position_2))) {
    abort("All `position_1` and `position_2` must be different.", "peptr_wrong_value")
  }
}


value_position_check <- function(value, position, range) {
  if (!all(are_na(value))) {
    if (!is.double(value)) {
      abort("`value` must be a double vector.", "peptr_wrong_type")
    }
  }

  position_check(position)
  if (any(duplicated(position))) {
    abort("`position` must be unique", "peptr_duplicated_position")
  }

  if (!all(are_na(range))) {
    if (!is.double(range)) {
      abort("`range` must be a double vector.", "peptr_wrong_type")
    }
  }
  if (vec_size(range) != 2L) {
    abort("`range` must be a vector of length 2.", "peptr_wrong_size")
  }
  if (range[[1]] > range[[2]]) {
    abort("`range` must be sorted.", "peptr_wrong_value")
  }
  range_value <- range(value)
  if (range_value[[1]] < range[[1]] | range_value[[2]] > range[[2]]) {
    abort("`value` must be comprised inside`range`.", "peptr_wrong_value")
  }
}
