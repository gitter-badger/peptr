#' Internal constructor to create `peptr_peptide` type
#'
#' Asserts that...
#'
#' @keywords internal
#' @importFrom vctrs vec_assert new_vctr new_rcrd
new_peptide <- function(sequence = character(),
                        ptms = list(), # todo: transform to list of peptr_ptm and peptr_ptm2
                        position = peptr_position()) {
  vec_assert(sequence, ptype = character(), size = 1L)
  vec_assert(ptms, ptype = list())
  vec_assert(position, ptype = peptr_position(), size = 1L)

  new_rcrd(
    fields = list(sequence = sequence),
    ptms = ptms,
    position = position,
    class = "peptr_peptide"
  )
}
