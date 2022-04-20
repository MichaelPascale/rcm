# to-integer.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-18


#' Safely typecast REDCap fields to integer.
#' @export
rcm_to_integer <-
  function (...) UseMethod('rcm_to_integer')

#' @rdname rcm_to_integer
#' @export
rcm_to_integer.rcm_data <-
  function(df_data, chr_field) rcm_to_integer(df_data[[chr_field]])

# TODO: Range validation.

#' @rdname rcm_to_integer
#' @export
rcm_to_integer.rcm_field_text <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'text')
  checkmate::assert_integerish(v_field)

  if (v_field |> rcm_validation() != 'integer')
    warning(str_glue('The REDCap field {rcm_field} is coerced to integer but is not using integer validation.'))

  as.integer(v_field)
}

#' @rdname rcm_to_integer
#' @export
rcm_to_integer.rcm_field_dropdown <- function (v_field) {
  .NotYetImplemented()
  checkmate::assert(v_field |> rcm_type() == 'dropdown')
  checkmate::assert_integerish(v_field)
  as.integer(v_field)
}

#' @rdname rcm_to_integer
#' @export
rcm_to_integer.rcm_field_radio <- function (v_field) {
  .NotYetImplemented()
  checkmate::assert(v_field |> rcm_type() == 'radio')
  checkmate::assert_integerish(v_field)
  as.integer(v_field)
}

#' @rdname rcm_to_integer
#' @export
rcm_to_integer.rcm_field_slider <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'slider')
  checkmate::assert_integerish(v_field)
  as.integer(v_field)
}
