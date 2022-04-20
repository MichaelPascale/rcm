# to-factor.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-19


#' Safely typecast REDCap fields to factor.
#' @export
rcm_to_factor <-
  function (...) UseMethod('rcm_to_factor')

#' @rdname rcm_to_factor
#' @export
rcm_to_factor.rcm_data <-
  function(df_data, chr_field) rcm_to_factor(df_data[[chr_field]])

#' @rdname rcm_to_factor
#' @export
rcm_to_factor.rcm_field_dropdown <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'dropdown')

  chr_codes <- rcm_choices(v_field)
  factor(v_field, chr_codes, names(chr_codes))
}

#' @rdname rcm_to_factor
#' @export
rcm_to_factor.rcm_field_radio <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'radio')
  checkmate::assert_integerish(v_field)
  as.integer(v_field)
}
