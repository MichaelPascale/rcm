# to-numeric.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-18


#' Safely typecast REDCap fields to numeric
#' @export
rcm_to_numeric <-
  function (...) UseMethod('rcm_to_numeric')

#' @rdname rcm_to_numeric
#' @export
rcm_to_numeric.rcm_data <-
  function(df_data, chr_field) rcm_to_numeric(df_data[[chr_field]])

# TODO: Range validation.

#' @rdname rcm_to_numeric
#' @export
rcm_to_numeric.rcm_field_text <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'text')
  checkmate::assert_numeric(v_field)

  as.numeric(v_field)
}

#' @rdname rcm_to_numeric
#' @export
rcm_to_numeric.rcm_field_slider <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'slider')
  checkmate::assert_numeric(v_field)

  as.numeric(v_field)
}

#' @rdname rcm_to_numeric
#' @export
rcm_to_numeric.rcm_field_calc <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'calc')
  checkmate::assert_numeric(v_field)

  as.numeric(v_field)
}



