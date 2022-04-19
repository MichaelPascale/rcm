# to-text.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-18


#' Safely typecast text fields.
#' @export
rcm_to_text <-
  function (...) UseMethod('rcm_to_text')

#' @rdname rcm_to_text
#' @export
rcm_to_text.rcm_data <-
  function(df_data, chr_field) rcm_to_text(df_data[[chr_field]])

#' @rdname rcm_to_text
#' @export
rcm_to_text.rcm_field_text <- function (v_field) {
  checkmate::assert(v_field |> rcm_type() == 'text')
  as.character(v_field)
}
