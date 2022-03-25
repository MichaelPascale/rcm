# by-field.R
# List arms, events, and forms by field.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-25

# TODO: rcm_arms_by_field.rcm_data
# TODO: rcm_events_by_field.rcm_data

#' List REDCap instruments given a REDCap field.
#' @export
rcm_forms_by_field <- function (...) UseMethod('rcm_forms_by_field')

#' @rdname rcm_forms_by_field
#' @export
rcm_forms_by_field.rcm_metadata <- function (df_metadata, chr_field) {
  df_metadata[df_metadata[, 1] %in% chr_field, 2]
}

#' @rdname rcm_forms_by_field
#' @export
rcm_forms_by_field.rcm_field <- function (rcm_field) {
  rcm_form(rcm_field)
}
