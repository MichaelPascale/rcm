# by-form.R
# List arms, events, and fields by form.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-24

#' List REDCap arm given an instrument name.
#' @export
rcm_arms_by_form <- function (...) UseMethod('rcm_arms_by_form')

#' @rdname rcm_arms_by_form
#' @export
rcm_arms_by_form.rcm_form_event_map <- function (df_form_event_map, chr_form) {
  df_form_event_map[df_form_event_map[,3] == chr_form, 1]
}

#' List REDCap events given an instrument name.
#' @export
rcm_events_by_form <- function (...) UseMethod('rcm_events_by_form')

#' @rdname rcm_events_by_form
#' @export
rcm_events_by_form.rcm_form_event_map <- function (df_form_event_map, chr_form) {
  df_form_event_map[df_form_event_map[,3] == chr_form, 2]
}

#' List REDCap fields given an instrument name.
#' @export
rcm_fields_by_form <- function (...) UseMethod('rcm_fields_by_form')

#' @rdname rcm_fields_by_form
#' @export
rcm_fields_by_form.rcm_metadata <- function (df_metadata, chr_form) {
  df_metadata[df_metadata[,2] == chr_form, 1]
}
