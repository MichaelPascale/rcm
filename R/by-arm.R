# by-arm.R
# List events, forms, and fields by arm.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-25

#' List REDCap events given an arm.
#' @export
rcm_events_by_arm <- function (...) UseMethod('rcm_events_by_arm')

#' @rdname rcm_events_by_arm
#' @export
rcm_events_by_arm.rcm_form_event_map <- function (df_form_event_map, int_arm) {
  df_form_event_map[df_form_event_map[, 1] %in% int_arm, 2] |> unique()
}

#' @rdname rcm_events_by_arm
#' @export
rcm_events_by_arm.rcm_data <- function (df_data, int_arm) {
  .form_event_map(df_data) |> rcm_events_by_arm(int_arm)
}

#' List REDCap instruments given an arm.
#' @export
rcm_forms_by_arm <- function (...) UseMethod('rcm_forms_by_arm')

#' @rdname rcm_forms_by_arm
#' @export
rcm_forms_by_arm.rcm_form_event_map <- function (df_form_event_map, int_arm) {
  df_form_event_map[df_form_event_map[, 1] %in% int_arm, 3] |> unique()
}

#' @rdname rcm_forms_by_arm
#' @export
rcm_forms_by_arm.rcm_data <- function (df_data, int_arm) {
  .form_event_map(df_data) |> rcm_forms_by_arm(int_arm)
}

#' List REDCap fields given an arm.
#' @export
rcm_fields_by_arm <- function (...) UseMethod('rcm_fields_by_arm')

#' @rdname rcm_fields_by_arm
#' @export
rcm_fields_by_arm.rcm_data <- function (df_data, int_arm) {
  vchr_forms <- .form_event_map(df_data) |> rcm_forms_by_arm(int_arm)
  attr(df_data, 'rcm-metadata') |> rcm_fields_by_form(vchr_forms)
}
