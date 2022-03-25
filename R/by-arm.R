# by-arm.R
# List events, forms, and fields by arm.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-24

#' List REDCap events given an arm.
#' @export
rcm_events_by_arm <- function (...) UseMethod('rcm_events_by_arm')

#' @rdname rcm_events_by_arm
#' @export
rcm_events_by_arm.rcm_form_event_map <- function (df_form_event_map, int_arm) {
  df_form_event_map[df_form_event_map[, 1] == int_arm, 1]
}

#' List REDCap instruments given an arm.
#' @export
rcm_forms_by_arm <- function (...) UseMethod('rcm_forms_by_arm')

#' @rdname rcm_forms_by_arm
#' @export
rcm_forms_by_arm.rcm_form_event_map <- function (df_form_event_map, int_arm) {
  df_form_event_map[df_form_event_map[, 1] == int_arm, 2]
}

#' @rdname rcm_forms_by_arm
#' @export
rcm_forms_by_arm.rcm_data <- function (df_data, int_arm) {
  df_form_event_map <- attr(df_data, 'rcm-form-event-map')

  if (is.null(df_form_event_map))
    stop('rcm_data object has no attached form-event map.')

  attr(df_data, 'rcm-form-event-map') |> rcm_forms_by_arm(int_arm)
}

#' List REDCap fields given an arm.
#' @export
rcm_fields_by_arm <- function (...) UseMethod('rcm_fields_by_arm')

#' @rdname rcm_fields_by_arm
#' @export
rcm_fields_by_arm.rcm_data <- function (df_data, int_arm) {
  df_form_event_map <- attr(df_data, 'rcm-form-event-map')

  if (is.null(df_form_event_map))
    stop('rcm_data object has no attached form-event map.')

  vchr_forms <- df_form_event_map |> rcm_forms_by_arm(int_arm)
  attr(df_data, 'rcm-metadata') |> rcm_fields_by_form(vchr_forms)
}
