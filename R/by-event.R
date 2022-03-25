# by-event.R
# List arms, forms, and fields by event.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-24

#' List REDCap arms given an event.
#' @export
rcm_arms_by_event <- function (...) UseMethod('rcm_arms_by_event')

#' @rdname rcm_arms_by_event
#' @export
rcm_arms_by_event.rcm_form_event_map <- function(df_form_event_map, chr_event) {
  df_form_event_map[df_form_event_map[, 2] == chr_event, 1]
}

#' List REDCap instruments given an event.
#' @export
rcm_forms_by_event <- function (...) UseMethod('rcm_forms_by_event')

#' @rdname rcm_forms_by_event
#' @export
rcm_forms_by_event.rcm_form_event_map <- function(df_form_event_map, chr_event) {
  df_form_event_map[df_form_event_map[, 2] == chr_event, 3]
}

#' List REDCap fields given an event.
#' @export
rcm_fields_by_event <- function (...) UseMethod('rcm_fields_by_event')

#' @rdname rcm_fields_by_event
#' @export
rcm_fields_by_event.rcm_data <- function(df_data, chr_event) {
  df_form_event_map <- attr(df_data, 'rcm-form-event-map')

  if (is.null(df_form_event_map))
    stop('rcm_data object has no attached form-event map.')

  vchr_forms <- df_form_event_map |> rcm_forms_by_event(chr_event)
  attr(df_data, 'rcm-metadata') |> rcm_fields_by_form(vchr_forms)
}
