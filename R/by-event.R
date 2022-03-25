# by-event.R
# List arms, forms, and fields by event.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-25

#' List REDCap arms given an event.
#' @export
rcm_arms_by_event <- function (...) UseMethod('rcm_arms_by_event')

#' @rdname rcm_arms_by_event
#' @export
rcm_arms_by_event.rcm_form_event_map <- function(df_form_event_map, chr_event) {
  df_form_event_map[df_form_event_map[, 2] %in% chr_event, 1]
}

#' @rdname rcm_arms_by_event
#' @export
rcm_arms_by_event.rcm_data <- function (df_data, chr_event) {
  .form_event_map(df_data) |> rcm_arms_by_event(chr_event)
}

#' List REDCap instruments given an event.
#' @export
rcm_forms_by_event <- function (...) UseMethod('rcm_forms_by_event')

#' @rdname rcm_forms_by_event
#' @export
rcm_forms_by_event.rcm_form_event_map <- function(df_form_event_map, chr_event) {
  df_form_event_map[df_form_event_map[, 2] %in% chr_event, 3]
}

#' @rdname rcm_forms_by_event
#' @export
rcm_forms_by_event.rcm_data <- function (df_data, chr_event) {
  .form_event_map(df_data) |> rcm_forms_by_event(chr_event)
}

#' List REDCap fields given an event.
#' @export
rcm_fields_by_event <- function (...) UseMethod('rcm_fields_by_event')

#' @rdname rcm_fields_by_event
#' @export
rcm_fields_by_event.rcm_data <- function(df_data, chr_event) {
  vchr_forms <- .form_event_map(df_data) |> rcm_forms_by_event(chr_event)
  .metadata(df_data) |> rcm_fields_by_form(vchr_forms)
}
