# data-accessors.R
# Extract REDCap Metadata from Attributes of rcm_data Objects.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-22

##### S3 Method Definitions #####



#' Extract the List of REDCap Instruments
#' @export
rcm_list_forms <-
  function (...) UseMethod('rcm_list_forms')

#' @rdname rcm_list_forms
#' @export
rcm_list_forms.rcm_data <- function(df_data) attr(df_data, 'rcm-instruments')

#' @rdname rcm_list_forms
#' @export
rcm_list_forms.rcm_metadata <- function(df_metadata) .discrete(df_metadata, 2)



#' Extract the List of REDCap Events
#' @export
rcm_list_events <-
  function (...) UseMethod('rcm_list_events')

#' @rdname rcm_list_events
#' @export
rcm_list_events.rcm_data <- function(df_data) {
  if (!attr(df_data, 'rcm-is-longitudinal'))
    stop('The REDCap data does not include longitudinal events.')

  if(is.null(attr(df_data, 'rcm-form-event-map')))
    stop('No form event map is present with the REDCap data.')

  .form_event_map(df_data) |> rcm_list_events()
}

#' @rdname rcm_list_events
#' @export
rcm_list_events.rcm_form_event_map <- function(df_form_event_map) .discrete(df_form_event_map, 2)



#' Extract the List of REDCap Fields
#' @export
rcm_list_fields <-
  function (...) UseMethod('rcm_list_fields')

#' @rdname rcm_list_fields
#' @export
rcm_list_fields.rcm_data <- function(df_data) attr(df_data, 'rcm-fields')

#' @rdname rcm_list_fields
#' @export
rcm_list_fields.rcm_metadata <- function(df_metadata) .discrete(df_metadata, 1)



#' Extract the List of Field Types
#' @export
rcm_list_types <-
  function (...) UseMethod('rcm_list_types')

#' @rdname rcm_list_types
#' @export
rcm_list_types.rcm_data <- function(df_data) attr(df_data, 'rcm-field-types')

#' @rdname rcm_list_types
#' @export
rcm_list_types.rcm_metadata <- function(df_metadata) .discrete(df_metadata, 4)



#' Extract the List of Repeating Forms
#' @export
rcm_list_repeating <-
  function (...) UseMethod('rcm_list_repeating')

#' @rdname rcm_list_repeating
#' @export
rcm_list_repeating.rcm_data <- function(df_data) {
  if (!attr(df_data, 'rcm-has-repeating'))
    stop('The REDCap data does not include repeating instruments.')

  .discrete(df_data, 'redcap_repeat_instrument')
}

