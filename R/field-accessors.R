# accessors.R
# Extract REDCap Metadata from Attributes of rcm_field Objects.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-22

#### Extract REDCap Metadata ####

##### S3 Method Declarations #####

#' Extract the REDCap Field Name
#' @export
rcm_field <-
  function (...) UseMethod('rcm_field')

#' Extract the REDCap Instrument Name
#' @export
rcm_form <-
  function (...) UseMethod('rcm_form')

#' Extract the REDCap Field Type
#' @export
rcm_type <-
  function (...) UseMethod('rcm_type')

#' Extract the REDCap Field Label
#' @export
rcm_label <-
  function (...) UseMethod('rcm_label')

#' Extract the REDCap Choices or Calculations
#' @export
rcm_choices <-
  function (...) UseMethod('rcm_choices')

#' Extract the REDCap Field Note
#' @export
rcm_note <-
  function (...) UseMethod('rcm_note')

#' Extract the REDCap Text Validation
#' @export
rcm_validation <-
  function (...) UseMethod('rcm_validation')

#' Extract the REDCap Validation Minimum Value
#' @export
rcm_min <-
  function (...) UseMethod('rcm_min')

#' Extract the REDCap Validation Maximum Value
#' @export
rcm_max <-
  function (...) UseMethod('rcm_max')

#' Extract the REDCap Identifier Status
#' @export
rcm_identifier <-
  function (...) UseMethod('rcm_identifier')

#' Extract the REDCap Branching Logic
#' @export
rcm_branching <-
  function (...) UseMethod('rcm_branching')

#' Extract the REDCap Required Status
#' @export
rcm_required <-
  function (...) UseMethod('rcm_required')

#' Extract the REDCap Field Annotation
#' @export
rcm_annotation <-
  function (...) UseMethod('rcm_annotation')

#' Extract the REDCap events at which the field's form is present
#' @export
rcm_events <-
  function (...) UseMethod('rcm_events')

##### rcm_field #####

#' @rdname rcm_field
#' @export
rcm_field.rcm_field <- function(v) attr(v, 'rcm-field')

#' @rdname rcm_type
#' @export
rcm_type.rcm_field <- function(v) attr(v, 'rcm-type')

#' @rdname rcm_form
#' @export
rcm_form.rcm_field <- function(v) attr(v, 'rcm-form')

#' @rdname rcm_validation
#' @export
rcm_validation.rcm_field <- function(v) attr(v, 'rcm-validation')

#' @rdname rcm_annotation
#' @export
rcm_annotation.rcm_field <- function(v) attr(v, 'rcm-annotation')

#' @rdname rcm_choices
#' @export
rcm_choices.rcm_field <- function(v, lgl_raw=FALSE) {
  if (lgl_raw)
    return(attr(v, 'rcm-choices'))

  .extract_choices(attr(v, 'rcm-choices'))
}

#' @rdname rcm_min
#' @export
rcm_min.rcm_field <- function(v) attr(v, 'rcm-min')

#' @rdname rcm_max
#' @export
rcm_max.rcm_field <- function(v) attr(v, 'rcm-max')

#' @rdname rcm_events
#' @export
rcm_events.rcm_field <- function(v) attr(v, 'rcm-events')

##### rcm_data #####

#' @rdname rcm_field
#' @export
rcm_field.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-field')

#' @rdname rcm_type
#' @export
rcm_type.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-type')

#' @rdname rcm_form
#' @export
rcm_form.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-form')

#' @rdname rcm_validation
#' @export
rcm_validation.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-validation')

#' @rdname rcm_annotation
#' @export
rcm_annotation.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-annotation')

#' @rdname rcm_choices
#' @export
rcm_choices.rcm_data <- function(df_data, chr_field, ...) rcm_choices(df_data[[chr_field]], ...)

#' @rdname rcm_min
#' @export
rcm_min.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-min')

#' @rdname rcm_max
#' @export
rcm_max.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-max')

#' @rdname rcm_events
#' @export
rcm_events.rcm_data <- function(df_data, chr_field) attr(df_data[[chr_field]], 'rcm-events')

##### rcm_metadata #####

#' @rdname rcm_type
#' @export
rcm_type.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 4]

#' @rdname rcm_form
#' @export
rcm_form.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 2]

#' @rdname rcm_validation
#' @export
rcm_validation.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 8]

#' @rdname rcm_annotation
#' @export
rcm_annotation.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 18]

#' @rdname rcm_choices
#' @export
rcm_choices.rcm_metadata <- function(df_metadata, chr_field, raw=FALSE) {
  chr_choices <- df_metadata[df_metadata[[1]] == chr_field, 6]

  if (raw)
    return(chr_choices)

  .extract_choices(chr_choices)
}

#' @rdname rcm_min
#' @export
rcm_min.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 9]

#' @rdname rcm_max
#' @export
rcm_max.rcm_metadata <- function(df_metadata, chr_field) df_metadata[df_metadata[[1]] == chr_field, 10]


#' Extract choice-code mappings from radio/dropdown fields.
#'
#' Expects comma separated codes and corresponding choices, with each pair
#' separated by a pipe character.
#'
#' e.g. 1,A|2,B|3,C
#'
#' @param chr_choices The choices string from the REDCap data dictionary.
#' @return A list of codes named by choice.
#'
#' @keywords internal
.extract_choices <- function(chr_choices) {
  str_match_all(chr_choices, '(?<value>\\w+),\\s*(?<label>.*?)\\s*(\\||$)')[[1]] ->
    mat_choices

  setNames(as.list(mat_choices[,'value']), mat_choices[,'label'])
}
