# accessors.R
# Extract REDCap Metadata from Attributes of rcm_* Objects.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-15

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
rcm_choices.rcm_field <- function(v, raw=FALSE) {
  chr_choices <- attr(v, 'rcm-choices')

  if (raw)
    return(chr_choices)

  str_match_all(chr_choices, '(?<value>\\w+),\\s*(?<label>.*?)\\s*(\\||$)')[[1]] ->
    mat_choices

  setNames(as.list(mat_choices[,'value']), mat_choices[,'label'])
}

#' @rdname rcm_min
#' @export
rcm_min.rcm_field <- function(v) attr(v, 'rcm-min')

#' @rdname rcm_max
#' @export
rcm_max.rcm_field <- function(v) attr(v, 'rcm-max')

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


#### Extract rcm_* Properties ####

##### S3 Method Definitions #####

#' Extract Metadata from an rcm_data Object
#' @export
rcm_metadata <-
  function (...) UseMethod('rcm_metadata')

#' @rdname rcm_metadata
#' @export
rcm_metadata.rcm_data <-
   function(df_data) attr(df_data, 'rcm-metadata')

#' Extract the List of REDCap Instruments from the Data Dictionary
#' @export
rcm_list_forms <-
  function (...) UseMethod('rcm_list_forms')

#' @rdname rcm_list_forms
#' @export
rcm_list_forms.rcm_data <- function(df_data) rcm_list_forms(rcm_metadata(df_data))

#' @rdname rcm_list_forms
#' @export
rcm_list_forms.rcm_metadata <- function(df_metadata) df_metadata[,2] |> unique()
