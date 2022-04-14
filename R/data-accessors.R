# data-accessors.R
# Extract REDCap Metadata from Attributes of rcm_data Objects.
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-14

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
rcm_list_forms.rcm_data <- function(df_data) attr(df_data, 'rcm-instruments')

#' @rdname rcm_list_forms
#' @export
rcm_list_forms.rcm_metadata <- function(df_metadata) df_metadata[,2] |> unique()

#' Extract the List of Field Types from the Data Dictionary
#' @export
rcm_list_types <-
  function (...) UseMethod('rcm_list_types')

#' @rdname rcm_list_types
#' @export
rcm_list_types.rcm_data <- function(df_data) attr(df_data, 'rcm-field-types')

#' @rdname rcm_list_types
#' @export
rcm_list_types.rcm_metadata <- function(df_metadata) df_metadata[,4] |> unique()