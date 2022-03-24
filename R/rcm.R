# rcm.R
# Definition of S3 Class for Combining REDCap Data and Metadata
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-18

# TODO: Create class constructor method for rcm_metadata.
# TODO: Expand this class.
# TODO: Create class for REDCap forms?
# TODO: Make accessor method for rcm_data
rcm_form_event_map <- function (df_form_event_map) {
  if (is.null(df_form_event_map))
    return(NULL)

  structure(df_form_event_map, class=prepend(class(df_form_event_map), 'rcm_form_event_map'))
}

#' REDCap Database with Metadata
#'
#' An S3 Class that attaches rows of the REDCap data dictionary to the columns
#' of the data itself.
#'
#' @param df_data Dataframe comprising the downloaded REDCap data. Download
#' from REDCap in CSV format with raw values through the Data Export Tool or the
#' export records API method.
#'
#' @param df_metadata Dataframe comprising the REDCap metadata as downloaded
#' from the Data Dictionary page or through the metadata API method.
#'
#' @param df_form_event_map Optional. Dataframe comprising the REDCap
#' instrument-event mappings as downloaded from the Project Settings page or the
#' formEventMapping API method.
#'
#' @export
rcm <- function (df_data, df_metadata, df_form_event_map=NULL) {

  checkmate::assert_data_frame(df_data, col.names='unique')
  checkmate::assert_data_frame(df_metadata, ncols=18)
  checkmate::assert_data_frame(df_form_event_map, ncols=3, null.ok=TRUE)

  checkmate::assert_names(names(df_data), must.include=c(
    'record_id',
    'redcap_event_name',
    'redcap_repeat_instrument',
    'redcap_repeat_instance'
  ))

  if (!is.null(df_form_event_map)) {
    checkmate::assert_names(names(df_form_event_map), must.include=c(
      'arm_num',
      'unique_event_name',
      'form'
    ))
  }

  int_reachable <- 0
  for (chr_name in names(df_data)) {
    chr_lookup <- chr_name

    if (!chr_lookup %in% df_metadata[, 1] ) {
      chr_lookup <- gsub('___[_a-zA-Z0-9]*$', '', chr_name)
      if (!chr_lookup %in% df_metadata[, 1])
        next

      # TODO: Handle "_complete" fields.
    }

    chr_form <- df_metadata[df_metadata[,1] == chr_lookup, 2]

    attr(df_data[[chr_name]], 'rcm-field') <- df_metadata[df_metadata[,1] == chr_lookup, 1]
    attr(df_data[[chr_name]], 'rcm-form') <- chr_form
    attr(df_data[[chr_name]], 'rcm-type') <- df_metadata[df_metadata[,1] == chr_lookup, 4]
    attr(df_data[[chr_name]], 'rcm-label') <- df_metadata[df_metadata[,1] == chr_lookup, 5]
    attr(df_data[[chr_name]], 'rcm-choices') <- df_metadata[df_metadata[,1] == chr_lookup, 6]
    attr(df_data[[chr_name]], 'rcm-note') <- df_metadata[df_metadata[,1] == chr_lookup, 7]
    attr(df_data[[chr_name]], 'rcm-validation') <- df_metadata[df_metadata[,1] == chr_lookup, 8]
    attr(df_data[[chr_name]], 'rcm-min') <- df_metadata[df_metadata[,1] == chr_lookup, 9]
    attr(df_data[[chr_name]], 'rcm-max') <- df_metadata[df_metadata[,1] == chr_lookup, 10]
    attr(df_data[[chr_name]], 'rcm-identifier') <- df_metadata[df_metadata[,1] == chr_lookup, 11]
    attr(df_data[[chr_name]], 'rcm-branching') <- df_metadata[df_metadata[,1] == chr_lookup, 12]
    attr(df_data[[chr_name]], 'rcm-required') <- df_metadata[df_metadata[,1] == chr_lookup, 13]
    attr(df_data[[chr_name]], 'rcm-annotation') <- df_metadata[df_metadata[,1] == chr_lookup, 18]

    if (!is.null(df_form_event_map)) {
      attr(df_data[[chr_name]], 'rcm-events') <- df_form_event_map[df_form_event_map[,3] == chr_form, 2]
    }

    class(df_data[[chr_name]]) <- prepend(class(df_data[[chr_name]]), 'rcm_field')
    # FIXME: Ensure this yields something useful for all REDCap types...
    class(df_data[[chr_name]]) <- prepend(class(df_data[[chr_name]]), str_glue('rcm_field_{df_metadata[df_metadata[,1] == chr_lookup, 4]}'))
    int_reachable = int_reachable + 1
  }

  # TODO: Implement optional attributes including events, labels, etc.
  structure(
    df_data,
    `rcm-metadata`=structure(df_metadata, class=prepend(class(df_metadata), 'rcm_metadata')),
    `rcm-form-event-map`=rcm_form_event_map(df_form_event_map),
    `rcm-instruments`=df_metadata[,2] |> unique() |> sort(),
    `rcm-field-types`=df_metadata[,4] |> unique() |> sort(),
    `rcm-reachable`=int_reachable,
    class=prepend(class(df_data), 'rcm_data')
  )
}
