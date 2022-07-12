# rcm.R
# Definition of S3 Class for Combining REDCap Data and Metadata
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-15

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
rcm <- function (df_data, df_metadata, df_form_event_map=NULL, chr_record_id_field='record_id') {

  checkmate::assert_data_frame(df_data, col.names='unique')
  checkmate::assert_data_frame(df_metadata, ncols=18)
  checkmate::assert_data_frame(df_form_event_map, ncols=3, null.ok=TRUE)

  checkmate::assert_names(names(df_data), must.include=chr_record_id_field)

  lgl_is_longitudinal <-
    checkmate::test_names(names(df_data), must.include='redcap_event_name')

  lgl_has_repeating <-
    checkmate::test_names(names(df_data), must.include=c(
      'redcap_repeat_instrument',
      'redcap_repeat_instance'
    ))

  if (!is.null(df_form_event_map)) {
    if (lgl_is_longitudinal) {
      checkmate::assert_names(names(df_form_event_map), must.include=c(
        'arm_num',
        'unique_event_name',
        'form'
      ))
    } else {
      stop('Form-event mappings specified but redcap_event_name not present.')
    }
  }

  vchr_reachable <- character()
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
    vchr_reachable <- append(vchr_reachable, chr_name)
  }

  df_metadata <- structure(df_metadata, class=prepend(class(df_metadata), 'rcm_metadata'))

  # TODO: Implement optional attributes including events, labels, etc.
  structure(
    df_data,
    `rcm-record-id-field`=chr_record_id_field,
    `rcm-metadata`=df_metadata,
    `rcm-is-longitudinal`=lgl_is_longitudinal,
    `rcm-has-repeating`=lgl_has_repeating,
    `rcm-form-event-map`=rcm_form_event_map(df_form_event_map),
    `rcm-instruments`=rcm_list_forms(df_metadata),
    `rcm-field-types`=rcm_list_types(df_metadata),
    `rcm-fields`=vchr_reachable,
    `rcm-non-fields`=names(df_data) |> discard(is.element, vchr_reachable),
    class=prepend(class(df_data), 'rcm_data')
  )
}


# Extract method for rcm_data.
`[.rcm_data` <- function (...) {
  warning('It is not recommended to extract directly from rcm_data objects. Use the provided methods or downgrade to data.frame.')
  NextMethod()
}
