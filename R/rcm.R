# rcm.R
# Definition of S3 Class for Combining REDCap Data and Metadata
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-15

#' REDCap Database with Metadata
#'
#' An S3 Class that attaches rows of the REDCap data dictionary to the columns
#' of the data itself.
#'
#' @export
rcm <- function (df_data, df_metadata) {

  checkmate::assert_data_frame(df_data, col.names='unique')
  checkmate::assert_data_frame(df_metadata, ncols=18)

  checkmate::assert_names(names(df_data), must.include=c(
    'record_id',
    'redcap_event_name',
    'redcap_repeat_instrument',
    'redcap_repeat_instance'
  ))


  int_reachable <- 0
  for (chr_name in names(df_data)) {
    chr_lookup <- chr_name

    if (!chr_lookup %in% df_metadata[, 1]) {
      chr_lookup <- gsub('___[_a-zA-Z0-9]*$', '', chr_name)

      if (!chr_lookup %in% df_metadata[, 1])
        next
    }

    attr(df_data[[chr_name]], 'rcm-field') <- df_metadata[df_metadata[,1] == chr_lookup, 1]
    attr(df_data[[chr_name]], 'rcm-form') <- df_metadata[df_metadata[,1] == chr_lookup, 2]
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

    class(df_data[[chr_name]]) <- prepend(class(df_data[[chr_name]]), 'rcm_field')
    int_reachable = int_reachable + 1
  }

  # TODO: Implement optional attributes including events, labels, etc.
  structure(
    df_data,
    `rcm-metadata`=structure(df_metadata, class=prepend(class(df_metadata), 'rcm_metadata')),
    `rcm-reachable`=int_reachable,
    class=prepend(class(df_data), 'rcm_data')
  )
}
