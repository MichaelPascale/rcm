# to-time.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-04-15


#' Safely typecast date/time/datetime fields.
#' @export
rcm_to_time <-
  function (...) UseMethod('rcm_to_time')

#' @rdname rcm_to_time
#' @export
rcm_to_time.rcm_data <-
  function(df_data, chr_field) rcm_to_time(df_data[[chr_field]])

#' @rdname rcm_to_time
#' @export
rcm_to_time.rcm_field_text <- function (chr_time, tz='America/New_York') {
  checkmate::assert_character(chr_time)
  checkmate::assert(chr_time |> rcm_type() == 'text')

  # TODO: Should this return Date class or posixct for ymd/dmy/mdy?
  # FIXME: An extra row appears to be present in the output of this function?

  chr_validation <- chr_time |> rcm_validation()

  if (chr_validation == c('time'))
    return(hm(chr_time))

  if (chr_validation == 'time_12hr') {
    warning(str_glue('The REDCap field {rcm_field(chr_time)} claims 12-hour time format. Ensure AM/PM captured if needed.'))
    return(hm(chr_time))
  }

  if (chr_validation == 'time_mm_ss')
    return(ms(chr_time))

  fn_parse <- switch(
    chr_validation,
    date_dmy=ymd,
    date_mdy=ymd,
    date_ymd=ymd,
    datetime_dmy=ymd_hm,
    datetime_mdy=ymd_hm,
    datetime_ymd=ymd_hm,
    datetime_seconds_dmy=ymd_hms,
    datetime_seconds_mdy=ymd_hms,
    datetime_seconds_ymd=ymd_hms,
    stop(str_glue('The REDCap field {rcm_field(chr_time)} does not use date or datetime validation.'))
  )

  chr_annotation <- chr_time |> rcm_annotation()
  if (str_detect(chr_annotation, '@(NOW|TODAY)')) {

    if (tz != 'UTC' && str_detect(chr_annotation, '@(NOW|TODAY)-UTC'))
      warning(str_glue('The REDCap field {rcm_field(chr_time)} uses UTC field annotation but local timezone specified.'))

    else if (tz == 'UTC' && !str_detect(chr_annotation, '@(NOW|TODAY)-UTC'))
      warning(str_glue('The REDCap field {rcm_field(chr_time)} uses local timezone field annotation but UTC specified.'))

    else if (str_detect(chr_annotation, '@(NOW|TODAY)-SERVER'))
      warning(str_glue('The REDCap field {rcm_field(chr_time)} specifies server timezone in field annotation.'))

  } else if (tz == 'UTC') {
    warning(str_glue('The REDCap field {rcm_field(chr_time)} might not specify UTC timezone.'))
  }

  fn_parse(chr_time, tz=tz)
}
