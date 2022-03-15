# casting.R
# Typecast REDCap Fields to Usable R Types
#
# Maintained by Michael Pascale <mppascale@mgh.harvard.edu>
# Last modified: 2022-03-15


# FIXME: Not reimplmented for rcm_field type.

#' Cast date/time/datetime fields with lubridate.

rcm_time <- function (chr_time, tz='America/New_York') {
  checkmate::assert_character(chr_time)
  checkmate::assert(chr_time |> rcm_type() == 'text')

  fn_format <- switch(
    chr_time |> rcm_validation(),
    date_dmy=ymd,
    date_mdy=ymd,
    date_ymd=ymd,
    datetime_dmy=ymd_hm,
    datetime_mdy=ymd_hm,
    datetime_ymd=ymd_hm,
    datetime_seconds_dmy=ymd_hms,
    datetime_seconds_mdy=ymd_hms,
    datetime_seconds_ymd=ymd_hms,
    time=hm,
    time_mm_ss=hms,
    stop('REDCap field is not a date or datetime.')
  )

  chr_annotation <- chr_time |> rcm_annotation()
  if (str_detect(chr_annotation, '@(NOW|TODAY)')) {

    if (tz != 'UTC' && str_detect(chr_annotation, '@(NOW|TODAY)-UTC'))
      warning('REDCap field uses UTC field annotation but local timezone specified.')

    else if (tz == 'UTC' && !str_detect(chr_annotation, '@(NOW|TODAY)-UTC'))
      warning('REDCap field uses local timezone field annotation but UTC specified.')

    else if (str_detect(chr_annotation, '@(NOW|TODAY)-SERVER'))
      warning('REDCap field specifies server timezone in field annotation.')

  } else if (tz == 'UTC') {
    warning('REDCap field might not specify UTC timezone.')
  }

  fn_format(chr_time, tz=tz)
}

# FIXME: Not reimplmented for rcm_field type.

#' Cast numerically coded yes/no and true/false questions to logical.

rcm_logical <- function (int_logical) {
  checkmate::assert_integer(int_logical, lower=0, upper=1)
  checkmate::assert(int_logical |> rcm_type() %in% c('yesno', 'truefalse'))
  as.logical(int_logical)
}

# FIXME: Not reimplmented for rcm_field type.

#' Cast floating point numbers.

rcm_numeric <- function(dbl_number) {
  checkmate::assert_numeric()
  checkmate::assert()
}
