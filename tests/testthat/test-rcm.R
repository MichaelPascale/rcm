
df_data <- read.csv('../../data/data-example.csv')
df_dictionary <- read.csv('../../data/dictionary-example.csv')
df_form_event_map <- read.csv('../../data/form-event-mappings-example.csv')

test_that('rcm generates object with class rcm_data',
          {
            df_rc <- rcm(df_data, df_dictionary)
            expect_data_frame(df_rc)
          }
)

test_that('list by form',
          {
            df_rc <- rcm(df_data, df_dictionary)
            df_rc2 <- rcm(df_data, df_dictionary, df_form_event_map)

            expect_setequal(
              rcm_arms_by_form(df_rc2, 'test_data_types'),
              df_form_event_map[,1] |> unique()
            )
            expect_setequal(
              rcm_events_by_form(df_rc2, 'test_data_types'),
              df_form_event_map[,2] |> unique()
            )
            expect_setequal(
              rcm_fields_by_form(df_rc2, 'test_data_types'),
              df_dictionary[,1] |> unique()
            )
          }
)

test_that('list by event',
          {
            df_rc <- rcm(df_data, df_dictionary)
            df_rc2 <- rcm(df_data, df_dictionary, df_form_event_map)

            expect_setequal(
              rcm_arms_by_event(df_rc2, 'event_2_arm_1'),
              1
            )
            expect_setequal(
              rcm_forms_by_event(df_rc2, 'event_2_arm_1'),
              'test_data_types'
            )
            expect_setequal(
              rcm_fields_by_event(df_rc2, 'event_2_arm_1'),
              df_dictionary[,1] |> unique()
            )
          }
)

test_that('list by arm',
          {
            df_rc <- rcm(df_data, df_dictionary)
            df_rc2 <- rcm(df_data, df_dictionary, df_form_event_map)

            expect_setequal(
              rcm_events_by_arm(df_rc2, 1),
              df_form_event_map[df_form_event_map[,1] == 1, 2]
            )
            expect_setequal(
              rcm_forms_by_arm(df_rc2, 1),
              df_form_event_map[df_form_event_map[,1] == 1, 3] |> unique()
            )
            expect_setequal(
              rcm_fields_by_arm(df_rc2, 1),
              df_dictionary[,1] |> unique()
            )
          }
)

test_that('type conversion to integer',
          {
            df_rc <- rcm(df_data, df_dictionary)
            expect_integer(
              rcm_to_integer(df_rc$test_integer)
            )
            expect_error(
              rcm_to_integer(df_rc$test_number)
            )
            expect_error(
              rcm_to_integer(df_rc$test_date_dmy)
            )
          }
)

test_that('type conversion to logical',
          {
            df_rc <- rcm(df_data, df_dictionary)
            expect_logical(
              rcm_to_logical(df_rc$test_yesno)
            )
            expect_logical(
              rcm_to_logical(df_rc$test_truefalse)
            )
            expect_error(
              rcm_to_logical(df_rc$test_integer)
            )
          }
)

test_that('type conversion to date/datetime',
          {
            df_rc <- rcm(df_data, df_dictionary)
            expect_posixct(rcm_to_time(df_rc$test_date_dmy))
            expect_posixct(rcm_to_time(df_rc$test_date_mdy))
            expect_posixct(rcm_to_time(df_rc$test_date_ymd))

            expect_posixct(rcm_to_time(df_rc$test_date_dmyhm))
            expect_posixct(rcm_to_time(df_rc$test_date_mdyhm))
            expect_posixct(rcm_to_time(df_rc$test_date_ymdhm))

            expect_posixct(rcm_to_time(df_rc$test_date_dmyhms))
            expect_posixct(rcm_to_time(df_rc$test_date_mdyhms))
            expect_posixct(rcm_to_time(df_rc$test_date_ymdhms))

            expect_warning(rcm_to_time(df_rc$test_time_hm))
            expect(lubridate::is.period(rcm_to_time(df_rc$test_time_hm, tz='UTC')), 'Expected a lubridate::period.')
            expect(lubridate::is.period(rcm_to_time(df_rc$test_time_ms)), 'Expected a lubridate::period.')

          }
)
