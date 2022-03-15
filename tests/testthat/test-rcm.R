
df_data <- read.csv('../../data/data-example.csv')
df_dictionary <- read.csv('../../data/dictionary-example.csv')

test_that('rcm generates object with class rcm_data',
          {
            dfx <- rcm(df_data, df_dictionary)


            expect(rcm_field(dfx, 'test_date_dmy') == rcm_field(dfx$test_date_dmy), 'rcm_field variations produce matching output')
            expect(rcm_field(dfx, 'test_date_dmy') == 'test_date_dmy', 'rcm_field produces correct output')

            expect(rcm_type(dfx, 'test_date_ymdhms') == rcm_type(dfx$test_date_ymdhms), 'rcm_type variations produce matching output')
            expect(rcm_type(dfx, 'test_date_ymdhms') == 'text', 'rcm_type produces correct output')

          }
)
