# thresholds
min_duration <- 100 # 100 seconds
min_completion <- .5 # responded to 50% of the questions
poverty_line <- 5
median_inc <- 8

# census numbers
## only necessary to make city-wide estimates
## unideal for weight to be greater than 2 or 3
census <- tribble(~race, ~target,
                  "asian", .14,
                  "black or african american", .241,
                  "hispanic or latinx", .291,
                  "white (non-hispanic or latino)", .321,
                  "american indian or alaska native", .01,
                  "hawaiin or pacific islander", .001,
                  "two or more races", .027) %>%
  mutate(col_name = glue::glue("race_{race}")) %>% arrange(race)
