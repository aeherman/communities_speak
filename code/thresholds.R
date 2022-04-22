# thresholds
min_duration <- 100 # 100 seconds
min_completion <- .5 # responded to 50% of the questions
poverty_line <- 5 # poverty level is $37K: at or below poverty line
median_inc <- 8 # median income level is $67K: at or below median income level

census <- tribble(~race, ~target,
                  "asian", .14,
                  "black or african american", .241,
                  "hispanic or latinx", .291,
                  "white (non-hispanic or latino)", .321,
                  "american indian or alaska native", .01,
                  "hawaiin or pacific islander", .001,
                  "two or more races", .027)