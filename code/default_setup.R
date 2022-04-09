# Default Setup for POA
library("tidyverse")
library("labelled")
source("functions/communities_speak_theme.R")
source("functions/make_plots.R")
poa <- readRDS("../data/output/poa.rds")

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
                  "two or more races", .027) #%>%
  #mutate(col_name = glue::glue("race_{race}")) %>% arrange(race)

demographics <-
  c("borough", # a
    "decade", # b (from poa data)
    "gen", # c
    "race_census", # d
    "not_eng", # e
    "mar", # f
    "sch_level_cat", # g
    "hh_ch_0_17_bi", # h
    "hh_64_bi", # i
    "inc_dist", # j figure out if it is past or present
    "emp_status_before", # k
    "emp_status_after", # k
    "res_cat" # l
    )
names(demographics) <- demographics
