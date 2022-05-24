# Default Setup for POA
library("tidyverse")
library("labelled")
source("communities_speak_theme.R")
source("../../code/functions/make_plots.R")
#poa <- readRDS("../data/output/poa.rds")
#wrangled <- readRDS("../data/output/wrangled20220410.rds")

# thresholds
min_duration <- 100 # 100 seconds
min_completion <- .5 # responded to 50% of the questions
poverty_line <- 2 # poverty line is 36.5K (top of category 2)
poverty_line_val <- 36500
median_inc <- 3 # median income level is 69.5K (top of category 3)
median_inc_val <- 69500


demographics <-
  c("borough", # a
    "decade", # b (from poa data)
    "gen", # c
    "race_census", # d
    "not_eng", # e
    "mar", # f
    "sch_level_cat", # g
    "hh_ch_0_17_bi", # h
    "hh_65_bi", # i
    "inc_dist", # j figure out if it is past or present
    "emp_status_before", # k
    "emp_status_after", # k
    "res_cat" # l
    )
names(demographics) <- demographics
