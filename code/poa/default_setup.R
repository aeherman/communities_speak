# Default Setup for POA
setwd("~/communities_speak/code/")
library("tidyverse")
library("labelled")
source("functions/communities_speak_theme.R")
source("functions/make_plots.R")
source("thresholds.R")
poa <- readRDS("../data/output/poa.rds")
wrangled <- readRDS("../data/output/wrangled20220410.rds")

# census numbers
## only necessary to make city-wide estimates
## unideal for weight to be greater than 2 or 3 #%>%
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
    "hh_65_bi", # i
    "inc_dist", # j figure out if it is past or present
    "emp_status_before", # k
    "emp_status_after", # k
    "res_cat" # l
    )
names(demographics) <- demographics
