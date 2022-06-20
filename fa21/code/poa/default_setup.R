# Default Setup for POA
#setwd("code/")
wd <- getwd()
print(wd)
#setwd("../")
library("tidyverse")
library("labelled")
source("functions/communities_speak_theme.R")
source("functions/make_plots.R")
source("thresholds.R")
poa <- readRDS("../data/output/poa.rds")
getwd()

listed <- list.files("../data/output/")
date <- max(str_extract(listed[str_detect(listed, "wrangled")], "[:digit:]+"))
wrangled <- readRDS(glue::glue("../data/output/wrangled{date}.rds"))

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
