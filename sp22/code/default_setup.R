# Default Setup for POA
library("tidyverse")
library("labelled")
library("rprojroot")

criterion <- rprojroot::has_file(".git/index")
root <- find_root_file("sp22", criterion = criterion)

# code
source(file.path(root, "code/communities_speak_theme.R"))
source(file.path(root, "code/functions/make_plots.R"))

# data
codebook <- read_csv(file.path(root, "data/output/codebook.csv"), show_col_types = FALSE)
load(file.path(root, "data/processed/survey_codebook_labeled.rdata"))
wrangled <- readRDS(file.path(root, "data/output/wrangled.rds"))
load(file.path(root, "data/processed/cleaned.rdata"))

# thresholds
min_duration <- 100 # 100 seconds
min_completion <- .5 # responded to 50% of the questions
poverty_line <- 2 # poverty line is 36.5K (top of category 2)
poverty_line_val <- 36500
median_inc <- 3 # median income level is 69.5K (top of category 3)
median_inc_val <- 69500


demographics <-
  c("borough", # a
    "gen", # c
    "race_census", # d
    "hh_ch_0_17_bi", # h
    "hh_sn_65_bi", # i
    "inc_dist" # j figure out if it is past or present
    )
names(demographics) <- demographics
