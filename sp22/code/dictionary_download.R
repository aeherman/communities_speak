
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

setwd("~/communities_speak/sp22")
load("data/processed/survey_codebook_types.rdata")
load("data/processed/survey_codebook_tolabel.rdata")


labelled_dummies <- id_labelling %>% read_sheet(sheet = "dummies_labelled", na = c("na", "NA", ""))
labelled_qs <- id_labelling %>% read_sheet(sheet = "qs_labelled", na = c("na", "NA", ""))

survey_codebook_labelled <- labelled_dummies %>%
  # to_label
  left_join(to_label) %>%
  left_join(labelled_qs %>% rename(q_stem = q) %>% select(q_stem, label)) %>%
  bind_rows(labelled_qs %>% filter(!q %in% labelled_dummies$q) %>%
              # survey_codebook
              left_join(survey_codebook)) %>%
  mutate(full_label = ifelse(is.na(sub_label), label, glue::glue("{label}_{sub_label}"))) %>%
  select(qid, q, full_label, type, selector, subselector, text, part, to_label, options, choices, block_title, question) %>%
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         origin = ifelse(q %in% labelled_dummies$q, "survey question dummy", "survey question")) %>%
  arrange(question)

new_vars <- id_labelling %>% read_sheet(sheet = "new_vars")# %>% na.omit

save(survey_codebook_labelled, new_vars, file = "data/processed/survey_codebook_labeled.rdata")
