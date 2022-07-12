
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

criterion <- rprojroot::has_file(".git/index")
root <- rprojroot::find_root_file("sp22", criterion = criterion)
setwd(root)

load("data/processed/survey_codebook_types.rdata")
load("data/processed/survey_codebook_toname.rdata")


named_dummies <- id_labeling %>% read_sheet(sheet = "dummies_named", na = c("na", "NA", ""))
named_qs <- id_labeling %>% read_sheet(sheet = "qs_named", na = c("na", "NA", ""))

survey_codebook_labelled <- named_dummies %>%
  # to_name
  left_join(to_name, by = c("qid", "q", "to_name")) %>%
  left_join(named_qs %>% rename(q_stem = q) %>% select(q_stem, name), by = "q_stem") %>%
  bind_rows(named_qs %>% filter(!q %in% named_dummies$q) %>%
              # survey_codebook
              left_join(survey_codebook, by = c("qid", "q", "block_title", "text", "part"))) %>%
  mutate(full_name = ifelse(is.na(sub_name), name, glue::glue("{name}_{sub_name}"))) %>%
  select(qid, q, full_name, type, selector, subselector, text, part, to_name, options, choices, block_title, question) %>%
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         origin = ifelse(q %in% named_dummies$q, "survey question dummy", "survey question")) %>%
  arrange(question)

new_vars <- id_labeling %>% read_sheet(sheet = "new_vars")# %>% na.omit

save(survey_codebook_labelled, new_vars, file = "data/processed/survey_codebook_labeled.rdata")
