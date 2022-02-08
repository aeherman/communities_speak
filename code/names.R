library(googlesheets4)
library(googledrive)
library(tidyverse)

var_dict <- gs4_find() %>% filter(name == "var_labels") %>% pull(id) %>%
  read_sheet(sheet = "vars", na = c("na")) %>%
  mutate(survey_q = as.character(survey_q),
         qid = paste0("qid", q_no + 1))