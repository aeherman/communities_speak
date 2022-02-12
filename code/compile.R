# compile
library(tidyverse)
library(googledrive)
library(googlesheets4)

ids <- gs4_find() %>% filter(str_detect(name, "Individual Survey")) %>% select(id, name) %>%
  mutate(name = str_replace_all(name, "Individual Survey- |_February.*", "")) %>%
  filter(name != "General Radio")

survey <- map2(ids$id, ids$name, ~read_sheet(.x, sheet = "Sheet1",
                                             na = c("NA", "", " ", "na", "Na", "n/a", "N/A", "N/a", "Not applicable")) %>%
                 filter(row_number() != 1) %>%
                 mutate_all(as.character) %>%
                 mutate(source = .y)) %>%
  reduce(bind_rows) %>% rename_all(str_to_lower) %>% mutate_all(str_to_lower) %>%
  rename(duration = "duration (in seconds)") %>%
  dplyr::select(responseid, source, duration, contains("q"), -q44, -q45) %>% na_if("null")

saveRDS(survey, "~/communities_speak/data/processed/survey.rds")

