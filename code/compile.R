# compile
library(tidyverse)
library(googledrive)
library(googlesheets4)

ids <- gs4_find() %>% filter(str_detect(name, "Individual Survey")) %>% select(id, name) %>%
  mutate(name = str_replace_all(name, "Individual Survey- |_February.*", "")) %>%
  filter(name != "General Radio")

listed <- map2(ids$id, ids$name, ~read_sheet(.x, sheet = "Sheet1",
                                             na = c("NA", "", " ", "na", "Na", "n/a", "N/A", "N/a", "Not applicable")) %>%
                        filter(row_number() != 1) %>%
                        mutate(source = .y) %>% 
                        rename_all(str_to_lower) %>%
                        mutate_at(vars(-recordeddate), ~str_to_lower(as.character(.))) %>%
                        #unnest(recordeddate) %>%
                        mutate_at(vars(recordeddate), as.list))

survey <- listed %>% reduce(bind_rows) %>%
  rename(duration = "duration (in seconds)") %>%
  mutate(q2 = ifelse(source == "prolific", "1", q2)) %>%
  dplyr::select(responseid, source, duration, recordeddate, progress, contains("q")) %>% na_if("null") %>%
  unnest(recordeddate)

general_radio_id <- gs4_find() %>% filter(str_detect(name, "General Radio")) %>% select(id, name) %>%
  mutate(name = str_replace_all(name, "Individual Survey- |_February.*", ""))

general_radio <- read_sheet(general_radio_id$id, sheet = "Sheet1", na = c("NA", "", " ", "na", "Na", "n/a", "N/A", "N/a", "Not applicable")) %>%
  filter(row_number() != 1) %>% mutate(source = general_radio_id$name) %>%
  rename_all(str_to_lower) %>% mutate_at(vars(-recordeddate), ~str_to_lower(as.character(.))) %>%
  rename(duration = "duration (in seconds)") %>%
  dplyr::select(responseid, source, duration, recordeddate, progress, contains("q")) %>% na_if("null") %>%
  unnest(recordeddate)

to_rename <- colnames(general_radio)[str_detect(colnames(general_radio), "[:digit:]")]
replacement <- as.character(as.integer(str_extract(to_rename, "[:digit:]{1,2}")) + 1)
new_names <- str_replace(to_rename, "[:digit:]{1,2}", replacement)
names(general_radio[to_rename]) <- new_names
names(general_radio)[match(to_rename, names(general_radio))] <- new_names

#na_to_numeric <- general_radio[colSums(is.na(general_radio)) == 10] %>% select(!contains("text")) %>% mutate_all(as.numeric)
#general_radio[colnames(na_to_numeric)] <- na_to_numeric

survey <- survey %>% bind_rows(general_radio)

saveRDS(survey, "~/communities_speak/data/processed/survey.rds")
