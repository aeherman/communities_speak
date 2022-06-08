library(googledrive)
library(googlesheets4)
library(tidyverse)

source("code/default_setup.R")

tracker_id <- gs4_find() %>% filter(name == "tracker_sp22") %>% pull(id)

write_sheet(final_clean %>% filter(is.na(test_response)) %>% select(responseid, aid),
            tracker_id, "eme_fix")