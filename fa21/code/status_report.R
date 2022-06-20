library(tidyverse)
source("~/communities_speak/code/thresholds.R")
load("~/communities_speak/data/processed/final_clean.rdata")
wrangled_today <- read_dta("~/communities_speak/data/output/wrangled20220225.dta")
today <- gsub("-|2022", "", Sys.Date())


#demographic

variables <- c("gen", "race", "hh_64_bi", "hh_ch_0_17_bi", "inc_a", "borough")

dems <- lapply(variables, function(col) {
  sym_col <- sym(col)
  
  if(is.character(wrangled_today[[col]])) {
    out <- wrangled_today %>%
      tidytext::unnest_tokens(output = !!sym_col,
                              token = "regex", input = !!sym_col, pattern = ";") %>%
      group_by(!!sym_col) %>% count %>% ungroup %>%
      mutate(n = paste0(round(n/sum(n)*100, digits = 2), "%"))
    
  } else if(sum(unique(wrangled_today[[col]]), na.rm = TRUE) == 1) {
    out <- wrangled_today %>% #group_by(!!sym_col) %>%
      summarize(n = as.character(paste0(round(mean(!!sym_col, na.rm = TRUE)*100, digits = 2), "%"))) %>% mutate(category = as.character(col))
    #colnames(out) <- as.character(col)
    
  } else {
    out <- wrangled_today %>%
      group_by(!!sym_col) %>% count %>% ungroup %>%
      mutate(n = paste0(round(n/sum(n)*100, digits = 2), "%"))
  }
  
  if(nrow(out) == 1) {
    return(out)
  } else {
    out %>% group_by(n) %>% transmute(category = stringr::str_to_title(haven::as_factor(!!sym_col)))
  }
  
  
}) %>% reduce(bind_rows) %>% select(category, n) %>% rename(prop = n)

colnames(dems) <- c("category", paste0("prop", today))

#write_csv(dems, "../data/output/demographics.csv")

update <-  final_clean %>%
  mutate(source = case_when(
    str_detect(source, "^(arabic|chinese|english|spanish)$") ~ source,
    TRUE ~ str_replace_all(source, "arabic|chinese|english|spanish|[:punct:]| ", "") 
  )) %>%
  #filter() %>%
  group_by(source, completion = completion >= min_completion, resi_ny = q2 == 1) %>% count %>%
  group_by(source) %>% mutate(total_responses = sum(n, na.rm = TRUE)) %>% filter(resi_ny) %>%
  pivot_wider(id_cols = c(source, total_responses), names_from = completion, values_from = n) %>%
  group_by(source, total_responses) %>%
  transmute(responded_more_than_half = `TRUE`,
            #total_responses = sum(`TRUE`, `FALSE`, na.rm = TRUE),
            proportion = responded_more_than_half/total_responses) %>%
  ungroup %>% filter(!is.na(responded_more_than_half))

status <- update %>% bind_rows(
  update %>% summarize(source = "total",
                       responded_more_than_half = sum(responded_more_than_half, na.rm = TRUE),
                       total_responses = sum(total_responses, na.rm = TRUE),
                       proportion = responded_more_than_half/total_responses)) %>%
  mutate(
    proportion = scales::percent(proportion),
    source = trimws(source)) %>% select(source, responded_more_than_half, total_responses, proportion)


sym_today <- sym(today)
track_id <- gs4_find() %>% filter(name == "Tracking Incoming Data") %>% pull(id)
#range_write(ss = track_id, data = dems,
#            sheet = "demographics", 
#            range = "E1:F37",
#            col_names = TRUE)
#range_write(ss = track_id, data = status,
#            sheet = "r_report",
#            range = "K1:N15",
#            col_names = TRUE)
#write_sheet(data = status, track_id, sheet = paste0("r_report", today))
#write_sheet(data = dems, track_id, sheet = paste0("demographics", today))
