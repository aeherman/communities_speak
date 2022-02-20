library(tidyverse)
source("~/communities_speak/code/thresholds.R")
final_clean <- read_csv("~/communities_speak/data/processed/final_clean.csv")
wrangled <- read_dta("~/communities_speak/data/output/wrangled20220218.dta")
today <- gsub("-|2022", "", Sys.Date())

#demographic

variables <- c("gen", "race", "hh_64_bi", "hh_ch_0_17_bi", "inc_a", "borough")

dems <- lapply(variables, function(col) {
  sym_col <- sym(col)
  
  if(is.character(wrangled[[col]])) {
    out <- wrangled %>%
      tidytext::unnest_tokens(output = !!sym_col,
                              token = "regex", input = !!sym_col, pattern = ";") %>%
      group_by(!!sym_col) %>% count %>% ungroup %>%
      mutate(n = paste0(round(n/sum(n)*100, digits = 2), "%"))
    
  } else if(sum(unique(wrangled[[col]]), na.rm = TRUE) == 1) {
    out <- wrangled %>% #group_by(!!sym_col) %>%
      summarize(n = as.character(paste0(round(mean(!!sym_col, na.rm = TRUE)*100, digits = 2), "%"))) %>% mutate(category = as.character(col))
    #colnames(out) <- as.character(col)
    
  } else {
    out <- wrangled %>%
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

write_csv(dems, "../data/output/demographics.csv")

update <-  final_clean %>%
  mutate(source = case_when(
    str_detect(source, "^(arabic|chinese|english)$") ~ source,
    TRUE ~ str_replace_all(source, "arabic|chinese|english|spanish|[:punct:]| ", "") 
  )) %>%
  #filter() %>%
  group_by(source, completion = completion >= min_completion) %>% count %>%
  pivot_wider(id_cols = source, names_from = completion, values_from = n) %>%
  transmute(responded_more_than_half = `TRUE`,
            total_responses = sum(`TRUE`, `FALSE`, na.rm = TRUE),
            proportion = responded_more_than_half/total_responses) %>%
  ungroup %>% filter(!is.na(responded_more_than_half))

status <- update %>% bind_rows(
  update %>% summarize(source = "total",
                       responded_more_than_half = sum(responded_more_than_half, na.rm = TRUE),
                       total_responses = sum(total_responses, na.rm = TRUE),
                       proportion = responded_more_than_half/total_responses)) %>%
  mutate(
    proportion = ifelse(is.na(proportion), proportion,
                        paste0(round(responded_more_than_half/total_responses*100,
                                     digits = 2), "%")),
    source = trimws(source))


sym_today <- sym(today)
track_id <- gs4_find() %>% filter(name == "Tracking Incoming Data") %>% pull(id)
range_write(ss = track_id, data = dems,
            sheet = "demographics", 
            range = "E1:F37",
            col_names = TRUE)
range_write(ss = track_id, data = status,
            sheet = "r_report",
            range = "K1:N14",
            col_names = TRUE)
#write_sheet(data = status, track_id, sheet = paste0("r_report", today))
#write_sheet(data = dems, track_id, sheet = paste0("demographics", today))
