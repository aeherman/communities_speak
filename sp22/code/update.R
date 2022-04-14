lang <- tribble(~userlanguage, ~language,
                "en", "English",
                "fr", "French",
                "zh-s", "Simplified Chinese",
                "es-es", "Spanish",
                "ar", "Arabic")

total <- final_clean %>%
  summarize(source = "total",
            language = NA_character_,
            total = sum(!is.na(valid), na.rm = TRUE),
            #`valid responses` = sum(valid, na.rm = TRUE),
            percent = scales::percent(mean(valid, na.rm = TRUE)))

final_clean %>% filter(is.na(valid)) %>% 
  group_by(source, userlanguage) %>%
  summarize(percent = scales::percent(mean(valid, na.rm = TRUE)),
            total = n(),
            `valid responses` = sum(valid, na.rm = TRUE)) %>%
  left_join(lang) %>% select(source, language, `valid responses`, total, percent) %>%
  bind_rows(total)


final_clean %>% transmute(week = lubridate::week(recordeddate)-14) %>%
  group_by(source) %>% count %>%
  ggplot


