wrangled

wrangled %>% select(contains("text"))

text_qs <- names(wrangled)[str_detect(names(wrangled), "text")]
qs <- str_replace_all(text_qs, "_text", "")

lapply(qs, function(col){
  wrangled[col] %>% mutate_at(col, haven::as_factor) %>% filter_at(col, ~str_detect(., "other") & !str_detect(., ";"))
})
