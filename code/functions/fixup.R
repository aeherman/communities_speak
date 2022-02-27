dict <- tribble(~pattern, ~replacement,
                "one", "1",
                "o", "0",
                "two", "2",
                "none", "0",
                "ninguna", "0",
                "zero", "0",
                "si", "1", # not sure if I should interpret yes as "1"
                "adult 62", "1", # don't like hardcoding...
                "n/a", NA_character_) %>%
  mutate(pattern = str_c("\\b", pattern, "\\b"))

fixup <- function(string = NULL, pattern = NULL) {
  if(!is.na(string)) {
    index <- which(str_detect(string, pattern = dict$pattern))
    out <- dict$replacement[index]
    
  } else {
    out <- NA_character_
    return(out)
  }
}