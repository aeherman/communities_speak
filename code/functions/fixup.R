dict <- tribble(~pattern, ~replacement,
                # replace with 0
                "o", "0",
                "no", "0",
                "none", "0",
                "ninguna", "0",
                "zero", "0",
                # replace with 1
                "one", "1",
                "si", "1", # not sure if I should interpret yes as "1"
                "adult 62", "1", # don't like hardcoding...
                "8 months", "1",
                "7ys", "1",
                "poorly worded 0 in addition to myself", "1",
                # replace with 2
                "42y51ys", "2",
                "two", "2",
                "n/a", NA_character_,
                # replace with values
                "i2", "82", # validated by looking at insurance (medicare) and household question
                "t6", "56", # validated by cross checking with household question
                "fifty seven", "57",
                # "u" validated by checking highest level of school
                ) %>%
  mutate(pattern = str_c("^", pattern, "$"))

fixup <- function(string = NULL, pattern = NULL) {
  if(!is.na(string)) {
    index <- which(str_detect(string, pattern = dict$pattern))
    out <- dict$replacement[index]
    
  } else {
    out <- NA_character_
    return(out)
  }
}