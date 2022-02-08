# make_codebook
make_codebook <- function(df) {
  var_names <- names(df)
  
  #lapply(df %>% dplyr::select_if(is.list), function(col) unnest(col))
  var_types <- map_chr(df, function(col) paste(class(col), collapse = ", "))
  
  cols_as_lists <- lapply(df, unique)
  var_lengths <- sapply(cols_as_lists, length) #%>% map_chr(format)
  
  var_values <- lapply(cols_as_lists, function(element) {
    
    # 1. if type is numeric
    if(is.numeric(element) & length(unique(element)) < 20) {
      value <- paste(sort(unique(element)), collapse = "\n")
      #value <- paste0(min(element, na.rm = TRUE), " to ", max(element, na.rm = TRUE))
      
      # 2. if type is many really long strings
    } else if(any(str_length(element) > 10 & !is.na(element) | length(element) >= 5)) {
      sub <- element[!is.na(element)][1:3]
      trunc <- str_trunc(sub, side = c("right"), width = 30)
      char <- paste0("'", trunc, "'")
      value <- paste(char, collapse = ", ")
      
      # 3. all other
    } else {
      value <- paste(sort(unique(element)), collapse = ", ")
    }
    
    if(anyNA(element)) {
      value <- paste(c(value, "NA"), collapse = "\n")
    }
    return(value)
  }) %>% map_chr(format)
  
  codebook <- tibble(variable = var_names,
                     type = var_types,
                     unique_values = var_lengths,
                     values = var_values)
  return(codebook)
}


sub <- survey_codebook_labelled %>% select(label, q, text, choices) %>% rename(variable = label) %>%
  mutate(choices = str_replace_all(choices, "; ", "\n"))
codebook <- make_codebook(wrangled) %>% left_join(sub)
View(codebook)
write_excel_csv(codebook, "../data/codebook.csv")
drive_upload(media = paste0("../data/codebook.csv"),
             path = paste0(googledrive_path, "data/output/codebook", today), type = "spreadsheet")

     