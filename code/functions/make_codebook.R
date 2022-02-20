# make_codebook
make_codebook <- function(df) {
  var_names <- names(df)
  
  var_types <- map_chr(df, function(col) paste(class(col), collapse = ", ")) %>%
    as_tibble() %>% mutate(variable = var_names) %>% rename(type = value)
  
  var_values <- lapply(colnames(df), function(col) {
    if(col %in% survey_codebook_labelled$label){
      row <- survey_codebook_labelled[survey_codebook_labelled$label == col, ]
      q <- row$q
      question <- row$text
      
    } else {
      q <- NA_character_
      question <- NA_character_
      #survey_q <- NA_integer_
    }

#for(col in colnames(df)) {
    variable <- as.character(col)  
    if(haven::is.labelled(df[[col]])) {
      pull_labels <- attributes(df[[col]])$labels
      
      value <- paste(pull_labels, collapse = "\n")
      label <- paste(names(pull_labels), collapse = "\n")
      
    } else if(col %in% c("responseid", "source", "duration")) {
      value <- "id column"
      label <- NA_character_
      
    } else if(col %in% survey_codebook_labelled$label){
      
      if(str_detect(as.character(col), "text")) {
        value <- "te"
        label <- NA_character_
      
        } else {
        value <- row$type
        label <- row$choices
      }
      
    } else {
      sub <- sort(unique(df[[col]][!is.na(df[[col]])]))
      value <- paste(sort(unique(sub)), collapse = "\n")
      if(length(sub) > 5) {
        value <- str_trunc(value, side = c("right"), width = 30)
      }
      if(anyNA(df[[col]])) {
        value <- paste(c(value, "NA"), collapse = "\n")
      }
      label <- NA_character_
    }
#    print(col)
#}
    
    return(tibble(q, variable, value, label, question))
  }) %>% reduce(bind_rows)
  
  codebook <- var_values %>% left_join(var_types, by = c("variable")) %>%
    select(q, variable, type, value, label, question)
  return(codebook)
}

codebook <- make_codebook(wrangled)

today <- gsub("-", "", Sys.Date())
View(codebook)

write_excel_csv(codebook, "~/communities_speak/data/codebook/codebook.csv")