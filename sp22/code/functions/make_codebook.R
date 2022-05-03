# make_codebook
make_codebook <- function(df) {
  var_names <- names(df)
  
  var_types <- map_chr(df, function(col) paste(class(col), collapse = ", ")) %>%
    as_tibble() %>% mutate(variable = var_names) %>% rename(type = value)
  
  var_values <- lapply(colnames(df), function(col) {
    if(col %in% survey_codebook_labelled$full_label){
      row <- survey_codebook_labelled[survey_codebook_labelled$full_label == col, ]
      q <- row$q
      description <- row$text
      
      part <- row$part
      if(!is.na(part)) {
        description <- glue::glue("{part}: {description}")
      }
      
    } else if (col %in% new_vars$var_name) {
      row <- new_vars[new_vars$var_name == col, ]
      q <- NA_character_
      description <- row$description
      #survey_q <- NA_integer_
    } else if (col %in% c("responseid", "recordeddate", "duration", "userlanguage", "source", "order")) {
      q <- "id"
      description <- "survey response metadata"
    } else {
      q <- NA_character_
      description <- NA_character_
    }

#for(col in colnames(df)) {
    variable <- as.character(col)  
    if(haven::is.labelled(df[[col]])) {
      pull_labels <- sort(attributes(df[[col]])$labels)
      
      value <- paste(pull_labels, collapse = "\n")
      label <- paste(names(pull_labels), collapse = "\n")
      
    } else if(col %in% c("responseid", "source", "duration")) {
      value <- "id column"
      label <- NA_character_
      
    } else if(col %in% survey_codebook_labelled$full_label){
      
      if(str_detect(as.character(col), "text")) {
        value <- "te"
        label <- NA_character_
      
        } else {
        value <- row$type
        label <- row$choices
      }
      
    } else {
      # edit spacing here
      sub <- sort(unique(df[[col]][!is.na(df[[col]])]))
      value <- paste(sort(unique(sub)), collapse = "; ")
      if(length(sub) > 5 & !is.numeric(sub)) {
        value <- str_trunc(value, side = c("right"), width = 30)
      } else if (is.numeric(sub)) {
        value <- paste(range(sub), collapse = " to ")
      }
      
      if(anyNA(df[[col]])) {
        value <- paste(c(value, "NA"), collapse = "; ")
      }
      label <- NA_character_
    }
#    print(col)
#}
    
    return(tibble(q, variable, value, label, description))
  }) %>% reduce(bind_rows)
  
  codebook <- var_values %>% left_join(var_types, by = c("variable")) %>%
    select(q, variable, type, value, label, description)
  return(codebook)
}

codebook <- make_codebook(wrangled)

today <- gsub("-", "", Sys.Date())
View(codebook)
write_excel_csv(codebook, glue::glue("~/communities_speak/sp22/data/output/codebook.csv"))
