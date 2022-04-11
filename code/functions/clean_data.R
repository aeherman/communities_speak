# must run dictionary first
#source("~/communities_speak/code/dictionary.R")

clean_data <- function(df = survey, col = NULL) {
  by_col <- lapply(setdiff(colnames(df), c("responseid", "recordeddate")), function(col) {
    
    index <- which(survey_codebook$q == col)
    values <- as.integer(unlist(stringr::str_split(survey_codebook$options[index], pattern = "[:punct:]")))
    tags <- unlist(str_split(survey_codebook$choices[index], "; "))
    
    recode_na <- str_detect(tags, "not applicable")
    
    if(any(all.equal(values, c(1,2)) == TRUE, !is.na(recode_na) & recode_na)) {
      df[[col]] <- 2 - as.integer(df[[col]] %>%
                                    # hard code 3 - not applicable
                                    na_if("3"))
      values <- (2 - values) %>% na_if(-1)
      
    }
    
    named <- setNames(values, tags) %>% na.omit
    
    if(col %in% c(likert, simple)) {
      out <- df[c("responseid", col)] %>%
        mutate_at(vars(col), ~labelled(as.integer(.), named))
      
      # multiple answer  
    } else if(col %in% mavr){
      
      sym_col <- sym(col)
      out <- df[c("responseid", col)] %>%
        fastDummies::dummy_cols(col, split = ",", ignore_na = TRUE) %>%
        tidytext::unnest_tokens(output = !!sym_col, token = "regex",
                                input = col, pattern = ",") %>%
        mutate_at(col, ~factor(as.integer(.), levels = values, labels = tags)) %>%
        group_by(responseid) %>% mutate_at(col, ~paste(., collapse = ";")) %>%
        distinct %>% na_if("NA")
      
      
      # don't double count prefer not to answer
      if(any(str_detect(tags, "prefer not to answer"))) {
        pnta <- which(names(named) == "prefer not to answer")
        pnta_col <- paste(c(col, pnta), collapse = "_")
        out <- out %>% mutate_at(pnta_col,
                                 funs(ifelse(str_detect(!!sym_col, "prefer not to answer;|;prefer not to answer"),
                                             0, !!sym(pnta_col)))) %>%
          mutate_at(col, ~str_replace(., "prefer not to answer;|;prefer not to answer", ""))
      }
      
      # haven label dummies
      cols_to_label <- out %>% ungroup %>% select_if(is.numeric) %>% colnames
      relabelled <- lapply(cols_to_label, function(dummy){
        i <- as.integer(str_replace(dummy, paste0(col, "_"), ""))
        values <- c(0, 1)
        names(values) <- c(paste("not", tags[i]), tags[i])
        out %>% ungroup %>% transmute_at(dummy, ~labelled(as.integer(.), values))
      }) %>% reduce(bind_cols)
      
      out[cols_to_label] <- relabelled
      
      #r_zaiyctblk6t1z33
    } else if(all(!str_detect(df[[col]], "[:alpha:]"), na.rm = TRUE) & any(!is.na(df[[col]]))) {
      out <- df[c("responseid", col)] %>% mutate_at(vars(col), as.integer)
    } else {
      out <- df[c("responseid", col)]  
    }
    
    # print(col)   
    #}    
    return(out)
  })
  
  by_col %>% reduce(full_join, by = c("responseid")) %>%
    ### replace specific patterns in age and household questions
    mutate_at(vars(contains("25"), "q5"), ~str_replace_all(., pattern = str_c(dict$pattern, collapse = "|"), replacement = fixup)) %>%
    ## transform age to double
    #mutate(q5 = lapply(df[["q5"]], function(element){
    # average age range if provided instead of an age
    #  if(str_detect(element, "[:digit:]{2}.*[:digit:]{2}") & !is.na(element)) {
    #    round(mean(as.integer(unlist(str_split(element, "[:punct:]")))))
    #  } else {
    # remove text data and transform to integer 
    #    as.double(str_extract(element, "[:digit:]{1,2}"))
    #  }
    #}) %>% unlist) %>%
    ## transform household
  mutate(across(c(contains("25"), q5), ~as.double(str_extract(., "[:digit:]{1,2}"))))
  
}

survey
final_clean <- survey[c('responseid', 'recordeddate')] %>% left_join(clean_data()) %>% left_join(distinct(boroughs), by = c("q3" = "zipcode"))

# all NA if any NA
q35_sub <- final_clean %>% select(contains("35"))
q35_sub %>% transmute(mean = rowMeans(is.na(q35_sub))) %>% table


completion <- lapply(final_clean$responseid, function(id) {
  #for(id in final_clean$responseid) {ghts
  row <- final_clean[final_clean$responseid == id, ] %>% select(contains("q")) %>%
    select(!matches("_"), contains("q25"), q41_1, q35_1, q34_1, q33_1)
  
  
  # employment logic
  if(!str_detect(row$q16, "unemp") | is.na(row$q16)) { # respondents who didn't mark unemployed
    row <- row %>% select(-q17, -q18)
  } else if(labelled::to_factor(row$q17) != "yes") {
    row <- row %>% select(-q18)
  }
  
  # household with children logic
  if(sum(row$q25_3, row$q25_4, na.rm = TRUE) == 0) { # respondents that don't have children
    row <- row %>% select(-q26, -q27, -q28, -q29, -q30, -q31)
  } else if(str_detect(row$q26, "home") & !str_detect(row$q26, ";") | is.na(row$q26)) { # respondents that only selected home school
    row <- row %>% select(-q27, -q28)
  } else if(row$q27 == 0 | is.na(row$q27)) { # respondents whose children aren't attending school in person
    row <- row %>% select(-q28)
  }
  
  if(row$q39 != 1 | is.na(row$q39)) {
    row <- row %>% select(-q40)
  }
  # residence logic
  ## will need to disqualify respondents not just based on completion, but also...
  if(row$q2 == 0 & !is.na(row$q2)) { # goes to the end because it removes all the columns (can't test the other conditions)
    row <- row %>% select(q2)
  }
  
  completion <- mean(!is.na(row))
  return(completion)
}) %>% unlist

final_clean$completion <- completion

hist(final_clean$completion)
mean(!is.na(final_clean$q2))
save(final_clean, file = "../data/processed/final_clean.rdata")