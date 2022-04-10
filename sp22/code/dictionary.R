#### Create Survey Dictionary for Spring 2022 Communities Speak Individual Survey ####
## Date: 9 April 2022

library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

setwd("~/communities_speak/sp22")

# This section would be for adding in labels in advance #### 

#id_var_labels <- gs4_find() %>% filter(name == "var_labels") %>% pull(id)
#var_dict <- id_var_labels %>%
#  read_sheet(sheet = "vars", na = c("na")) %>%
#  #group_by(no) %>%
#  mutate(survey_q = as.character(survey_q),
#         qid = paste0("qid", no + 1),
#         #q = ifelse(str_detect(helpnyc, "_"), paste(helpnyc, "text", sep = "_"), helpnyc)
#         ) %>%
#  select(qid, q, no, label)
#new_vars <- id_var_labels %>% read_sheet(sheet = "new_vars") %>% na.omit

file <- fromJSON(file = "~/communities_speak/sp22/data/input/individual_survey_s22.qsf")$SurveyElements
elements <- unlist(lapply(file, function(element) element$PrimaryAttribute))

# record survey question blocks ####
blocks <- lapply(file[[1]]$Payload, function(block) {
  block_title = block$Description
  qid <- lapply(block$BlockElements, function(element){
    type = element$Type
    if(type == "Question") {
      qid <- element$QuestionID
    } else {
      qid <- NULL
    }
    return(qid)
  }) %>% unlist
  
  return(tibble(block_title, qid))
}) %>% bind_rows() %>% mutate(across(everything(), stringr::str_to_lower))


index <- which(lapply(file, function(element) element$PrimaryAttribute) == "QID33")
element <- file[[index]]

# pull out question information ####
survey_codebook <- lapply(file[str_detect(elements, "QID")], function(element) {
#for (i in 13:61) {
  
  #element <- file[[index]]
  qid = element$PrimaryAttribute
  q = element$Payload$DataExportTag
  type = element$Payload$QuestionType
  selector = element$Payload$Selector
  subselector = element$Payload$SubSelector
  text = element$SecondaryAttribute
  
  unlisted = unlist(lapply(element$Payload$Choices, function(element) trimws(element$Display)))
  
 if(type == "Matrix"){
    part = unlisted
    unlisted = unlist(lapply(element$Payload$Answers, function(element) trimws(element$Display)))
  }  else {
    part = NA_character_
  }
  
  if("RecodeValues" %in% names(element$Payload)) {
    names(unlisted) <- element$Payload$RecodeValues
    unlisted <- unlisted[order(as.integer(names(unlisted)))]
  }
  
  choices = paste(unlisted, collapse = "; ")
  
  # handling free form entry
  if(selector == "FORM") {
    part = unlisted
    choices = "free form entry"
    options = NA_character_
  }
  
  if(length(unlisted) > 0){
    options = paste(as.integer(names(unlisted)), collapse = ",")
  } else {
    options = NA_character_
    choices = NA_character_
  }
  
  
  # text entry items
  text_entry = unlist(sapply(element$Payload$Choices, function(choice) choice$TextEntry))
  
  # label
  #index = which(var_dict$q == stringr::str_to_lower(q))
  #label = var_dict$label[index]
  
  if(all(text_entry == "true", !is.null(text_entry))) {
    if("RecodeValues" %in% names(element$Payload)) {
      translation <- element$Payload$RecodeValues
      index <- which(names(translation) == names(text_entry))
      names(text_entry) <- translation[index]
    }
    
    post = glue::glue("_{names(text_entry)}_text")
    q = paste0(q, c("", post))
    #label = c(label, glue::glue("{label}_text"))
    #qid = paste0(qid, c("", post))
  }
  
  out <- tibble(qid, q, type, selector, subselector, text, #label,
                part,
                options, choices)
  
  if(nrow(out) > 1 & any(text_entry != "true", is.null(text_entry))){
    out <- mutate(out, q = paste0(q, "_", row_number()))
  }
  
  return(out)
  #print(i)
}) %>% bind_rows() %>% mutate_all(str_to_lower) %>% filter(q != "") %>%
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         choices = ifelse(str_detect(q, "text"), NA, choices),
         options = ifelse(str_detect(q, "text"), NA, options)) %>%
  arrange(question) %>% left_join(blocks) %>% filter(!str_detect(block_title, "trash"))

simple <- survey_codebook %>% filter(type == "mc", selector == "savr", !str_detect(q, "text")) %>% pull(q)
text <- survey_codebook %>% filter(type == "te" | str_detect(q, "text")) %>% pull(q) # this should have the text questions in them
likert <- survey_codebook %>% filter(selector == "likert", subselector != "multipleanswer") %>% pull(q)
mavr <- survey_codebook %>% filter(type == "mc" & selector == "mavr" | subselector == "multipleanswer", !str_detect(q, "text")) %>% pull(q)


dummies <- survey_codebook %>% filter(q %in% mavr) %>%
  #mutate(to_label = ifelse(!is.na(part), part, choices)) %>%
  #filter(str_detect(q, "35"))
  tidytext::unnest_tokens(output = to_label, token = "regex", input = choices, pattern = ";", drop = FALSE) %>%
  group_by(q) %>%
  mutate(q = glue::glue("{q}_{row_number()}")) %>%
  bind_rows(survey_codebook) %>% arrange(question) %>%
  mutate(to_label = trimws(to_label))

dummies_to_label <- dummies %>% ungroup %>% select(part, to_label) %>% unlist %>% unique %>%
  as_tibble %>% rename(to_label = value) %>% filter(!is.na(to_label))

#write_sheet(dummies_to_label, id_var_labels, "dummies")

#from_r <- id_var_labels %>% read_sheet(sheet = "Copy of dummies", na = c("na"))
#from_r

#dummies_labelled <- dummies %>% left_join(from_r) %>% left_join(from_r, by = c("part" = "to_label")) %>%
#  mutate(sub_label = paste(na.omit(c(sub_label.y, sub_label.x)), collapse = "_")) %>% na_if("") %>%
#  select(-contains("label."), to_label)

#survey_codebook_labelled <- survey_codebook %>% filter(!q %in% dummies_labelled$q) %>%
#  bind_rows(dummies_labelled) %>% arrange(question) %>% group_by(q) %>% mutate(
#    label = paste(na.omit(c(label, sub_label)), collapse = "_")) %>% na_if("")

#column_names <- survey_codebook_labelled %>% select(q, label)
