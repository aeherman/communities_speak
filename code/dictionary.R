library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

setwd("~/communities_speak/code")

id_var_labels <- gs4_find() %>% filter(name == "var_labels") %>% pull(id)
var_dict <- id_var_labels %>%
  read_sheet(sheet = "vars", na = c("na")) %>%
  #group_by(no) %>%
  mutate(survey_q = as.character(survey_q),
         qid = paste0("qid", no + 1),
         #q = ifelse(str_detect(helpnyc, "_"), paste(helpnyc, "text", sep = "_"), helpnyc)
         ) %>%
  select(qid, q, no, label)

fileNYC <- fromJSON(file = "../data/input/Individual_Survey-_English_-_helpNYC.qsf")
file <- fromJSON(file = "../data/input/Individual_Survey-_English_-_helpNYC_1.qsf")
elementsNYC <- lapply(fileNYC$SurveyElements, function(element) element$PrimaryAttribute)
elements <- lapply(file$SurveyElements, function(element) element$PrimaryAttribute)

# make survey codebook
#element <- file$SurveyElements[[43]]
#element <- file$SurveyElements[[52]]# 9:53
index <- which(lapply(file$SurveyElements, function(element) element$PrimaryAttribute) == "QID16")
element <- file$SurveyElements[[index]]

survey_codebook <- lapply(file$SurveyElements[9:54], function(element) {
#for (i in 9:54) {
#  element <- file$SurveyElements[[i]]
  qid = element$PrimaryAttribute
  q = element$Payload$DataExportTag
  type = element$Payload$QuestionType
  selector = element$Payload$Selector
  subselector = element$Payload$SubSelector
  text = element$SecondaryAttribute
  
  unlisted = unlist(lapply(element$Payload$Choices, function(element) trimws(element$Display)))
  
    #
  #unlist(element$Payload$RecodeValues)
  #
  
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
  index = which(var_dict$q == stringr::str_to_lower(q))
  label = var_dict$label[index]
  
  if(!is.null(text_entry)) {
    if("RecodeValues" %in% names(element$Payload)) {
      translation <- element$Payload$RecodeValues
      index <- which(names(translation) == names(text_entry))
      names(text_entry) <- translation[index]
    }
    
    post = glue::glue("_{names(text_entry)}_text")
    q = paste0(q, c("", post))
    label = c(label, glue::glue("{label}_text"))
    #qid = paste0(qid, c("", post))
  }
  
  out <- tibble(qid, q, type, selector, subselector, text, label, part, options, choices)
  
  if(nrow(out) > 1 & is.null(text_entry)){
    out <- mutate(out, q = paste0(q, "_", row_number()))
  }
  
  return(out)
  #print(i)
}) %>% bind_rows() %>% mutate_all(str_to_lower) %>% filter(q != "") %>%
  #mutate(options = as.integer(options)) %>%
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         choices = ifelse(str_detect(q, "text"), NA, choices)) %>%
  arrange(question) #%>%
  # combine dataframes

# to_label <-
#to_label <- var_dict %>% left_join(survey_codebook, by = c("qid")) %>%
#  filter(str_detect(notes, "subquestions")) %>%
#  select(-q.x) %>% rename(q = q.y) %>% bind_rows(
#    survey_codebook %>%
#      full_join(var_dict, by = c("q", "qid")) %>%
#      filter(str_detect(notes, "options|first word"))) %>%
#  tidytext::unnest_tokens(output = choices_unnest,
#                          token = "regex", input = choices, pattern = ";") %>%
#    mutate_at(vars(choices_unnest), trimws) %>%
#    mutate(choices_to_label = str_replace(word(choices_unnest), "[:punct:]", ""),
#           choices_to_label = str_replace(choices_to_label, "dont", "dk")) %>%
#    #filter(str_detect(notes, "label")) %>%
#  arrange(no) %>%
#  filter(!str_detect(q, "text")) %>%
#  select(survey_q, q, label, text, choices_unnest, choices_to_label)
  

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

write_sheet(dummies_to_label, id_var_labels, "dummies")
write_sheet(to_label, id_var_labels, "from_r")

from_r <- id_var_labels %>% read_sheet(sheet = "Copy of dummies", na = c("na"))
from_r

dummies_labelled <- dummies %>% left_join(from_r) %>% left_join(from_r, by = c("part" = "to_label")) %>%
  mutate(sub_label = paste(na.omit(c(sub_label.y, sub_label.x)), collapse = "_")) %>% na_if("") %>%
  select(-contains("label."), to_label)

survey_codebook_labelled <- survey_codebook %>% filter(!q %in% dummies_labelled$q) %>%
  bind_rows(dummies_labelled) %>% arrange(question) %>% group_by(q) %>% mutate(
    label = paste(na.omit(c(label, sub_label)), collapse = "_")) %>% na_if("")

column_names <- survey_codebook_labelled %>% select(q, label)
