#### Create Survey Dictionary for Spring 2022 Communities Speak Individual Survey ####
## Date: 9 April 2022

library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

setwd("~/communities_speak/sp22")

# This section would be for adding in labels in advance #### 
id_labelling <- gs4_find() %>% filter(name == "labelling") %>% pull(id)

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


index <- which(lapply(file, function(element) element$PrimaryAttribute) == "QID35")
element <- file[[index]]

# pull out question information ####
survey_codebook <- lapply(file[str_detect(elements, "QID")], function(element) {
#for (i in 23:71) {
  
  #element <- file[[i]]
  qid = element$PrimaryAttribute
  q = element$Payload$DataExportTag
  type = element$Payload$QuestionType
  selector = element$Payload$Selector
  subselector = element$Payload$SubSelector
  text = stringr::str_replace_all(element$Payload$QuestionText, c("<.*?>" = "", "&nbsp;" = " "))
  
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
    out <- out %>%
      mutate(temp = ifelse(type == "Matrix", names(part), names(unlisted)),
             q = glue::glue("{q}_{temp}")) %>% select(-temp)
    #out <- mutate(out, q = paste0(q, "_", row_number()))
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


to_label <-
  # filter for variables to be dummied
  survey_codebook %>% filter(q %in% mavr) %>%
  # list them out in long format
  tidytext::unnest_tokens(output = to_label, token = "regex", input = choices, pattern = ";", drop = FALSE) %>%
  group_by(q) %>%
  # number them according to their coding in qualtrics, but leave behind the q_stem for later merging
  mutate(q_stem = q, q = glue::glue("{q}_{row_number()}")) %>%
  mutate(to_label = trimws(to_label))

# label question stems
qs_to_label <- survey_codebook %>% select(qid, q, block_title, text, part)
# label dummy variable labels
dummies_to_label <- to_label %>% ungroup %>% select(qid, q, to_label) %>% unique %>% filter(!is.na(to_label))

write_sheet(qs_to_label, id_labelling, "qs_to_label")
write_sheet(dummies_to_label, id_labelling, "dummies_to_label")

# hand write in variables

labelled_dummies <- id_labelling %>% read_sheet(sheet = "dummies_labelled", na = c("na", "NA", ""))
labelled_qs <- id_labelling %>% read_sheet(sheet = "qs_labelled", na = c("na", "NA", ""))

survey_codebook_labelled <- labelled_dummies %>% left_join(to_label) %>%
  left_join(labelled_qs %>% rename(q_stem = q) %>% select(q_stem, label)) %>%
  bind_rows(labelled_qs %>% filter(!q %in% labelled_dummies$q) %>% left_join(survey_codebook)) %>%
  mutate(full_label = ifelse(is.na(sub_label), label, glue::glue("{label}_{sub_label}"))) %>%
  select(qid, q, full_label, type, selector, subselector, text, part, to_label, options, choices, block_title, question) %>%
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}"))) %>%
  arrange(question)

view(survey_codebook_labelled)

new_vars <- id_labelling %>% read_sheet(sheet = "new_vars")# %>% na.omit
