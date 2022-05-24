# upload

today <- gsub("-", "", Sys.Date())

googledrive_path <- "Communities Speak/Subteams/Data Subteam/cleaning/sp22/"
project_path <- "~/communities_speak/sp22/"


#drive_upload(media = paste0("~/communities_speak/code/functions/make_plots.R"), path = paste0(googledrive_path, "code/functions/make_plots", today), overwrite = TRUE)

# upload data/processed

lapply(list.files(glue::glue("{project_path}data/processed"), full.names = TRUE), function(file) {
  split <- unlist(str_split(file, pattern = "[:punct:]"))
  name <- nth(split, -2)
  ending <- last(split)
  
  spreadsheet <- NULL
  if(ending == "csv") {
    spreadsheet <- "spreadsheet"
  }
  
  drive_upload(media = file, path = glue::glue("{googledrive_path}data/processed/{name}{today}.{ending}"),
               type = spreadsheet)
  
})

# upload data/output

lapply(list.files(glue::glue("{project_path}data/output"), full.names = TRUE), function(file) {
  split <- unlist(str_split(file, pattern = "[:punct:]"))
  name <- nth(split, -2)
  ending <- last(split)
  
  if(ending == "csv") {
    spreadsheet <- "spreadsheet"
  } else {
    spreadsheet <- NULL
  }
  
  drive_upload(media = file, path = glue::glue("{googledrive_path}data/output/{name}{today}.{ending}"),
               type = spreadsheet)
})

# upload code/functions

lapply(list.files(glue::glue("{project_path}code/functions"), full.names = TRUE), function(file){
  partial <- last(unlist(str_split(file, pattern = "/")))
  drive_upload(media = file, path = glue::glue("{googledrive_path}code/functions/{partial}"), overwrite = TRUE) 
})

drive_upload(media = glue::glue("{project_path}code/cleaning.Rmd"), path = glue::glue("{googledrive_path}code/cleaning{today}.Rmd"), overwrite = TRUE) 
