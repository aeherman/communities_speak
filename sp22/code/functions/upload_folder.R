upload_folder <- function(googledrive_path = "Communities Speak/Subteams/Data Subteam/cleaning/sp22",
                          project_path = rprojroot::find_root_file("sp22",
                                                                   criterion = rprojroot::has_file(".git/index")),
                          today = gsub("-", "", Sys.Date()),
                          pattern. = NULL,
                          file_path)
  {
  
  # error handling
  if(str_sub(file_path, -1) == "/") {
    message("File path must point to a folder without slash '/'")
    stop(str_sub(file_path, -1) == "/")
  }
  
  files <- list.files(glue::glue("{project_path}/{file_path}"), full.names = TRUE)
  
  if(!is.null(pattern.)) {
    files <- grep(files, pattern = pattern., value = TRUE)
  }
  
  lapply(files, function(file) {
    split <- unlist(stringr::str_split(file, pattern = "\\.|\\/"))
    name <- str_replace_all(dplyr::nth(split, -2),
                            c("survey_codebook_" = ""))
    ending <- dplyr::last(split)
    modified <- stringr::str_replace_all(as.Date(file.info(file)$mtime), "-", "")
    
    spreadsheet <- NULL
    if(ending == "csv") {
      spreadsheet <- "spreadsheet"
    }
    
    if(today > modified) {
      message(glue::glue("{name} last modified on {modified}."))
      return(NULL)
    } else {
      drive_upload(media = file,
                   path = glue::glue("{googledrive_path}/{file_path}/{name}{today}.{ending}"),
                   type = spreadsheet)
    }
    
  })
  
}