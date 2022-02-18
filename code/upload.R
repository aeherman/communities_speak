# upload
today <- gsub("-", "", Sys.Date())

googledrive_path <- "Communities Speak/Subteams/Data Subteam/Individual Survey-2(POA/Codebook/Census Figures)/cleaning/"

drive_upload(media = paste0("~/communities_speak/data/codebook/codebook.csv"), path = paste0(googledrive_path, "data/output/codebook", today), type = "spreadsheet")
drive_upload(media = paste0("../data/output/wrangled", today,".dta"), path = paste0(googledrive_path, "data/output/wrangled", today, ".dta"))
drive_upload(media = paste0("../data/output/wrangled", today,".csv"), path = paste0(googledrive_path, "data/output/wrangled", today, ".csv"), type = "spreadsheet")