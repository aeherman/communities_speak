# upload
today <- gsub("-", "", Sys.Date())

googledrive_path <- "Communities Speak/Subteams/Data Subteam/Individual Survey-2(POA/Codebook/Census Figures)/cleaning/"

drive_upload(media = paste0("~/communities_speak/data/codebook/codebook.csv"), path = paste0(googledrive_path, "data/output/codebook", today), type = "spreadsheet")
drive_upload(media = paste0("~/communities_speak/data/output/wrangled", today,".dta"), path = paste0(googledrive_path, "data/output/wrangled", today, ".dta"))
drive_upload(media = paste0("~/communities_speak/data/output/wrangled", today,".rds"), path = paste0(googledrive_path, "data/output/wrangled", today, ".rds"))
drive_upload(media = paste0("~/communities_speak/data/output/wrangled", today,".csv"), path = paste0(googledrive_path, "data/output/wrangled", today, ".csv"), type = "spreadsheet")

drive_upload(media = paste0("~/communities_speak/code/validation.pdf"), path = paste0(googledrive_path, "visuals/nonresponse", today, ".pdf"), overwrite = TRUE)

# maps
drive_upload(media = paste0("~/communities_speak/visuals/respondents_by_borough.png"), path = paste0(googledrive_path, "visuals/respondents_by_borough", today, ".png"), overwrite = TRUE)
drive_upload(media = paste0("~/communities_speak/visuals/respondents_by_zip.png"), path = paste0(googledrive_path, "visuals/respondents_by_zip", today, ".png"), overwrite = TRUE)
drive_upload(media = paste0("~/communities_speak/visuals/respondents_child_by_zip.png"), path = paste0(googledrive_path, "visuals/respondents_child_by_zip", today, "children.png"), overwrite = TRUE)
drive_upload(media = paste0("~/communities_speak/visuals/respondents_child_by_bor.png"), path = paste0(googledrive_path, "visuals/respondents_child_by_bor", today, "children.png"), overwrite = TRUE)
drive_upload(media = paste0("~/communities_speak/visuals/p_find_cc.png"), path = paste0(googledrive_path, "visuals/find_cc_borough", today, ".png"), overwrite = TRUE)
drive_upload(media = paste0("~/communities_speak/visuals/geocode.pdf"), path = paste0(googledrive_path, "visuals/geocode", today, ".pdf"), overwrite = TRUE)


#drive_upload(media = "~/communities_speak/code/geocode.pdf", path = paste0(googledrive_path, "visuals/geocode", today, ".pdf"))
