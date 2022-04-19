# upload
today <- gsub("-", "", Sys.Date())

googledrive_path <- "Communities Speak/Subteams/Data Subteam/cleaning/fa21/"

drive_upload(media = paste0("~/communities_speak/code/functions/make_plots.R"), path = paste0(googledrive_path, "code/functions/make_plots", today), overwrite = TRUE)

drive_upload(media = glue::glue("~/communities_speak/data/codebook/codebook{today}.csv"), path = paste0(googledrive_path, "data/output/codebook", today), type = "spreadsheet")
drive_upload(media = paste0("~/communities_speak/data/output/weights", today, ".csv"), path = paste0(googledrive_path, "data/output/weights", today), type = "spreadsheet")
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
