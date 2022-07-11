# Instructions

## Part I: Preparation
Run these scripts if this is the first time running the code on your laptop or if the data dictionary has changed input data has changed in any way.
1. [upload_dictionary.R](https://github.com/aeherman/communities_speak/blob/main/sp22/code/dictionary_upload.R) | pulls important info from the qualtrics survey dictionary and adds it to the [labeling](https://docs.google.com/spreadsheets/d/1qYNIz9QxsSlmqjD5Ym5BKI4Z160T2EBe3SeDq2ABIdw/edit#gid=261128975) sheet.
2. if any changes have been made to the data dictionary, review the following sheets for inconsistencies: [qs_named](https://docs.google.com/spreadsheets/d/1qYNIz9QxsSlmqjD5Ym5BKI4Z160T2EBe3SeDq2ABIdw/edit#gid=238259965) and [dummies_named](https://docs.google.com/spreadsheets/d/1qYNIz9QxsSlmqjD5Ym5BKI4Z160T2EBe3SeDq2ABIdw/edit#gid=1159455677)
3. [download_dictionary.R](https://github.com/aeherman/communities_speak/blob/main/sp22/code/dictionary_download.R) | pulls the labeled information from the labeling sheet for use in cleaning the data in R and creating the codebook.

## Part II: Cleaning
Run these scripts after adding a new version of the data from Qualtrics to GoogleDrive
1. [cleaning.Rmd]([url](https://github.com/aeherman/communities_speak/blob/main/sp22/code/cleaning.Rmd)) | cleans the data with automatic steps (any manual necessary manual steps must be performed on wrangled after this step).
2. [upload.R](https://github.com/aeherman/communities_speak/blob/main/sp22/code/upload.R) | uploads new outputs to googledrive
