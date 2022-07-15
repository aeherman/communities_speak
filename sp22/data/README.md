# Folder Instructions
1. Use upload.R to upload new copies of outputs
2. Place old copies in the “old” folder specified in each of the child directories (processed, input, or output).
3. Remove copies that are over two months old
NOTE: only the cleaned and wrangled data outputs, in addition to the codebook, are pushed to github.  In order to create the other files, one must start with the upload_dictionary, download_dictionary, and cleaning.R.

# Input
Contains all data inputs (except shapefiles).
1. Survey | old and most recent copies of the Communities Speak survey data
2. Census | relevant census data from 2020 decennial census and CPS monthly
3. Labeling sheet (only on google drive)
4. boroughs (only on github)

# Processed
This folder contains dataframes created in rstudio and were used to make the final data outputs.

1. types<date>.rdata | variable types in the survey
2. toname<date>.rdata | variables and values to be relabeled by in the labeling sheet.
3. labeled<date>.rdata | the downloaded hand labeled information
4. cleaned<date>.rdata | the cleaned data.  Does not contain analysis variables, and invalid responses are indicated but not filtered out.
5. cleaned<date> | cleaned data for easy viewing in google drive
6. weighted<date> | the weight variable calculations using our survey and the census race percentage breakdown.

# Output
Output Folder Contents
1. wrangled<date>.rds | for easy use in Rstudio
2. wrangled<date>.dta | for easy use in Stata
3. wrangled<date> | for easy viewing in google drive
4. codebook<date>

## Wrangled Data:
The wrangled data is cleaned, filtered for valid responses (see selection criteria), and wrangled to create analysis variables. Please see description of cleaning process for more information on the creation of the variables and types.

## Codebook:
The codebook is generated from the wrangled data.  It describes the following:
1. survey origin of the question
2. the variable name
3. the variable type (please see README - sp22 for more information regarding the variable type handling).
4. the variable values (for haven_labelled variables, these values are derived from the labels, which are derived from the Qualtrics survey dictionary)
5. The label associated with the values
6. The description of the variable, or the question associated with the variable.
