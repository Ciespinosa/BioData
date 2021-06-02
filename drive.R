library(googledrive)
library(googlesheets4)
library(googleAuthR)
library(googlesheets)

options(gargle_oauth_cache = ".secrets")# check the value of the option, if you like
gargle::gargle_oauth_cache()
googlesheets4::sheets_auth()



shiny_token <- list.files(".secrets/")[1] # authenticate w/ your desired Google identity here
saveRDS(shiny_token, "shiny_app_token.rds")


ss <- gs4_get("https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o/edit?usp=sharing") # do the authentication once, manually.

data_to_write <- read_sheet('https://docs.google.com/spreadsheets/d/1NC7KJAd8r-hFIKbK5A2lCHIg0-Ry1b6W-CxtwP0Rk2o')

data_to_add <- data_to_write[1,]
data_to_add[1,] <- matrix(rep(6,11), 1,11)

sheet_append(ss,
             data_to_add)



