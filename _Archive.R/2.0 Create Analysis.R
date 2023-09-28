library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(jsonlite)
library(geojsonsf)
library(httr)


# Initial Setup -----------------------------------------------------------

# Set StreetLight  key
StlKey <- "DgO7zCdYednFilSOR1dTX7jsmsdcoz31"
email <- "chris.ryan@hdrinc.com"

tmap_mode("view")
proj <- 6595

load("shape/stationArea.RData")


# Define StreetLight API Call Functions ------------------------------------

#Preset variables for testing functions
mode <- "Pedestrian"
zoneset <- "StationAreas"

StL_API_OD <- function(mode, zoneset) {
  
  analysisName <- paste0(zoneset, "_", mode, "_", "OD")
    
  bodyA <- list(
    insight_login_email = email,
    analysis_name = analysisName,
    analysis_type = "OD_Analysis",
    travel_mode_type = mode, #All_Vehicles/Truck/Bicycle/Pedestrian/Bus/Rail
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    dz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    #geography_type = "blkgrp",
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2019",
      end_date = "12/31/2019"
    )),
    day_types = "Mon|11, Tue|22, Wed|33, Thu|44, Fri|55, Sat|66, Sun|77",
    day_parts = "All Day|0023,12am|0000,1am|0101,2am|0202,3am|0303,4am|0404,5am|0505,6am|0606,7am|0707,8am|0808,9am|0909,10am|1010,11am|1111,12pm|1212,1pm|1313,2pm|1414,3pm|1515,4pm|1616,5pm|1717,6pm|1818,7pm|1919,8pm|2020,9pm|2121,10pm|2222,11pm|2323",
    output_type = "index"
    #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  print(analysisName)
}
StL_API_PresetOD <- function(mode, zoneset) {
  
  analysisName <- paste0(zoneset, "_", mode, "_", "PresetOD")
  
  bodyA <- list(
    insight_login_email = email,
    analysis_name = analysisName,
    analysis_type = "OD_Preset_Geography",
    travel_mode_type = mode, #All_Vehicles/Truck/Bicycle/Pedestrian/Bus/Rail
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    #dz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    geography_type = "blkgrp",
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2019",
      end_date = "12/31/2019"
    )),
    day_types = "Mon|11, Tue|22, Wed|33, Thu|44, Fri|55, Sat|66, Sun|77",
    day_parts = "All Day|0023,12am|0000,1am|0101,2am|0202,3am|0303,4am|0404,5am|0505,6am|0606,7am|0707,8am|0808,9am|0909,10am|1010,11am|1111,12pm|1212,1pm|1313,2pm|1414,3pm|1515,4pm|1616,5pm|1717,6pm|1818,7pm|1919,8pm|2020,9pm|2121,10pm|2222,11pm|2323",
    output_type = "index"
    #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  print(analysisName)
}
StL_API_TopRoute <- function(mode, zoneset) {
  
  analysisName <- paste0(zoneset, "_", mode, "_", "TopRoute")
  
  bodyA <- list(
    insight_login_email = email,
    analysis_name = analysisName,
    analysis_type = "Top_Routes_ZA",
    travel_mode_type = mode, #All_Vehicles/Truck/Bicycle/Pedestrian/Bus/Rail
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    dz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    geography_type = "blkgrp",
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2019",
      end_date = "12/31/2019"
    )),
    day_types = "Mon|11, Tue|22, Wed|33, Thu|44, Fri|55, Sat|66, Sun|77",
    day_parts = "All Day|0023,12am|0000,1am|0101,2am|0202,3am|0303,4am|0404,5am|0505,6am|0606,7am|0707,8am|0808,9am|0909,10am|1010,11am|1111,12pm|1212,1pm|1313,2pm|1414,3pm|1515,4pm|1616,5pm|1717,6pm|1818,7pm|1919,8pm|2020,9pm|2121,10pm|2222,11pm|2323",
    output_type = "index",
    segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  print(analysisName)
}

# Create Analyses -------------------------------------------------

# Loop to create Ped/Bike OD for Hex Grids
for(i in 1:nrow(stationArea)) {
  zoneset <- paste0(stationPoints[i,] %>% pull(name), 1)
  StL_API_OD("Pedestrian", zoneset) 
  StL_API_OD("Bicycle", zoneset) 
  print(zoneset)
}


# API call for Top Route and Preset OD    
StL_API_PresetOD("All_Vehicles", "StationAreas2021-09-17")
StL_API_TopRoute("All_Vehicles", "StationAreas2021-09-17")


# Handling Forward Slash Name Issues --------------------------------------

# Redo function to be able to have separately defined zoneset and analysis name
StL_API_ODv2 <- function(mode, zoneset, analysisName) {
  
  analysisName <- paste0(analysisName, "_", mode, "_", "OD")
  
  bodyA <- list(
    insight_login_email = email,
    analysis_name = analysisName,
    analysis_type = "OD_Analysis",
    travel_mode_type = mode, #All_Vehicles/Truck/Bicycle/Pedestrian/Bus/Rail
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    dz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
    #geography_type = "blkgrp",
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2019",
      end_date = "12/31/2019"
    )),
    day_types = "Mon|11, Tue|22, Wed|33, Thu|44, Fri|55, Sat|66, Sun|77",
    day_parts = "All Day|0023,12am|0000,1am|0101,2am|0202,3am|0303,4am|0404,5am|0505,6am|0606,7am|0707,8am|0808,9am|0909,10am|1010,11am|1111,12pm|1212,1pm|1313,2pm|1414,3pm|1515,4pm|1616,5pm|1717,6pm|1818,7pm|1919,8pm|2020,9pm|2121,10pm|2222,11pm|2323",
    output_type = "index"
    #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  print(analysisName)
}

# Rerun analyses to handle forward slash in Broad Run and Franconia
extra <- stationPoints %>% filter(str_detect(name, pattern = "/")) %>% mutate(aName = str_replace(name, "/", "-"))
i <- 1
for(i in 1:nrow(extra)) {
  analysisName <- paste0(extra[i,] %>% pull(aName), 1)
  zoneset <- paste0(extra[i,] %>% pull(name), 1)
  StL_API_ODv2("Pedestrian", zoneset, analysisName) 
  StL_API_ODv2("Bicycle", zoneset, analysisName) 
  print(zoneset)
}

# Rerunning analyses for Norfolk (First try was using incorrect zone set)
StL_API_ODv2("Pedestrian", "NorfolkStation1 - Copy", "Norfolk1")
StL_API_ODv2("Bicycle", "NorfolkStation1 - Copy", "Norfolk1")


