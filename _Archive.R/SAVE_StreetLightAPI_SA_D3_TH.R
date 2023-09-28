library(tidyverse)
library(httr)
library(tidycensus)
library(sf)
library(tmap)
library(tmaptools)
library(jsonlite)
library(geojsonsf)
library(tigris)
library(osmdata)
library(lubridate)

tmap_mode("view")

# Analysis created on 2021-03-03
# Test case to see if StreetLight data can serve the same purpose as the Iteris data
# for MnDOT D3 crash prediction project

#### Create Zone File ####

# Download MN geographic boundary and District/ATP boundaries; define bounding box for OSM
MNshp <- states(cb = TRUE) %>% filter(STUSPS == "MN") %>% st_transform(4326)
D3shp <- read_sf("shape/Area_Transportation_Partnership_Boundaries_in_Minnesota.shp") %>% 
  filter(ATP_CODE == "3") %>% st_buffer(10) %>% st_transform(4326) #slight buffer added to avoid clipping issue
bbox <- st_bbox(D3shp)

# Read in OSM data (CAN SKIP TO LOAD DATA STEP IF THIS HAS BEEN COMPLETED)
roadsOSMshp <- opq(bbox) %>% #be sure bounding box is in WGS84 lat/lon
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary")) %>% 
  osmdata_sf()

MND3_OSM <- roadsOSMshpFilter
save(MND3_OSM, file = "MND3_OSM.RData")

roadsOSMshp <- roadsOSMshp$osm_lines %>% st_transform(4326)


# Read in MnDOT Centerline Shapefile to ID MN 210
MNTH <- read_sf("shape/Trunk_Highways_in_Minnesota.shp") %>% st_transform(4326)
MNTHBuff <- MNTH %>% st_zm() %>% #drop elevation metrics
  st_intersection(D3shp) %>% #intersect with D3 boundary
  st_transform(2810) %>% st_buffer(50) %>% st_transform(4326) %>% #50 meter buffer
  summarize()

roadsOSMshpFilter <- roadsOSMshp %>% st_intersection(D3shp) %>% filter(lengths(st_within(., MNTHBuff)) > 0) 


tm_shape(D3shp) + tm_polygons(col = "dodgerblue1", alpha = 0.2) +
  tm_shape(MNTHBuff) + tm_polygons() +
  tm_shape(MND3_OSM) + tm_lines(col = "red")


# Steps if data has been downloaded
load("MND3_OSM.RData") #Use if data already downloaded
roadsOSMshp <- MND3_OSM

#### StreetLight Setup ####

# Set StreetLight  key
StlKey <- "mNT5hHj4PNlZzPaTxu6tybFMY7saxOBp"
email <- "chris.ryan@hdrinc.com"


#### Create Zones ####

# Add properties needed for StL Analysis
osmShpFilter <- MND3_OSM %>%
  transmute(
    id = osm_id, #Set variable as id value
    name = osm_id, #Set variable as name value
    is_pass = 1, #Set features as pass-through (1) or not (0)
    direction = NA, #Set feature direction or leave as null/NA
    rowNum = row_number()
  ) 

st_write(osmShpFilter, "shape", "D3_OSM_THseg.shp", driver = "ESRI Shapefile", overwrite = TRUE)

ggplot(osmShpFilter) +
  geom_sf()



rowTot <- 800 #Set number of rows per shapefile (Max zones is 1,000)
i <- 1
# Loop to create StreetLight zones in batches
for(i in 1:(nrow(osmShpFilter) / rowTot + 1)) {
  filename <- paste0("D3_batch", i, "of", nrow(osmShpFilter) %/% rowTot+1)

  # Filter out rows based on rowTot setting
  shp <- osmShpFilter %>%
    filter(
      rowNum <= i * rowTot, 
      rowNum > (i-1) * rowTot
    ) 
  
  # Create list to be converted into JSON file
  body <- list(
    insight_login_email = email, #Must include account login email
    geom_type = "line", #polygon or line
    zone_set_name = "testD3THOld", #Set name of zone set
    zones = jsonlite::fromJSON(geojsonsf::sf_geojson(shp))) #Convert shp to geoJSON, then to list format
  
  # Convert to final JSON file
  bodyJSON <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # API call to create Zone Set
  RETRY(
    "POST",
    times = 3,
    url = paste0("https://insight.streetlightdata.com/api/v2/zone_sets", "?key=", StlKey),
    content_type_json(), 
    body = bodyJSON
  )
  
  Sys.sleep(1)  #Max zone creation is 1 per second 
  
}  
  
  
#### Create Analysis ####

# Loop to create StreetLight analyses
startDate <- ymd("2019-01-01", tz = "America/Chicago")
endDate <- ymd("2020-01-01", tz = "America/Chicago")
aDate <- startDate

for(i in 1:(nrow(osmShpFilter) / rowTot + 1)) {
  filename <- paste0("D3_batch", i, "of", nrow(osmShpFilter) %/% rowTot+1)
  
  
  while (aDate < endDate) {
    analysisName <- paste0(filename, "_v1_", as.character(aDate))
    zoneset = filename
    
    bodyA <- list(
      insight_login_email = email,
      analysis_name = analysisName,
      analysis_type = "Segment_Analysis",
      travel_mode_type = "All_Vehicles", #All_Vehicles/Truck
      enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
      oz_sets = list(list(name = zoneset)), #double list required to get square brackets in JSON
      date_ranges = list(list( #double list required to get square brackets in JSON
        start_date = paste0(
                      str_pad(month(aDate), width = 2, pad = "0"),"/",
                      str_pad(day(aDate), width = 2, pad = "0"),"/",
                      as.character(year(aDate))),
        end_date = paste0(
                      str_pad(month(aDate + days(7)), width = 2, pad = "0"),"/",
                      str_pad(day(aDate + days(7)), width = 2, pad = "0"),"/",
                      as.character(year(aDate + days(7))))
        )),
      day_types = "Mon|11, Tue|22, Wed|33, Thu|44, Fri|55, Sat|66, Sun|77",
      day_parts = "All Day|0023,12am|0000,1am|0101,2am|0202,3am|0303,4am|0404,5am|0505,6am|0606,7am|0707,8am|0808,9am|0909,10am|1010,11am|1111,12pm|1212,1pm|1313,2pm|1414,3pm|1515,4pm|1616,5pm|1717,6pm|1818,7pm|1919,8pm|2020,9pm|2121,10pm|2222,11pm|2323",
      output_type = "index"
      #enable_15mins = TRUE #Only works for OD and ZA analyses
      #trip_speed_bins = TRUE, #"0-5, 6-10, 11-15, 16-99",
      #trip_duration_bins = FALSE
      #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
    )
    
    bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
    
    # Call to create Analysis
    RETRY(
      "POST",
      times = 3,
      url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
      content_type_json(), 
      body = bodyAJSON
    )
    
    aDate <- aDate + days(7) #change analysis date to the next week
    Sys.sleep(6) #Max analysis creation is 10 per minute
    
  }
  aDate <- startDate #reset startDate for each batch
}


#### Get Results ####


# Loop to download and combine analysis results

finalResults <- tibble() #Initialize final table
aDate <- startDate

for(i in 1:(nrow(osmShpFilter) / rowTot + 1)) {
  filename <- paste0("D3_batch", i, "of", nrow(osmShpFilter) %/% rowTot+1)
  
  
  while (aDate < endDate) {
    analysisName <- paste0(filename, "_v1_", as.character(aDate))
    zoneset = filename
    
    # Call to download results
    results <- as.data.frame(content(RETRY(
      "GET",
      times = 3,
      url = paste0(
        "https://insight.streetlightdata.com/api/v2/analyses/download/name/",
        analysisName, "/",
        "sa_all", #tr_za/od_pers/od_comm/zone_od_pers/zone_od_comm/sa_pers/sa_comm/sa_all 
        "?key=", StlKey)
    ))) %>%
      mutate(name = analysisName, startDate = aDate) %>%
      mutate_all(as.character) #columns were coming in randomly as character or numeric
                               #this forces all to be character, to be converted later
    
    # Append and filter to keep data size manageable but keep error messages 
    finalResults <- bind_rows(finalResults, results)#%>%
    #filter(`Trip Proportion` >= 0.05 | is.na(`Trip Proportion`)) 
    
    print(analysisName)
    
    aDate <- aDate + days(7)
    
  }
  
  aDate <- startDate
  print("done")
}


#### Final Cleanup ####
finalResults <- finalResults %>% mutate(
  aDOW = (as.numeric(str_sub(`Day Type`, 1, 1)) + 1) %% 7 + 1, #convert StL day of week to 1:sun-7:sat
  sDOW = wday(startDate),
  aDate = case_when(
    aDOW < sDOW ~ date(startDate) + days(aDOW - sDOW + 7),
    aDOW >= sDOW ~ date(startDate) + days(aDOW - sDOW)
  )
) %>%
  select(-sDOW, -aDOW) %>%
  arrange(`Zone ID`, aDate)
 
write_csv(finalResults, "D3_TH_2019.csv")
D3_TH_2019 <- read_csv("D3_TH_2019.csv")


D3_TH_2019_simp <- D3_TH_2019 %>% transmute(
  osmid = `Zone ID`,
  date = aDate,
  hour = str_sub(`Day Part`, 1, 2),
  avgSpd = `Avg Segment Speed (mph)`,
  StLindex = `Average Daily Segment Traffic (StL Index)`
)
write_csv(D3_TH_2019_simp, "D3_TH_2019_simp.csv")



finalResultsPivot <- finalResults %>% select(
  `Zone ID`,
  name,
  aDate,
  `Day Part`,
  `Average Daily Segment Traffic (StL Index)`,
  `Avg Segment Speed (mph)`,
  `Free Flow Factor`
) %>% pivot_wider(
  names_from = `Day Part`, 
  values_from = c(
    `Average Daily Segment Traffic (StL Index)`, 
    `Avg Segment Speed (mph)`,
    `Free Flow Factor`)
  )

write_csv(finalResultsPivot, "D3ExampleTidied.csv")



results %>% group_by(`Day Part`) %>% 
  ggplot(mapping = aes(x=`Day Part`)) + geom_bar() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))





#### Post Data Checking ####
tmap_mode("view")
osm <- read_sf("shape/D3_OSM_THseg.shp")
data <- read_csv("D3_TH_2019_simp.csv")
dataSumm <- data %>% filter(!is.na(avgSpd)) %>% group_by(osmid) %>% summarize(count = n()) %>%
  mutate(percCov = count / max(count),
         osmidT = as.character(osmid))

osmJoin <- left_join(osm, dataSumm, by = c("id" = "osmidT"))

osmAnti <- anti_join(osm, dataSumm, by = c("id" = "osmidT"))

tm_shape(osmAnti) + tm_lines(lty = "dashed") +
tm_shape(osmJoin) + tm_lines(lwd = "percCov", col = "percCov", scale = 10, palette = "RdYlBu")

#### OLD ####

# Read in All OSM data for MN; Can use if the osmdata API is not working
OSM <- read_sf("shape/gis_osm_roads_free_1.shp") %>%
  filter(fclass %in% c("motorway", "trunk"))
