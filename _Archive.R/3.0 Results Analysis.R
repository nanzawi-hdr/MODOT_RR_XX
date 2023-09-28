library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(jsonlite)
library(geojsonsf)
library(httr)
library(tigris)

# Initial Setup -----------------------------------------------------------

# Define functions
read_sf_zipLoc <- function(path) { 
  temp <- tempfile() 
  unzip(zipfile = path, exdir = temp) 
  read_sf(temp) 
}
write_sf_zip <- function(SFobject, path, name) { 
  temp <- tempdir() 
  SFobject %>% write_sf(temp, name, driver = "ESRI Shapefile") 
  files <- list.files(temp, pattern = paste0(name, ".*"), full.names = TRUE) 
  zip(zipfile = paste0(getwd(), path, name), files = files, flags = " a -tzip", zip = "C:\\Program Files\\7-Zip\\7Z") 
}
StL_API_Status <- function(analysisName) {
  body <- list(analyses = list(list(name = analysisName)))
  bodyJSON <- jsonlite::toJSON(body, auto_unbox = TRUE)
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com:/api/v2/analyses/status", "?key=", StlKey),
    content_type_json(), 
    body = bodyJSON
  )
}
StL_Download <- function(analysisName, type) {
  as.data.frame(content(RETRY(
    "GET",
    times = 2,
    url = paste0(
      "https://insight.streetlightdata.com/api/v2/analyses/download/name/",
      analysisName, "/", type, "?key=", StlKey)
  ))) 
  
}

# Set StreetLight  key
StlKey <- "DgO7zCdYednFilSOR1dTX7jsmsdcoz31"
email <- "chris.ryan@hdrinc.com"

tmap_mode("view")
proj <- 6595

load("shape/stationArea.RData")



# Vehicular ---------------------------------------------------------------


#### Top Route ####
# NOTE: osm IDs included in the csv download don't match actual osm id values. 
# Need to download the osm shape from streetlight in order to map things
StL_API_Status("StationAreas2021-09-17_All_Vehicles_TopRoute")
resultsTRraw <- StL_Download("StationAreas2021-09-17_All_Vehicles_TopRoute", "tr_za")
resultsTRosm <- 
  read_sf_zipLoc("StL/Results/276860_StationAreas2021_09_17_All_Vehicles_za/Shapefile/276860_StationAreas2021_09_17_All_Vehicles_osm_segment.zip") %>% 
  mutate(osm_id = as.character(segment_id))


# Process to consolidate directionality
resultsTRproc <- 
  resultsTRraw %>% janitor::clean_names() %>%
  transmute(
    target = if_else(origin_zone_source == "Input", origin_zone_name, destination_zone_name),
    osm_id = if_else(origin_zone_source == "Input", as.character(destination_zone_id), as.character(origin_zone_id)),
    stlOD = average_daily_o_d_traffic_st_l_index,
    day_type = day_type,
    day_part = day_part,
    day_type_short = str_sub(day_type, 1, 1),
    day_part_short = str_sub(day_part, 1, 2)
    ) %>%
  group_by(target, osm_id, day_type, day_part, day_type_short, day_part_short) %>%
  summarize(stlTot = sum(stlOD)) 

# Combine all-day data with osm segments
resultsTRsf <- resultsTRproc %>%
  left_join(resultsTRosm) %>%
  st_as_sf() %>%
  filter(day_type_short == "0", day_part_short == "00", stlTot > 1)

# Map of Station Top Routes
unique(resultsTRsf$target)
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(resultsTRsf %>% filter(target == "Alexandria")) + 
    tm_lines(col = "stlTot", lwd = "stlTot", palette = "Reds", style = "log10_pretty", scale = 10) +
  tm_shape(stationArea %>% filter(name == "Alexandria")) + tm_polygons(col = "blue", alpha = 0.5)


# Export
    #resultsTRraw %>% write_csv("output/StationTopRouteRaw.csv")
resultsTRproc %>% write_csv("output/StationTopRouteProcessed.csv")  
    #write_sf_zip(resultsTRosm, "/output/", "StationTopRoute_OSMsegs")  
write_sf_zip(resultsTRsf, "/output/", "StationTopRouteShp")


#### Origin-Destination ####
StL_API_Status("StationAreas2021-09-17_All_Vehicles_PresetOD")
resultsODraw <- StL_Download("StationAreas2021-09-17_All_Vehicles_PresetOD", "odg_all") 
vaBGs <- map_dfr(c("VA", "DC", "MD"), ~block_groups(state = ., cb = TRUE) %>% st_transform(4326))
  tm_shape(vaBGs) + tm_polygons()


# Process to consolidate directionality
resultsODproc <- 
  resultsODraw %>% janitor::clean_names() %>%
  transmute(
    target = if_else(origin_zone_source == "Input", origin_zone_name, destination_zone_name),
    GEOID = if_else(origin_zone_source == "Input", str_sub(destination_zone_name, 2, -2), str_sub(origin_zone_name, 2, -2)),
    stlOD = as.numeric(average_daily_o_d_traffic_st_l_index),
    day_type = day_type,
    day_part = day_part,
    day_type_short = str_sub(day_type, 1, 1),
    day_part_short = str_sub(day_part, 1, 2)
  ) %>%
  group_by(target, GEOID, day_type, day_part, day_type_short, day_part_short) %>%
  summarize(stlTot = sum(stlOD)) 

resultsODsf <- 
  resultsODproc %>% 
  filter(day_type_short == "0", day_part_short == "00", stlTot > 1) %>%
  left_join(vaBGs) %>% st_as_sf()


tm_shape(resultsODsf) + tm_fill(col = "stlTot", palette = "Reds", style = "log10_pretty")

# Map of Station Vehicle ODs with TR
#tm <- 
  tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(resultsODsf) + tm_polygons(col = "stlTot", palette = "BuPu", style = "jenks", alpha = 0.5) +
  tm_shape(resultsTRsf) + tm_lines(col = "stlTot", lwd = "stlTot", palette = "Reds", style = "jenks", scale = 10) +
  tm_shape(stationPoints) + tm_dots(col = "blue")
tmap_save(tm, filename = "output/htmlMaps/TopRoute_OD.html")

# Export
    #resultsODraw %>% write_csv("output/StationODRaw.csv")
resultsODproc %>% write_csv("output/StationODProcessed.csv")  
write_sf_zip(resultsODsf, "/output/", "StationODShp")


#### Vehicular Station Activity ####
resultsOD_ZA <- StL_Download("StationAreas2021-09-17_All_Vehicles_PresetOD", "zone_odg_all") %>% 
  janitor::clean_names() %>%
  transmute(
    target = zone_name,
    targetID = zone_id,
    stlOD = average_daily_zone_traffic_st_l_index,
    dir = intersection_type,
    day_type = day_type,
    day_part = day_part,
    day_type_short = str_sub(day_type, 1, 1),
    day_part_short = str_sub(day_part, 1, 2), 
    DOW = str_sub(day_type, 1, 7)
  ) %>%
  group_by(target, targetID, day_type, day_part, day_type_short, day_part_short, DOW) %>%
  summarize(stlTot = sum(stlOD))

# Loop to create vehicle activity chart 
stations <- unique(resultsOD_ZA$target)
i <- 4
for(i in 1:length(stations)) {
  resultsOD_ZA %>% filter(target == stations[i], day_type_short != "0", day_part_short != "00") %>%
    ggplot(aes(x = day_part_short, y = stlTot, fill = DOW)) + 
    geom_col(position = "dodge") + scale_fill_brewer(palette = "Dark2") +
    facet_grid(rows = vars(DOW)) +
    ggtitle(stations[i]) +
    xlab("Hour of Day") + 
    ylab("StreetLight Index - Passenger Vehicles") +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
  ggsave(filename = paste0("output/VehicleStationActivityCharts/", str_replace(stations[i], "/", ""), ".png"))
}


# Pedestrian --------------------------------------------------------------


#### Pedestrian OD ####
StL_API_Status("Franconia-Springfield1_Pedestrian_OD")
filenames <- paste0(stationArea$name, "1_Pedestrian_OD") %>% str_replace("/", "-")
resultsPedODraw <- map_dfr(filenames, ~StL_Download(., "od_ped") %>% mutate_all(as.character))
  
# Process to consolidate directionality
resultsPedODproc <- 
  resultsPedODraw %>% janitor::clean_names() %>%
  filter(origin_zone_id == "999" | destination_zone_id == "999") %>%
  transmute(
    target = if_else(origin_zone_id == 999, origin_zone_name, destination_zone_name),
    name = if_else(origin_zone_id == 999, destination_zone_name, origin_zone_name),
    origin_zone_id,
    destination_zone_id,
    stlOD = average_daily_o_d_traffic_st_l_index,
    day_type = day_type,
    day_part = day_part,
    day_type_short = str_sub(day_type, 1, 1),
    day_part_short = str_sub(day_part, 1, 2),
    DOW = str_sub(day_type, 1, 7)
  ) %>%
  group_by(target, name, day_type, day_part, day_type_short, day_part_short, DOW) %>%
  summarize(stlTot = sum(as.numeric(stlOD))) 

PedHexSF <- 
  allGrid %>% left_join(resultsPedODproc %>% filter(day_type_short == "0", day_part_short == "00"))

resultsPedODraw %>% write_csv("output/StationPedODRaw.csv")
resultsPedODproc %>% write_csv("output/StationPedODProcessed.csv")
write_sf_zip(PedHexSF, "/output/", "StationPedODShp")
write_sf_zip(allGrid, "/output/", "PedGrid")


# Map of Pedestrian Hex Grid Activity
#tm <- 
  tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(PedHexSF %>% filter(day_type_short == "0", day_part_short == "00", 
                               target == "Fredericksburg")) + 
    tm_polygons(col = "stlTot", style = "fisher", alpha = 0.5)
#tmap_save(tm, filename = "output/htmlMaps/PedestrianShed.html")


# Loop to create pedestrian activity chart 
stations <- 
  unique(resultsPedODproc$target)
for(i in 1:length(stations)) {
  resultsPedODproc %>% filter(target == stations[i], day_type_short != "0", day_part_short != "00") %>%
    ggplot(aes(x = day_part_short, y = stlTot, fill = DOW)) + 
    geom_col(position = "dodge") + scale_fill_brewer(palette = "Dark2") +
    facet_grid(rows = vars(DOW)) +
    ggtitle(stations[i]) +
    xlab("Hour of Day") + 
    ylab("StreetLight Index - Pedestrians") +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
  ggsave(filename = paste0("output/PedestrianStationActivityCharts/", str_replace(stations[i], "/", ""), ".png"))
}


# Bicycle -----------------------------------------------------------------


#### Bicycle OD ####
StL_API_Status("EttrickStation1_Bicycle_OD")
filenames <- paste0(stationPoints$name, "1_Bicycle_OD") %>% str_replace("/", "-")
resultsBikeODraw <- map_dfr(filenames, ~StL_Download(., "od_bike") %>% mutate_all(as.character))


# Process to consolidate directionality
resultsBikeODproc <- 
  resultsBikeODraw %>% janitor::clean_names() %>%
  filter(origin_zone_id == "999" | destination_zone_id == "999") %>%
  transmute(
    target = if_else(origin_zone_id == 999, origin_zone_name, destination_zone_name),
    name = if_else(origin_zone_id == 999, destination_zone_name, origin_zone_name),
    origin_zone_id,
    destination_zone_id,
    stlOD = average_daily_o_d_traffic_st_l_index,
    day_type = day_type,
    day_part = day_part,
    day_type_short = str_sub(day_type, 1, 1),
    day_part_short = str_sub(day_part, 1, 2),
    DOW = str_sub(day_type, 1, 7)
  ) %>%
  group_by(target, name, day_type, day_part, day_type_short, day_part_short, DOW) %>%
  summarize(stlTot = sum(as.numeric(stlOD))) 


BikeHexSF <- allGrid %>% left_join(resultsBikeODproc %>% filter(day_type_short == "0", day_part_short == "00"))

# Map of Pedestrian Hex Grid Activity
tm <-  
  tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(BikeHexSF %>% filter(target == "Roanoke", day_type_short == "0", day_part_short == "00")) + 
  tm_polygons(col = "stlTot", style = "fisher", alpha = 0.5)
tmap_save(tm, filename = "output/BicycleShed.html")


resultsBikeODraw %>% write_csv("output/StationBikeODRaw.csv")
resultsBikeODproc %>% write_csv("output/StationBikeODProcessed.csv")
write_sf_zip(BikeHexSF, "/output/", "StationBikeODShp")


# Loop to create bicycle activity chart 
stations <- unique(resultsBikeODproc$target)
for(i in 1:length(stations)) {
  resultsBikeODproc %>% filter(target == stations[i], day_type_short != "0", day_part_short != "00") %>%
    ggplot(aes(x = day_part_short, y = stlTot, fill = DOW)) + 
    geom_col(position = "dodge") + scale_fill_brewer(palette = "Dark2") +
    facet_grid(rows = vars(DOW)) +
    ggtitle(stations[i]) +
    xlab("Hour of Day") + 
    ylab("StreetLight Index - Bicyclists") +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
  ggsave(filename = paste0("output/BicycleStationActivityCharts/", str_replace(stations[i], "/", ""), ".png"))
}
