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

bufferSize = 5280 * 3
gridSize = 5280 * 0.33

# Read in KML file designating point locations for stations and polygons for Station Areas

stationPoints <- 
  read_sf("shape/stationPoints.shp") %>% st_transform(proj) %>% arrange(name)
stationArea <- 
  read_sf("shape/RailStationAreas.shp") %>% st_transform(proj) %>% select(PropName) %>%
  st_join(stationPoints) %>% select(name, type) %>% arrange(name)

# Map check
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(stationArea) + tm_polygons(alpha = 0.5, col = "orange") + 
  tm_shape(stationPoints) + tm_dots(col = "type", palette = "Set1")

# NOTE: Confirm that both sf files have same number of locations and are in same order for Name
stationPoints[,1] %>% st_drop_geometry() == stationArea[,1] %>% st_drop_geometry()




# Create StL Zone Sets ----------------------------------------------------

# Create Station Area Zones
stationArea_ZS <- 
  stationArea %>% arrange(name) %>%
  transmute(
    id = row_number(),
    name = name,
    is_pass = 0,
    direction = NA
  ) %>% st_transform(4326) %>% st_zm()

# Create list to be converted into JSON file
body <- list(
  insight_login_email = email, #Must include account login email
  geom_type = "polygon", #polygon or line
  zone_set_name = "StationAreas2021-09-17", #Set name of zone set
  zones = jsonlite::fromJSON(geojsonsf::sf_geojson(stationArea_ZS))) #Convert grid to geoJSON, then to list format

# Convert to final JSON file
bodyJSON <- jsonlite::toJSON(body, auto_unbox = TRUE)

# API call to create Zone Set
RETRY(
  "POST",
  times = 2,
  url = paste0("https://insight.streetlightdata.com/api/v2/zone_sets", "?key=", StlKey),
  content_type_json(), 
  body = bodyJSON
) 


# Loop to create hex grid zone set for each station area
i <- 2

allGrid <- st_sf(st_sfc(crs = proj))
for(i in 1:nrow(stationPoints)) {
  
  # Create grid for each station row by row based on buffer size and grid size set above
  grid <- 
    stationPoints[i,] %>% 
    st_buffer(bufferSize) %>%
    st_make_grid(cellsize = gridSize, square = FALSE)  %>% st_as_sf() %>%
    filter(lengths(st_intersects(., stationPoints[i,] %>% st_buffer(bufferSize))) > 0) %>%
    st_difference(stationArea[i,]) %>% #clip out the station area to avoid intrazone trips
    transmute(
      id = row_number(),
      name = paste0(stationPoints[i,] %>% pull(name), id)
      ) %>%
    rename(geometry = x) %>%
    bind_rows(stationArea[i,] %>% transmute(
                                name = stationPoints[i,] %>% pull(name),
                                id = 999
                                )
              ) %>%
    st_transform(4326) %>% st_zm() %>%
    mutate(
      is_pass = 0,
      direction = NA, 
      .before = geometry
      ) %>%
    st_cast("MULTIPOLYGON")
  
  if(allGrid %>% nrow() > 0) {allGrid <- bind_rows(allGrid, grid)} else {allGrid <- grid} 
  

  # Create list to be converted into JSON file
  body <- list(
    insight_login_email = email, #Must include account login email
    geom_type = "polygon", #polygon or line
    zone_set_name = grid[1,] %>% pull(name), #Set name of zone set
    zones = jsonlite::fromJSON(geojsonsf::sf_geojson(grid))) #Convert grid to geoJSON, then to list format

  # Convert to final JSON file
  bodyJSON <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # API call to create Zone Set
  RETRY(
    "POST",
    times = 2,
    url = paste0("https://insight.streetlightdata.com/api/v2/zone_sets", "?key=", StlKey),
    content_type_json(),
    body = bodyJSON
  )

  print(grid[1,] %>% pull(name))
}

tm_shape(allGrid) + tm_sf(alpha = 0.5)

# Save data for Step 2.0
save(allGrid, stationArea, stationPoints, file = "shape/stationArea.RData")

