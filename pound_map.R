#Map of Maine's lobster pounds
#Ruby Krasnow
#Last modified: Dec. 29, 2023

library(tidyverse)
library(sf)
library(leaflet)

pounds_orig <- st_read("./MaineDMR_Lobster_Pounds_2019-shp/MaineDMR_-_Lobster_Pounds_2019.shp")
states <- st_read("./cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
states <- st_transform(states, 4326) %>% filter(!(NAME %in% c("Hawaii", "Alaska", "Puerto Rico")))

plot(pounds[1])

pounds_dots <- st_centroid(pounds)
plot(states["NAME"])

ggplot()+
  geom_sf(data=states %>% filter(NAME=="Maine"), aes(fill=NAME))+
  geom_sf(data=pounds_dots)

st_crs(states)

farm_ids <- c(20,49,50,22,31,30,34,8,9,44,43,21,17,23,65,67,68,74,35)
pounds_dots <- pounds_dots %>% mutate(farm = case_when(
  OBJECTID %in% farm_ids ~ TRUE,
  .default=FALSE
))

pal <- colorNumeric(palette = c("black", "red"), domain = pounds_dots$farm)

leaflet(data=states %>% filter(NAME %in% c("New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New York"))) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addCircles(data = pounds_dots, color = ~pal(farm), opacity = 1) %>%
  fitBounds(-70,43.7,-68, 45) %>% 
  addLegend("bottomright", colors=c("black", "red"), 
            labels=c("No farm", "Farm"),opacity = 1, title="")
