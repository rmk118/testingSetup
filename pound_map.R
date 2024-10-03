#Map of Maine's lobster pounds
#Ruby Krasnow
#Last modified: Dec. 29, 2023

library(tidyverse)
library(sf)
library(leaflet)

pounds <- st_read("./data/MaineDMR_Lobster_Pounds_2019-shp/MaineDMR_-_Lobster_Pounds_2019.shp")
states <- st_read("./data/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
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

# Community shellfish map
ggplot()+
  geom_sf(data=states %>% filter(NAME=="Maine"), aes(fill=NAME))+
  geom_sf(data=st_sfc(st_point(x=c(-69.412884, 43.985878)), crs=4326), size=6)+
  theme_classic()+
  scale_fill_manual(values="gray87")+theme(legend.position = "none", text = element_text(size=15))

library(leaflet.providers)

leaflet(data=states %>% filter(NAME %in% c("New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New York"))) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addCircles(data = st_sfc(st_point(x=c(-69.412884, 43.985878)), crs=4326), opacity = 1) %>%
  fitBounds(-70,43.7,-68, 45) %>% 
  addMarkers(lat = -69, lng = 44, label = "Gulf of Maine")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addProviderTiles(providers$Stadia.StamenTonerLines) %>% 
  addCircleMarkers(data = st_sfc(st_point(x=c(-69.412884, 43.985878)), crs=4326),
                   color = "black", stroke = FALSE, fillOpacity = 1,
                   radius = 7) %>%
  fitBounds(-73, 47, -64, 44)
  #fitBounds(-70,43.7,-68, 45) %>% 
  # addLabelOnlyMarkers(lng = -69, lat = 43.5, 
  #                     label = "Gulf of Maine",
  #            labelOptions = labelOptions(noHide = TRUE,
  #                                        direction = "bottom",
  #                                        textOnly = TRUE,
  #                                        style = list("font-size" = "12px",
  #                                                     "font-family" = "Helvetica-Bold")))
                                                      #"font-name" = "Noto Sans")))
# library(tmap)
# 
# tmap_mode("plot")
# 
# pound <- st_sfc(st_point(x=c(-69.412884, 43.985878)), crs=4326)
# 
# tm_shape(pound) + 
# tm_basemap("CartoDB.PositronNoLabels") +
# tm_bubbles(col = "red")

library(ggmap)

us <- c(left = -73, bottom = 41.5, right = -64, top = 48)
get_stadiamap(us, zoom = 8, maptype = "stamen_toner_background") |> ggmap() 
