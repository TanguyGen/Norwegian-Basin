rm(list=ls())

packages <- c("tidyverse", "sf","ggplot2")                   # List packages
lapply(packages, library, character.only = TRUE)      
fiskedir<- read_sf("./Data/Fiskeridirektoratet/fiskeriaktvitet_etter_redskap_norsk_tilnov2024")

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%
  st_union()

gear <- read.csv("./Data/MiMeMo_gears.csv") 

guild <- read.csv("./Data/MiMeMo fish guilds.csv")%>%
  rename("hovedart"="Fiskeridir")

habitats <- readRDS("./Objects/Habitats.rds") 

fiskedir<-fiskedir%>%
  filter(aar>=2010&aar<=2019)

fiskedir <- st_transform(fiskedir, crs = st_crs(habitats))
intersections <- st_intersection(fiskedir, habitats)


# Summarize by gear and habitat
habitat_weights <- corrected_fiskedir %>%
  as.data.frame()%>%
  left_join(gear)%>%
  filter(Aggregated_gear != "Dropped") %>% 
  drop_na(Aggregated_gear) %>%
  group_by(Habitat, Aggregated_gear) %>%  # Group by Habitat and fishing gear
  summarise(
    point_count = n(),  # Count number of points
    .groups = "drop"
  )%>%group_by(Aggregated_gear) %>%
  mutate(
    percentage_presence = (point_count / sum(point_count))
  ) %>%
  ungroup()%>%
  select(Habitat, Aggregated_gear, percentage_presence)


saveRDS(habitat_weights,"~/Downloads/habitat_weights.RDS")

#habitat_weights2<-readRDS("~/Downloads/habitat_weights.RDS")

library(ncdf4)
library(raster)
gebco_raster <- raster("gebco_2024_n75.0_s60.0_w-7.0_e16.0.nc")

raster_df <- as.data.frame(gebco_raster, xy = TRUE)
raster_df<-raster_df %>% filter(Elevation.relative.to.sea.level >= -2000 & Elevation.relative.to.sea.level <= -600)


test<-corrected_fiskedir%>%
  filter(redskap=="Bunntrål"|redskap=="Flytetrål")

test <- st_transform(test, crs = st_crs(raster_df))

# Create breaks for the 100m intervals
breaks <- seq(min(raster_df$Elevation.relative.to.sea.level, na.rm = TRUE), 
              max(raster_df$Elevation.relative.to.sea.level, na.rm = TRUE), 
              by = 100)

# Format the labels to show a range (e.g., -2000 to -1900 m)
labels <- sapply(1:(length(breaks) - 1), function(i) {
  paste0(format(breaks[i], scientific = FALSE), " to ", 
         format(breaks[i + 1], scientific = FALSE), " m")
})

ggplot() +
  # Raster plot
  geom_raster(data = raster_df, aes(x = x, y = y, fill = cut(Elevation.relative.to.sea.level, 
                                                             breaks = breaks))) +
  scale_fill_viridis_d(name = "Elevation (m)", 
                       labels = labels)  + 
  geom_sf(data=test,aes(color=redskap))+
  geom_sf(data = habitats, fill = NA, size = 2) +
  coord_sf() +
  theme_minimal() +
  labs(title = "GEBCO with Points from Shapefile", fill = "Elevation")+
  facet_wrap(~redskap)


bathymetry_raster <- raster::raster(Bathymetry, 
                                    xmn = min(lon), xmx = max(lon), 
                                    ymn = min(lat), ymx = max(lat), 
                                    crs = "+proj=longlat +datum=WGS84")




ggplot()+
  geom_tile(data = bathymetry_df, aes(x = Longitude, y = Latitude, fill = Depth)) +
  geom_sf(data=test,aes(color=redskap))+
  geom_sf(data=habitats,alpha=0.1)+
  theme_minimal()

grid <- expand.grid(lon=lon, lat=lat)

levelplot(Depth ~ Longitude * Latitude, data=bathymetry_df, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(12,"RdBu"))))+ 
  layer(sp.lines(countries))



ggplot(Regions)+
  geom_sf(aes(fill=as.factor(Region)))+
  geom_raster(data=GFW_trawlers,aes(x = x, y = y, fill = Value))

ggplot()+
  geom_sf(data=test,aes(color=Guild))+
  geom_sf(data=habitats,alpha=0.1)+
  theme_minimal()


test<-corrected_fiskedir%>%
  filter(redskap=="Bunntrål")%>%
  left_join(guild)%>%
  filter(Guild=="Demersal (non quota)"|Guild=="Demersal (quota limited)"|Guild=="Benthos carnivore/scavenge feeder"|Guild=="Benthos filter/deposit feeder")%>%
  filter(Shore=="Offshore")

ggplot()+
  geom_sf(data=test,aes(color=Guild))+
  geom_sf(data=habitats,alpha=0.1)+
  theme_minimal()

