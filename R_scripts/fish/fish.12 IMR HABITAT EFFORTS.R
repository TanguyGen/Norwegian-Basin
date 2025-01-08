
#**# Distribute IMR fishing effort over seabed habitats using GFW effort data

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326)         # reproject to match EU data

habitats <- readRDS("./Objects/Habitats.rds")                                 # Load habitat polygons

gear <- read.csv("./Data/MiMeMo_gears.csv") 

target <- expand.grid(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = unique(gear$Aggregated_gear))         # Get combinations of gear and habitat

Regions <- sf::read_sf(dsn="./Data/IMR/Regions/") %>%                      # Import IMR regions shapefile
  st_as_sf() %>%                                                              # Convert to SF
  st_join(Domains) %>%                                                        # Which regions are in the model domain?
  drop_na() %>%                                                               # Drop those outside
  dplyr::select(Region = havomr) %>%                                          # Select and rename region column
  unique()                                                            # Keep unique shapes
Regions<-st_transform(Regions,crs=st_crs(Domains))


fiskedir<- read_sf("./Data/Fiskeridirektoratet/fiskeriaktvitet_etter_redskap_norsk_tilnov2024") #Data for inflation from fiskeridirektoratet
fiskedir<-fiskedir%>%
  filter(aar>=2010&aar<=2019)

fiskedir <- st_transform(fiskedir, crs = st_crs(habitats))
corrected_fiskedir <- st_intersection(fiskedir, habitats)%>% 
  st_intersection(Regions)

saveRDS(corrected_fiskedir,"./Objects/Fiskedir effort.RDS")
#corrected_fiskedir<-readRDS("./Objects/Fiskedir effort.RDS")
#### Proportion IMR effort across seabed habitats by IMR region ####

# Summarize by gear and habitat
habitat_weights <- corrected_fiskedir %>%
  mutate(Habitat = paste0(Shore, " ", Habitat))%>%
  as.data.frame()%>%
  left_join(gear)%>%
  filter(Aggregated_gear != "Dropped") %>% 
  group_by(Habitat, Aggregated_gear,Region) %>%  # Group by Habitat and fishing gear
  summarise(
    point_count = n(),  # Count number of points
    .groups = "drop"
  )%>%                                                            # Drop habitats labelled as NA
  group_by(Aggregated_gear,Region)  %>%
  mutate(
    percentage_presence = (point_count / sum(point_count))
  ) %>%
  ungroup() %>% 
  right_join(target) %>%     # Ditch the unneeded gear class
  select(Habitat, Region,Aggregated_gear, percentage_presence)

check <- habitat_weights %>%                                                  # Check shares sum to 1 by gear
  group_by(Region, Aggregated_gear) %>% 
  summarise(check = sum(percentage_presence, na.rm = T))


#### Absolute effort scaled by proportion of GFW activity in an IMR region falling in the model domain ####

IMR_effort <- readRDS("./Objects/IMR regional absolute effort and landings.rds") %>% 
  st_drop_geometry()%>%
  group_by(Aggregated_gear, Region)%>%
  summarise(corrected_effort = sum(corrected_effort, na.rm = T))%>%
  ungroup()

#### Scale and sum efforts by habitat type and gear ####

Absolute_effort_habitats <- left_join( habitat_weights,IMR_effort) %>%        # Combine actual effort with habitat distribution
  mutate(effort = corrected_effort * percentage_presence )%>%                       # Scale
  group_by(Aggregated_gear, Habitat) %>%                                      # Sum by gear and habitat combination
  summarise(effort = sum(effort, na.rm = T)) %>%                              # Total by group
  ungroup() %>%                                                               # Ungroup for speed
  drop_na() %>%                                                               # Drop habitats labelled as NA
  right_join(target) %>%                                                      # Join to combinations of all gears and habitats
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = effort) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                           # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns
saveRDS(Absolute_effort_habitats, "./Objects/IMR absolute habitat effort.rds")# Save

#Absolute_effort_habitats<-readRDS("./Objects/IMR absolute habitat effort.rds")
## How much of the corrected effort is allocated a habitat type?
sum(Absolute_effort_habitats, na.rm = T) / sum(IMR_effort$corrected_effort, na.rm = T)

