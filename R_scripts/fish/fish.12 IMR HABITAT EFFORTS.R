
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

GFW_longlines <- brick("./Objects/GFW_longlines.nc", varname = "NOR-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_pots <- brick("./Objects/GFW_pots.nc", varname = "NOR-pots_and_traps") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_seiners <- brick("./Objects/GFW_seiners.nc", varname = "NOR-Seiners") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_ptrawlers <- brick("./Objects/GFW_ptrawlers.nc", varname = "NOR-Pelagic_trawlers") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_strawlers <- brick("./Objects/GFW_strawlers.nc", varname = "NOR-Shelf_trawlers") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_dredge <- brick("./Objects/GFW_dredge.nc", varname = "NOR-dredge_fishing") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(RE = "character", HO = "character")) %>% # Overwriting default column types  
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Region", "Location_Norway", "Vessel_length", "IMR.code", "Weight")) %>% # Set column names
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region) %>% 
  left_join(gear) %>%                                                         # Attach labels
  filter(Aggregated_gear != "Dropped", Region %in% Regions$Region,            # Limit to regions and gears of interest
         between(Year, 2011, 2019))   

Unrepresented <- expand.grid(Aggregated_gear = unique(IMR$Aggregated_gear),   # Averages were being inflated because years with 0 effort in gears
                             Gear_type = unique(IMR$Gear_type),               # Aren't represented. 
                             Region = unique(IMR$Region), 
                             Year = unique(IMR$Year))

IMR <- full_join(IMR, Unrepresented) %>%                                      # Add in 0s
  replace_na(list(Fishing_time = 0)) %>% 
  group_by(Aggregated_gear, Gear_type, Region, Year) %>% 
  summarise(Effort = sum(Fishing_time, na.rm = T)) %>%
  summarise(Effort = mean(Effort, na.rm = T)) %>%                             # Get mean effort per region and gear across years
  ungroup() %>% 
  merge(Regions, .)                                                           # Bind to sf polygons for regions

#### Proportion IMR effort across seabed habitats by IMR region ####

habitat_weights <- rownames_to_column(IMR, var = "Feature") %>%               # Create a column to track each IMR region and gear combination
  st_intersection(habitats) %>%                                               # Crop the IMR region polygons to habitat types
  mutate(GFW = case_when(Gear_type == "Shelf_trawlers" ~ exact_extract(GFW_strawlers, ., fun = "sum"), # Depending on gear type
                         Gear_type == "Pelagic_trawlers" ~ exact_extract(GFW_ptrawlers, ., fun = "sum"),
                         Gear_type == "Seiners" ~ exact_extract(GFW_seiners, ., fun = "sum"),
                         Gear_type == "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets" ~ exact_extract(GFW_longlines, ., fun = "sum"),
                         Gear_type == "pots_and_traps" ~ exact_extract(GFW_pots, ., fun = "sum"),
                         Gear_type == "dredge_fishing" ~ exact_extract(GFW_dredge, ., fun = "sum")),            # From GFW by mobile and static gear
         Habitat = paste0(Shore, " ", Habitat)) %>%                           # Combine habitat labels
  group_by(Region, Aggregated_gear) %>%                                       # Per region and gear
  mutate(habitat_share = GFW / sum(GFW, na.rm = T)) %>%                       # Work out the proportion of activity in each piece split over habitats
  ungroup() %>% 
  st_drop_geometry() %>% 
  dplyr::select(habitat_share, Region, Aggregated_gear, Habitat)

check <- habitat_weights %>%                                                  # Check shares sum to 1 by gear
  group_by(Region, Aggregated_gear) %>% 
  summarise(check = sum(habitat_share, na.rm = T))

#### Absolute effort scaled by proportion of GFW activity in an IMR region falling in the model domain ####

IMR_effort <- readRDS("./Objects/IMR regional absolute effort and landings.rds") %>% 
st_drop_geometry() %>% 
  dplyr::select(corrected_effort, Aggregated_gear, Region)

#### Scale and sum efforts by habitat type and gear ####

Absolute_effort_habitats <- left_join(habitat_weights, IMR_effort) %>%        # Combine actual effort with habitat distribution
  mutate(effort = corrected_effort * habitat_share) %>%                       # Scale
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

## How much of the corrected effort is allocated a habitat type?
sum(Absolute_effort_habitats, na.rm = T) / sum(IMR_effort$corrected_effort, na.rm = T)

