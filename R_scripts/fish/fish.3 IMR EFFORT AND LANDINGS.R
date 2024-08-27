
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse", "sf", "raster", "exactextractr")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%     # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

gear <- read.csv("./Data/MiMeMo_gears.csv",check.names = F)                                   # Import gear names

guild <- read.csv2("./Data/MiMeMo fish guilds.csv",check.names = F) %>%                        # Import guild names
  dplyr::select(Guild, `IMR code`) %>%                                          # Limit to IMR system
  drop_na() %>%                                                               # Drop those without an IMR code
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(`IMR code`) %>%                                                      # 1 duplicated IMR code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

Regions <- sf::read_sf(dsn="./Data/IMR/Regions/") %>%                      # Import IMR regions shapefile                                                            # Convert to SF
  st_join(Domains) %>%                                                        # Which regions are in the model domain?
  drop_na() %>%                                                               # Drop those outside
  dplyr::select(Region = havomr)                                                          # Keep unique shapes

GFW_longlines <- brick("./Objects/GFW_longlines.nc", varname = "NOR-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_pots <- brick("./Objects/GFW_pots.nc", varname = "NOR-pots_and_traps") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_seiners <- brick("./Objects/GFW_seiners.nc", varname = "NOR-Seiners") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_strawlers <- brick("./Objects/GFW_strawlers.nc", varname = "NOR-Shelf_trawlers") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_ptrawlers <- brick("./Objects/GFW_ptrawlers.nc", varname = "NOR-Pelagic_trawlers") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_dredge <- brick("./Objects/GFW_dredge.nc", varname = "NOR-dredge_fishing") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

IMR <- data.table::fread("./Data/IMR/logbookNOR_00to20_b.lst", sep = ';',     # Import IMR fishing data
                         colClasses = c(RE = "character", HO = "character")) %>% # Overwriting default column types  
  `colnames<-`(c("Year", "Month", "Day", "Gear_code", "Fishing_time", "Area_code", "Economic_zone", 
                 "Region", "Location_Norway", "Vessel_length", "IMR code", "Weight")) %>% # Set column names
  dplyr::select(Year, Month, Day, Gear_code, Fishing_time, Region, `IMR code`, "Weight") %>% # Ditch unnecessary columns
  left_join(gear) %>%                                                         # Attach gear labels
  left_join(guild) %>%                                                        # Attach guild labels
  filter(Aggregated_gear != "Dropped", Region %in% Regions$Region,            # Limited to gears and regions of interest
         between(Year, 2010, 2019))                                           # To when the electronic reporting system started to the last complete year

Unrepresented <- expand.grid(Aggregated_gear = unique(IMR$Aggregated_gear),   # Averages were being inflated because years with 0 landings
                             Gear_type = unique(IMR$Gear_type),               # Aren't represented. 
                             Guild = unique(IMR$Guild),                       # This object will add the 0s back in to correct things
                             Region = unique(IMR$Region), 
                             Year = unique(IMR$Year))
  
IMR <- full_join(IMR, Unrepresented) %>%                                      # Add in 0s
  replace_na(list(Fishing_time = 0, Weight = 0)) %>% 
  filter(ifelse(Guild == "Cetacean" & Year < 2013, F, T)) %>%                 # Remove excess 0s because whale records seem to start after 2012
  group_by(Aggregated_gear, Gear_type, Guild, Region, Year) %>%                         
  summarise(Effort = sum(Fishing_time, na.rm = T),                            # Total up effort within years
            Weight = sum(Weight, na.rm = T)/1000) %>%                         # Total up landings within years
  summarise(Effort = mean(Effort, na.rm = T),                                 # Then average across years
            Weight = mean(Weight, na.rm = T)) %>%                             
  ungroup() %>% 
  merge(Regions, .)%>%
  st_as_sf()%>%
  st_transform(crs=st_crs(Domains))

#### Correct IMR by overlap with model domain ####

Regions_GFW <- Regions %>% 
  mutate(longlines_total = exact_extract(GFW_longlines, ., fun = "sum"),            # Get all longlines fishing effort from GFW in an IMR region
         pots_total = exact_extract(GFW_pots, ., fun = "sum"),
         seiners_total = exact_extract(GFW_seiners, ., fun = "sum"),
         ptrawlers_total = exact_extract(GFW_ptrawlers, ., fun = "sum"),
         strawlers_total = exact_extract(GFW_strawlers, ., fun = "sum"),
         dredge_total = exact_extract(GFW_dredge, ., fun = "sum"),) %>%        # This is the total effort to scale features to within a polygon
  st_drop_geometry()                                                          # Drop geometry for a non-spatial join

corrected_IMR <- rownames_to_column(IMR, var = "Feature") %>%  # Create a column to track each polygon
  st_intersection(Domains) %>%                             # Crop the IMR polygons to the model domain
  filter(st_is(.,c("POLYGON","MULTIPOLYGON")))%>%
  mutate(longlines_feature = exact_extract(GFW_longlines, ., fun = "sum"),          # Get the GFW fishing effort in each shape
         pots_feature = exact_extract(GFW_pots, ., fun = "sum"),
         seiners_feature = exact_extract(GFW_seiners, ., fun = "sum"),
         ptrawlers_feature = exact_extract(GFW_ptrawlers, ., fun = "sum"),
         strawlers_feature = exact_extract(GFW_strawlers, ., fun = "sum"),
         dredge_feature = exact_extract(GFW_dredge, ., fun = "sum")) %>%      # Depending on gear type
  left_join(Regions_GFW) %>%                                                  # Attach total GFW effort by IMR region
  mutate(
    GFW_Scale = case_when(
      Gear_type == "Shelf_trawlers" ~ (strawlers_feature) / strawlers_total, # Depending on gear type
      Gear_type == "Seiners" ~ (seiners_feature) /seiners_total,
      Gear_type == "Pelagic_trawlers" ~ (ptrawlers_feature) /ptrawlers_total,
      Gear_type == "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets" ~ (longlines_feature) / longlines_total,
      Gear_type == "pots_and_traps" ~ (pots_feature) / pots_total,
      Gear_type == "dredge_fishing" ~ (dredge_feature) / dredge_total)) %>%   # Get proportion of GFW effort from a region within a feature
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the region replace NA with 1 to not use this for scaling
  mutate(corrected_effort = Effort*GFW_Scale,                                 # Scale whole region effort per gear by the proportion of GFW activity by gear type in the model domain 
         corrected_weight = Weight*GFW_Scale)

saveRDS(corrected_IMR, "./Objects/IMR regional absolute effort and landings.rds")

#### Convert IMR landings to a matrix by guild and gear ####
guild <- read.csv2("./Data/MiMeMo fish guilds.csv")

landings_target <- expand.grid(Guild = unique(guild$Guild), # Forces birds and pinnipeds back in 
                      Aggregated_gear = unique(gear$Aggregated_gear) )      # Get combinations of gear and guild

landings <- st_drop_geometry(corrected_IMR) %>% 
  group_by(Aggregated_gear, Guild) %>% 
  summarise(corrected_weight = sum(corrected_weight, na.rm = TRUE)) %>%     # Total fishing effort across regions
  ungroup() %>% 
  right_join(landings_target) %>%                                           # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the unneeded gear class
  replace_na(replace = list(corrected_weight = 0)) %>%                      # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = corrected_weight) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

saveRDS(landings, "./Objects/IMR landings by gear and guild.rds")

heatmap(landings)

#### Summarise IMR Effort by gear ####

effort_target <- dplyr::select(gear, Aggregated_gear) %>%                     # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                        # Drop unused gears
  
effort <- st_drop_geometry(corrected_IMR) %>%                                 # Remove geometries
  group_by(Aggregated_gear) %>%                                               # By gear
  summarise(Hours = sum(corrected_effort, na.rm = T)) %>%                     # Total fishing effort
  right_join(effort_target) %>%                                               # Reintroduce unobserved gears
  replace_na(replace = list(Hours = 0)) %>%                                   # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(effort, "./Objects/IMR absolute fishing effort.rds")                  # Save
#### visual checks ####

# library(ggnewscale)
# library(stars)
# 
# star <- st_as_stars(GFW_trawlers)    # convert GFW to stars objects for use in sf plots
# star2 <- st_as_stars(GFW_seiners)
# star3 <- st_as_stars(GFW_pots)
# star4 <- st_as_stars(GFW_longlines)
# star5 <- st_as_stars(GFW_dredge)
# 
# # Overlay domain, GFW, and IMR
# 
# ggplot() +
#   geom_sf(data = Regions, fill = "yellow", colour = "black") +
#   geom_sf(data = IMR_effort, aes(fill = GFW_Scale)) +
#   new_scale("fill") +
#   geom_stars(data=star) +
#   scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt") +
#   labs(title = str_glue("{round(sum(IMR_effort$corrected_effort, na.rm = T)/
#                         sum(IMR$Effort, na.rm = T), 2) *100}% of fishing effort allocated"))
