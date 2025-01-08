
# Calculate absolute fishing effort by gear across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R_scripts/@_Region file.R")                                         # Get region mask

plan(multisession)

Region_mask <- st_transform(Region_mask, crs = 4326)                          # reproject to match EU data

GFW_longlines <- brick("./Objects/GFW_longlines.nc", varname = "EU+UK-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets") %>%      # Get mean fishing effort across years from Global fishing watch
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_pots <- brick("./Objects/GFW_pots.nc", varname = "EU+UK-pots_and_traps") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_seiners <- brick("./Objects/GFW_seiners.nc", varname = "EU+UK-seiners") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_trawlers <- brick("./Objects/GFW_trawlers.nc", varname = "EU+UK-trawlers") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

GFW_dredge <- brick("./Objects/GFW_dredge.nc", varname = "EU+UK-dredge_fishing") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                               # Drop unnecessary columns
  st_union() %>%                                                              # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                                # Drop holes so we don't accidentally lose fishing near Svalbard
  st_make_valid() %>%                                                         # Still some residual holes so make valid
  nngeo::st_remove_holes()                                                    # And drop again

gears <- read.csv("./Data/MiMeMo_gears.csv")                                  # Load fishing gear classifications

EU <- sf::read_sf(dsn="./Data/EU_fish/spatial_effort_2015-2018/") %>%      # Import EU effort shapefile
  st_as_sf() %>%                                                              # Convert to SF
  dplyr::select(year, quarter, ger_typ, rctngl_, ttfshdy) %>%                 # Drop some columns, ttfshdy is "total fishing days"
  rename(Gear_code = ger_typ)

tictoc::tic()
EU_Arctic <- st_contains(Region_mask, EU, sparse = F) %>%                     # Which EU polygons are in the model mask?
  t() %>%                                                                     # Transpose to indicate row not columns
  EU[.,] %>%                                                                  # Subset the Eu data spatially
  left_join(gears) %>%                                                        # Attach gear classifications
  filter(Aggregated_gear != "Dropped") %>%
  rownames_to_column(var = "EU_polygon") 
tictoc::toc()
#### Locate EU effort inside the model domain ####

tictoc::tic()
oopts <- options(future.globals.maxSize = 800 * 1024 ^ 2)  ## 1.0 GB
on.exit(options(oopts))
corrected_effort <- dplyr::select(EU_Arctic, EU_polygon, Gear_type) %>%       # Limit to information needed to calculate the proportion of fishing effort in the model domain
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%                                                     # Isolate each shape for fast paralel processing
  future_map( ~{                                                              # In parallel
    mutate(.x, total=case_when(Gear_type == "trawlers" ~ exact_extract(GFW_trawlers, .x, fun = "sum"), # Depending on gear type
                               Gear_type == "seiners" ~ exact_extract(GFW_seiners, .x, fun = "sum"),
                               Gear_type == "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets" ~ exact_extract(GFW_longlines, .x, fun = "sum"),
                               Gear_type == "pots_and_traps" ~ exact_extract(GFW_pots, .x, fun = "sum"),
                               Gear_type == "dredge_fishing" ~ exact_extract(GFW_dredge, .x, fun = "sum")))  %>%                        # If this is a mobile gear) 
      # This is the total effort to scale features to within a polygon
      st_intersection(Domains) %>%     # Crop the polygons to the model domain
      mutate(feature=case_when(Gear_type == "trawlers" ~ exact_extract(GFW_trawlers, .x, fun = "sum"), # Depending on gear type
                               Gear_type == "seiners" ~ exact_extract(GFW_seiners, .x, fun = "sum"),
                               Gear_type == "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets" ~ exact_extract(GFW_longlines, .x, fun = "sum"),
                               Gear_type == "pots_and_traps" ~ exact_extract(GFW_pots, .x, fun = "sum"),
                               Gear_type == "dredge_fishing" ~ exact_extract(GFW_dredge, .x, fun = "sum")))  %>% 
               st_drop_geometry()}, .progress = T) %>%            
  data.table::rbindlist() %>%                                                 # Bind each fishing feature back into a DF
  mutate(GFW_Scale = feature/total) %>%                                       # Get the proportion of effort per polygon in the domain
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the polygon replace NA with 1 to not use this for scaling
  dplyr::select(GFW_Scale, EU_polygon) %>% 
  left_join(EU_Arctic) %>% 
  mutate(effort_contributions = ttfshdy*GFW_Scale*24)                         # Scale fishing effort by area in the model domain
tictoc::toc()    

saveRDS(corrected_effort, "./Objects/EU corrected pixel fishing effort.rds")  # Save

#### Summarise ####

target <- expand.grid(Aggregated_gear = unique(gears$Aggregated_gear),        # Select gear names
                      year = 2015:2018) %>%                                   # Drop duplicates
  filter(Aggregated_gear != "Dropped")                               # Drop unused gears


summary <- group_by(corrected_effort, Aggregated_gear, year) %>%              # By year and gear
  summarise(effort = sum(effort_contributions, na.rm = TRUE)) %>%             # total the fishing effort
  ungroup() %>% 
  drop_na() %>%                                                               # Drop unassigned gears
  right_join(target) %>%                                                      # Reintroduce unobserved gears and years otherwise you miss 0s when averaging
  replace_na(replace = list(effort = 0)) %>%                                  # Nas are actually effort of 0
  group_by(Aggregated_gear) %>% 
  summarise(effort = mean(effort, na.rm = TRUE)) %>%                          # Average for each gear across years
  remove_rownames() %>% 
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

saveRDS(summary, "./Objects/EU absolute fishing effort.rds")                  # Save

#### Visualise ####

# library(ggnewscale)
# library(stars)
# 
# star <- st_as_stars(GFW_mobile)    # convert GFW to stars objects for use in sf plots
# star2 <- st_as_stars(GFW_static)
# 
# ggplot() +
#   geom_sf(data = domain, fill = "yellow", colour = "black") +
#   geom_sf(data = EU_Arctic, aes(fill = ttfshdy), alpha = 0.3, colour = NA) +
#   new_scale("fill") +
#   geom_stars(data=star) +
#   scale_fill_continuous(na.value = NA, low = "purple", high = "limegreen", trans = "sqrt")
