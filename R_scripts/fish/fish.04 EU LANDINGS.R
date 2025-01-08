
# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R_scripts/@_Region file.R")                                         # Get region mask

future::plan(multisession)                                                            # Set up parallel processing



Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

Region_mask<-Region_mask%>%
  st_transform(crs = st_crs(Domains))

  # reproject to match EU data

gear <- read.csv("./Data/MiMeMo_gears.csv")                                   # Load fishing gear classifications

guild <- read.csv("./Data/MiMeMo fish guilds.csv") %>%                        # Get guilds for FAO codes
  dplyr::select(FAO, Guild) %>% 
  rename(species = FAO) %>% 
  drop_na() %>% 
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(species) %>%                                                       # 1 duplicated code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()


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

landings_target <- expand.grid(Guild = unique(read.csv("./Data/MiMeMo fish guilds.csv")$Guild), # reintroduces guilds not in FAO 
                               Aggregated_gear = unique(gear$Aggregated_gear),
                               year = 2015:2018) # Get combinations of gear, guild, and year to reintroduce unrepresented levels

EU_landings <- str_glue("./Data/EU_fish/spatial_landings_{2015:2018}/") %>%   # Programmatically build folder names
  future_map(~{ read_sf(.x) %>%                                        # Import each EU effort shapefile
                st_as_sf() %>%                                                # Convert to SF
                dplyr::select(year, ger_typ, rctngl_, species, ttwghtl, ttvllnd)}, # Drop some columns, ttwghtl is "total weight landed",
            .progress = T) %>%                                                # ttvllnd is "total value landed"
  bind_rows() %>% 
  rename(Gear_code = ger_typ)
tic()
EU_Arctic <- st_contains(Region_mask, EU_landings, sparse = F)  %>% # Which EU polygons are in the model mask? 
  t() %>%                                                                     # Transpose to indicate row not columns
  EU_landings[.,] %>%                                                         # Subset the Eu data spatially
  rownames_to_column(var = "EU_polygon") %>% 
  left_join(gear) %>%                                                         # Attach gear classifications
  left_join(guild)     
toc()
#Attach guild classifications
  
#### Scale EU landings by the proportion of fishing effort according to GFW in the model domain ####

tictoc::tic()
oopts <- options(future.globals.maxSize = 800 * 1024 ^ 2)  ## 1.0 GB
on.exit(options(oopts))
weights <- dplyr::select(EU_Arctic, EU_polygon, Gear_type) %>%                # Limit to information needed to calculate the proportion of fishing effort in the model domain
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%# Isolate each shape for fast parallel processing
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
                             Gear_type == "dredge_fishing" ~ exact_extract(GFW_dredge, .x, fun = "sum"))) %>%  
    st_drop_geometry()}, .progress = T) %>%                                   # Drop geometry for a non-spatial join
  data.table::rbindlist() %>%                                                 # Bind into a dataframe
  mutate(GFW_Scale = feature/total) %>%                                       # Get the proportion of effort per polygon in the domain
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the polygon replace NA with 1 to not use this for scaling
  dplyr::select(GFW_Scale, EU_polygon)
tictoc::toc()

#### Convert EU landings to a matrix by guild and gear ####

corrected_landings <- st_drop_geometry(EU_Arctic) %>%
  left_join(weights) %>% 
  mutate(corrected_weight = ttwghtl * GFW_Scale) %>%                          # Scale features effort per gear by the proportion of GFW activity by gear type in the model domain 
  group_by(Aggregated_gear, Guild, year) %>% 
  summarise(corrected_weight = sum(corrected_weight, na.rm = TRUE)) %>%       # Total within years by guild and gear
  ungroup() %>% 
  drop_na()%>%
  right_join(landings_target) %>%                                             # Reintroduce unrepresented combinations of gear, guild, and year
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(corrected_weight = 0)) %>%                        # Nas are actually landings of 0
  group_by(Aggregated_gear, Guild) %>% 
  summarise(corrected_weight = mean(corrected_weight, na.rm = TRUE)) %>%      # Average across years 
  ungroup() %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = corrected_weight) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns

saveRDS(corrected_landings, "./Objects/EU landings by gear and guild.rds")
heatmap(corrected_landings)

