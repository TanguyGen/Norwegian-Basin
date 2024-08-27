
## Estimate the discard rates for the EU fishing fleet in the Barents Sea

#### Set up ####

# Wipe environment and load required data
rm(list = ls())

packages <- c("data.table","tidyr","purrr", "furrr", "future", "tictoc", "raster", "sf","progressr","tictoc","dplyr","tibble","exactextractr")
lapply(packages, library, character.only = TRUE)


gear <- read.csv("./Data/MiMeMo_gears.csv")                     # Limit to FAO system
  
guild <- read.csv2("./Data/MiMeMo fish guilds.csv")

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%
  st_as_sf() %>%
  mutate(Keep = TRUE)

Files <- list.files(path = "./Data/Fiskeridirektoratet/Open_data",
                    pattern = "*.csv",
                    full.names = T) # Get a list of files to import

# Set up parallel execution with furrr
plan(multisession)
tic()
options(future.globals.maxSize = 2 * 1024 ^ 3)
with_progress({
  p <- progressor(along = Files)
  Landings <- future_map(Files, ~ {
    p()
    # For each csv
    fread(.x, , sep = ";", dec = ",") %>%
      dplyr::select(
        `Redskap (kode)`,
        `Art FAO (kode)`,
        `Produktvekt`,
        `Lon (lokasjon)`,
        `Lat (lokasjon)`
      ) %>%
      mutate(
        Gear_code = `Redskap (kode)`,
        FAO = `Art FAO (kode)`,
        Gear_code = as.character(Gear_code),
      ) %>%
      left_join(gear) %>%                                                         # Attach gear labels
      left_join(guild) %>%                                                        # Attach guild labels
      filter(Aggregated_gear == "Dredging")%>%
      mutate(Points=st_sfc(map2(`Lon (lokasjon)`, `Lat (lokasjon)`, ~ st_point(c(.x, .y))), crs = 4326),
             Within=map_lgl(Points, ~ any(st_within(., Domains, sparse = FALSE)))
      )%>%
      filter(between(`Lat (lokasjon)`, 62, 75),                                 # Keep fishing location in the Norwegian Sea
             between(`Lon (lokasjon)`, -7, 16),
             Within==TRUE) %>%
      dplyr::select(`Produktvekt`, Aggregated_gear, Guild) %>%
      as.data.frame()                                           # Convert to data frame to play nicely with rasters
  })%>%
    data.table::rbindlist() %>%
    group_by(Aggregated_gear, Guild) %>%         # Remove missing data
    summarise(`Produktvekt` = mean(`Produktvekt`, na.rm = TRUE)) %>%                            # Mean the offal percentage for each gear and guild
    ungroup()
})
toc()

landings_target <- expand.grid(Guild = unique(guild$Guild), # Forces birds and pinnipeds back in 
                               Aggregated_gear = unique(gear$Aggregated_gear) ) 


Dredge_Landings<- Landings%>% 
  right_join(landings_target)%>% # Create one big data frame
  filter(Aggregated_gear != "Dropped") %>%                                  # Ditch the unneeded gear class
  replace_na(replace = list(Produktvekt = 0)) %>%                      # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Produktvekt) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                           # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] 

saveRDS(Dredge_Landings, "./Objects/Mollusc dredge landings.rds")
#Dredge_Landings<-readRDS("./Objects/Mollusc dredge landings.rds")

GFW_dredge <- brick("./Objects/GFW_dredge.nc", varname = "NOR-dredge_fishing") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

effort_target <- dplyr::select(gear, Aggregated_gear) %>%                     # Select gear names
  distinct() %>%                                                              # Drop duplicates
  filter(Aggregated_gear != "Dropped")  

habitats <- readRDS("./Objects/Habitats.rds") %>%                           # Load habitat polygons
  mutate(area = as.numeric(st_area(.)))

habitat_target <- expand.grid(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = unique(gear$Aggregated_gear))         # Get combinations of gear and habitat



Effort_dredge <-c(
  "NOR-dredge_fishing",
  "FRO-dredge_fishing",
  "ISL-dredge_fishing",
  "RUS-dredge_fishing",
  "EU+UK-dredge_fishing",
  "REST-dredge_fishing"
) %>%
  future_map(~{ brick("./Objects/GFW_dredge.nc", varname = .x ) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()%>%
  pivot_longer(starts_with("sum"), names_to = "Year", values_to = "Hours") %>%# Reshape so all years are in a column
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-") %>%# Split variable name into flag and gear type
  left_join(gear)%>%
  mutate(Year = as.numeric(str_remove(Year, "sum.X")) + 2011,
         Habitat = paste0(Shore, " ", Habitat))%>%
  dplyr::select(Year,Hours,Habitat,Aggregated_gear)%>%
  group_by(Aggregated_gear,Habitat)%>%
  summarise(Hours=mean(Hours,na.rm=TRUE))%>%
  ungroup()

total_effort<-Effort_dredge%>%
  group_by(Aggregated_gear)%>%
  summarise(Hours=sum(Hours,na.rm=TRUE))%>%
  ungroup()%>%
  right_join(effort_target)%>%
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(Hours = 0)) %>%                                   # Nas are actually effort of 0
  column_to_rownames('Aggregated_gear') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),] 


habitat_effort<-Effort_dredge%>%
  right_join(habitat_target)%>%
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(Hours = 0)) %>%                                  # Nas are actually landings of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Hours) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                           # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]

saveRDS(total_effort, "./Objects/Mollusc dredge effort.rds")
saveRDS(habitat_effort, "./Objects/Mollusc dredge habitat effort.rds")

