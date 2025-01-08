
## Combine EU and Norwegian fishing effort, then inflate by missing Russian landings to get International effort

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr", "sf")        # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multisession)

domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need effort scaled per m^2
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

IMR <- readRDS("./Objects/IMR absolute fishing effort.rds")                 # Import Norwegian fishing effort        

EU <- readRDS("./Objects/EU absolute fishing effort.rds")                   # Import EU fishing effort

Dredge<- readRDS("./Objects/Mollusc dredge effort.rds") # Import dredge fishing effort

target <- read.csv("./Data/MiMeMo_gears.csv") %>%                           # Load fishing gear classifications
  dplyr::select(Aggregated_gear, Gear_type) %>%                             # Select gear names
  distinct() %>%                                                            # Drop duplicates
  filter(Aggregated_gear != "Dropped")                                      # Drop unused gears

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%    # reproject to match EU data
  dplyr::select(-c(Shore, Elevation, area)) %>%                             # Drop unnecessary columns
  st_union() %>%                                                            # Create whole domain shape 
  nngeo::st_remove_holes() %>%                                              # Drop holes so we don't accidentally lose fishing near Svalbard
  st_make_valid() %>%                                                       # Still some residual holes so make valid
  nngeo::st_remove_holes()                                                  # And drop again

#### Get Russian correction factor ####
Inflation_pole_and_line <- c(
  "NOR-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "FRO-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "ISL-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "RUS-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "EU+UK-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "REST-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets"
)%>% 
  future_map(~{ brick("./Objects/GFW_longlines.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T)%>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-")

Inflation_pots_and_traps <- c(
  "NOR-pots_and_traps",
  "FRO-pots_and_traps",
  "ISL-pots_and_traps",
  "RUS-pots_and_traps",
  "EU+UK-pots_and_traps",
  "REST-pots_and_traps"
)%>% 
  future_map(~{ brick("./Objects/GFW_pots.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T)%>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-")

Inflation_seiners <- c(
  "NOR-seiners",
  "FRO-seiners",
  "ISL-seiners",
  "RUS-seiners",
  "EU+UK-seiners",
  "REST-seiners"
)%>% 
  future_map(~{ brick("./Objects/GFW_seiners.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T)%>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-") 


Inflation_trawlers <- c(
  "NOR-trawlers",
  "FRO-trawlers",
  "ISL-trawlers",
  "RUS-trawlers",
  "EU+UK-trawlers",
  "REST-trawlers"
)%>% 
  future_map(~{ brick("./Objects/GFW_trawlers.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T)%>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-")

Inflation_dredge <- c(
  "NOR-dredge_fishing",
  "FRO-dredge_fishing",
  "ISL-dredge_fishing",
  "RUS-dredge_fishing",
  "EU+UK-dredge_fishing",
  "REST-dredge_fishing"
)%>% 
  future_map(~{ brick("./Objects/GFW_dredge.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>%                                             # Take the mean across years
      exact_extract(st_as_sf(domain), fun = "sum") %>%                      # Sum fishing hours within the model domain 
      data.frame(Hours = ., Variable = .x)}, .progress = T)%>%             # Attach the variable name to keep track
  data.table::rbindlist() %>% 
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-") %>%          # Split variable name into flag and gear type
  mutate(Gear_type = str_to_sentence(Gear_type)) 





Inflation <- rbind(Inflation_pole_and_line, Inflation_pots_and_traps, Inflation_seiners, Inflation_trawlers, Inflation_dredge) %>%
  group_by(Gear_type) %>%
  mutate(total_gear_effort = sum(Hours)) %>% 
  filter(!Flag %in% c("RUS","FRO","ISL","REST")) %>%
  summarise(Inflation = mean(total_gear_effort) / sum(Hours),
            Total_Hours = sum(Hours)) %>%  # Track total hours for weighting
  ungroup() %>%
  right_join(target) %>% 
  drop_na(Aggregated_gear) %>%
  group_by(Aggregated_gear) %>%
  summarise(Inflation = ifelse(Aggregated_gear %in% c("Harpoons", "Rifles", "Kelp harvesting", "Recreational", "Dredging"),
                               1,sum(Inflation * Total_Hours) / sum(Total_Hours))) %>%  # Weighted average
  ungroup() %>%
  distinct()%>% #drop_duplicates rownames
  column_to_rownames('Aggregated_gear') %>%
  dplyr::select(Inflation) %>%
  as.matrix() %>%
  .[order(row.names(.)),]

####  Scale to international effort ####

Alien <- (EU + IMR+ Dredge) * Inflation - (IMR+ Dredge) 

names(Alien) <- ifelse(
  names(Alien) == "Pelagic_Trawlers", "Pelagic_Trawlers_ALIEN",
  ifelse(names(Alien) == "Pelagic_Seiners", "Pelagic_Seiners_ALIEN", names(Alien))
)
Alien["Pelagic_Trawlers_NORW"] <- 0
Alien["Pelagic_Seiners_NORW"] <- 0

Alien<-Alien[order(names(Alien))]

Norway<-IMR+Dredge
names(Norway) <- ifelse(
  names(Norway) == "Pelagic_Trawlers", "Pelagic_Trawlers_NORW",
  ifelse(names(Norway) == "Pelagic_Seiners", "Pelagic_Seiners_NORW", names(Norway))
)
Norway["Pelagic_Trawlers_ALIEN"] <- 0
Norway["Pelagic_Seiners_ALIEN"] <- 0

Norway<-Norway[order(names(Norway))]

International<-Alien+Norway

International["Recreational"] <- 2363693.491  # Hours from Mike's stories

transformed_International <- (International / 365) *                        # Convert to daily effort
                             (60 * 60) /                                    # in seconds (from hours) 
                             domain_size                                    # per m^2

transformed_International["Kelp harvesting"] <- 1                           # We need a nominal effort even though real values are 0

saveRDS(transformed_International, "./Objects/International effort by gear.rds")


#International<-readRDS("./Objects/International effort by gear.rds")

