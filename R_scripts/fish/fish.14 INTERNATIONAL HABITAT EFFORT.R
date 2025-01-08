
## Combine EU, Norwegian, Russian fishing effort, then get international effort proportion across habitats by gear

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr", "sf")        # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multisession)

gear <- read.csv("./Data/MiMeMo_gears.csv") 

habitats <- readRDS("./Objects/Habitats.rds") %>%                           # Load habitat polygons
  mutate(area = as.numeric(st_area(.)))

target <- expand.grid(Habitat = paste0(habitats$Shore, " ", habitats$Habitat), 
                      Aggregated_gear = unique(gear$Aggregated_gear))       # Get combinations of gear and guild


domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need effort scaled per m^2
  sf::st_union() %>%                                                        # To match the final units for international effort
  sf::st_area() %>% 
  as.numeric()

Dredge<- readRDS("./Objects/Mollusc dredge habitat effort.rds")

Non_Rest_habitat <- (readRDS("./Objects/IMR absolute habitat effort.rds") + Dredge + # Add Norwegian fishing effort        
                  readRDS("./Objects/EU absolute habitat effort.rds"))  /    # to EU fishing effort
  365 * (60 * 60) / domain_size                                             # Convert to same units as international effort

Non_Rest_total <- (readRDS("./Objects/IMR absolute fishing effort.rds")+ colSums(Dredge)  + # Import Norwegian fishing effort        
  readRDS("./Objects/EU absolute fishing effort.rds"))/                     # Import EU fishing effort
  365 * (60 * 60) / domain_size    # Convert to same units as international effort 



#### Get Russian + Faroe + Iceland effort only ####

Total_effort <- as.data.frame(t(readRDS("./Objects/International effort by gear.rds")))%>%
  mutate(Pelagic_Trawlers=Pelagic_Trawlers_NORW+Pelagic_Trawlers_ALIEN,
         Pelagic_Seiners=Pelagic_Seiners_NORW+Pelagic_Seiners_ALIEN)%>%
  select(-c(Pelagic_Trawlers_NORW,Pelagic_Trawlers_ALIEN,Pelagic_Seiners_NORW,Pelagic_Seiners_ALIEN))%>%
  as.matrix()                                                            # Convert to matrix

Total_effort <- Total_effort[, order(colnames(Total_effort))]


Rest_effort <- Total_effort-Non_Rest_total 

#### Russian effort over habitat types ####
Habitat_weights_longlines <- c("RUS-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
                               "REST-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
                               "ISL-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
                               "FRO-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets") %>% 
  future_map(~{ brick("./Objects/GFW_longlines.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() 

Habitat_weights_pots <- c("RUS-pots_and_traps",
                          "REST-pots_and_traps",
                          "ISL-pots_and_traps",
                          "FRO-pots_and_traps") %>% 
  future_map(~{ brick("./Objects/GFW_pots.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() 

Habitat_weights_seiners <- c("RUS-seiners",
                             "REST-seiners",
                             "ISL-seiners",
                             "FRO-seiners") %>% 
  future_map(~{ brick("./Objects/GFW_seiners.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() 

Habitat_weights_trawlers <- c("RUS-trawlers",
                              "REST-trawlers",
                              "ISL-trawlers",
                              "FRO-trawlers") %>% 
  future_map(~{ brick("./Objects/GFW_trawlers.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() 


Habitat_weights_dredge <- c("RUS-dredge_fishing",
                            "REST-dredge_fishing",
                            "ISL-dredge_fishing",
                            "FRO-dredge_fishing") %>% 
  future_map(~{ brick("./Objects/GFW_dredge.nc", varname = .x) %>%                 # Import a brick of all years
      calc(mean, na.rm = T) %>% 
      exact_extract(habitats, fun = "sum") %>%                              # Sum fishing hours within habitat types 
      cbind(st_drop_geometry(habitats)) %>% 
      mutate(Variable = .x)}) %>%                                           # Attach habitat metadata
  data.table::rbindlist() 

Habitat_weights<- data.table::rbindlist(list(Habitat_weights_longlines,Habitat_weights_pots,Habitat_weights_seiners,Habitat_weights_trawlers,Habitat_weights_dredge))%>% 
  separate(Variable, into = c(NA, "Gear_type", NA), sep = "-") %>%          # Split variable name into flag and gear type
  group_by(Gear_type) %>% 
  transmute(Proportion = ifelse(sum(`.`) == 0, 0, `.`/sum(`.`)),                                      # Calculate the proportion of fishing effort in each row
            Habitat = paste0(Shore, " ", Habitat)) %>%                     # Capitalise to allow a join     
  ungroup() %>%
  left_join(distinct(dplyr::select(gear, Aggregated_gear, Gear_type))) %>%  # Join gear type to aggregated gears
  right_join(target) %>% 
  filter(Aggregated_gear != "Dropped") %>% 
  dplyr::select(-Gear_type) %>% 
  replace_na(list(Proportion = 0)) %>%  # Nas are actually landings of 0
  mutate(Proportion=as.numeric(Proportion))%>%
  group_by(Habitat,Aggregated_gear)%>%
  summarise(Proportion=sum(Proportion))%>%
  ungroup()%>%
  pivot_wider(names_from = Aggregated_gear, values_from = Proportion) %>%   # Spread dataframe to look like a matrix
  column_to_rownames('Habitat') %>%                                         # Remove character column
  as.matrix() %>%                                                           # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                # Alphabetise rows and columns

Rest <- t(t(Habitat_weights) * Rest_effort)                           # Scale effort by proportion over habitats

####  Scale to international effort ####

International <- Non_Rest_habitat + Rest                            # Get international effort by gear and habitat

International[,"Recreational"] <- c(habitats$area[1:4],0)             # Distribute recreational activity across the inshore zone according to area
International[,"Kelp harvesting"] <- 0                                      # We force all kelp harvesting to happen
International["Inshore Rock", "Kelp harvesting"] <- 1                       # over inshore rock.

International_proportion <-  t(t(International)/colSums(International))      # Scale as proportions within gears

heatmap(International_proportion)                                           # Visualise

saveRDS(International_proportion, "./Objects/International effort proportion by gear and habitat.rds")

