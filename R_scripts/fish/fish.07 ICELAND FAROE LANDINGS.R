#Obtain total landings across Faroe and Iceland

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
packages <- c("tidyverse","sf")                   # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%     
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

gear <- read.csv("./Data/MiMeMo_gears.csv", check.names = FALSE) # Import gear names

#################### For Iceland

guild <- read.csv2("./Data/MiMeMo fish guilds.csv",check.names = FALSE) %>%                        # Import guild names
  dplyr::select(Guild, `Statistics Iceland`) %>%
  setNames(c("Guild","Species"))%>% 
  drop_na() %>% 
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(Species) %>%                                                      # 1 duplicated IMR code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

landings_target <- expand.grid(Guild = unique(guild$Guild), # reintroduces guilds not in FAO 
                               Aggregated_gear = unique(gear$Aggregated_gear),
                               Year = 2010:2019) # Get combinations of gear, guild, and year to reintroduce unrepresented levels
Iceland_landings <- read.csv2("./Data/Iceland_Faroe/Iceland_landings.csv",check.names = FALSE)%>%
  mutate(across(everything(), ~ifelse(. == "..", NA, .)))%>%  #Change empty boxes  written with a "." by NAs
  setNames(c("Species","Fishing gear","Fishing.area","Unit","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))%>%
  pivot_longer(cols = `2010`:`2019`,names_to = "Year",values_to = "Tonnes")%>%
  left_join(gear)%>%
  left_join(guild)%>%
  mutate(Tonnes=as.numeric(Tonnes),Year=as.numeric(Year))%>%
  dplyr::select(Aggregated_gear, Guild, Year,Tonnes)%>%
  group_by(Aggregated_gear, Guild, Year) %>% 
  summarise(Tonnes = sum(Tonnes, na.rm = TRUE))%>%
  ungroup() %>% 
  right_join(landings_target) %>%                                             # Reintroduce unrepresented combinations of gear, guild, and year
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(Tonnes = 0))%>%
  group_by(Aggregated_gear, Guild) %>% 
  summarise(Tonnes = mean(Tonnes, na.rm = TRUE)) %>%      # Average across years 
  ungroup() %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = Tonnes) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] 

saveRDS(Iceland_landings, "./Objects/Iceland landings by gear and guild.rds")  


#################### For Faroe

guild <- read.csv2("./Data/MiMeMo fish guilds.csv") %>%                        # Import guild names
  dplyr::select(Guild, `Hagstova.fororya`) %>%
  setNames(c("Guild","species"))%>%
  drop_na() %>%                                                               # Drop those without correspondance with Faroe data
  distinct() %>%                                                              # Drop duplicated rows which hang around after ditching other systems
  group_by(species) %>%                                                      # 1 duplicated IMR code to remove ()
  slice_head() %>%                                                            # So only take the first instance of each code
  ungroup()

landings_target_fro <- expand.grid(Guild = unique(guild$Guild), # reintroduces guilds not in FAO 
                               Aggregated_gear = unique(gear$Aggregated_gear),
                               Year = 2015:2019) # Get combinations of gear, guild, and year to reintroduce unrepresented levels
Faroe_landings <- read.csv2("./Data/Iceland_Faroe/Faroe_landings.csv", check.names = FALSE)%>%
  pivot_longer(cols = matches("^\\d{4}"), names_to = "Year_Month", values_to = "Tonnes") %>%
  mutate(across(Tonnes, ~ifelse(. == "-", NA, .)))%>% #Change empty boxes written with a "-" by NAs
  mutate(Tonnes=as.numeric(Tonnes))%>%
  separate(Year_Month, into = c("Year", "Month"), sep = " ", convert = TRUE)%>%
  group_by(Year,species,`fishing gear`) %>%
  mutate(Tonnes = sum(Tonnes, na.rm = TRUE))%>%
  dplyr::select(species,`fishing gear`,Year,Tonnes)%>%
  unique()%>%
  left_join(gear)%>%
  left_join(guild)%>%
  mutate(Year=as.numeric(Year))%>%
  dplyr::select(Aggregated_gear, Guild, Year,Tonnes)%>%
  group_by(Aggregated_gear, Guild, Year) %>% 
  summarise(Tonnes = sum(Tonnes, na.rm = TRUE))%>%
  ungroup() %>% 
  right_join(landings_target_fro) %>%                                             # Reintroduce unrepresented combinations of gear, guild, and year
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(Tonnes = 0))%>%
  group_by(Aggregated_gear, Guild) %>% 
  summarise(Tonnes = mean(Tonnes, na.rm = TRUE)) %>%      # Average across years 
  ungroup() %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = Tonnes) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] 


saveRDS(Faroe_landings, "./Objects/Faroe landings by gear and guild.rds")

