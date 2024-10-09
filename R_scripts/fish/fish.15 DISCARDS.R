rm(list=ls())                                                                # Wipe the brain
packages <- c("tidyverse","data.table","sf")                  # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages

guild<-read.csv("./Data/MiMeMo fish guilds.csv")[-1]

gear<-read.csv("./Data/MiMeMo_gears.csv")
target <- expand.grid(Guild = unique(read.csv2("./Data/MiMeMo fish guilds.csv")$Guild), # Create our target matrix to fill
                      Aggregated_gear = unique(read.csv("./Data/MiMeMo_gears.csv")$Aggregated_gear))

IMR_landings <- readRDS("./Objects/IMR landings by gear and guild.rds") # Import corrected IMR landings

domain_IMR <- readRDS("./Objects/Domains.rds") %>%                         # We need landings as tonnes per m^2
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

IMR<-t(IMR_landings/domain_IMR)
#----------------------------------------------
#Pelagic discards


pelagic_discards<-read.table("./Data/IMR/Discards/Discards.txt",sep=";",dec=".",header = TRUE)%>% #from reference fleet Tom Clegg unpublished data
  filter(estimator=="cluster unit")%>%       #Keep only estimates of catch summarised by fishing operation
  rename(Scientific.name = sciname, #to align to Guild dataset
         Discards=est,
         Gear_group=geargroup)%>%     #For clarification
  filter(area %in% c("ICES2aS","ICES2aN"))%>% #Only keep ICES area 27.2.a which corresponds to Norwegian Sea
  left_join(guild)%>%
  left_join(gear)%>%
  select(Aggregated_gear,Guild,year,Discards,area)%>%
  drop_na()%>%
  group_by(Aggregated_gear,Guild,year)%>%
  summarise(Discards=sum(Discards,na.rn=TRUE))%>% #sum the discarded biomass of the 2 areas
  ungroup()%>%
  group_by(Aggregated_gear,Guild)%>%
  summarise(Discards=mean(Discards,na.rn=TRUE))%>%
  right_join(target) %>%                                                         # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                       # Ditch the uneeded gear class
  replace_na(replace = list(Discards = 0)) %>%                               # Nas get a rate of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Discards) %>%      # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                                # Remove character column
  as.matrix() %>%                                                                # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
  t()

ICES_area_size<-1.414532e+12        #size of ICES area 27.2.a

pelagic_discards<-(pelagic_discards)/
  ICES_area_size



# #---------------------
# 
# Barents1<-read.csv("./Data/IMR/Discards/Barents1.csv")%>%
#   select(-landings)
# Barents2<-read.csv("./Data/IMR/Discards/Barents2.csv")
# 
# pelagic_discard<-rbindlist(list(Barents1,Barents2))%>% #from reference fleet Tom Clegg published data fror Barents sea
#   select(sciname,year,est_clus_unit)%>%       #Keep only estimates of catch summarised by fishing operation
#   rename(Scientific.name = sciname, #to align to Guild dataset
#          Discards=est_clus_unit)%>%     #For clarification
#   mutate(Aggregated_gear="Longlines_and_Gillnets")%>%
#   left_join(guild)%>%
#   left_join(gear)%>%
#   select(Aggregated_gear,Guild,year,Discards)%>%
#   drop_na()%>%
#   group_by(Aggregated_gear,Guild)%>%
#   summarise(Discards=mean(Discards,na.rn=TRUE))%>%
#   right_join(target) %>%                                                         # Join to all combinations of gear and guild
#   filter(Aggregated_gear != "Dropped") %>%                                       # Ditch the uneeded gear class
#   replace_na(replace = list(Discards = 0)) %>%                               # Nas get a rate of 0
#   pivot_wider(names_from = Aggregated_gear, values_from = Discards) %>%      # Spread dataframe to look like a matrix
#   column_to_rownames('Guild') %>%                                                # Remove character column
#   as.matrix() %>%                                                                # Convert to matrix
#   .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
#   t()
# 
# area_size_barents <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
#   filter(havomr %in% c("04","05","12","20","23"))%>%
#   sf::st_union() %>% 
#   sf::st_area() %>% 
#   as.numeric()
# 
# pelagic_discard<-pelagic_discard/area_size_barents

#----------------------------------------------
#Gillnets fish

area00<-read.csv2("./Data/IMR/Discards/Discards_fish_gillnets00.csv")
area06<-read.csv2("./Data/IMR/Discards/Discards_fish_gillnets06.csv")
area07<-read.csv2("./Data/IMR/Discards/Discards_fish_gillnets07.csv")

gillnets_fish<-rbindlist(list(area00,area06,area07),use.names=FALSE)%>%
  mutate(Fiskeridir=Art,  #adjust to guild
         Discards=Estimert.total.vekt.utkast..tonn.*10^6)%>% #Change name and change unit to kg
  group_by(Fiskeridir,Discards)%>%
  summarise(Discards=sum(Discards,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(Aggregated_gear="Longlines_and_Gillnets")%>% #add a column to adjust to gears
  right_join(guild)%>%
  group_by(Guild,Aggregated_gear)%>%
  summarise(Discards=mean(Discards,na.rm=TRUE))%>%
  drop_na()%>%
  right_join(target) %>%                                                         # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                       # Ditch the uneeded gear class
  replace_na(replace = list(Discards = 0)) %>%                               # Nas get a rate of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Discards) %>%      # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                                # Remove character column
  as.matrix() %>%                                                                # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
  t()

area_size <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr %in% c("00","06","07"))%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

gillnets_fish<-gillnets_fish/area_size



Discards<-gillnets_fish+pelagic_discards #Create a matrix with all discards

Discards<-Discards*1e-3#let's convert to tonnes



#-------------------------------------------------
#### Set up ####

library(tidyverse)

gear <- read.csv("./Data/MiMeMo_gears.csv") %>%                                   # Import gear names
  dplyr::select(Aggregated_gear, `gear type` = Gear_code)                         # Limit to FAO system

guild <- read.csv2("./Data/MiMeMo fish guilds.csv") %>%                            # Import guild names
  dplyr::select(Guild, species = FAO) %>%                                         # Limit to FAO system
  drop_na() %>%                                                                   # Drop those without a code
  distinct() %>%                                                                  # Drop duplicated rows which hang around after ditching other systems
  group_by(species) %>%                                                           # 1 duplicated code to remove ()
  slice_head() %>%                                                                # So only take the first instance of each code
  ungroup()

target <- expand.grid(Guild = unique(read.csv2("./Data/MiMeMo fish guilds.csv")$Guild), # Create our target matrix to fill
                      Aggregated_gear = unique(read.csv("./Data/MiMeMo_gears.csv")$Aggregated_gear))

#### Import data ####

data <- map(2:6,                                                                 # For each year in it's own sheet
    ~{readxl::read_excel("./Data/EU_fish/FDI-catches-by-country.xlsx", sheet = .x) %>% # Import data
    filter(`sub-region` %in% c("27.2.A"))}) %>%    # Limit to regions of interest
  data.table::rbindlist() %>%                                                    # Combine
  left_join(guild) %>%                                                           # Classify species by guild
  left_join(gear) %>%                                                            # Classify gears by group
  drop_na(Guild, Aggregated_gear)                                                # Drop NAs as we known these aren't in our domain from other spatially explicit data

#### Get discard rates ####

known <- data %>%                                                                # NK = not known, C = confidential, basically fewer than 3 ships
  mutate(reports = ifelse(!`total discards (tonnes)` %in% c("NK", "C"), "Value", `total discards (tonnes)`)) %>%
  count(reports) %>%                                                             # Count how many reports for discards are confidential, known, or just missing
  mutate(n = n/sum(n))                                                           # Calculate proportion

discards_EU <- filter(data, !`total discards (tonnes)` %in% c("NK", "C")) %>%           # Limit to discards we know about
  mutate(discards = as.numeric(`total discards (tonnes)`)) %>%                 # Convert to a number                     # Calculate discard rate
  drop_na() %>%                                                                  # There are a couple of 0 landings and 0 discards so drop these
  group_by(Guild, Aggregated_gear) %>%
  summarise(discards = mean(discards)) %>%                               # Get the average discard rate per guild and gear combo
  ungroup() %>%
  right_join(target) %>%                                                         # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                        # Ditch the values we already have          
  replace_na(replace = list(discards = 0)) %>%                               # Nas get a rate of 0
  pivot_wider(names_from = Aggregated_gear, values_from = discards) %>%      # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                                # Remove character column
  as.matrix() %>%                                                                # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
  t()

landings_EU <- filter(data, !`total discards (tonnes)` %in% c("NK", "C")) %>%           # Limit to discards we know about
  mutate(landed = as.numeric(`total live weight landed (tonnes)`)) %>%                      # Calculate discard rate
  drop_na() %>%                                                                  # There are a couple of 0 landings and 0 discards so drop these
  group_by(Guild, Aggregated_gear) %>%
  summarise(landed = mean(landed)) %>%                               # Get the average discard rate per guild and gear combo
  ungroup() %>%
  right_join(target) %>%                                                         # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                        # Ditch the values we already have          
  replace_na(replace = list(landed = 0)) %>%                               # Nas get a rate of 0
  pivot_wider(names_from = Aggregated_gear, values_from = landed) %>%      # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                                # Remove character column
  as.matrix() %>%                                                                # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
  t()

discards_EU<-discards_EU/ICES_area_size
landings_EU<-discards_EU/ICES_area_size

rate<-(Discards+discards_EU)/(landings_EU+IMR+Discards+discards_EU)
rate[is.na(rate)]<-0
rate[,c("Cetacean", "Macrophyte", "Birds", "Pinnipeds")] <- 1                    # Overwrite guilds which are always discarded
rate["Harpoons", "Cetacean"] <- 0                                                # Except for specific targeted gears
rate["Kelp harvesting", "Macrophyte"] <- 0

saveRDS(rate, "./Objects/Discard rates.rds")




#Bycatch weight

Bycatch<-matrix(0, nrow = 10, ncol = 12,
                        dimnames = list(
                          sort(unique(read.csv("./Data/MiMeMo_gears.csv")$Aggregated_gear)),
                          sort(unique(read.csv2("./Data/MiMeMo fish guilds.csv")$Guild))
                          )
                )
Bycatch<-Bycatch[rownames(Bycatch)!="Dropped",]
area_size <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr %in% c("00","06","07"))%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()
#----------------------------------------------
#Gillnets cetaceans

#Using the average of 4 methods of estimation


## Estimate the annual discard rates for the EU fishing fleet in the Barents Sea from (Moan et al., 2020) for regions 2 and 3 estimation 2014 to 2018

porpoise_abundance<-((292+228)+(219+347)+(628+368)+(554+219))/4
porpoise<-porpoise_abundance*53 /#mean mass
  area_size

#Values from (Hilde Sofie Fantoft and Nedreaas, 2021)

area_size_5 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="05")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

white_sided<-307* #abundance
  110/#mean mass
  area_size_5

Bycatch["Longlines_and_Gillnets","Cetacean"]<-porpoise+white_sided

#----------------------------------------------
#Gillnets pinnipeds (Moan 2021)


Harbour_seals<-(85.2+15.8+2.6)* #estimated abundance
  87    #mean weight
Grey_seals<-(116.2+23.9+3.7)*
  193.81 #mean weight

seals<-(Harbour_seals+Grey_seals)/area_size
Bycatch["Longlines_and_Gillnets","Pinnipeds"]<-seals

#----------------------------------------------
#Gillnets seabirds

#We are using (Hilde Sofie Fantoft and Nedreaas, 2021) values for barents and nNorwegian sea

area_size_9 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="09")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

razorbill<-0.711*87/
  area_size_9

area_size_0 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="00")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()
area_size_6 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="06")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

fulmar<-0.82*(107+76)/
  (area_size_0+area_size_6)

kittiwake<-76*0.409/
  area_size_5

guillemot<-1.028*153/
  area_size_5

area_size_28 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="28")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

gull<-0.745*(181+87)/
  (area_size_28+area_size_9)

area_size_8 <- read_sf(dsn="./Data/IMR/Fdir_hovedomraader_shp") %>% #get the fiskeridir areas
  filter(havomr=="08")%>%
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

cormorant<-(3.250)*1840/
  (area_size_5)


black_guillemot<-0.410*4141/
  (area_size_5)

eider<-1.630*45/
  area_size_28

birds<-razorbill+fulmar+kittiwake+guillemot+gull+cormorant+black_guillemot+eider


Bycatch["Longlines_and_Gillnets","Birds"]<-birds

Bycatch<-Bycatch* 1e3 / 360 #convert to g/d

Bycatch<-t(Bycatch)

saveRDS(Bycatch, "./Objects/Bycatch weight.rds")

