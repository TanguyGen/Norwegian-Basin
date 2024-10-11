rm(list=ls())   
packages<-c(
  "icesSAG",
  "data.table",
  "dplyr",
  "purrr",
  "furrr",
  "tidyr",
  "sf",
  "ggplot2",
  "tibble"
)
lapply(packages, library, character.only = TRUE) 

#Fish harvest ratios

guild <- read.csv("./Data/MiMeMo fish guilds.csv",check.names = FALSE) %>%                        # Import guild names
  dplyr::select(Guild, Scientific.name) %>%
  setNames(c("Guild","SpeciesName"))


plan(multisession)

stock<-2010:2019%>%
  future_map(~{
    read.csv(paste0("./Data/ICES_fish/SAG",.x,".csv"))%>%
      mutate(across(everything(), as.character))%>%
      left_join(guild)%>%
      group_by(FishStock) %>%
      slice(1) %>%
      ungroup()%>%
      select(FishStock,Guild,StockDescription)%>%
      drop_na()%>%
      arrange(Guild)
    
  })%>%
  rbindlist()
  

test<-getSAG(stock=stock$FishStock[1],year = 2010:2019)

Fish<-1:nrow(stock)%>%
  future_map(~{
    getSAG(stock=stock$FishStock[.x],year = 2010:2019)%>%
      mutate(Guild=stock$Guild[.x])%>%
      filter(Year %in% 2010:2019,
           stockSizeDescription=="SSB"|stockSizeDescription=="Biomass index"&units=="tonnes")%>%
      drop_na(SSB)%>%
      group_by(Year,Guild)%>%
      summarise(SSB=sum(SSB,na.rm = T),catches=sum(catches,na.rm=T))%>%
      ungroup()
  })%>%
  rbindlist()%>%
  group_by(Guild)%>%
  summarise(Biomass=mean(SSB))


#----------------------
domain<- readRDS("./Objects/Domains.rds") %>%
  st_transform(crs=4326)
# Cetaceans Biomass from IWC

# Biomass Cetaceans #Leonard (abundance) +Pedersen (biomass)


Minke <- 104700*4520
Fin <- 11387*42279
Humpback <- 10708*31782
Sperm <- 5704*34322
Killer <- 255979*2350
Porpoise <- 255929*57.5
White_sided <- 192767*225
Bottlenose<-7800*6500

Biomass_cetaceans<-sum(Minke, Fin, Humpback, Sperm, Killer, Porpoise, White_sided, Bottlenose)

#Biomass Pinnipeds

Harbour_seal<-(634+1200+170+1549)*95
Grey_seal<-6496*134
Seals_biomass<-Grey_seal+Harbour_seal



#Biomass benthos

#Data from Mareano

Beamtrawl <- read.table("./Data/IMR/Mareano/Beamtrawl/extendedmeasurementorfact.txt", sep = '\t',header=TRUE)%>%
  mutate(id=as.character(id), 
         occurrenceID=as.character(occurrenceID))
Biomass_beamtrawl<-read.table("./Data/IMR/Mareano/Beamtrawl/occurrence.txt", sep = '\t',header=TRUE)%>%
  mutate(id=as.character(id), 
         occurrenceID=as.character(occurrenceID))%>%
  right_join(Beamtrawl)%>%
  drop_na("decimalLongitude", "decimalLatitude")%>%
  st_as_sf( coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(domain))%>%
  filter(measurementType=="Biomass",
         st_within(., domain, sparse = FALSE) %>% apply(1, any))


ggplot() +
  geom_sf(data = domain, fill = "lightblue", color = "black") +   # Plot the shapefile
  geom_sf(data = Biomass_beamtrawl, color = "red") +                  # Plot the filtered points
  theme_minimal()

Biomass_benthos<- Biomass_beamtrawl%>%
  rename(SpeciesName=scientificName)%>%
  group_by(SpeciesName)%>%
  summarize(measurementValue=sum(measurementValue))%>%
  ungroup()%>%
  left_join(guild)%>%
  group_by(Guild)%>%
  summarise(Biomass=mean(measurementValue/100,na.rm=T))%>%
  mutate(total_known_biomass = sum(Biomass[!is.na(Guild)]),      # Total biomass of known species
         Inflation=(total_known_biomass + Biomass[is.na(Guild)]) / total_known_biomass,
         Biomass=Biomass*Inflation)%>%
  filter(Guild=="Benthos carnivore/scavenge feeder"|Guild=="Benthos filter/deposit feeder")%>%
  as.data.frame()%>%
  select(Guild,Biomass)
  

Abundance_beamtrawl<-read.table("./Data/IMR/Mareano/Beamtrawl/occurrence.txt", sep = '\t',header=TRUE)%>%
  mutate(id=as.character(id), 
         occurrenceID=as.character(occurrenceID))%>%
  right_join(Beamtrawl)%>%
  drop_na("decimalLongitude", "decimalLatitude")%>%
  st_as_sf( coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(domain))%>%
  filter(measurementType=="Abundance",
         st_within(., domain, sparse = FALSE) %>% apply(1, any))

Abundance_benthos<- Abundance_beamtrawl%>%
  rename(SpeciesName=scientificName)%>%
  group_by(SpeciesName)%>%
  summarize(measurementValue=sum(measurementValue))%>%
  ungroup()%>%
  left_join(guild)%>%
  group_by(Guild)%>%
  summarise(Abundance=mean(measurementValue/100,na.rm=T))%>%
  mutate(total_known_abundance = sum(Abundance[!is.na(Guild)]),      # Total biomass of known species
         Inflation=(total_known_abundance + Abundance[is.na(Guild)]) / total_known_abundance,
         Abundance=Abundance*Inflation)%>%
  filter(Guild=="Benthos carnivore/scavenge feeder"|Guild=="Benthos filter/deposit feeder")%>%
  as.data.frame()%>%
  select(Guild,Abundance)

# Values from (Laubier et al., 1976)

abundance1976<-mean(c(1.077, 0.843, 0.732, 1.618, 1.650, 0.814, 0.478, 0.567, 
                 0.827, 0.542, 0.702, 2.430, 3.171, 0.119, 2.567, 1.837))

Inflation_Abundance_Biomass <- abundance1976/(sum(Abundance_benthos$Abundance)+abundance1976)

Offshore_area <- domain %>% 
  filter(Shore == "Offshore")%>%
  st_area()

Inshore_area <- domain %>% 
  filter(Shore == "Inshore")%>%
  st_area()

Total_biomass_benthos <- Biomass_benthos %>%
  mutate(
    Biomass_Offshore = Biomass *
      Inflation_Abundance_Biomass * #Get the deflation rate to represent offshore
      10 ^ -6 * #In tonnes
      as.numeric(Offshore_area),
    Biomass_Inshore = Biomass *
      10 ^ -6 * #In tonnes
      as.numeric(Inshore_area),
    Biomass=Biomass_Offshore+Biomass_Inshore
  )%>%
  select(Guild,Biomass)

target <- expand.grid(Guild = unique(guild$Guild)) 

Total_biomass<-Fish%>% 
  right_join(target)%>%
  left_join(Total_biomass_benthos,by="Guild")%>%
  mutate(Biomass=ifelse(is.na(Biomass.x), Biomass.y, Biomass.x))%>%
  select(Guild,Biomass)%>%
  remove_rownames() %>% 
  column_to_rownames('Guild') %>%                                   # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)),]                                                     # Alphabetise rows to ensure a match with other objects

Total_biomass["Cetacean"]<-Biomass_cetaceans
Total_biomass["Birds"]<-3800 #(Barrett, 2002)
Total_biomass["Pinnipeds"]<-Seals_biomass
Total_biomass["Zooplankton carnivore"]<-2.5*st_area(st_union(domain))*10^-6 #(Skjodal, 2004)


discard_rate <- readRDS("./Objects/Discard rates.rds")

bycatch<-readRDS("./Objects/Bycatch weight.rds")*st_area(st_union(domain))*360*10^-6 # tonnes/year

landings <- readRDS("./Objects/International landings.rds") *st_area(st_union(domain)) # Units tonnes/year


catch <- landings / (1-discard_rate)                                        # Inflate landings with discards to total catch.

catch[!is.finite(catch)] <- landings[!is.finite(catch)]                     # 0s and infinities mean no discard, so are overwritten with landings

catch<-catch+bycatch

guild_catch<-colSums(catch)

Total_biomass["Demersal"]<-Total_biomass["Demersal (quota limited)"]+Total_biomass["Demersal (non quota)"]
guild_catch["Demersal"]<-guild_catch["Demersal (quota limited)"]+guild_catch["Demersal (non quota)"]

Harvest_ratios<-guild_catch/Total_biomass


