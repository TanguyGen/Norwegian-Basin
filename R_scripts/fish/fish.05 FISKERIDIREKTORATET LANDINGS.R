# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

library(tidyverse)

gears <- unique(read.csv("./Data/MiMeMo_gears.csv")$Aggregated_gear)          # Load fishing gear classifications

guild <- unique(read.csv2("./Data/MiMeMo fish guilds.csv",check.names = FALSE)$Guild)              # Get guilds

landings_target <- expand.grid(Guild = guild, 
                               Aggregated_gear = gears) %>%                   # Get combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                      # Ditch the uneeded gear class
  mutate(Tonnes = 0) %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = Tonnes) %>%         # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns

#### Extract macroalgae landings ####

macroalgae<-read.csv("./data/Fiskeridirektoratet/landingsfylke_art.csv",dec=".",sep=";",header=TRUE)

macroalgae <- macroalgae %>% 
    setNames(c("Commune", "Gear","Length group", "Species", "2019","2018","2017","2016","2015","2014","2013","2012","2011")) %>%
    filter(Species=="Brunalger")%>% 
    pivot_longer(cols = `2019`:`2011`,names_to = "Year",values_to = "Tonnes")%>%
    drop_na()%>%
    mutate(Tonnes=as.numeric(Tonnes))
    

summary <- macroalgae %>%
  summarise(Tonnes = mean(Tonnes,na.rm=T)) 
landings_target["Macrophyte", "Kelp harvesting"] <- summary$Tonnes
saveRDS(landings_target, "./Objects/fiskeridirektoratet landings by gear and guild.rds")

