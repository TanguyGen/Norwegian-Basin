
## Estimate the discard rates for the EU fishing fleet in the Barents Sea

#### Set up ####

library(tidyverse)

gear <- read.csv2("./Data/MiMeMo gears.csv") %>%                                   # Import gear names
  dplyr::select(Aggregated_gear, `gear type` = Gear_code)                         # Limit to FAO system
  
guild <- read.csv2("./Data/MiMeMo fish guilds.csv") %>%                            # Import guild names
  dplyr::select(Guild, species = FAO) %>%                                         # Limit to FAO system
  drop_na() %>%                                                                   # Drop those without a code
  distinct() %>%                                                                  # Drop duplicated rows which hang around after ditching other systems
  group_by(species) %>%                                                           # 1 duplicated code to remove ()
  slice_head() %>%                                                                # So only take the first instance of each code
  ungroup()

target <- expand.grid(Guild = unique(read.csv2("./Data/MiMeMo fish guilds.csv")$Guild), # Create our target matrix to fill
                      Aggregated_gear = unique(read.csv2("./Data/MiMeMo gears.csv")$Aggregated_gear))

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

rate <- filter(data, !`total discards (tonnes)` %in% c("NK", "C")) %>%           # Limit to discards we know about
  mutate(discards = as.numeric(`total discards (tonnes)`),                       # Convert to a number
         landed = as.numeric(`total live weight landed (tonnes)`)) %>%              
  mutate(Discard_rate = discards/(discards + landed)) %>%                        # Calculate discard rate
  drop_na() %>%                                                                  # There are a couple of 0 landings and 0 discards so drop these
  group_by(Guild, Aggregated_gear) %>% 
  summarise(Discard_rate = mean(Discard_rate)) %>%                               # Get the average discard rate per guild and gear combo
  ungroup() %>% 
  right_join(target) %>%                                                         # Join to all combinations of gear and guild
  filter(Aggregated_gear != "Dropped") %>%                                       # Ditch the uneeded gear class
  replace_na(replace = list(Discard_rate = 0)) %>%                               # Nas get a rate of 0
  pivot_wider(names_from = Aggregated_gear, values_from = Discard_rate) %>%      # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                                # Remove character column
  as.matrix() %>%                                                                # Convert to matrix
  .[order(row.names(.)), order(colnames(.))] %>%                                 # Alphabetise rows and columns
  t()

rate[,c("Cetacean", "Macrophyte", "Birds")] <- 1                    # Overwrite guilds which are always discarded
rate["Harpoons", "Cetacean"] <- 0                                                # Except for specific targeted gears
rate["Kelp harvesting", "Macrophyte"] <- 0

saveRDS(rate, "./Objects/EU discard rates.rds")
write.csv(rate,"./Target/discard rates.csv", row.names=TRUE) # Save out target data

