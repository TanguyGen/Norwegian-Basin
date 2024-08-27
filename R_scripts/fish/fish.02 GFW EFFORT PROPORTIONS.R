
# Calculate the proportion of fishing effort by gear and flag across habitat types according to global fishing watch data

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "furrr","data.table")                # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

plan(multisession)

habitats <- readRDS("./Objects/habitats.rds")                                 # Load habitat polygons

#### Extracting a domain wide summary ####

Proportion_effort_pole_and_line <- c(                                                 #Split the proportion into different groups as the data was too large to process
  "NOR-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "FRO-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "ISL-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "RUS-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "EU+UK-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
  "REST-pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets"
)%>%
  future_map(~{ brick("./Objects/GFW_longlines.nc", varname = .x) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()

Proportion_effort_pots_and_traps <- c(
  "NOR-pots_and_traps",
  "FRO-pots_and_traps",
  "ISL-pots_and_traps",
  "RUS-pots_and_traps",
  "EU+UK-pots_and_traps",
  "REST-pots_and_traps"
)%>%
  future_map(~{ brick("./Objects/GFW_pots.nc", varname = .x) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()

Proportion_effort_seiners <- c(
  "NOR-Seiners",
  "FRO-Seiners",
  "ISL-Seiners",
  "RUS-Seiners",
  "EU+UK-Seiners",
  "REST-Seiners"
)%>%
  future_map(~{ brick("./Objects/GFW_seiners.nc", varname = .x) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()
Proportion_effort_ptrawlers <- c(
  "NOR-Pelagic_trawlers",
  "FRO-Pelagic_trawlers",
  "ISL-Pelagic_trawlers",
  "RUS-Pelagic_trawlers",
  "EU+UK-Pelagic_trawlers",
  "REST-Pelagic_trawlers"
)%>%
  future_map(~{ brick("./Objects/GFW_ptrawlers.nc", varname = .x) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()

Proportion_effort_strawlers <- c(
  "NOR-Shelf_trawlers",
  "FRO-Shelf_trawlers",
  "ISL-Shelf_trawlers",
  "RUS-Shelf_trawlers",
  "EU+UK-Shelf_trawlers",
  "REST-Shelf_trawlers"
)%>%
  future_map(~{ brick("./Objects/GFW_strawlers.nc", varname = .x) %>%                   # Import a brick of all years
      exact_extract(habitats, fun = "sum") %>%                             # Sum fishing hours within habitat types 
      mutate(Variable = .x) %>%                                            # Attach the variable name to keep track
      cbind(sf::st_drop_geometry(habitats))})%>%
  rbindlist()

Proportion_effort_dredge <-c(
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
  rbindlist()

gear <- read.csv("./Data/MiMeMo_gears.csv")   

Proportion_effort<-data.table::rbindlist(list(Proportion_effort_pole_and_line,Proportion_effort_pots_and_traps,Proportion_effort_seiners,Proportion_effort_strawlers,Proportion_effort_ptrawlers,Proportion_effort_dredge)) %>% 
  pivot_longer(starts_with("sum"), names_to = "Year", values_to = "Hours") %>%# Reshape so all years are in a column
  separate(Variable, into = c("Flag", "Gear_type"), sep = "-") %>%# Split variable name into flag and gear type
  left_join(gear)%>%
  mutate(Year = as.numeric(str_remove(Year, "sum.X")) + 2011) %>%                  # Fix year column
  group_by(Year) %>%                                                          # Within a year 
  mutate(Proportion = Hours/sum(Hours)) %>%                                   # Calculate the proportion of fihing effort in each row
  ungroup()

ggplot(Proportion_effort) +
  geom_path(aes(x = Year, y = Proportion, colour = paste0(Shore, " ", Habitat))) +
  facet_grid(rows = vars(Flag), cols = vars(Aggregated_gear)) +
  labs(colour = "Habitat") +
  theme_minimal()

ggsave("./Figures/GFW_habitat.png")

## Ignoring habitat types
  
Flag <- group_by(Proportion_effort, Flag, Aggregated_gear, Year) %>% 
  summarise(Proportion = sum(Proportion)) %>% 
  ungroup()

ggplot(Flag) +
  geom_col(aes(x = Flag, y = Proportion, fill = Flag), position = "dodge2") +
  geom_text(data = filter(Flag, Flag == "NOR"), aes(x = Flag, y = 0, group = Year, label = Year), 
            position = position_dodge(0.9), angle = 90,  colour = "firebrick4", fontface = "bold", hjust = 0,
            family = "AvantGarde") +
  labs(y = "Proportion of total fishing hours") +
  facet_grid(rows = vars(Aggregated_gear)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./Figures/GFW_no habitats.png")

