
packages <- c("dplyr", "data.table", "future", "furrr", "progressr")        # List packages
lapply(packages, library, character.only = TRUE)   

Files <- list.files(path = "./Data/Fiskeridirektoratet/Open_data",
                    pattern = "*.csv",
                    full.names = T) # Get a list of files to import

plan(multisession)

handlers("progress")


gear <- read.csv("./Data/MiMeMo_gears.csv", check.names = FALSE)                                   # Import gear names

guild <- read.csv2("./Data/MiMeMo fish guilds.csv", check.names = F) %>%                        # Import guild names
  dplyr::select(Guild, FAO) %>%
  drop_na() %>%
  distinct() %>%
  group_by(FAO) %>%
  slice_head() %>%
  ungroup()

options(future.globals.maxSize = 2 * 1024 ^ 3)
with_progress({
  p <- progressor(along = Files)
  Mollusc_landings <- future_map(Files, ~ {
    p()
    # For each csv
    fread(.x, , sep = ";", dec = ",") %>%
      select(
        `Redskap (kode)`,
        `Art FAO (kode)`,
        `Produktvekt`,
        `Lon (lokasjon)`,
        `Lat (lokasjon)`
      )%>%
      mutate(
        Lon=`Lon (lokasjon)`,
        Lat=`Lat (lokasjon)`
      )%>%
      filter(between(Lat, 62, 73.5),                                 # Keep fishing location in the Norwegian Sea
             between(Lon, -7, 16)) %>%
      mutate(
        Gear_code = `Redskap (kode)`,
        FAO = `Art FAO (kode)`,
        Landings= Produktvekt,
        point = st_sfc(map2(Lon, Lat, ~ st_point(c(.x, .y))), crs = 4326),
        within =map(point,~st_within(.x, domain, sparse = FALSE))
      ) %>%
      filter(within)%>%
      as.data.frame()                                                           # Convert to data frame to play nicely with rasters
  })
  })%>%
    data.table::rbindlist() %>%                                                 # Create one big data frame
    mutate(Gear_code = as.character(Gear_code)) %>%
    left_join(gear) %>%                                                         # Attach gear labels
    left_join(guild) %>%                                                        # Attach guild labels
    filter(Aggregated_gear != "Mollusc_dredge")%>%
    select(Aggregated_gear, Guild, Landings) %>%
    group_by(Aggregated_gear, Guild) %>%
    filter(is.na(Aggregated_gear) == FALSE, is.na(Guild) == FALSE) %>%          # Remove missing data
    summarise(Offal = mean(Offal, na.rm = TRUE)) 
  