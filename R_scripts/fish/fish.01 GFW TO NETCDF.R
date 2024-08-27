
# Wipe environment and load required data
rm(list = ls())
countries <- read.csv("./Data/country_code.csv")
EU <- countries$Alpha.3.code[countries$EU == "YES"]

# List handy packages and load them
packages <- c("MiMeMo.tools", "data.table", "furrr", "future", "tictoc", "raster", "sf")
lapply(packages, library, character.only = TRUE)

# Set up parallel execution with furrr
plan(multisession)

# Get list of files to import
Files <- list.files(path = "./Data/GFW_daily_csvs", pattern = "*.csv", full.names = TRUE)

target <- raster(xmn = -15, xmx = 20, ymn = 60, ymx = 75, res = c(0.01, 0.01), vals = NA)

# Load Domains and reproject
Domains <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%
  st_as_sf() %>%
  mutate(Keep = TRUE)

# Precalculate points within domains for efficiency
Effort <- future_map(Files, ~ {
  
  data <- fread(.x) %>%
    filter(
      between(cell_ll_lat, 62, 75),
      between(cell_ll_lon, -7, 16)
    ) %>%
    mutate(
      lat_bin = cell_ll_lat,
      lon_bin = cell_ll_lon,
      point = st_sfc(map2(lon_bin, lat_bin, ~ st_point(c(.x, .y))), crs = 4326),
      flag = ifelse(flag %in% EU | flag == "GBR", "EU+UK", flag),
      flag = ifelse(!flag %in% c("NOR", "FRO", "RUS", "ISL", "EU+UK"), "REST", flag)
    )
  
  # Precompute st_within for the points and both domains
  within_pelagic <- st_within(data$point, Domains[1,], sparse = FALSE)[, 1]
  within_shelf <- st_within(data$point, Domains[2,], sparse = FALSE)[, 1]
  
  # Update geartype based on st_within results
  data <- data %>%
    mutate(
      geartype = case_when(
        geartype %in% c("pole_and_line", "set_longlines", "squid_jigger", "drifting_longlines", "set_gillnets","trollers") ~
          "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
        geartype == "pots_and_traps" ~ "pots_and_traps",
        geartype == "trawlers" & within_pelagic ~ "Pelagic_trawlers",
        geartype %in% c("seiners","purse_seines","other_seines","tuna_purse_seines","other_purse_seines")  ~ "Seiners",
        geartype == "trawlers" & within_shelf ~ "Shelf_trawlers",
        geartype == "dredge_fishing" ~ "dredge_fishing"
      )
    ) %>%
    group_by(lat_bin, lon_bin, flag, geartype, date) %>%
    summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = geartype, values_from = fishing_hours) %>%
    as.data.frame()
  
  return(data)
}, .progress = TRUE) %>%
  rbindlist(fill = TRUE)

setDT(Effort)                                                                 # Convert to data.table for speed

Effort <- Effort[, Year := year(as.Date(date))][                              # Extract year from date column
  , lapply(.SD, sum, na.rm = TRUE),                                           # Sum values
  by = .(lon_bin, lat_bin, flag, Year),                                       # By pixel, nation, gear, and year
  .SDcols = c("pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets","pots_and_traps","Pelagic_trawlers","Seiners","Shelf_trawlers","dredge_fishing")] %>%                              # For each gear 
  as.data.frame()                                                             # Convert to data frame to play nicely with rasters

#### Make rasters ####

vars <- c("pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets","pots_and_traps","Pelagic_trawlers","Seiners","Shelf_trawlers","dredge_fishing")

tic()
map( c("NOR","RUS","ISL","REST","EU+UK","FRO"), ~{                                               # For each flag
  
  data <- filter(Effort, flag == .x)                                          # Limit to one in turn

  print(glue::glue("Now processing {.x}"))                                    # Keeping track
  
  new <- map(vars, ~ {                                                        # Then for each fishing variable
    dplyr::select(data, c("lon_bin", "lat_bin", "Year", all_of(.x))) %>%      # Select it along with spatial and temporal variables
    split(f = .$Year) %>%                                                     # Split by year so we get one raster per time step
    map(function(x, target, var) {                                            # For each year
    
      raster <- target                                                        # Take the target grid
      cells <- cellFromXY(target, x[,c("lon_bin", "lat_bin")])                # Find which cells on the grid our observations fall on
      raster[cells] <- x[,var]                                                # Copy over the data
      return(raster)},                                                        # Return an updated raster
    target = target, var = .x) %>%                                            # Specify the target raster
    brick() }, data = data)                                                   # And bind each year into a raster brick of one variable for one flag
  
  print(glue::glue("Now saving {.x}"))                                        # Keeping track
  
  future_map2(new, paste0(.x, "-", vars), ~ {                                 # Then save to netcdf
    writeRaster(.x, filename = paste0("./Data/GFW_daily_csvs/", .y, ".nc"), overwrite = TRUE, # Building a name from flag and variable
    format = "CDF", varname= .y, varunit = "Hours", xname = "Longitude",
    yname="Latitude", zname = "Year") }, .progress = T)

})
toc()

#### Combine all files ####

netcdfs_Longlines <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*set_gillnets.nc", full.names = T) # Get a list of files to import 
netcdfs_Pots <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*pots_and_traps.nc", full.names = T) # Get a list of files to import 
netcdfs_PTrawlers <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*Pelagic_trawlers.nc", full.names = T) # Get a list of files to import 
netcdfs_STrawlers <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*Shelf_trawlers.nc", full.names = T)
netcdfs_Seiners <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*Seiners.nc", full.names = T)
netcdfs_Dredge_fishing <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*dredge_fishing.nc", full.names = T) # Get a list of files to import 


file.rename(from=netcdfs_Longlines[1],to="./Objects/GFW_longlines.nc")
file.rename(from=netcdfs_Pots[1],to="./Objects/GFW_pots.nc")
file.rename(from=netcdfs_PTrawlers[1],to="./Objects/GFW_ptrawlers.nc")
file.rename(from=netcdfs_STrawlers[1],to="./Objects/GFW_strawlers.nc")
file.rename(from=netcdfs_Seiners[1],to="./Objects/GFW_seiners.nc")
file.rename(from=netcdfs_Dredge_fishing[1],to="./Objects/GFW_dredge.nc")

walk(netcdfs_Longlines[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_longlines.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_Pots[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_pots.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_PTrawlers[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_ptrawlers.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_STrawlers[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_strawlers.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_Seiners[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_seiners.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_Dredge_fishing[-1], ~{
  system(str_glue("ncks -A '{.x}' ./Objects/GFW_dredge.nc")) # In turn bind a variable to the main file
})  #for windows


unlink(netcdfs_Longlines[-1]) 
unlink(netcdfs_STrawlers[-1])  
unlink(netcdfs_PTrawlers[-1])  
unlink(netcdfs_Seiners[-1])  
unlink(netcdfs_Dredge_fishing[-1]) 
unlink(netcdfs_Pots[-1])  # Delete the redundant files

#### faster merges? ###

# tic()
# system(str_glue("ncrcat {'netcdfs'}./Objects/GFW_ncrcat.nc"))                        # In turn bind a variable to the main file
# toc()
# 
# tic()
# system("ncecat './Data/GFW daily_csvs/*.nc' ./Objects/GFW_ncecat.nc")                        # In turn bind a variable to the main file
# toc()