
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain
countries<-read.csv("./Data/country_code.csv")
EU<-countries$Alpha.3.code[countries$EU == "YES"]

packages <- c("MiMeMo.tools", "data.table", "furrr","future", "tictoc", "raster")      # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

plan(multisession)                                                            # Choose the method to parallelise by with furrr

Files <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*.csv", full.names = T) # Get a list of files to import 

target <- raster(xmn = -15, xmx = 20, ymn = 60, ymx = 75, res = c(0.01, 0.01), vals = NA)

#### Limit to the obervations of interest ###

Effort <- future_map(Files, ~ {                                               # For each csv
   
  fread(.x) %>%                                                               # Read in
    mutate(lat_bin = cell_ll_lat,                                             # Change names to fit with the past code
           lon_bin = cell_ll_lon,
           flag = ifelse(flag %in% EU|flag=="GBR", "EU+UK", flag),
           flag = ifelse(!flag %in% c("NOR","FRO","RUS","ISL","EU+UK"), "REST", flag),     # Simplify flags of interest 
           geartype = case_when(geartype %in% c("pole_and_line", "set_longlines", "squid_jigger","drifting_longlines","set_gillnets") ~ "pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets",
                                geartype %in% c("pots_and_traps") ~ "pots_and_traps",
                                geartype %in% c("seiners") ~ "seiners",                #
                                geartype %in% c("trawlers") ~ "trawlers",
                                geartype %in% c("dredge_fishing") ~ "dredge_fishing")) %>%   #Split the gears into groups that will correspond to the model
    filter(between(lat_bin, 62, 75),                                          # Rough crop to region
           between(lon_bin, -7, 16)) %>%
    group_by(lat_bin, lon_bin, flag, geartype, date) %>% 
    summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = geartype, values_from = fishing_hours) %>% 
    as.data.frame()                                                             # Convert to data frame to play nicely with rasters
    },          # Get each fishing gear as a column
    .progress = TRUE) %>%
  rbindlist(fill = TRUE)

setDT(Effort)                                                                 # Convert to data.table for speed

Effort <- Effort[, Year := year(as.Date(date))][                              # Extract year from date column
  , lapply(.SD, sum, na.rm = TRUE),                                           # Sum values
  by = .(lon_bin, lat_bin, flag, Year),                                       # By pixel, nation, gear, and year
  .SDcols = c("pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets","pots_and_traps","seiners","trawlers","dredge_fishing")] %>%                              # For each gear 
  as.data.frame()                                                             # Convert to data frame to play nicely with rasters

#### Make rasters ####

vars <- c("pole_and_line+set_longlines+squid_jigger+drifting_longlines+set_gillnets","pots_and_traps","seiners","trawlers","dredge_fishing")

tic()
map( c("FRO"), ~{                                               # For each flag
  
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

netcdfs_longlines <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*set_gillnets.nc", full.names = T) # Get a list of files to import 
netcdfs_pots <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*pots_and_traps.nc", full.names = T) # Get a list of files to import 
netcdfs_seiners <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*seiners.nc", full.names = T) # Get a list of files to import 
netcdfs_trawlers <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*trawlers.nc", full.names = T)
netcdfs_dredge_fishing <- list.files(path = "./Data/GFW_daily_csvs", pattern ="*dredge_fishing.nc", full.names = T) # Get a list of files to import 


file.rename(from=netcdfs_longlines[1],to="./Objects/GFW_longlines.nc")
file.rename(from=netcdfs_pots[1],to="./Objects/GFW_pots.nc")
file.rename(from=netcdfs_seiners[1],to="./Objects/GFW_seiners.nc")
file.rename(from=netcdfs_trawlers[1],to="./Objects/GFW_trawlers.nc")
file.rename(from=netcdfs_dredge_fishing[1],to="./Objects/GFW_dredge.nc")

walk(netcdfs_longlines[-1], ~{
  system(str_glue("wsl ncks -A '{.x}' ./Objects/GFW_longlines.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_pots[-1], ~{
  system(str_glue("wsl ncks -A '{.x}' ./Objects/GFW_pots.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_seiners[-1], ~{
  system(str_glue("wsl ncks -A '{.x}' ./Objects/GFW_seiners.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_trawlers[-1], ~{
  system(str_glue("wsl ncks -A '{.x}' ./Objects/GFW_trawlers.nc")) # In turn bind a variable to the main file
})  #for windows

walk(netcdfs_dredge_fishing[-1], ~{
  system(str_glue("wsl ncks -A '{.x}' ./Objects/GFW_dredge.nc")) # In turn bind a variable to the main file
})  #for windows

#unlink(netcdfs_longlines[-1]) 
#unlink(netcdfs_trawlers[-1])  
#unlink(netcdfs_seiners[-1])  
#unlink(netcdfs_dredge_fishing[-1]) 
#unlink(netcdfs_pots[-1])  # Delete the redundant files

#### faster merges? ###

# tic()
# system(str_glue("ncrcat {'netcdfs'}./Objects/GFW_ncrcat.nc"))                        # In turn bind a variable to the main file
# toc()
# 
# tic()
# system("ncecat './Data/GFW daily_csvs/*.nc' ./Objects/GFW_ncecat.nc")                        # In turn bind a variable to the main file
# toc()