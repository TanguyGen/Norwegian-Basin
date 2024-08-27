rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse","exactextractr","raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)   

GFW_dredge <- brick("./Objects/GFW_dredge.nc", varname = "EU+UK-dredge_fishing") %>%      # For each class of gear
  calc(mean, na.rm = T)%>%
  projectRaster(crs = crs(Domains))

dredge_feature = exact_extract(GFW_dredge, ., fun = "sum")