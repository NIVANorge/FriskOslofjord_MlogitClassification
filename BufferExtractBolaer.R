# Extracting signal values from drone imagery to ground truth points
# Guri Sogn Andersen, 2020

# Packages, settings and data ---------------------------------------------

# Load packages into your workspace
require(tidyverse)
require(raster)
require(rgdal)
require(data.table)

# Memory
memory.limit(size = 30000)

# Rastersettings
rasterOptions()
rasterOptions(tmpdir="./Tempfiles")
rasterOptions(maxmemory = 3e+10) # øker minne tilgjengelig for rasterpakka

# Ground truth data
bolaerne

# Drone images - local
b_imagepath <- "C:/RProjects/DroneFriskOslofjord/Dronebilder/0_Bolaerne"
dir(imagepath)

imagefilelist <- list.files(path = b_imagepath, pattern = ".tif$")  # ramser alle filer med .tif
imagefilepaths <- paste(b_imagepath, imagefilelist, sep = "\\") # ramser opp stier
imagefilelistnames <- gsub("2019-08-29_Flight15_MS_Reprocess_index_*", "", imagefilelist) # lager navn
imagefilelistnames <- gsub("*.tif", "", imagefilelistnames) # lager navn
imagefilelistnames <- gsub("_", "", imagefilelistnames) # lager navn
imagefilelistnames

# Laster inn tif-filer fra mappa som liste 
rasterlist <- lapply(setNames(imagefilepaths, 
                              make.names(imagefilelistnames)), 
                     raster)
rasterlist # OK

signalstack <- stack(rasterlist)
signalstack

# CRS def
CRSproject <- proj4string(signalstack)
CRSproject


# Extract signals ---------------------------------------------------------

# SpatialPoints
bol_sp <- SpatialPoints(coords = cbind(bolaerne$longitude, bolaerne$latitude),
                         proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
  spTransform(., CRS(CRSproject))

bol_sp # transformert

image(signalstack)
points(bol_sp) #OK

# Extracting with buffer here as well
bol_signals <- raster::extract(signalstack, bol_sp, buffer = 0.2) # rundere tall...

PointID = c(1:length(bol_sp)) # point IDs

# trying rbindlist from data.table-package
bol_signals_df <- mapply(cbind, bol_signals, IDpoint = PointID, SIMPLIFY = F) %>% # adding point ID to colum
  lapply(., data.frame) %>%
  rbindlist(., idcol=TRUE, fill = TRUE)

bol_signals_df # Finally!!

# Joining field data and image data ---------------------------------------
bolaerne_bext <- bolaerne %>% mutate(IDpoint = c(1:nrow(bolaerne))) %>% 
  full_join(., bol_signals_df)
bolaerne_bext

bolaerne_bext %>% group_by(Sp04_la) %>% summarise(N = n())

# writing to file
write.csv(bolaerne_bext, file = "./Ground truth/bolaerne_buffer20_extract.csv", row.names = FALSE)

# reload
bolaerne_bext <- read.csv("./Ground truth/bolaerne_buffer20_extract.csv", stringsAsFactors = FALSE)
