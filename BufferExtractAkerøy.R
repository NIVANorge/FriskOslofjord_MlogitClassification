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
akeroya

# Manual image truth data
akeroya_manual

# Drone images - local
imagepath <- "C:/RProjects/DroneFriskOslofjord/Dronebilder/reflectance"
dir(imagepath)

imagefilelist <- list.files(path = imagepath, pattern = ".tif$")  # ramser alle filer med .tif
imagefilepaths <- paste(imagepath, imagefilelist, sep = "\\") # ramser opp stier
imagefilelistnames <- gsub("Merge_MS_Akeroy_V4_transparent_reflectance_*", "", imagefilelist) # lager navn
imagefilelistnames <- gsub("*.tif", "", imagefilelistnames) # lager navn
imagefilelistnames <- gsub(" ", "", imagefilelistnames) # lager navn
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
aker_sp <- SpatialPoints(coords = cbind(akeroya$longitude, akeroya$latitude),
                          proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
  spTransform(., CRS(CRSproject))

aker_sp # transformert

image(signalstack)
points(aker_sp) #OK

# Testing on subset
aker_sub <- sample(aker_sp, size = 5, replace = FALSE)
image(signalstack)
points(aker_sub)

subPointID = c(1:length(aker_sub))
extract_test <- raster::extract(signalstack, aker_sub, buffer = 0.09024*2) # resolution = 0.09024

extract_test_df <- mapply(cbind, extract_test, PointID = subPointID, SIMPLIFY = F) %>% # adding point ID to colum
  do.call(rbind,.) # collapsing list

extract_test_df # through ID this can be joined with field data

# Full data
aker_signals <- raster::extract(signalstack, aker_sp, buffer = 0.2) # rundere tall...
manual_signals <- raster::extract(signalstack, akeroya_manual, buffer = 0.2) 

PointID = c(1:length(aker_sp)) # point IDs
MPointID = c(1:length(akeroya_manual))

# aker_signals_df <- mapply(cbind, aker_signals, IDpoint = PointID, SIMPLIFY = F) %>% # adding point ID to colum
# lapply(., data.frame) %>%   
# do.call(rbind, .) # get error on column numbers, which is odd ...

colnumbers <- sapply(aker_signals, ncol) %>% do.call(rbind, .)
table(colnumbers) # strange

# trying rbindlist from data.table-package
aker_signals_df <- mapply(cbind, aker_signals, IDpoint = PointID, SIMPLIFY = F) %>% # adding point ID to colum
  lapply(., data.frame) %>%
  rbindlist(., idcol=TRUE, fill = TRUE)

aker_signals_df # Finally!!

manual_signals_df <- mapply(cbind, manual_signals, IDpoint = MPointID, SIMPLIFY = F) %>% # adding point ID to colum
  lapply(., data.frame) %>%
  rbindlist(., idcol=TRUE, fill = TRUE)

aker_signals_df # Finally!!
manual_signals_df

# Joining field data and image data ---------------------------------------
akeroya_bext <- akeroya %>% mutate(IDpoint = c(1:nrow(akeroya))) %>% 
  full_join(., aker_signals_df)
akeroya_bext

akeroya_bext %>% group_by(Sp04_la) %>% summarise(N = n())

# still not a whole lot of rock ... add visual by same method? Check area for each point first...
# writing to file

write.csv(akeroya_bext, file = "./Ground truth/akeroya_buffer20_extract.csv", row.names = FALSE)

# Joining manually classified data and drone data -------------------------
manual_bext <- akeroya_manual@data %>% mutate(IDpoint = c(1:nrow(akeroya_manual@data))) %>% 
  full_join(., manual_signals_df)
manual_bext

manual_bext %>% group_by(man_cat) %>% summarise(N = n())

# writing to file
write.csv(manual_bext, file = "./Ground truth/manual_buffer20_extract.csv", row.names = FALSE)
