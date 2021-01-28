# Classification of drone images based on mlogit
# Guri Sogn Andersen, 2020

# Packages, settings and data ---------------------------------------------

# Load packages into your workspace
require(tidyverse)
require(raster)
require(rgdal)
require(mlogit)
require(viridis)
require(tmap)

# Memory
memory.limit(size = 30000)

# Rastersettings
rasterOptions()
rasterOptions(tmpdir="./Tempfiles")
#rasterOptions(maxmemory = 1.5e+10) # øker minne tilgjengelig for rasterpakka (skipper nå siden jeg kjører QGIS samtidig)

# Data
model_data_b <- read.csv("./Models/model_data_bolaer.csv", stringsAsFactors = FALSE)
summary(model_data_b)

# Drone images - local
b_imagepath <- "C:/RProjects/DroneFriskOslofjord/Dronebilder/0_Bolaerne"
dir(b_imagepath)

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

# stack images
signalstack <- stack(rasterlist)
signalstack


# Model -------------------------------------------------------------------
B_m1 <- readRDS("./Models/B_m1.rds") 

# Model summary
summary(B_m1)
formula(B_m1)

# Predictions -------------------------------------------------------------

# Responses
responses <- unique(model_data_b$Category)
responses

# rm(model_data_b, maxpred_B, pred.B_m1) # clean-up
gc()

# Number of layers equal to number of prediction layers
ncat = length(responses)
ncat

# red edge is not included in the model

predr <- brick(signalstack, values = FALSE, nl = ncat) # empty raster brick with same specs as images
predr

tr <- blockSize(signalstack, n = ncat) # *10 was not so smart, smaller number prob wiser ...
tr

# for keeping track 
nr.round <- c(1:tr$n)
mean(nr.round)

# Predictions - does it all in one go. # WARNING - This takes time! 
# Would be nice to make this into a more general function?
# Removed all project objects and data before running
# Test on chuncks 1000 to 1050?
# Tok ca 1 time og 45 min med James forslag til fillagring, str ble 103246 kB
# Med .grd tok det (test2 startet 11.33) ca det samme, str ble ca 4 GB ...

# remove leftovers...
rm(pred, preddata, predmatr, v_long, v_mlogit)
gc()
# start writing - saving layers with bit depth of 8!!

predr <- writeStart(predr, filename = "./Predictions/Bolaerne_Mlogit_predictions8bit.grd", 
                    #format = "GTiff", 
                    datatype = "INT1U", 
                    #options=c("COMPRESS=LZW", "INTERLEAVE=BAND"), 
                    overwrite = TRUE)
# writing stuff 
for (i in 1:tr$n) { 
  v <- getValuesBlock(signalstack, row = tr$row[i], nrows = tr$nrows[i])
  blocklength <- length(v)
  # creating long format
  v_mlogit <- data.frame(v)
  # creating dummy response (arbritary)
  v_mlogit$Category <- base::sample(responses, nrow(v), replace=TRUE)
  # creating mlogit data.frame
  v_long <- dfidx(v_mlogit, shape = "wide", choice = "Category")
  # removing rasterdata from memory
  rm(v)
  gc()
  # running predictions
  pred <- data.frame(round(predict(B_m1, newdata = v_long)*100)) # probabilities as integers 0-100
  # dealing with NA-rows (missing in prediction dataset) ...
  pred$index <- as.numeric(rownames(pred)) # creating index of predicted rows
  rows <- data.frame(c(1:blocklength)) # creating index of all rows in chunk
  names(rows) <- "index" # assigning name 
  preddata <- left_join(rows, pred) # gives NA where no predictions are made
  predmatr <- preddata %>% dplyr::select(-index) %>% as.matrix()
  # writing probability-layers
  predr <- writeValues(predr, predmatr, tr$row[i]) 
  # keeping track of where we are...
  print(paste(nr.round[i], "of", length(nr.round), sep = " "))
}

# kicking ass and taking names ...
names(predr) <- colnames(predmatr)
# stop writing
predr <- writeStop(predr)

predr
# Start: Fredag kl 15:23
# 41 av 52 kl 18:52
# Stop:  Fredag kl 19:50
# Modified script - now hopefully down to 8 bit integer

# if reload needed
predr <- brick("./Predictions/Bolaerne_Mlogit_predictions8bit.grd")
predr
names(predr) <- gsub("\\.", "_", colnames(predmatr))
names(predr)
# Is there a good way of plotting rasterbricks?

tm_shape(predr) +
  tm_raster(palette = viridis(10), title = "Modellert sannsynlighet") + #, legend.hist = TRUE
  tm_layout(#scale = .8,
    #legend.outside = TRUE,
    legend.just = "bottom",
    legend.position = c("right","bottom"),
    frame = FALSE, 
    legend.bg.color = "white", legend.bg.alpha = 0)

# Composite maps ----------------------------------------------------------

# MAXPROB - extract the highest probability from any layer for each cell
maxprob <- writeRaster(max(predr, na.rm = TRUE),
                       filename = "./Predictions/maxprob_bolaerne", overwrite = TRUE, format="GTiff", progress = "text")

maxprob <- raster("./Predictions/maxprob_bolaerne.tif")
maxprob
plot(maxprob, col = viridis(10), zlim = c(0,100), axes = FALSE, bty="n")
gc()

tm_shape(maxprob) +
  tm_raster(palette = viridis(10), title = "Sannsynlighet") + #, legend.hist = TRUE
  tm_layout(#scale = .8,
    #legend.outside = TRUE,
    legend.just = "bottom",
    legend.position = c("right","bottom"),
    frame = FALSE, 
    legend.bg.color = "white", legend.bg.alpha = 0, legend.outside = TRUE)


# RASTERMAP of most likely categories present in each raster cell based on channel signals and mlogit
# This is the predicted category - the meaning of the numbers are listed below
catmap <- writeRaster(which.max(predr), 
                      filename = "./Predictions/catmap_bolaerne", format="GTiff", overwrite=TRUE, progress = "text")

catmap <- raster("./Predictions/catmap_bolaerne.tif")
catmap
plot(catmap)
gc()

### Reclassify raster 
#( but be aware that "Acophyllum" also contains unknowns - like much of the water...)
# Best way to handle this right now is probably to crop layer to a maximum depth that is shallow or to tidal zone
# Did also multiply by maxprob-mask to remove areas with maxprob = 0

# Decided to keep Ascophyllum separate from other brown, since the signal is quite distinct
catmap_masked <- raster("./Predictions/catmap_bolaerne_masked.tif")
catmap_reclass <- writeRaster(reclassify(catmap_masked, c(2, 3, 2)), 
                              filename = "./Predictions/catmap_bolarne_masked_reclass", format="GTiff", overwrite=TRUE, progress = "text")

catmap_reclass

#### Polygonizing in GRASS #### (much faster)

# Loading shapefile created in GRASS
catmap_v <- readOGR(dsn = "./Predictions/v_catmap_bolaerne")

#### Assigning categories to polygons ####
# Value corresponds to order of names in predr

names(predr)
names(predr) <- gsub("\\.", "_", names(predr))
gsub("_", " ", names(predr))


categories <- data.frame(DN = c(1:9), Category = gsub("_", " ", names(predr)))
categories

catmap_data <- catmap_v@data %>% left_join(categories)
head(catmap_data)

catmap_v@data <- catmap_data

writeOGR(catmap_v, dsn = "./Predictions/v_catmap_bolaerne", layer = "v_catmap_bolaerne_kodet", driver = "ESRI Shapefile")
