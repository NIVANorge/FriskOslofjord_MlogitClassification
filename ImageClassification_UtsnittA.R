# Classification of drone images based on different mlogit-models
# Guri Sogn Andersen, 2021

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
rasterOptions(maxmemory = 3e+10) # øker minne tilgjengelig for rasterpakka (skipper nå siden jeg kjører QGIS samtidig)

# Data

# Akerøya
model_data_a <- read.csv("./Models/model_data.csv", stringsAsFactors = FALSE) %>% mutate(Area = "Akeroya")
str(model_data_a)

# Bolærne
model_data_b <- read.csv("./Models/model_data_bolaer.csv", stringsAsFactors = FALSE) %>% mutate(Area = "Bolaerne")
str(model_data_b)

# Joint + change "Bladder wrack" to "Fucus vesiculosus"
model_data <- model_data_a %>% full_join(model_data_b) %>% 
  mutate(Category = ifelse(Category == "Bladder wrack", "Fucus vesiculosus", Category))
str(model_data)

# Drone images - local
imagepath <- "C:/RProjects/DroneFriskOslofjord/Utsnitt A"
dir(imagepath)

imagefilelist <- list.files(path = imagepath, pattern = ".tif$")  # ramser alle filer med .tif
imagefilepaths <- paste(imagepath, imagefilelist, sep = "\\") # ramser opp stier
imagefilelistnames <- gsub("SectionA_Merge_MS_Akeroy_V4_transparent_reflectance_*", "", imagefilelist) # lager navn
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
m1_joint <- readRDS("./Models/m1_joint.rds")

# Model summary
summary(m1_joint) # does not look very good
str(m1_joint)

# Predictions -------------------------------------------------------------

# Responses
responses <- unique(model_data$Category)
responses

gc()

# Number of layers equal to number of prediction layers
ncat = length(responses)
ncat

# red edge is not included in the model

predr <- brick(signalstack, values = FALSE, nl = ncat) # empty raster brick with same specs as images
predr

tr <- blockSize(signalstack, n = ncat*2) # *10 was not so smart, smaller number prob wiser ...
tr

# for keeping track 
nr.round <- c(1:tr$n)
mean(nr.round)

# remove leftovers...
rm(pred, preddata, predmatr, v_long, v_mlogit)
gc()
# start writing - saving layers with bit depth of 8!!

predr <- writeStart(predr, filename = "./Utsnitt A/PredictionsA/JOINT_Mlogit_predictions8bit.grd", 
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
  pred <- data.frame(round(predict(m1_joint, newdata = v_long)*100)) # probabilities as integers 0-100
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
# Start: Onsdag kl 11:52
# 
# Stop:  

# if reload needed
predr <- brick("./Utsnitt A/PredictionsA/JOINT_Mlogit_predictions8bit.grd")
predr
names(predr) <- gsub("\\.", "_", colnames(predmatr))
names(predr)
# Is there a good way of plotting rasterbricks?

plot(predr)

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
                       filename = "./Utsnitt A/PredictionsA/maxprob_joint", overwrite = TRUE, format="GTiff", progress = "text")

maxprob <- raster("./Utsnitt A/PredictionsA/maxprob_joint.tif")
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
                      filename = "./Utsnitt A/PredictionsA/catmap_joint", format="GTiff", overwrite=TRUE, progress = "text")

catmap <- raster("./Utsnitt A/PredictionsA/catmap_joint.tif")
catmap
plot(catmap)
gc()

### I think this will be directly comparable to the model based on Akerøya-data. BUT, Check that the order of categories are the same ...
### Reclassify

catmap_reclass <- writeRaster(reclassify(catmap, c(0, 4, 1)), 
                              filename = "./Utsnitt A/PredictionsA/catmap_joint_reclass", format="GTiff", overwrite=TRUE, progress = "text")

catmap_reclass

# Mask in QGIS to remove areas where maxprob = 0