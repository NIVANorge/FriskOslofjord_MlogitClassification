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
rasterOptions(maxmemory = 1.5e+10) # øker minne tilgjengelig for rasterpakka

# Data
model_data <- read.csv("./Models/model_data.csv")
summary(model_data)

# Drone images - local
imagepath <- "./Dronebilder/reflectance"
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

# rededge is not included in the model
rasterlist[-4]

signalstack <- stack(rasterlist[-4])
signalstack


# Refitting model ---------------------------------------------------------
# Data
ml_akeroya <- dfidx(model_data, shape = "wide", choice = "Category")
dim(ml_akeroya)

# Model

# m2 <- mlogit(Category ~ 1|blue + green + nir + red +
#                 blue:nir + green:nir + red:nir, data = ml_akeroya) 

# Save model object
saveRDS(m2, "./Models/m2.rds")
m2 <- readRDS("./Models/m2.rds")

# Model summary
summary(m2)
formula(m2)

# Predictions -------------------------------------------------------------

# Responses
responses <- unique(model_data$Category)
responses

rm(model_data) # clean-up
gc()

# Number of layers equal to number of prediction layers
ncat = length(responses)
ncat

# red edge is not included in the model

predr <- brick(signalstack, values = FALSE, nl = ncat) # empty raster brick with same specs as images
predr

tr <- blockSize(signalstack, n = ncat*4) # *10 was not so smart, smaller number prob wiser ...
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

predr <- writeStart(predr, filename = "./Predictions/Mlogit_predictions8bit.grd", 
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
  pred <- data.frame(round(predict(m2, newdata = v_long)*100)) # probabilities as integers 0-100
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
# KL 20:23, LØRDAG -> 2377
# KL 13:42, MANDAG -> 5870 
# (veeeldig tregt! fila er kun 20 + GB foreløpig)
# Think I need to look into the 8 bits vs. 16 bits image stuff ...
# This is 32 bit float!
# Modified script - now hopefully down to 8 bit integer

# if reload needed
predr <- brick("./Predictions/Mlogit_predictions8bit.grd")
predr
names(predr) <- gsub("\\.", "_", colnames(predmatr))
names(predr)
# Is there a good way of plotting rasterbricks?

tm_shape(predr$Brown_algae) +
  tm_raster(palette = viridis(10), title = "Modellert sannsynlighet") + #, legend.hist = TRUE
  tm_layout(#scale = .8,
    #legend.outside = TRUE,
    legend.just = "bottom",
    legend.position = c("right","bottom"),
    frame = FALSE, 
    legend.bg.color = "white", legend.bg.alpha = 0)

# Handling errors ---------------------------------------------------------
# Got error messages indicating missing data in some parts of the prediction area
# Looked at the layers in GRASS, and seems to be minor problems
# Not able to open/plot the .grd-files in QGIS or R
# Exported GTiff from GRASS, and it seems fine ...

r_Layer1 <- raster("./Predictions/Mlogit_Akeroya_8bit_layer1_GRASS.tif")
image(r_Layer1) # OK

# Exported entire .grd as Gtiff from GRASS
predr <- brick("./Predictions/Mlogit_8bit_GRASS.tif")
predr
names(predr) <- colnames(predmatr)
names(predr)

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
                       filename = "./Predictions/maxprob", overwrite = TRUE, format="GTiff", progress = "text")

maxprob <- raster("./Predictions/maxprob.tif")
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
                      filename = "./Predictions/catmap", format="GTiff", overwrite=TRUE, progress = "text")
#catmap <- raster("./Predictions/catmap.tif")
catmap
plot(catmap)
gc()

names(predr)
gsub("\\.", " ", names(predr))

brownalgae = c("Bladder.wrack", "Brown.algae", "Fucus.serratus.", "Fucus.vesiculosus")


categories <- data.frame(CatID = c(1:16), Category = names(predr)) %>% 
  mutate(Col_cat = ifelse(Category %in% brownalgae, "Brown algae", gsub("\\.", " ", Category)))
categories

write.csv(categories, file = "./Leveranse/KategorierRaster.csv")

newcat <- data.frame(Col_cat = unique(categories$Col_cat), NewID = seq_along(along.with = unique(categories$Col_cat)))
newcat

categories <- categories %>% left_join(newcat)
categories

### Reclassify raster 
#( but be aware that "Bladder wrack" also contains unknowns - like much of the water...)
# Best way to handle this right now is probably to crop layer to a maximum depth that is shallow or to tidal zone
# Did also multiply by maxprob-mask to remove areas with maxprob = 0

catmap_reclass <- writeRaster(reclassify(catmap, c(0, 4, 1)), 
                              filename = "./Predictions/catmap_reclass", format="GTiff", overwrite=TRUE, progress = "text")

catmap_reclass

#### Polygonizing in GRASS #### (much faster)

# Loading shapefile created in GRASS
catmap_v <- readOGR(dsn = "./Predictions/v_catmap_akeroya")
catmap_v

#### Assigning categories to polygons ####
# Value corresponds to order of names in predr

names(predr)
gsub("_", " ", names(predr))

categories <- data.frame(CatID = c(1:16), Category = gsub("_", " ", names(predr)))
categories

catmap_data <- catmap_v@data %>% left_join(categories)
head(catmap_data)

catmap_v@data <- catmap_data

writeOGR(catmap_v, dsn = "./Predictions/v_catmap_akeroya", layer = "v_catmap_akeroya_kodet", driver = "ESRI Shapefile")



# Writing rasterbrick to several GTiff ------------------------------------
writeRaster(predr, filename = paste("./Predictions/Probmaps/", gsub("\\.", "_", names(predr)), sep =""), bylayer = TRUE, format = "GTiff", overwrite = TRUE)

summary(predr)


# Testing map vs point predictions ----------------------------------------

# Try extracting points to compare with validation set
aker_sp
map_predictions <- raster::extract(predr, aker_sp)

