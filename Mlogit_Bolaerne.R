# Fitting mlogit model on Bolaerne datasett, including all channels in model
# Guri Sogn Andersen, 2020

# Packages, settings and data ---------------------------------------------
require(mlogit) 
require(tidyverse)
require(corrplot)
require(dismo)
require(caret)
require(e1071)
require(pROC)
require(randomForest)

# Data
bolaerne_bext <- read.csv("./Ground truth/bolaerne_buffer20_extract.csv", stringsAsFactors = FALSE)
str(bolaerne_bext)
summary(bolaerne_bext)

# House keeping -----------------------------------------------------------

# Fix spelling problems and reduce dataset
model_data_b <- bolaerne_bext %>% 
  mutate(Category = str_to_sentence(Sp04_la)) %>% # Fix spelling
  filter(!is.na(blue), !Category %in% c("", "-")) %>%  # remove NA
  dplyr::select(Category, blue:rededge) # reduce dataset

model_data_b %>% group_by(Category) %>% summarise(N = n())

write.csv(model_data_b, file = "./Models/model_data_bolaer.csv", row.names = FALSE)

# reload
model_data_b <- read.csv("./Models/model_data_bolaer.csv", stringsAsFactors = FALSE)

str(model_data_b)

# Feel like checking up on differences between Harrys extracted values and mine... (I included a buffer)
bolaerne_bext %>% ggplot() +
  geom_point(aes(x = bolaerne.red., y = red, col = "red")) +
  geom_point(aes(x = bolaerne.blue, y = blue, col = "blue")) +
  geom_point(aes(x = bolaerne.green., y = green, col = "green")) +
  geom_point(aes(x = bolaerne.red.edge, y = rededge, col = "rededge")) +
  geom_point(aes(x = bolaerne.nir, y = nir, col = "nir"))
# Looks OK. Remember that Harrys points are duplicated because I've extracted signals with a buffer...


# Fitting model -----------------------------------------------------------
# NB! NOT ENOUGH DATA TO SPLIT SET INTO TEST AND TRAINING AT THIS POINT
# NEED TO ADD MANUAL GT-POINTS FROM RGB-IMAGES FOR THIS TO BE DONE

# Checking for colinearity
model_data_b %>% dplyr::select(blue:rededge) %>% cor(., use = "complete.obs") %>% 
  corrplot.mixed(.)
# keeping rededge for now
# adding interactions with corr coeff < 0.7? Not sure about interactions since datapoints are so few. 
# Skipping interactions for now.

# Long format
BM1 <- dfidx(model_data_b %>% filter(!is.na(rededge)), shape = "wide", choice = "Category")
dim(BM1)

# Model
B_m1 <- mlogit(Category ~ 1|blue + green + nir + red + rededge, data = BM1) 

# Save model object
# saveRDS(B_m1, "./Models/B_m1.rds")
B_m1 <- readRDS("./Models/B_m1.rds")

# Model summary
summary(B_m1)

# Testing model -----------------------------------------------------------

# ON SELF
pred.B_m1 <- data.frame(predict(B_m1, newdata = BM1))
pred.B_m1

# Overview of highest prediction in relation to data - count/porportions?  
maxpred_B <- data.frame(Predcat = as.character(colnames(pred.B_m1)[apply(pred.B_m1, 1, which.max)]))
dim(maxpred_B)
head(maxpred_B)

model_data_b %>% filter(!is.na(rededge)) %>% cbind(., maxpred_B) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  ggplot(aes(x = Category, fill = Predcat)) +
  geom_bar() +
  labs(title = "Predicted outcome in relation to observations", x ="Observed", fill = "Predicted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))
# Looks almost too good to be true!!

# True / False
model_data_b %>% filter(!is.na(rededge)) %>%   
  cbind(., maxpred_B) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Hit = ifelse(Category == Predcat, TRUE, FALSE)) %>% 
  ggplot(aes(Category, fill = Hit)) +
  geom_bar(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Exact prediction in relation to observations", x ="Observed", fill = "Predicted")

