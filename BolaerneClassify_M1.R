# Using model fitted to Akerøya dataset to classify drone images from Bolærne
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

# Model
m1 <- readRDS("./Models/m1.rds")

# Data
bolaerne_bext <- read.csv("./Ground truth/bolaerne_buffer20_extract.csv")
summary(bolaerne_bext)

# Simplify
signals_bol <- bolaerne_bext %>% rename(Category = Sp04_la) %>% 
  select(Category, blue:rededge) %>% 
  mutate(Category = str_to_sentence(Category))

signals_bol %>% filter(!Category %in% c("", "-")) %>%  
  group_by(Category) %>% summarise(N = n())

# House keeping -----------------------------------------------------------

# remove empty category
signals_bol %>% filter(!Category %in% c("", "-")) %>% summary()
# remove rows with NA in predictorvars
signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue)) %>% 
  summary() # OK



modelcats <- unique(model_data$Category)
modelcats

bol_data <- signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue)) %>% 
  mutate(Category = rep_len(modelcats, length.out = 250)) # Creating dummy variable
bol_data %>% group_by(Category) %>% summarise(N = n())

# Testing model -----------------------------------------------------------

# ON NEW AREA

ml_bol <- dfidx(bol_data, shape = "wide", choice = "Category")
dim(ml_bol)

bol.m1 <- data.frame(predict(m1, newdata = ml_bol))
bol.m1

# Overview of highest prediction in relation to data - count/porportions?  
maxpredbol <- data.frame(Predcat = as.character(colnames(bol.m1)[apply(bol.m1, 1, which.max)]))
dim(maxpredbol)
head(maxpredbol)

signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue)) %>%
  cbind(., maxpredbol) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  ggplot(aes(x = Category, fill = Predcat)) +
  geom_bar() +
  labs(title = "Predicted outcome in relation to observations", x ="Observed", fill = "Predicted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# True / False
signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue)) %>%
  cbind(., maxpredbol) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Hit = ifelse(Category == Predcat, TRUE, FALSE)) %>% 
  ggplot(aes(Category, fill = Hit)) +
  geom_bar(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Exact prediction in relation to observations", x ="Observed", fill = "Predicted")


# Confusion matrix --------------------------------------------------------

# use caret package to create confusion matrix ... ?
# https://en.wikipedia.org/wiki/Confusion_matrix

bol_confusedata <- signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue)) %>%
  cbind(., maxpredbol) %>%  
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>%  
  mutate(id = row.names(.)) %>% 
  pivot_longer(cols = c(Category, Predcat), names_to = "Variable") %>% 
  mutate(value2 = as.factor(value)) %>% 
  pivot_wider(id_cols = id, names_from = Variable, values_from = value2) %>% 
  data.frame()

head(bol_confusedata)
str(bol_confusedata)

bol_results <- confusionMatrix(bol_confusedata$Category, bol_confusedata$Predcat)
bol_results

# Veeeldig dårlig til å skille klasser!
# https://towardsdatascience.com/decoding-the-confusion-matrix-bb4801decbb 

# Multiclass ROC ----------------------------------------------------------

colnames(bol.m1) <- gsub("\\.", " ", colnames(bol.m1)) 
bolpred <- signals_bol %>% filter(!Category %in% c("", "-")) %>% filter(!is.na(blue))
mROC <- multiclass.roc(bolpred$Category, bol.m1)
mROC

# Blir knot fordi det er forskjellige klasser i de to settene.
