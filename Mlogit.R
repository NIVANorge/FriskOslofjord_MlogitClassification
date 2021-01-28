# Fitting mlogit model on Akerøya datasett, including all channels in model
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
signals_aker <- read.csv("./Pixelextracts/signals_aker.csv")
str(signals_aker)
summary(signals_aker)

# House keeping -----------------------------------------------------------

# Need to remove NAs
signals_aker %>% group_by(Category) %>% summarise(N = n())
# remove empty category
signals_aker %>% filter(Category != "") %>% summary()
# remove rows with NA in predictorvars
signals_aker %>% filter(Category != "") %>% filter(!is.na(blue)) %>% 
  summary() # OK

model_data <- signals_aker %>% filter(Category != "") %>% filter(!is.na(blue))
model_data %>% group_by(Category) %>% summarise(N = n())
# probably enough data ...
write.csv(model_data, file = "./Models/model_data.csv", row.names = FALSE)

# reload
model_data <- read.csv("./Models/model_data.csv", stringsAsFactors = FALSE)

# Collaps

# Training and test data --------------------------------------------------

kfoldset <- kfold(model_data, k = 5, by = model_data$Category)

test     <- model_data[kfoldset == 1, ]
train    <- model_data[kfoldset != 1, ]

# write.csv(train, file = "./Models/train.csv", row.names = FALSE)
# write.csv(test, file = "./Models/test.csv", row.names = FALSE)

test <- read.csv("./Models/test.csv", stringsAsFactors = FALSE)
train <- read.csv("./Models/train.csv", stringsAsFactors = FALSE)

test %>% group_by(Category) %>% summarise(N = n())
train %>% group_by(Category) %>% summarise(N = n())

# Perhaps a bit few testpoints...

ml_test <- dfidx(test, shape = "wide", choice = "Category")
dim(ml_test)
ml_train <- dfidx(train, shape = "wide", choice = "Category")
dim(ml_train)


# Fitting model -----------------------------------------------------------

# Checking for colinearity
model_data %>% select(blue:red) %>% cor(.) %>% 
  corrplot.mixed(.)
# removing rededge
# adding interactions with corr coeff < 0.7

# Model
# m1 <- mlogit(Category ~ 1|blue + green + nir + red +
#                blue:nir + green:nir + red:nir, data = ml_train) 

# Save model object
# saveRDS(m1, "./Models/m1.rds")
m1 <- readRDS("./Models/m1.rds")

# Model summary
summary(m1)
str(m1)

# Testing model -----------------------------------------------------------

# ON SELF
pred.m1 <- data.frame(predict(m1, newdata = ml_train))
pred.m1

# Overview of highest prediction in relation to data - count/porportions?  
maxpred <- data.frame(Predcat = as.character(colnames(pred.m1)[apply(pred.m1, 1, which.max)]))
dim(maxpred)
head(maxpred)

train %>% cbind(., maxpred) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  ggplot(aes(x = Category, fill = Predcat)) +
  geom_bar() +
  labs(title = "Predicted outcome in relation to observations", x ="Observed", fill = "Predicted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# True / False
train %>%   
  cbind(., maxpred) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Hit = ifelse(Category == Predcat, TRUE, FALSE)) %>% 
  ggplot(aes(Category, fill = Hit)) +
  geom_bar(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Exact prediction in relation to observations", x ="Observed", fill = "Predicted")

# ON TEST

test.m1 <- data.frame(predict(m1, newdata = ml_test))
test.m1

write.csv(test.m1, file = "./Models/test_m1.csv", row.names = FALSE)

# Overview of highest prediction in relation to data - count/porportions?  
maxpredtest <- data.frame(Predcat = as.character(colnames(test.m1)[apply(test.m1, 1, which.max)]))
dim(maxpredtest)
head(maxpredtest)

test %>% cbind(., maxpredtest) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  ggplot(aes(x = Category, fill = Predcat)) +
  geom_bar() +
  labs(title = "Predicted outcome in relation to observations", x ="Observed", fill = "Predicted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# True / False
test %>%   
  cbind(., maxpredtest) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Hit = ifelse(Category == Predcat, TRUE, FALSE)) %>% 
  ggplot(aes(Category, fill = Hit)) +
  geom_bar(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Exact prediction in relation to observations", x ="Observed", fill = "Predicted")

# Til bruk i Rmarkdown logg:
test %>% cbind(., maxpredtest) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  write.csv("./Validering/testval.csv", row.names = FALSE)

# Confusion matrix --------------------------------------------------------

# use caret package to create confusion matrix ... ?
# https://en.wikipedia.org/wiki/Confusion_matrix

confusedata <- test %>%   
  cbind(., maxpredtest) %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>%  
  mutate(id = row.names(.)) %>% 
  pivot_longer(cols = c(Category, Predcat), names_to = "Variable") %>% 
  mutate(value2 = as.factor(value)) %>% 
  pivot_wider(id_cols = id, names_from = Variable, values_from = value2) %>% 
  data.frame()

head(confusedata)
str(confusedata)

results <- confusionMatrix(confusedata$Category, confusedata$Predcat)
results

# Some tables :

as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")

as.matrix(results, what = "classes") %>% write.csv2(., file = "./Tabeller/ConfusionMatrixResultsClasses.csv")
as.matrix(results, what = "overall") %>% write.csv2(., file = "./Tabeller/ConfusionMatrixResultsOverall.csv")

# https://towardsdatascience.com/decoding-the-confusion-matrix-bb4801decbb 

# Multiclass ROC ----------------------------------------------------------

colnames(test.m1) <- gsub("\\.", " ", colnames(test.m1)) 
mROC <- multiclass.roc(test$Category, test.m1)
mROC

# multiclass AUC: 0.9601 :-)

