# Fitting mlogit model on joint datasett, including all channels in model
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

# Datasett
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

# All obs are kept
2919 + 250 # OK

### Housekeeping ###
# Rydd opp i kategorier?
model_data %>% group_by(Category) %>% summarise(N = n())

# Sjekk signalstyrker i de to datsettene
model_data %>% pivot_longer(cols = blue:red, names_to = "Channel", values_to = "Signal") %>% 
  ggplot(aes(x = Area, y = Signal, colour = Area)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(Channel ~ Category, scales = "free")
# Litt skeptisk til å samle disse i en modell! Forskjellen i signalstyrke mellom de ulike lokalitetene er i noen tilfeller betydelig!

# Training and test data --------------------------------------------------

kfoldset <- kfold(model_data, k = 5, by = model_data$Category)

test     <- model_data[kfoldset == 1, ]
train    <- model_data[kfoldset != 1, ]

# write.csv(train, file = "./Models/train_joint.csv", row.names = FALSE)
# write.csv(test, file = "./Models/test_joint.csv", row.names = FALSE)

test <- read.csv("./Models/test_joint.csv", stringsAsFactors = FALSE)
train <- read.csv("./Models/train_joint.csv", stringsAsFactors = FALSE)

test %>% group_by(Category) %>% summarise(N = n())
train %>% group_by(Category) %>% summarise(N = n())

# Long format

ml_test <- dfidx(test, shape = "wide", choice = "Category")
dim(ml_test)
ml_train <- dfidx(train, shape = "wide", choice = "Category")
dim(ml_train)


# Fitting model -----------------------------------------------------------

# Checking for colinearity
model_data %>% select(blue:red) %>% cor(., use = "pairwise.complete.obs") %>% 
  corrplot.mixed(.)
# removing rededge
# adding interactions with corr coeff < 0.7
# could not include any of the interactions without getting error
# suspect green, blue and red are too correlated

# Model
# m1_joint <- mlogit(Category ~ 1|blue + green + nir + red, data = ml_train) 

# Save model object
# saveRDS(m1_joint, "./Models/m1_joint.rds")
m1_joint <- readRDS("./Models/m1_joint.rds")

# Model summary
summary(m1_joint) # does not look good
str(m1_joint)

# Testing model -----------------------------------------------------------

# ON SELF
pred.m1 <- data.frame(predict(m1_joint, newdata = ml_train))
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

test.m1 <- data.frame(predict(m1_joint, newdata = ml_test))
test.m1

write.csv(test.m1, file = "./Models/test_m1_joint.csv", row.names = FALSE)

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

# # Til bruk i Rmarkdown logg:
test %>% cbind(., maxpredtest) %>%
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>%
  write.csv("./Validering/testval_joint.csv", row.names = FALSE)

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

as.matrix(results, what = "classes") %>% write.csv2(., file = "./Tabeller/ConfusionMatrixResultsClasses_joint.csv")
as.matrix(results, what = "overall") %>% write.csv2(., file = "./Tabeller/ConfusionMatrixResultsOverall_joint.csv")

# https://towardsdatascience.com/decoding-the-confusion-matrix-bb4801decbb 

# Multiclass ROC ----------------------------------------------------------

colnames(test.m1) <- gsub("\\.", " ", colnames(test.m1)) 
mROC <- multiclass.roc(test$Category, test.m1)
mROC

# multiclass AUC: 0.935

