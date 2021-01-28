# For report -------------------------------------------

require(tidyverse)
require(readxl)
require(corrplot)
require(caret)
require(pROC)

# Oversettelse av kategorier til norsk
signals_aker <- read.csv(file = "./Pixelextracts/signals_aker.csv", stringsAsFactors = FALSE)

# Kategorier:
names(signals_aker)
signals_aker %>% select(Category) %>% distinct

# Skriver ut til csv og redigerer der:
signals_aker %>% select(Category) %>% distinct %>% write.csv(file = "./Tabeller/Aker_cat.csv", row.names = FALSE)

# Laster inn oversettelse
transl_aker <- read_xlsx(path = "./Tabeller/Aker_transl.xlsx")
transl_aker

# Oversikt over observasjoner i ulike kategorier
signals_aker %>% left_join(transl_aker) %>% 
  group_by(Kategori) %>% summarise(`Antall obs.` = n()) %>% filter(!is.na(Kategori))

# Skriver ut til excel:
signals_aker %>% left_join(transl_aker) %>% 
  group_by(Kategori) %>% summarise(`Antall obs.` = n()) %>% filter(!is.na(Kategori)) %>% 
  write.csv(file = "./Tabeller/KategorierObs.csv", row.names = FALSE)

# Korrelasjonstester
model_data <- signals_aker %>% filter(Category != "") %>% filter(!is.na(blue))
# Checking for colinearity
model_data %>% dplyr::select(blue:red) %>% cor(.) %>% 
  corrplot.mixed(.)
# removing rededge
# adding interactions with corr coeff < 0.7

# Validering av modell ---------------------------
testval <- read.csv("./Validering/testval.csv")
testval %>% select(Category) %>% distinct()
brownalgae <- c("Brown algae", "Fucus serratus", "Fucus vesiculosus", "Fucus serratus ", "Bladder wrack")

# Joint (data fra begge områder i en modell)
testval_joint <- read.csv("./Validering/testval_joint.csv")

# oversette kategorier
testval %>% 
  left_join(transl_aker) %>% 
  rename(Obskat = Kategori) %>% 
  left_join(transl_aker, by = c("Predcat" = "Category")) %>% 
  rename(Predkat = Kategori) 

testval %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Predcat = ifelse(Predcat %in% brownalgae, "Brown algae", Predcat)) %>% 
  mutate(Category = ifelse(Category %in% brownalgae, "Brown algae", Category)) %>% 
  left_join(transl_aker) %>% 
  rename(Obskat = Kategori) %>% 
  left_join(transl_aker, by = c("Predcat" = "Category")) %>% 
  rename(Predkat = Kategori) %>% 
  ggplot(aes(x = Obskat, fill = Predkat)) +
  geom_bar() +
  labs(title = "Klassifisering sett i forhold til observasjoner", 
       x ="Observert", y = "Antall",
       fill = "Modellert") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

p1 <- testval %>%
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Predcat = ifelse(Predcat %in% brownalgae, "Brown algae", Predcat)) %>% 
  mutate(Category = ifelse(Category %in% brownalgae, "Brown algae", Category)) %>% 
  left_join(transl_aker) %>% 
  rename(Obskat = Kategori) %>% 
  left_join(transl_aker, by = c("Predcat" = "Category")) %>% 
  rename(Predkat = Kategori) %>% 
  mutate(Treff = ifelse(Obskat == Predkat, "Rett", "Feil")) %>% 
  ggplot(aes(Obskat, fill = Treff)) +
  geom_bar(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Klassifisering sett i forhold til observasjoner (testdata)", 
       x ="", y = "Antall", fill = "Modell")

p1


# Confusion matrix etc. -----------------------------------------------------

confusedata <- testval %>% 
  mutate(Predcat = gsub("\\.", " ", Predcat)) %>% 
  mutate(Predcat = ifelse(Predcat %in% brownalgae, "Brown algae", Predcat)) %>% 
  mutate(Category = ifelse(Category %in% brownalgae, "Brown algae", Category)) %>% 
  left_join(transl_aker) %>% 
  rename(Obskat = Kategori) %>% 
  left_join(transl_aker, by = c("Predcat" = "Category")) %>% 
  rename(Predkat = Kategori) %>%  
  mutate(id = row.names(.)) %>% 
  pivot_longer(cols = c(Obskat, Predkat), names_to = "Variable") %>% 
  mutate(value2 = as.factor(value)) %>% 
  pivot_wider(id_cols = id, names_from = Variable, values_from = value2) %>% 
  data.frame()

head(confusedata)
str(confusedata)

results <- confusionMatrix(confusedata$Obskat, confusedata$Predkat)
results

# Some tables :

as.table(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")

p2 <- as.matrix(results, what = "classes") %>% data.frame() %>% 
  rownames_to_column("Stat") %>%  
  filter(Stat == "Balanced Accuracy") %>% 
  pivot_longer(cols = `Brunalger`:`Tangvoll`, values_to = "Balanced Accuracy", names_to = "Kategori") %>% 
  ggplot() +
  ylim(0,1) +
  coord_flip() +
  geom_col(aes(x = Kategori, y = `Balanced Accuracy`)) +
  labs(x = "") +
  theme(legend.position = "none")

p2

require(patchwork)
p1 + p2 + plot_layout(nrow = 2) 

# Multiclass ROC ----------------------------------------------------------
test.m1 <- read.csv("./Models/test_m1.csv")
colnames(test.m1) <- gsub("\\.", " ", colnames(test.m1)) 
mROC <- multiclass.roc(testval$Category, test.m1)
mROC

# multiclass AUC: 0.9601 :-)