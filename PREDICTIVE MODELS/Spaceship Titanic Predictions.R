# Business Challenge 2: Spaceship Titanic
# Team 3: Alessio Morgantini, Gabriel Chouraqui, Giovanni Ghedini, Masaya Inoue, Victoria Baust

options(scipen = 999)

# Datasets
my_df <-read.csv("~/Desktop/train.csv")
my_df_train <- my_df
my_df_test <- read.csv("~/Desktop/test.csv")

remove.packages("conflicted")

# Libraries
library(rpart)
library(rpart.plot)
library(caret)
library(zoo)
library(caret)
library(ggplot2)
library(dplyr)
library(tseries)
library(rugarch)
library(plotly)
library(rugarch)
library(corrplot)
library(stringr)
library(tidyr)
library(dplyr)
# install.packages("FNN")  # Install FNN package
library(FNN)
library(randomForest)
# install.packages('xgboost')
library(xgboost)
#install.packages('gbm')
library(gbm)

## Functions
# NORMALIZATION ("min max")
min_max <- function(x){   #define unique function of x
  normalize <- (x-min(x))/(max(x)-min(x)) 
  return(normalize)
}


### Cleaning

# TRAIN DATASET
colSums(my_df == "")
colSums(is.na(my_df))

# Convert blank to NA
my_df_train[my_df_train == ""] <- NA
colSums(is.na(my_df_train))

# Cost: create column Total Cost
my_df_train$TotCost <- rowSums(my_df_train[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = TRUE)
sum(is.na(my_df_train$TotCost))

# Substitute outilers with 80th percentile -- create upper limit
boxplot(my_df_train$TotCost)

q80 <- quantile(my_df_train$TotCost, 0.80)

# Create a boxplot to identify outliers
boxplot(my_df_train$TotCost, main="Boxplot of TotCost")

# Identify and replace outliers
outliers <- which(my_df_train$TotCost > 1.5 * IQR(my_df_train$TotCost, na.rm = TRUE) + q80)
my_df_train$TotCost[outliers] <- q80

# Create Column Group Id
my_df_train[c('GroupId', 'GroupSize')] <- str_split_fixed(my_df_train$PassengerId, '_', 2)

colSums(my_df_train == "")
colSums(is.na(my_df_train))

# Split Cabin column
split_columns <- strsplit(my_df_train$Cabin, "/")
my_df_train$Deck <- sapply(split_columns, function(x) x[1])
my_df_train$CabinNumber <- sapply(split_columns, function(x) x[2])
my_df_train$Side <- sapply(split_columns, function(x) x[3])

# Create Column First Name and Last name
my_df_train[c('FirstName', 'LastName')] <- str_split_fixed(my_df_train$Name, ' ', 2)

# Sort by Last Name
my_df_train <- arrange(my_df_train, desc(LastName))


# Destination -- filling for same GroupId
for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$Destination[i] <- ifelse(is.na(my_df_train$Destination[i]) & 
                                         my_df_train$GroupId[i] == my_df_train$GroupId[i + 1], 
                                       my_df_train$Destination[i + 1],
                                       ifelse(is.na(my_df_train$Destination[i]) & 
                                                my_df_train$GroupId[i] == my_df_train$GroupId[i - 1], 
                                              my_df_train$Destination[i - 1], my_df_train$Destination[i]))
}

# Destination -- filling for same LastName
for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$Destination[i] <- ifelse(is.na(my_df_train$Destination[i]) & 
                                         my_df_train$LastName[i] == my_df_train$LastName[i + 1], 
                                       my_df_train$Destination[i + 1],
                                       ifelse(is.na(my_df_train$Destination[i]) & 
                                                my_df_train$LastName[i] == my_df_train$LastName[i - 1], 
                                              my_df_train$Destination[i - 1], my_df_train$Destination[i]))
}


# Home Planets -- filling for same Last Name
for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$HomePlanet[i] <- ifelse(is.na(my_df_train$HomePlanet[i]) & 
                                        my_df_train$LastName[i] == my_df_train$LastName[i + 1], 
                                      my_df_train$HomePlanet[i + 1],
                                      ifelse(is.na(my_df_train$HomePlanet[i]) & 
                                               my_df_train$LastName[i] == my_df_train$LastName[i - 1], 
                                             my_df_train$HomePlanet[i - 1], my_df_train$HomePlanet[i]))
}


## Remaining Home Planet -- fill with corresponding Deck
my_df_train$HomePlanet <- ifelse(is.na(my_df_train$HomePlanet) & my_df_train$Deck %in% c('A', 'B', 'C', 'T'), 'Europa', my_df_train$HomePlanet)
my_df_train$HomePlanet <- ifelse(is.na(my_df_train$HomePlanet) & my_df_train$Deck == 'G', 'Earth', my_df_train$HomePlanet)


# Home planet remaining (only 5) -- replace with MODE
non_null_home<- my_df_train$HomePlanet[!is.na(my_df_train$HomePlanet)]
mode_home <- names(sort(table(non_null_home), decreasing = TRUE))[1]
my_df_train$HomePlanet[is.na(my_df_train$HomePlanet)] <- mode_home

# Destination remaining (only 13) -- replace with MODE
non_null_destination<- my_df_train$Destination[!is.na(my_df_train$Destination)]
mode_destination <- names(sort(table(non_null_destination), decreasing = TRUE))[1]
my_df_train$Destination[is.na(my_df_train$Destination)] <- mode_destination


# Fill the nulls in Side with the values of people with the same Last Name
for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$Side[i] <- ifelse(my_df_train$LastName[i] == my_df_train$LastName[i + 1] &
                                  is.na(my_df_train$Side[i]), my_df_train$Side[i + 1], 
                                ifelse(my_df_train$LastName[i] == my_df_train$LastName[i - 1] &
                                         is.na(my_df_train$Side[i]), my_df_train$Side[i - 1], 
                                       my_df_train$Side[i]))
}
# Side remaining (only 10) -- replace with MODE
non_null_side<- my_df_train$Side[!is.na(my_df_train$Side)]
mode_side <- names(sort(table(non_null_side), decreasing = TRUE))[1]
my_df_train$Side[is.na(my_df_train$Side)] <- mode_side



my_df_train$CabinNumber <- as.numeric(my_df_train$CabinNumber)


# Cabin Nulls: If they have same last name, same deck, same side --> same Cabin (if group size <=4)
my_df_train$LastName_Deck_Side <- paste0(my_df_train$LastName, "_", my_df_train$Deck, "_", my_df_train$Side)

my_df_train <- arrange(my_df_train, desc(LastName_Deck_Side))

for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$CabinNumber[i] <- ifelse(my_df_train$LastName_Deck_Side[i] == my_df_train$LastName_Deck_Side[i + 1] &
                                         is.na(my_df_train$CabinNumber[i]), my_df_train$CabinNumber[i + 1], 
                                       ifelse(my_df_train$LastName_Deck_Side[i] == my_df_train$LastName_Deck_Side[i - 1] &
                                                is.na(my_df_train$CabinNumber[i]), my_df_train$CabinNumber[i - 1], 
                                              my_df_train$CabinNumber[i]))
}

# Cabin Nulls: filling with same group id
my_df_train <- arrange(my_df_train, desc(GroupId))

for (i in 1:(nrow(my_df_train) - 1)) {
  my_df_train$CabinNumber[i] <- ifelse(my_df_train$GroupId[i] == my_df_train$GroupId[i + 1] &
                                         is.na(my_df_train$CabinNumber[i]), my_df_train$CabinNumber[i + 1], 
                                       ifelse(my_df_train$GroupId[i] == my_df_train$GroupId[i - 1] &
                                                is.na(my_df_train$CabinNumber[i]), my_df_train$CabinNumber[i - 1], 
                                              my_df_train$CabinNumber[i]))
}

## Cabin nulls rimaning (96) -- fill with MODE
non_null_cabinNum<- my_df_train$CabinNumber[!is.na(my_df_train$CabinNumber)]
mode_cabinNum <- names(sort(table(non_null_cabinNum), decreasing = TRUE))[1]
my_df_train$CabinNumber[is.na(my_df_train$CabinNumber)] <- mode_cabinNum



# Deck missing values -- in G if they are in Cryo and didn't get transported (90% chance)
my_df_train$Deck <- ifelse(is.na(my_df_train$Deck) & 
                             my_df_train$CryoSleep == 'True' & 
                             my_df_train$Transported == 'False', 'G', my_df_train$Deck)


# Deck Nulls -- give the same distribution that not-nulls have
 #distribution of non-null values
deck_distribution <- my_df_train %>%
  filter(!is.na(Deck)) %>%
  group_by(Deck) %>%
  summarise(count = n())

# impute missing values based on the distribution
my_df_train <- my_df_train %>%
  group_by(LastName) %>%
  mutate(Deck = ifelse(is.na(Deck), sample(deck_distribution$Deck, sum(is.na(Deck)), 
                                           replace = TRUE, 
                                           prob = deck_distribution$count / sum(deck_distribution$count)), Deck))



# DUMMIES
# For the side P = 1 and S = 0
my_df_train$Side <- ifelse(my_df_train$Side == "P", 1, 0)

my_df_train$CryoSleep <- ifelse(my_df_train$CryoSleep == "True", 1, 0)

## Filling CryoSleep nulls --> if they spent money they are not in cryosleep
my_df_train$CryoSleep <- ifelse(is.na(my_df_train$CryoSleep) & my_df_train$TotCost > 0, 0, my_df_train$CryoSleep)

my_df_train$Transported <- ifelse(my_df_train$Transported == "True", 1, 0)

my_df_train$Europa <- ifelse(my_df_train$HomePlanet == "Europa", 1, 0)
my_df_train$Earth <- ifelse(my_df_train$HomePlanet == "Earth", 1, 0)
#my_df_train$Mars <- ifelse(my_df_train$HomePlanet == "Mars", 1, 0)

my_df_train$Trappist <- ifelse(my_df_train$Destination == "TRAPPIST-1e", 1, 0)
my_df_train$PSO <- ifelse(my_df_train$Destination == "PSO J318.5-22", 1, 0)
#my_df_train$Cancri <- ifelse(my_df_train$Destination == "55 Cancri e", 1, 0)

unique(my_df_train$Deck)
my_df_train$DeckA <- ifelse(my_df_train$Deck == "A", 1, 0)
my_df_train$DeckB <- ifelse(my_df_train$Deck == "B", 1, 0)
my_df_train$DeckC <- ifelse(my_df_train$Deck == "C", 1, 0)
my_df_train$DeckD <- ifelse(my_df_train$Deck == "D", 1, 0)
my_df_train$DeckE <- ifelse(my_df_train$Deck == "E", 1, 0)
my_df_train$DeckF <- ifelse(my_df_train$Deck == "F", 1, 0)
my_df_train$DeckG <- ifelse(my_df_train$Deck == "G", 1, 0)
#my_df_train$DeckT <- ifelse(my_df_train$Deck == "T", 1, 0)


# VIP
my_df_train$VIP <- ifelse(my_df_train$VIP == "True", 1, 0)
my_df_train$VIP <- ifelse(is.na(my_df_train$VIP), 0, my_df_train$VIP) # fill the nulls with 0


summary(my_df_train)
# Convert in Numeric
my_df_train$CabinNumber <- as.numeric(my_df_train$CabinNumber)
my_df_train$GroupSize <- as.numeric(my_df_train$GroupSize)
my_df_train$GroupId <- as.numeric(my_df_train$GroupId)



# Nulls Costs
mode_value_roomService <- names(sort(table(my_df_train$RoomService), decreasing = TRUE))[1]
my_df_train$RoomService <- ifelse(is.na(my_df_train$RoomService), mode_value_roomService ,my_df_train$RoomService)

mode_value_FoodCourt <- names(sort(table(my_df_train$FoodCourt), decreasing = TRUE))[1]
my_df_train$FoodCourt <- ifelse(is.na(my_df_train$FoodCourt), mode_value_FoodCourt ,my_df_train$FoodCourt)

mode_value_ShoppingMall <- names(sort(table(my_df_train$ShoppingMall), decreasing = TRUE))[1]
my_df_train$ShoppingMall <- ifelse(is.na(my_df_train$ShoppingMall), mode_value_ShoppingMall ,my_df_train$ShoppingMall)

mode_value_Spa <- names(sort(table(my_df_train$Spa), decreasing = TRUE))[1]
my_df_train$Spa <- ifelse(is.na(my_df_train$Spa), mode_value_Spa ,my_df_train$Spa)

mode_value_VRDeck <- names(sort(table(my_df_train$VRDeck), decreasing = TRUE))[1]
my_df_train$VRDeck <- ifelse(is.na(my_df_train$VRDeck), mode_value_VRDeck ,my_df_train$VRDeck)

colSums(is.na(my_df_train))

# in numeric
my_df_train$RoomService <- as.numeric(my_df_train$RoomService)
my_df_train$FoodCourt <- as.numeric(my_df_train$FoodCourt)
my_df_train$ShoppingMall <- as.numeric(my_df_train$ShoppingMall)
my_df_train$Spa <- as.numeric(my_df_train$Spa)
my_df_train$VRDeck <- as.numeric(my_df_train$VRDeck)


# Normalizing
my_df_train$TotCost_norm <- min_max(my_df_train$TotCost)
my_df_train$GroupId_norm <- min_max(my_df_train$GroupId)
my_df_train$GroupSize_norm <- min_max(my_df_train$GroupSize)
my_df_train$CabinNumber_norm <- min_max(my_df_train$CabinNumber)
my_df_train$RoomService_norm <- min_max(my_df_train$RoomService)
my_df_train$FoodCourt_norm <- min_max(my_df_train$FoodCourt)
my_df_train$ShoppingMall_norm <- min_max(my_df_train$ShoppingMall)
my_df_train$Spa_norm <- min_max(my_df_train$Spa)
my_df_train$VRDeck_norm <- min_max(my_df_train$VRDeck)

str(my_df_train)

# Predict the age with KKN
rows_with_missing_age <- which(is.na(my_df_train$Age))
unknown_ages <- my_df_train[rows_with_missing_age, sapply(my_df_train, is.numeric) & !(names(my_df_train) %in% c("Age", "RoomService", "FoodCourt", "ShoppingMall", 
                                                                                                                 "Spa", "VRDeck", "TotCost", "GroupId", "GroupSize", 
                                                                                                                 "CabinNumber", "CryoSleep"))]
rows_with_known_age <- !is.na(my_df_train$Age)
known_ages <- my_df_train[rows_with_known_age, names(unknown_ages)]

numeric_features <- my_df_train[rows_with_known_age, ]
known_age_list <- numeric_features$Age

# Predict missing ages using KNN
knn_predictions <- knn.reg(train = known_ages, test = unknown_ages, y = known_age_list, k = 2)

# Age: Fill nulls values with predicted 
my_df_train$Age[rows_with_missing_age] <- knn_predictions$pred


# Substitute outilers with 80th percentile - Upper limit to reduce variability
boxplot(my_df_train$Age)

q80 <- quantile(my_df_train$Age, 0.80)

# Create a boxplot to identify outliers
boxplot(my_df_train$Age, main="Boxplot of Age")

# Identify and replace outliers
outliers <- which(my_df_train$Age > 1.5 * IQR(my_df_train$Age, na.rm = TRUE) + q80)
my_df_train$Age[outliers] <- q80


# Normalize Age after filing the nulls
my_df_train$Age_norm <- min_max(my_df_train$Age)


# Correlation
cor_matrix<- cor(my_df_train[,c("Age_norm", "CryoSleep", "VIP", "Transported", "TotCost_norm",
                                "GroupId_norm", "GroupSize_norm", "CabinNumber_norm", "Side",
                                "Europa", "Earth", "Trappist", "PSO",
                                "DeckA", "DeckB", "DeckC", "DeckD", "DeckE", "DeckF",
                                "DeckG")])
corrplot(cor_matrix, method="circle", tl.col="black", tl.srt = 45)

## DECISION TREE to predict CryoSleep
# CryoSleep is not null
cryo_not_null_dataset <- subset(my_df_train, !is.na(CryoSleep))

# CryoSleep is null
cryo_null_dataset <- subset(my_df_train, is.na(CryoSleep))

my_tree_cryo <- rpart(CryoSleep ~ Age_norm+VIP+Transported+TotCost_norm+GroupId_norm
                      +GroupSize_norm+CabinNumber_norm+Side+Europa+Earth+Trappist+PSO
                      +DeckA+DeckB+DeckC+DeckD+DeckE+DeckF+DeckG, 
                      data = cryo_not_null_dataset, method="class", cp=0.005) # add "Method = Class" cause it's a Classification
#PRINT the tree
rpart.plot(my_tree_cryo, type=1, extra=1)

# Predict Missing Cryo
my_prediction_tree <- predict(my_tree_cryo, cryo_null_dataset, type="prob")

my_prediction_tree_df <- as.data.frame(my_prediction_tree)

cryo_null_dataset$CryoSleep <- ifelse(my_prediction_tree_df$`0` < 0.5, 0, 1)

my_df_train <- rbind(cryo_not_null_dataset,cryo_null_dataset)

colSums(is.na(my_df_train))



####### TEST DATASET
colSums(my_df == "")
colSums(is.na(my_df))

# Convert blank to NA
my_df_test[my_df_test == ""] <- NA
colSums(is.na(my_df_test))

# Cost: create column Total Cost
my_df_test$TotCost <- rowSums(my_df_test[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = TRUE)
sum(is.na(my_df_test$TotCost))

# Substitute outliers with 80th percentile
boxplot(my_df_test$TotCost)

q80 <- quantile(my_df_test$TotCost, 0.80)

# Create a boxplot to identify outliers
boxplot(my_df_test$TotCost, main="Boxplot of TotCost")

# Identify and replace outliers
outliers <- which(my_df_test$TotCost > 1.5 * IQR(my_df_test$TotCost, na.rm = TRUE) + q80)
my_df_test$TotCost[outliers] <- q80


# Create Column Group Id
my_df_test[c('GroupId', 'GroupSize')] <- str_split_fixed(my_df_test$PassengerId, '_', 2)

colSums(my_df_test == "")
colSums(is.na(my_df_test))

# Split Cabin column
split_columns <- strsplit(my_df_test$Cabin, "/")
my_df_test$Deck <- sapply(split_columns, function(x) x[1])
my_df_test$CabinNumber <- sapply(split_columns, function(x) x[2])
my_df_test$Side <- sapply(split_columns, function(x) x[3])

# Create Column First Name and Last name
my_df_test[c('FirstName', 'LastName')] <- str_split_fixed(my_df_test$Name, ' ', 2)

# Sort by Last Name
my_df_test <- arrange(my_df_test, desc(LastName))


# Destination -- filling for same GroupId
for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$Destination[i] <- ifelse(is.na(my_df_test$Destination[i]) & 
                                        my_df_test$GroupId[i] == my_df_test$GroupId[i + 1], 
                                      my_df_test$Destination[i + 1],
                                      ifelse(is.na(my_df_test$Destination[i]) & 
                                               my_df_test$GroupId[i] == my_df_test$GroupId[i - 1], 
                                             my_df_test$Destination[i - 1], my_df_test$Destination[i]))
}

# Destination -- filling for same LastName
for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$Destination[i] <- ifelse(is.na(my_df_test$Destination[i]) & 
                                        my_df_test$LastName[i] == my_df_test$LastName[i + 1], 
                                      my_df_test$Destination[i + 1],
                                      ifelse(is.na(my_df_test$Destination[i]) & 
                                               my_df_test$LastName[i] == my_df_test$LastName[i - 1], 
                                             my_df_test$Destination[i - 1], my_df_test$Destination[i]))
}


# Home Planets -- filling for same Last Name
for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$HomePlanet[i] <- ifelse(is.na(my_df_test$HomePlanet[i]) & 
                                       my_df_test$LastName[i] == my_df_test$LastName[i + 1], 
                                     my_df_test$HomePlanet[i + 1],
                                     ifelse(is.na(my_df_test$HomePlanet[i]) & 
                                              my_df_test$LastName[i] == my_df_test$LastName[i - 1], 
                                            my_df_test$HomePlanet[i - 1], my_df_test$HomePlanet[i]))
}


## Remaining Home Planet -- fill with corresponding Deck
my_df_test$HomePlanet <- ifelse(is.na(my_df_test$HomePlanet) & my_df_test$Deck %in% c('A', 'B', 'C', 'T'), 'Europa', my_df_test$HomePlanet)
my_df_test$HomePlanet <- ifelse(is.na(my_df_test$HomePlanet) & my_df_test$Deck == 'G', 'Earth', my_df_test$HomePlanet)


# Home planet remaining (only 5) -- replace with MODE
non_null_home<- my_df_test$HomePlanet[!is.na(my_df_test$HomePlanet)]
mode_home <- names(sort(table(non_null_home), decreasing = TRUE))[1]
my_df_test$HomePlanet[is.na(my_df_test$HomePlanet)] <- mode_home

# Destination remaining (only 13) -- replace with MODE
non_null_destination<- my_df_test$Destination[!is.na(my_df_test$Destination)]
mode_destination <- names(sort(table(non_null_destination), decreasing = TRUE))[1]
my_df_test$Destination[is.na(my_df_test$Destination)] <- mode_destination


# Fill the nulls in Side with the values of people with the same Last Name
for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$Side[i] <- ifelse(my_df_test$LastName[i] == my_df_test$LastName[i + 1] &
                                 is.na(my_df_test$Side[i]), my_df_test$Side[i + 1], 
                               ifelse(my_df_test$LastName[i] == my_df_test$LastName[i - 1] &
                                        is.na(my_df_test$Side[i]), my_df_test$Side[i - 1], 
                                      my_df_test$Side[i]))
}
# Side remaining (only 10) -- replace with MODE
non_null_side<- my_df_test$Side[!is.na(my_df_test$Side)]
mode_side <- names(sort(table(non_null_side), decreasing = TRUE))[1]
my_df_test$Side[is.na(my_df_test$Side)] <- mode_side



# Cabin Nulls: If they have same last name, same deck, same side --> same Cabin (if group size <=4)
my_df_test$LastName_Deck_Side <- paste0(my_df_test$LastName, "_", my_df_test$Deck, "_", my_df_test$Side)

my_df_test <- arrange(my_df_test, desc(LastName_Deck_Side))

for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$CabinNumber[i] <- ifelse(my_df_test$LastName_Deck_Side[i] == my_df_test$LastName_Deck_Side[i + 1] &
                                        is.na(my_df_test$CabinNumber[i]), my_df_test$CabinNumber[i + 1], 
                                      ifelse(my_df_test$LastName_Deck_Side[i] == my_df_test$LastName_Deck_Side[i - 1] &
                                               is.na(my_df_test$CabinNumber[i]), my_df_test$CabinNumber[i - 1], 
                                             my_df_test$CabinNumber[i]))
}

my_df_test$CabinNumber <- as.numeric(my_df_test$CabinNumber)
# Cabin Nulls: filling with same group id
my_df_test <- arrange(my_df_test, desc(GroupId))

for (i in 1:(nrow(my_df_test) - 1)) {
  my_df_test$CabinNumber[i] <- ifelse(my_df_test$GroupId[i] == my_df_test$GroupId[i + 1] &
                                        is.na(my_df_test$CabinNumber[i]), my_df_test$CabinNumber[i + 1], 
                                      ifelse(my_df_test$GroupId[i] == my_df_test$GroupId[i - 1] &
                                               is.na(my_df_test$CabinNumber[i]), my_df_test$CabinNumber[i - 1], 
                                             my_df_test$CabinNumber[i]))
}

## Cabin nulls rimaning (96) -- fill with MODE
non_null_cabinNum<- my_df_test$CabinNumber[!is.na(my_df_test$CabinNumber)]
mode_cabinNum <- names(sort(table(non_null_cabinNum), decreasing = TRUE))[1]
my_df_test$CabinNumber[is.na(my_df_test$CabinNumber)] <- mode_cabinNum


# Deck Nulls -- give the same distribution that not-nulls have
# distribution of non-null values
deck_distribution <- my_df_test %>%
  filter(!is.na(Deck)) %>%
  group_by(Deck) %>%
  summarise(count = n())

# impute missing values based on the distribution
my_df_test <- my_df_test %>%
  group_by(LastName) %>%
  mutate(Deck = ifelse(is.na(Deck), sample(deck_distribution$Deck, sum(is.na(Deck)), 
                                           replace = TRUE, 
                                           prob = deck_distribution$count / sum(deck_distribution$count)), Deck))


# DUMMIES
# For the side P = 1 and S = 0
my_df_test$Side <- ifelse(my_df_test$Side == "P", 1, 0)

my_df_test$CryoSleep <- ifelse(my_df_test$CryoSleep == "True", 1, 0)

## Filling CryoSleep nulls --> if they spent money they are not in cryosleep
my_df_test$CryoSleep <- ifelse(is.na(my_df_test$CryoSleep) & my_df_test$TotCost > 0, 0, my_df_test$CryoSleep)

my_df_test$Europa <- ifelse(my_df_test$HomePlanet == "Europa", 1, 0)
my_df_test$Earth <- ifelse(my_df_test$HomePlanet == "Earth", 1, 0)
#my_df_test$Mars <- ifelse(my_df_test$HomePlanet == "Mars", 1, 0)

my_df_test$Trappist <- ifelse(my_df_test$Destination == "TRAPPIST-1e", 1, 0)
my_df_test$PSO <- ifelse(my_df_test$Destination == "PSO J318.5-22", 1, 0)
#my_df_test$Cancri <- ifelse(my_df_test$Destination == "55 Cancri e", 1, 0)

unique(my_df_test$Deck)
my_df_test$DeckA <- ifelse(my_df_test$Deck == "A", 1, 0)
my_df_test$DeckB <- ifelse(my_df_test$Deck == "B", 1, 0)
my_df_test$DeckC <- ifelse(my_df_test$Deck == "C", 1, 0)
my_df_test$DeckD <- ifelse(my_df_test$Deck == "D", 1, 0)
my_df_test$DeckE <- ifelse(my_df_test$Deck == "E", 1, 0)
my_df_test$DeckF <- ifelse(my_df_test$Deck == "F", 1, 0)
my_df_test$DeckG <- ifelse(my_df_test$Deck == "G", 1, 0)
#my_df_test$DeckT <- ifelse(my_df_test$Deck == "T", 1, 0)


# VIP
my_df_test$VIP <- ifelse(my_df_test$VIP == "True", 1, 0)
my_df_test$VIP <- ifelse(is.na(my_df_test$VIP), 0, my_df_test$VIP) # fill the nulls with 0

colSums(is.na(my_df_test))
summary(my_df_test)
my_df_test$CabinNumber <- as.numeric(my_df_test$CabinNumber)
my_df_test$GroupSize <- as.numeric(my_df_test$GroupSize)
my_df_test$GroupId <- as.numeric(my_df_test$GroupId)


# Nulls COsts
mode_value_roomService <- names(sort(table(my_df_test$RoomService), decreasing = TRUE))[1]
my_df_test$RoomService <- ifelse(is.na(my_df_test$RoomService), mode_value_roomService ,my_df_test$RoomService)

mode_value_FoodCourt <- names(sort(table(my_df_test$FoodCourt), decreasing = TRUE))[1]
my_df_test$FoodCourt <- ifelse(is.na(my_df_test$FoodCourt), mode_value_FoodCourt ,my_df_test$FoodCourt)

mode_value_ShoppingMall <- names(sort(table(my_df_test$ShoppingMall), decreasing = TRUE))[1]
my_df_test$ShoppingMall <- ifelse(is.na(my_df_test$ShoppingMall), mode_value_ShoppingMall ,my_df_test$ShoppingMall)

mode_value_Spa <- names(sort(table(my_df_test$Spa), decreasing = TRUE))[1]
my_df_test$Spa <- ifelse(is.na(my_df_test$Spa), mode_value_Spa ,my_df_test$Spa)

mode_value_VRDeck <- names(sort(table(my_df_test$VRDeck), decreasing = TRUE))[1]
my_df_test$VRDeck <- ifelse(is.na(my_df_test$VRDeck), mode_value_VRDeck ,my_df_test$VRDeck)

# in numeric
my_df_test$RoomService <- as.numeric(my_df_test$RoomService)
my_df_test$FoodCourt <- as.numeric(my_df_test$FoodCourt)
my_df_test$ShoppingMall <- as.numeric(my_df_test$ShoppingMall)
my_df_test$Spa <- as.numeric(my_df_test$Spa)
my_df_test$VRDeck <- as.numeric(my_df_test$VRDeck)



# Normalizing
my_df_test$TotCost_norm <- min_max(my_df_test$TotCost)
my_df_test$GroupId_norm <- min_max(my_df_test$GroupId)
my_df_test$GroupSize_norm <- min_max(my_df_test$GroupSize)
my_df_test$CabinNumber_norm <- min_max(my_df_test$CabinNumber)
my_df_test$RoomService_norm <- min_max(my_df_test$RoomService)
my_df_test$FoodCourt_norm <- min_max(my_df_test$FoodCourt)
my_df_test$ShoppingMall_norm <- min_max(my_df_test$ShoppingMall)
my_df_test$Spa_norm <- min_max(my_df_test$Spa)
my_df_test$VRDeck_norm <- min_max(my_df_test$VRDeck)


# Predict the Age with KKN
rows_with_missing_age <- which(is.na(my_df_test$Age))
unknown_ages <- my_df_test[rows_with_missing_age, sapply(my_df_test, is.numeric) & !(names(my_df_test) %in% c("Age", "RoomService", "FoodCourt", "ShoppingMall", 
                                                                                                              "Spa", "VRDeck", "TotCost", "GroupId", "GroupSize", 
                                                                                                              "CabinNumber", "CryoSleep"))]
rows_with_known_age <- !is.na(my_df_test$Age)
known_ages <- my_df_test[rows_with_known_age, names(unknown_ages)]

numeric_features <- my_df_test[rows_with_known_age, ]
known_age_list <- numeric_features$Age

# Predict missing ages using KNN
knn_predictions <- knn.reg(train = known_ages, test = unknown_ages, y = known_age_list, k = 2)

# Age: Fill nulls values with predicted 
my_df_test$Age[rows_with_missing_age] <- knn_predictions$pred


# Substitute outilers with 80th percentile
boxplot(my_df_test$Age)

q80 <- quantile(my_df_test$Age, 0.80)

# Create a boxplot to identify outliers
boxplot(my_df_test$Age, main="Boxplot of Age")

# Identify and replace outliers
outliers <- which(my_df_test$Age > 1.5 * IQR(my_df_test$Age, na.rm = TRUE) + q80)
my_df_test$Age[outliers] <- q80



# Normalize Age after filing the nulls
my_df_test$Age_norm <- min_max(my_df_test$Age)


# Correlation
cor_matrix<- cor(my_df_test[,c("Age_norm", "CryoSleep", "VIP", "TotCost_norm",
                               "GroupId_norm", "GroupSize_norm", "CabinNumber_norm", "Side",
                               "Europa", "Earth", "Trappist", "PSO",
                               "DeckA", "DeckB", "DeckC", "DeckD", "DeckE", "DeckF",
                               "DeckG")])
corrplot(cor_matrix, method="circle", tl.col="black", tl.srt = 45)

## DECISION TREE to predict CryoSleep 
# CryoSleep is not null
cryo_not_null_dataset <- subset(my_df_test, !is.na(CryoSleep))
colSums(is.na(cryo_not_null_dataset))
# CryoSleep is null
cryo_null_dataset <- subset(my_df_test, is.na(CryoSleep))

my_tree_cryo <- rpart(CryoSleep ~ Age_norm+VIP+TotCost_norm+GroupId_norm
                      +GroupSize_norm+CabinNumber_norm+Side+Europa+Earth+Trappist+PSO
                      +DeckA+DeckB+DeckC+DeckD+DeckE+DeckF+DeckG, 
                      data = cryo_not_null_dataset, method="class", cp=0.005) # add "Method = Class" because it's a Classification
#PRINT the tree
rpart.plot(my_tree_cryo, type=1, extra=1)

# Predict Missing Cryo
my_prediction_tree <- predict(my_tree_cryo, cryo_null_dataset, type="prob")

my_prediction_tree_df <- as.data.frame(my_prediction_tree)

cryo_null_dataset$CryoSleep <- ifelse(my_prediction_tree_df$`0` < 0.5, 0, 1)

my_df_test <- rbind(cryo_not_null_dataset,cryo_null_dataset)

colSums(is.na(my_df_test))


# Correlation_norm - TEST
cor_matrix_norm<- cor(my_df_test[,c("Age_norm", "CryoSleep", "VIP", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck",
                               "TotCost_norm", "GroupId_norm", "GroupSize_norm", "CabinNumber_norm", "Side",
                               "Europa", "Earth", "Trappist", "PSO",
                               "DeckA", "DeckB", "DeckC", "DeckD", "DeckE", "DeckF",
                               "DeckG")])
corrplot(cor_matrix_norm, method="circle", tl.col="black", tl.srt = 45)

# Correlation - TEST
cor_matrix <- cor(my_df_test[,c("Age", "CryoSleep", "VIP", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck",
                                "TotCost", "GroupId", "GroupSize", "CabinNumber", "Side",
                                "Europa", "Earth", "Trappist", "PSO",
                                "DeckA", "DeckB", "DeckC", "DeckD", "DeckE", "DeckF",
                                "DeckG")])
corrplot(cor_matrix, method="circle", tl.col="black", tl.srt = 45)

# Correlation - TRAIN
#cor_matrix <- cor(my_df_test[,c("Transported", "Age", "CryoSleep", "VIP", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck",
 #                               "TotCost", "GroupId", "GroupSize", "CabinNumber", "Side",
#                                "Europa", "Earth", "Trappist", "PSO",
 #                               "DeckA", "DeckB", "DeckC", "DeckD", "DeckE", "DeckF",
  #                              "DeckG")])
#corrplot(cor_matrix, method="circle", tl.col="black", tl.srt = 45)



# Defining Dataset for modeling - TRAIN
names(my_df_train)
numeric_columns <- sapply(my_df_train, is.numeric) # list of numeric columns
my_df_train_model <- my_df_train[, numeric_columns] # filter for numeric
my_df_train_model <- subset(my_df_train_model, select = -c(TotCost_norm, TotCost)) # exclude tot costs 
names(my_df_train_model)
# dataset with normalized data
my_df_train_model_norm <- subset(my_df_train_model, select = -c(Age, GroupId, GroupSize, CabinNumber, RoomService,
                                                                FoodCourt, ShoppingMall, Spa, VRDeck))
# dataset with non normalized data
my_df_train_model_unnorm <- subset(my_df_train_model, select = -c(Age_norm, GroupId_norm, GroupSize_norm, CabinNumber_norm,
                                                                  RoomService_norm, FoodCourt_norm, ShoppingMall_norm, Spa_norm, VRDeck_norm))

# Defining Dataset for modeling - TEST
names(my_df_test)
numeric_columns <- sapply(my_df_test, is.numeric) # list of numeric columns
my_df_test_model <- my_df_test[, numeric_columns] # filter for numeric
my_df_test_model <- subset(my_df_test_model, select = -c(TotCost_norm, TotCost)) # exclude tot costs 
names(my_df_test_model)
# dataset with normalized data
my_df_test_model_norm <- subset(my_df_test_model, select = -c(Age, GroupId, GroupSize, CabinNumber, RoomService,
                                                                FoodCourt, ShoppingMall, Spa, VRDeck))
# dataset with non normalized data
my_df_test_model_unnorm <- subset(my_df_test_model, select = -c(Age_norm, GroupId_norm, GroupSize_norm, CabinNumber_norm,
                                                                  RoomService_norm, FoodCourt_norm, ShoppingMall_norm, Spa_norm, VRDeck_norm))



# Correlation
cor_matrix_train <- cor(my_df_train_model_norm[,],
                        use = "pairwise.complete.obs")
corrplot(cor_matrix_train, method="circle", tl.col="black", tl.srt = 45)
 


##### RANDOM FOREST -- is the most performing one. We used this for the final prediction

rfModel <- randomForest(Transported ~ . -VIP -DeckA, 
                        data = my_df_train_model_unnorm, 
                        mtry = 3, ntree = 200, nodesize = 1)
# Variable importance
print(importance(rfModel))

# Prediction
my_rf_prediction <- predict(rfModel, my_df_test_model_unnorm)
my_rf_prediction_df <- as.data.frame(my_rf_prediction)

my_prediction <- ifelse(my_rf_prediction_df$my_rf_prediction > 0.5, 'True', 'False')


######### Submission CSV #########
sampleSubmission <- data.frame(PassengerId = my_df_test$PassengerId, Transported = my_prediction)

colSums(is.na(sampleSubmission))

write.csv(sampleSubmission, "~/Desktop/Business Challenge II MBAN DD/Submission_Final.csv", row.names = FALSE)





############ Below there are Different Models we tried --> SKIP IF NOT INTERESTED ##############


###################### Models #############################

#### LOGISTIC REGRESSION with normalized train ####
my_logit_norm <- glm(Transported ~ ., data = my_df_train_model_norm, family = "binomial")
summary(my_logit_norm)

# Modify with only statistically significant variables
my_logit_norm <- glm(Transported ~ CryoSleep+Side+Europa+Earth+Trappist+PSO+DeckC+RoomService_norm+FoodCourt_norm
                       +ShoppingMall_norm+Spa_norm+VRDeck_norm+Age_norm, 
                data = my_df_train_model_norm, family = "binomial")
summary(my_logit_norm)

# Prediction
my_prediction <- predict(my_logit_norm, my_df_test_model_norm, type="response") #probability for every client to bring business success
my_prediction <- ifelse(my_prediction > 0.5, "True", "False")

# Evaluate the accuracy
predictions <- predict(my_logit_norm, newdata = my_df_train_model_norm, type = "response")
accuracy <- mean((predictions >= 0.5 & my_df_train_model_norm$Transported == 1) | (predictions < 0.5 & my_df_train_model_norm$Transported == 0))
print(accuracy)



#### LOGISTIC REGRESSION train  ####
my_logit_unnorm <- glm(Transported ~ ., data = my_df_train_model_unnorm, family = "binomial")
summary(my_logit_unnorm)

# Modify with only statistically significant variables
my_logit_unnorm <- glm(Transported ~ CryoSleep+Age+TotCost+Side+Europa+Earth+Trappist+PSO, 
                     data = my_df_train_model_unnorm, family = "binomial")
summary(my_logit_unnorm)

# Logistic Reg with NOT normalized train
# Prediction
my_prediction <- predict(my_logit_unnorm, my_df_test_model_unnorm, type="response") #probability for every client to bring business success
my_prediction_Binary <- ifelse(my_prediction > 0.5, "True", "False")

# Evaluate the accuracy
predictions_forcheck <- predict(my_logit_unnorm, newdata = my_df_train_model_unnorm, type = "response")
accuracy <- mean((predictions_forcheck >= 0.5 & my_df_train_model_unnorm$Transported == 1) | (predictions_forcheck < 0.5 & my_df_train_model_norm$Transported == 0))
print(accuracy)



##### Decision Tree with normalized train #####   0.75263 Accuracy
my_tree_norm <- rpart(Transported ~ . -GroupId_norm -DeckB - DeckC - DeckG, 
                      data = my_df_train_model_norm, method="class", cp=0.003) # add "Method = Class" because it's a Classification
#PRINT the tree
rpart.plot(my_tree_norm, type=1, extra=1)

# Predict
my_prediction_tree_norm <- predict(my_tree_norm, my_df_test_model_norm, type="prob")

my_prediction <- ifelse(my_prediction_tree_norm < 0.5, "False", "True")

my_prediction_tree_norm_df <- as.data.frame(my_prediction)
# Drop the column called 0
my_prediction <- subset(my_prediction_tree_norm_df, select = -1)


##### Decision Tree train ##### 
my_tree <- rpart(Transported ~ ., 
                      data = my_df_train_model_unnorm, method="class", cp=0.003) # add "Method = Class" because it's a Classification
#PRINT the tree
rpart.plot(my_tree, type=1, extra=1)

# Predict
my_prediction_tree <- predict(my_tree, my_df_test_model_unnorm, type="prob")

my_prediction <- ifelse(my_prediction_tree < 0.5, "False", "True")

my_prediction_tree_df <- as.data.frame(my_prediction)

# Drop the column called 0
my_prediction <- subset(my_prediction_tree_df, select = -1)


## DECISION TREE MULTIPLE CPs normalized 
# Fit a decision tree with caret
set.seed(1234)
fit <- train(as.factor(Transported) ~ ., 
             data = my_df_train_model_unnorm, #data in
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1, 0.0005)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
fit
plot(fit)
# Plot tree
prp(fit$finalModel, extra = 1)

# Prediction
tree_prediction <- predict(fit, my_df_test_model_unnorm)
tree_prediction_df <- as.data.frame(tree_prediction)
tree_prediction_df$tree_prediction <- as.numeric(tree_prediction_df$tree_prediction) # convert in number
tree_prediction_df$tree_prediction <- tree_prediction_df$tree_prediction - 1
my_prediction_tr <- ifelse(tree_prediction_df$tree_prediction < 0.5, "False", "True")
my_prediction_tr <- data.frame(my_prediction_tr)


##### RANDOM FOREST normalization ######
rfModel_norm <- randomForest(Transported ~ . -VIP -PSO -DeckA -DeckD, 
                             data = my_df_train_model_norm, 
                             mtry = 3, ntree = 200, nodesize = 1)
# Variable importance
print(importance(rfModel_norm))
# Prediction
my_rf_prediction <- predict(rfModel_norm, my_df_test_model_norm)

my_rf_prediction_df <- as.data.frame(my_rf_prediction)

my_rf_prediction_df$my_rf_prediction <- as.numeric(my_rf_prediction_df$my_rf_prediction) - 1

my_prediction <- ifelse(my_rf_prediction_df$my_rf_prediction > 0.5, 'True', 'False')



# RANDOM Forest new Try
ctrl <- trainControl(method = "cv", number = 5)

# Fit the random forest model with cross-validation
rfModel <- train(Transported ~ . -VIP -PSO -DeckA -DeckD -GroupId, 
                 data = my_df_train_model_unnorm, 
                 method = "rf",
                 trControl = ctrl,
                 mtry = 3, ntree = 200, nodesize = 1)
# Variable importance
print(importance(rfModel))
# Prediction
my_rf_prediction <- predict(rfModel, my_df_test_model_unnorm)
my_rf_prediction_df <- as.data.frame(my_rf_prediction)

my_prediction <- ifelse(my_rf_prediction_df$my_rf_prediction > 0.5, 'True', 'False')


# Evaluate the accuracy
predictions_forcheck <- predict(my_tree, newdata = my_df_train_model_norm, type = "prob")
accuracy <- mean((predictions_forcheck >= 0.5 & my_df_train_model_unnorm$Transported == 1) | (predictions_forcheck < 0.5 & my_df_train_model_norm$Transported == 0))
print(accuracy)



#### Improve XGboost
# Define parameter grid for hyperparameter tuning
param_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,  # Example value for gamma
  colsample_bytree = 1,  # Example value for colsample_bytree
  min_child_weight = 1,  # Example value for min_child_weight
  subsample = 1  # Example value for subsample
)

# Check if 'Transported' is a factor and convert it to a factor if necessary
if (!is.factor(my_df_train_model_unnorm$Transported)) {
  Y_train <- as.factor(my_df_train_model_unnorm$Transported)
} else {
  Y_train <- my_df_train_model_unnorm$Transported
}

# Exclude the 'Transported' column from your dataset
X_train <- my_df_train_model_unnorm[, !names(my_df_train_model_unnorm) %in% "Transported"]

# Perform grid search with cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
model <- train(
  x = X_train,
  y = Y_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid,
  # Set iteration_range instead of ntree_limit
  iteration_range = c(1, max(param_grid$nrounds))
)

# Print the best model and its performance
print(model)

# Make predictions using the trained model
predictions <- predict(model, newdata = my_df_test_model_unnorm)
prediction_df <- as.data.frame(predictions)
prediction_df$predictions <- as.numeric(prediction_df$predictions) - 1
my_prediction_xg <- ifelse(prediction_df$predictions > 0.5, 'True', 'False')
my_prediction_xg <- data.frame(my_prediction_xg)

### FINAL PREDICTION
#my_prediction_final <- cbind(my_prediction_tr, my_prediction_tr, my_prediction_xg)
                                               
# Count the number of "True" values in each row
#num_true <- rowSums(my_prediction_final == "True")

# Create a logical vector indicating whether there are at least 2 "True" values
#at_least_2_true <- num_true >= 2

# Convert the logical vector to "True" or "False"
#my_prediction_final$my_prediction <- ifelse(at_least_2_true, "True", "False")

#prediction_final <- my_prediction_final$my_prediction



## Gradient Boosting
# Define your model parameters
# For example, number of trees, learning rate, and depth of each tree
n_trees <- 200
learning_rate <- 0.05
max_depth <- 5

# Train the Gradient Boosting model
model <- gbm(
  formula = Transported ~ .,  # Define your formula
  data = my_df_train_model_unnorm,  # Your training data
  distribution = "bernoulli",  # Bernoulli distribution for binary classification
  n.trees = n_trees,  # Number of trees
  shrinkage = learning_rate,  # Learning rate
  interaction.depth = max_depth,  # Maximum depth of each tree
  verbose = TRUE  # Output progress information
)

# Make predictions on the test data
predictions <- predict(model, my_df_test_model_unnorm, n.trees = n_trees, type = "response")

# If you have binary classification, you might want to convert predictions to classes
# For example, if predictions > 0.5, classify as 1, otherwise 0
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

predicted_df <- data.frame(Transported = predicted_classes)

my_prediction <- ifelse(predicted_df$Transported > 0.5, 'True', 'False')
my_prediction <- data.frame(Transported = my_prediction)



## SVM - Normalized dataset
install.packages('e1071') 
library(e1071) 
library(caret)
library(e1071)

# Define the training control with cross-validation
ctrl <- trainControl(method = "cv",   # Cross-validation method
                     number = 5,      # Number of folds
                     verboseIter = TRUE)  # Print verbose output

# Train the SVM model using cross-validation
svm_poly <- train(Transported ~ .- Europa - CabinNumber_norm, 
                  data = my_df_train_model_norm,
                  method = "svmPoly",   # Polynomial Kernel
                  trControl = ctrl, 
                  preProcess = c("center", "scale"))  # Preprocessing steps

# Predicting the Test set results 
y_pred <- predict(svm_poly, newdata = my_df_test_model_norm) 
my_prediction <- as.numeric(y_pred) - 1
my_prediction <- data.frame(Transported = my_prediction)
my_prediction$Transported <- ifelse(my_prediction$Transported > 0.5, 'True', 'False')




# End
