#' Author: Gabriel Chouraqui
#' Jan, 2024
#' Purpose: Bedding Bathing & Yonder (BBY)

# Libraries
library(rpart)
library(caret)
library(plyr)
library(dplyr)
library(MLmetrics)
library(vtreat)
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(ModelMetrics)
library(rpart.plot)
library(randomForest)

# WD
setwd("~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/Personal Files")

# Data
allTrainingFiles <- list.files(path = '~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A2_Household_Spend/studentTables',
                               pattern = 'training',
                               full.names = T)

allTestingFiles <- list.files(path = '~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A2_Household_Spend/studentTables',
                              pattern = 'testing',
                              full.names = T)

allProspectFiles <- list.files(path = '~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A2_Household_Spend/studentTables',
                               pattern = 'prospect',
                               full.names = T)

# Load the files and arrange them with a left join
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by='tmpID', type='left')

allTestingDF <- lapply(allTestingFiles, read.csv)
allTestingDF <- join_all(allTestingDF, by='tmpID', type='left')

allProspectDF <- lapply(allProspectFiles, read.csv)
allProspectDF <- join_all(allProspectDF, by='tmpID', type='left')

####################################################
# SAMPLE
####################################################

# Train 80% / Validation 20% Partitioning
splitPercent <- round(nrow(allTrainingDF) * 0.8)

# Sample index for training
trainIdx <- sample(1:nrow(allTrainingDF), splitPercent)

# Identify the rows not in the training set
remainingRows <- setdiff(1:nrow(allTrainingDF), trainIdx)

# Create another sample 
validationIdx <- sample(remainingRows, nrow(allTrainingDF) - splitPercent)

# Create training and validation sets
trainSet <- allTrainingDF[trainIdx, ]
validationSet <- allTrainingDF[validationIdx, ]

# Check
dim(trainSet)
dim(validationSet)

####################################################
# EXPLORE
####################################################

# Display the structure of the data
str(allTestingDF)

# Explore missing values
plot_missing(allTestingDF)
head(allTestingDF)
colnames(allTestingDF)

# Sort prospects by descending yHat
sortedProspects <- allTestingDF %>%
  arrange(desc(yHat))

# Calculate cumulative sum of predicted spending
sortedProspects$cumulative_spending <- cumsum(sortedProspects$yHat)

# Find the cutoff point where 80% of the revenue is reached
revenue_cutoff <- quantile(sortedProspects$cumulative_spending, 0.8)

# Select the customers who contribute to 80% of the revenue
bestCustomers <- sortedProspects %>%
  filter(cumulative_spending <= revenue_cutoff)

# Compute mean yHat for each property type
property_means <- bestCustomers %>%
  group_by(PropertyType) %>%
  summarise(mean_yHat = mean(yHat, na.rm = TRUE)) %>%
  arrange(desc(mean_yHat))

# Merge the property means with the original data frame
allTrainingDF_properties <- merge(bestCustomers, property_means, by = "PropertyType")

# Create the box plot
ggplot(allTrainingDF_properties, aes(x=reorder(PropertyType, -mean_yHat), y=yHat, fill=PropertyType)) +
  geom_boxplot() +
  labs(title="Distribution of Household Spend with BBY by Property Type",
       x="Property Type",
       y="Average Household Spend with BBY (yHat)") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  scale_fill_brewer(palette="Set2")

# Descriptive statistics
summary(allTestingDF)

# List of columns to drop
columns_to_drop <- c(
  "ResidenceHHGenderDescription",
  "EthnicDescription",
  "BroadEthnicGroupings",
  "PresenceOfChildrenCode",
  "HorseOwner",
  "CatOwner",
  "DogOwner",
  "OtherPetOwner",
  "BookBuyerInHome",
  "BuyerofAntiquesinHousehold",
  "BuyerofArtinHousehold",
  "GeneralCollectorinHousehold",
  "BooksAudioReadinginHousehold",
  "ComputerOwnerInHome",
  "ReligiousContributorInHome",
  "PoliticalContributerInHome",
  "DonatesEnvironmentCauseInHome",
  "DonatesToCharityInHome",
  "DonatestoAnimalWelfare",
  "DonatestoArtsandCulture",
  "DonatestoChildrensCauses",
  "DonatestoHealthcare",
  "DonatestoInternationalAidCauses",
  "DonatestoVeteransCauses",
  "DonatestoHealthcare1",
  "DonatestoInternationalAidCauses1",
  "DonatestoWildlifePreservation",
  "DonatestoLocalCommunity",
  "FirstName",
  "LastName",
  "TelephonesFullPhone",
  "lat",
  "lon",
  "county",
  "stateFips",
  "DwellingUnitSize",
  "storeVisitFrequency",
  "EstHomeValue",
  "FamilyMagazineInHome",
  "FemaleOrientedMagazineInHome",
  "ReligiousMagazineInHome",
  "GardeningMagazineInHome",
  "CulinaryInterestMagazineInHome",
  "HealthFitnessMagazineInHome",
  "DoItYourselfMagazineInHome",
  "FinancialMagazineInHome",
  "InterestinCurrentAffairsPoliticsInHousehold",
  "PartiesDescription",
  "ReligionsDescription",
  "LikelyUnionMember",
  "GunOwner",
  "Veteran",
  "supportsAffordableCareAct",
  "supportsGayMarriage",
  "supportsGunControl",
  "supportsTaxesRaise",
  "overallsocialviews",
  "DonatestoConservativeCauses",
  "DonatestoLiberalCauses"
)

# Drop the columns
allTestingDF <- allTestingDF %>%
  select(-one_of(columns_to_drop))

allTrainingDF <- allTrainingDF %>%
  select(-one_of(columns_to_drop))

allProspectDF <- allProspectDF %>%
  select(-one_of(columns_to_drop))

# Count missing values for each column
colSums(is.na(allTestingDF))

plot_density(allTestingDF$yHat) 
plot_histogram(allTestingDF)
plot_density(allTestingDF)
plot_missing(allTestingDF)


# Convert variables to double numeric
allTestingDF$ISPSA <- as.numeric(allTestingDF$ISPSA)
allTestingDF$MedianEducationYears <- as.numeric(allTestingDF$MedianEducationYears)

# Cleaning the NetWorth column
# Remove dollar signs and commas
allTestingDF$NetWorth <- gsub("[$,]", "", allTestingDF$NetWorth)

calculate_mode_categorical <- function(x) {

    x <- x[!is.na(x)]
  
  # Calculate the mode
  mode_result <- table(x)
  mode_values <- as.numeric(names(mode_result[mode_result == max(mode_result)]))
  
  # Return the first mode value
  return(mode_values[1])
}

# Specify columns to replace missing values
columns_to_replace_empty <- c("NetWorth", "ISPSA", "Age", "MedianEducationYears")

# Replace missing values with mode for each specified column
for (col_name in columns_to_replace_empty) {
  mode_value <- calculate_mode_categorical(allTestingDF[[col_name]])
  allTestingDF[[col_name]][is.na(allTestingDF[[col_name]])] <- mode_value
}

nrow(allTestingDF)

head(allTestingDF)

colnames(allTestingDF)

####################################################
# MODIFY
####################################################

# Identify all columns as informative features except 'yHat'
informativeFeatures <- setdiff(names(allTrainingDF), 'yHat')
targetName <- 'yHat'

# Create a design treatment plan for the training set
plan <- vtreat::designTreatmentsN(
  dframe = trainSet,
  varlist = informativeFeatures,
  outcomename = targetName
)


# Apply the plan to all sections of the data
treatedTrain <- vtreat::prepare(plan, trainSet)
treatedValidation <- vtreat::prepare(plan, validationSet)
treatedTest <- vtreat::prepare(plan, allTestingDF)
treatedProspects <- vtreat::prepare(plan, allProspectDF)


####################################################
# MODEL
####################################################

# Linear model
fit <- lm(yHat~., treatedTrain)
summary(fit)

# Get the variable and p-values
pVals <- data.frame(varNames = names(na.omit(coef(fit))),
                    pValues = summary(fit)$coefficients[,4])

# Determine which variable names to keep 
keeps <- subset(pVals$varNames, pVals$pValues<0.1)

# Remove unwanted columns
treatedTrainParsimony <- treatedTrain[,names(treatedTrain) %in% keeps]

# Append the y-variable
treatedTrainParsimony$yHat <- treatedTrain$yHat

# Refit a model
fit2 <- lm(yHat ~ ., treatedTrainParsimony)
summary(fit2)

trainingPreds <- predict(fit2, treatedTrainParsimony)

# Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$yHat,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$yHat-trainingPreds )
head(trainingResults)

####################################################
# CROSS VALIDATION (at least a try)
####################################################

# Set the seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
num_folds <- 5  

# Create a data frame containing your features and target variable
data_for_cv <- treatedTrain

# Specify the training control
ctrl <- trainControl(method = "cv", number = num_folds)

# Perform cross-validation
model <- train(yHat ~ ., data = data_for_cv, method = "lm", trControl = ctrl)

print(model)

####################################################
# ASSESS
####################################################

# Make predictions
linearTrainPredictions      <- predict(fit, treatedTrain)
linearValidationPredictions <- predict(fit, treatedValidation)

# Calculate RMSE for training set
trainRMSE <- MLmetrics::RMSE(linearTrainPredictions, treatedTrain$yHat)
cat("Training RMSE:", trainRMSE, "\n")

# Calculate RMSE for validation set
validationRMSE <- MLmetrics::RMSE(linearValidationPredictions, treatedValidation$yHat)
cat("Validation RMSE:", validationRMSE, "\n")

# Once you are done, get predictions for the test set
linearTestPredictions <- predict(fit, treatedTest) 

# Compare the training, validation, and test set RMSE.  Look for consistency.
# Print the RMSE for the test set
testRMSE <- MLmetrics::RMSE(linearTestPredictions, treatedTest$yHat)
cat("Test RMSE:", testRMSE, "\n")

# Using the best possible model make predictions on the prospect set.
prospectPredictions <- predict(fit, treatedProspects)

# Create a data frame with tmpID and predictions columns
predictions_df <- data.frame(tmpID = treatedProspects$tmpID, predictions = prospectPredictions)

# Save the results
write.csv(predictions_df, "prospect_predictions.csv", row.names = FALSE)

####################################################
# END
####################################################