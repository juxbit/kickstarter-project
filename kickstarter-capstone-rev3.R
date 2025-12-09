#title: "Kickstarter Project Capstone"
#author: "Rob Shipley"
#date: "2025-12-8"

#-------------------------------------------------------- 
# Load needed libraries
#-------------------------------------------------------- 

if(!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
library(ranger)


#-------------------------------------------------------- 
# Read Kickstarter dataset 
#-------------------------------------------------------- 
kickstarter_ds <- read_csv("kickstarter_projects.csv")

#-------------------------------------------------------- 
# Create a column with time between Launch and Deadline
# This is useful to determine how much time is allowed to raise funds
#-------------------------------------------------------- 
kickstarter_ds$DeadlineTime <- difftime(kickstarter_ds$Deadline, kickstarter_ds$Launched, units = "hours")
head(kickstarter_ds)

#-------------------------------------------------------- 
# Show unique States of all Kickstarter projects
# Remove all State except Successful and Failed.
# The other states of projects are not useful for determining if Successful or Failed
#-------------------------------------------------------- 
kickstarter_unique_state <- unique(kickstarter_ds$State)
print(kickstarter_unique_state)

kickstarter_ds <- filter(kickstarter_ds, State != "Canceled")
kickstarter_ds <- filter(kickstarter_ds, State != "Live")
kickstarter_ds <- filter(kickstarter_ds, State != "Suspended")
kickstarter_state <- unique(kickstarter_ds$State)
print(kickstarter_state)

#-------------------------------------------------------- 
# Run a summary and view the column Headers
# View the columns and determine context
#-------------------------------------------------------- 
summary(kickstarter_ds)
head(kickstarter_ds)

#-------------------------------------------------------- 
# Plot a bargraph of the count of Successful projects per Category
# Compares the number of successful vs. failed projects
#--------------------------------------------------------
ggplot(kickstarter_ds, aes(x = State)) +
  geom_bar(fill = "darkgray") +
  labs(title = "Number of Projects by State", x = "Project State", y = "Count") +
  theme_minimal()

#-------------------------------------------------------- 
# Evaluate unique values for each column, Country, Category and Subcategory
# Show what is unique for each of the categorical columns
#-------------------------------------------------------- 

kickstarter_unique_country <- unique(kickstarter_ds$Country)
print(kickstarter_unique_country)
kickstarter_unique_category <- unique(kickstarter_ds$Category)
print(kickstarter_unique_category)
kickstarter_unique_subcategory <- unique(kickstarter_ds$Subcategory)
print(kickstarter_unique_subcategory)

#-------------------------------------------------------- 
# Take a look at Failed projects 
#-------------------------------------------------------- 
failed_projects <- kickstarter_ds %>% filter(State == "Failed")
failed_projects$Year <- year(failed_projects$Launched)
# Count failed projects by Year and Category
failed_by_year_category <- failed_projects %>%
  group_by(Year, Category) %>%
  summarise(Count = n())
print(failed_by_year_category)

#-------------------------------------------------------- 
# Plot a stack bar of count of Failed projects per Year
#
# Plot themes assisted by Claude 4 
# (Sonnet 4.5) by Anthropic (2024)
# https://claude.ai
#--------------------------------------------------------

ggplot(failed_by_year_category, aes(x = Year, y = Count, fill = Category)) +
  geom_bar(stat = "identity" , color = "black") + 
  labs(
    title = "Failed Kickstarter Projects Per Year by Category",
    x = "Year",
    y = "Number of Failed Projects",
    fill = "Category"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Year lables at 45 degrees 
  scale_x_continuous(breaks = seq(min(failed_by_year_category$Year), 
                                  max(failed_by_year_category$Year), 
                                  by = 1))

#-------------------------------------------------------- 
# Take a look at Successful projects
#-------------------------------------------------------- 

successful_projects <- kickstarter_ds %>% filter(State == "Successful")
successful_projects$Year <- year(successful_projects$Launched)

successful_by_year_category <- successful_projects %>%
  group_by(Year, Category) %>%
  summarise(Count = n())
print(successful_by_year_category)


#-------------------------------------------------------- 
# Plot a stack bar of count of Successful projects per Year
#
# Plot themes assisted by Claude 4 
# (Sonnet 4.5) by Anthropic (2024)
# https://claude.ai
#--------------------------------------------------------

ggplot(successful_by_year_category, aes(x = Year, y = Count, fill = Category)) +
  geom_bar(stat = "identity" , color = "black") + # stack with black borders per category
  labs(
    title = "Successful Kickstarter Projects Per Year by Category",
    x = "Year",
    y = "Number of Successful Projects",
    fill = "Category" # Legend
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +# Rotate 45 degree labels for x-axis
  scale_x_continuous(breaks = seq(min(successful_by_year_category$Year), 
                                  max(successful_by_year_category$Year), 
                                  by = 1))

#-------------------------------------------------------- 
# Plot a bar graph of the count of Successful projects per Category
#--------------------------------------------------------

successful_by_category <- kickstarter_ds %>%
  filter(tolower(State) == "successful") %>%
  group_by(Category) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(successful_by_category, aes(x = reorder(Category, Count), y = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Category", y = "Successful Project Count") +
  theme_minimal()


#-------------------------------------------------------- 
# Evaluate the Success percentage per Category
#--------------------------------------------------------

success_rate_per_category <- kickstarter_ds %>%
  group_by(Category) %>%
  summarise(
    Total = n(),
    Successful = sum(tolower(State) == "successful"),
    Success_Percentage = (Successful / Total) * 100
  ) %>%
  arrange(desc(Success_Percentage))

ggplot(success_rate_per_category, aes(x = Success_Percentage, y = reorder(Category, Success_Percentage))) +
  geom_bar(stat = "identity") +
  labs(x = "Success %", y = "Category") +
  theme_minimal()

#-------------------------------------------------------- 
# Now evaluate the Success percentage per Country
#--------------------------------------------------------
success_rate_by_country <- kickstarter_ds %>%
  group_by(Country) %>%
  summarise(
    Total = n(),
    Successful = sum(tolower(State) == "successful"),
    Success_Percentage = (Successful / Total) * 100
  ) %>%
  arrange(desc(Success_Percentage))

#-------------------------------------------------------- 
# Plot a bar graph of the percent Successful projects per Country
#--------------------------------------------------------
ggplot(success_rate_by_country, aes(x = Success_Percentage, y = reorder(Country, Success_Percentage))) +
  geom_bar(stat = "identity") +
  labs(x = "Success %", y = "Country") +
  theme_minimal()

#-------------------------------------------------------- 
# Clean up some memory from successful and failed projects datasets
#--------------------------------------------------------
rm(failed_projects,successful_projects)

#-------------------------------------------------------- 
# Modify the Kickstarter dataset to a dataframe for preparing for applying models
# Drop columns that have no relevance to results.
#-------------------------------------------------------- 
kickstarter_df <- select(kickstarter_ds, Category, Country, Pledged, Backers,DeadlineTime, State)
head(kickstarter_df)


#-------------------------------------------------------- 
# To apply the data model the State needs to be changed to a binary 0 or 1 factor
# If you look at the columns it is changed to fct type
#-------------------------------------------------------- 

kickstarter_df$State <- factor(
  ifelse(tolower(kickstarter_df$State) == "successful", 1, 0),
  levels = c(0, 1),
  labels = c("Failed", "Successful")
)

#-------------------------------------------------------- 
# To apply the data models we need to change the character columns to factors
#-------------------------------------------------------- 

kickstarter_df$Category <- as.factor(kickstarter_df$Category)
kickstarter_df$Country <- as.factor(kickstarter_df$Country)

#-------------------------------------------------------- 
# Get rid of data that is NA within the Kickstarter dataframe
#-------------------------------------------------------- 

kickstarter_df <- na.omit(kickstarter_df)
summary(kickstarter_df)


#-------------------------------------------------------- 
# It is going to be an 80-20 split for train test sets since we have enough data
#-------------------------------------------------------- 

set.seed(3)  # Set this so split is repeatable.
train_index <- createDataPartition(kickstarter_df$State, p = 0.8, list = FALSE)
train_data <- kickstarter_df[train_index, ]
test_data <- kickstarter_df[-train_index, ]

#-------------------------------------------------------- 
# Evaluate another summary and column heads for the train set
#-------------------------------------------------------- 

summary(train_data)
head(train_data)

#-------------------------------------------------------- 
# Model 1: Random Forest Ranger Model
#-------------------------------------------------------- 
# For random forest ranger model to work, we need to define the controls and tuning
# Going to use cross-validation method, 10 fold, keep the probability estimates
# For tuning, mtry is generally sqrt of feature count, so 3 is good, split rule gini is default, 1 is minumum size
#-------------------------------------------------------- 

train_control <- trainControl(
  method = "cv",       # Using Cross-validation       
  number = 10,         # Use 10 fold to increase accuracy        
  classProbs = TRUE    # Get probability estimates      
)

tune_grid <- expand.grid(
  mtry = 3,            # Number of column features to use at a time
  splitrule = "gini",  # Default index for split
  min.node.size = 1    # This is the minimum columns possible for classification
)

#-------------------------------------------------------- 
# Run the model on train set using control and tuning parameters
#-------------------------------------------------------- 

ranger_model <- train(
  State ~ Category + Country + Pledged + Backers + DeadlineTime,
  data = train_data,
  method = "ranger",          # Using ranger random forest algorithm
  trControl = train_control,
  tuneGrid = tune_grid,
  num.trees = 100,            # 100 trees to start, can increase to improve accuracy but takes longer
  importance = "impurity"     # Determine the importance of each variable to decision tree
)


#-------------------------------------------------------- 
# Show the results for the model parameters
#-------------------------------------------------------- 
print(ranger_model)

#-------------------------------------------------------- 
# Show the accuracy for train set
#-------------------------------------------------------- 
cat("Accuracy:", ranger_model$results$Accuracy)

#-------------------------------------------------------- 
# ranger_predict will make a prediction on success failed projects in test set
#-------------------------------------------------------- 

ranger_predict <- predict(ranger_model, newdata = test_data)
summary(ranger_predict)

#-------------------------------------------------------- 
# Now apply the tuned ranger model to the test set
#-------------------------------------------------------- 

ranger_predict <- factor(ranger_predict, levels = levels(test_data$State))

#-------------------------------------------------------- 
# Run the confusion Matrix to determine if prediction is correct
#--------------------------------------------------------

ranger_confusionmatrix <- confusionMatrix(ranger_predict, test_data$State)
print(ranger_confusionmatrix)

#-------------------------------------------------------- 
# Show the accuracy result for the ranger model predictions
#--------------------------------------------------------

cat("Accuracy:", ranger_confusionmatrix$overall['Accuracy'])


#-------------------------------------------------------- 
# Model 2: Logistic Regression Model
#-------------------------------------------------------- 

#-------------------------------------------------------- 
# Run the logistic model on the train set
# We are going to suppress the warnings because we know the model will be highly successful with some Pledge amounts
#-------------------------------------------------------- 

logistic_model <- suppressWarnings(glm(State ~ ., data = train_data, family = binomial))

#-------------------------------------------------------- 
# Apply the logistic model to the test set
# The State is a 0 and 1s factor, predict based on near value
#
# factor assisted by Claude 4 
# (Sonnet 4.5) by Anthropic (2024)
# https://claude.ai
#-------------------------------------------------------- 
logistic_predict <- predict(logistic_model, test_data, type = "response")
logistic_predict <- factor(ifelse(logistic_predict > 0.5, "Successful", "Failed"),
                        levels = c("Failed", "Successful"))

summary(logistic_predict)

#-------------------------------------------------------- 
# Run the confusion matrix to determine if prediction is correct with the test set
#--------------------------------------------------------
logistic_confusionmatrix <- confusionMatrix(logistic_predict, test_data$State)
print(logistic_confusionmatrix)

#-------------------------------------------------------- 
# Show the accuracy result for the logistic regression model
#--------------------------------------------------------

cat("Accuracy:", logistic_confusionmatrix$overall['Accuracy'])

#-------------------------------------------------------- 
# Graph to compare models
#
# model_compare assisted by Claude 4 
# (Sonnet 4.5) by Anthropic (2024)
# https://claude.ai
#--------------------------------------------------------

model_compare <- data.frame(
  Model = c("Ranger", "Logistic Regression"),
  Accuracy = c(
    ranger_confusionmatrix$overall['Accuracy'] * 100,    # Ranger accuracy as percentage
    logistic_confusionmatrix$overall['Accuracy'] * 100   # Logistic regression accuracy as percentage

  )
)

ggplot(model_compare, aes(x = Model, y = Accuracy)) +
  geom_bar(stat = "identity") +
  ylim(0, 100) +
  labs(x = "Model", y = "Accuracy (%)") +
  theme_minimal()


# References
# Anthropic. (2024). Claude 4 (Sonnet 4.5) [Large language model]. https://claude.ai