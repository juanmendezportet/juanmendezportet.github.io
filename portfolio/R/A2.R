#Installing necessary packages
install.packages("caret")
library(caret)
library(ggplot2)
library(dplyr)

setwd("/Users/juanmanumango/Desktop/Hult/R for data visualization/R programming")


#Reading the Bank Churn Dataset as churn and changing blank values to NA
churn <- read.csv(("BankChurnDataset.csv"),na.strings = c(""))

#Reading the New Customer Dataset as newcus and changing blank values to NA
newcus <- read.csv(("NewCustomerDataset.csv"),na.strings = c(""))

#Revising the data for missing values and special characters
head(x = churn,  n = 100)
head(x = newcus, n = 100)

#Removing "'" from the Surname column in both dataframes
churn$Surname  <- gsub("'", "", churn$Surname) 
newcus$Surname <- gsub("'", "", newcus$Surname) 

#Removing the "?" from the Surname column in both dataframes
churn$Surname  <- gsub("\\?", "", churn$Surname) 
newcus$Surname <- gsub("\\?", "", newcus$Surname) 

#Revising the structure of the dataframes to make sure all the columns with numbers are in numeric or interger type
str(churn)
str(newcus)

#Checking for na values in both dataframes
na_count_churn  <- colSums(is.na(churn))
na_count_newcus <- colSums(is.na(newcus))
print(na_count_churn)
print(na_count_newcus)

#Creating a back up copy of churn before modifying the data anymore
backup_churn <- churn

#Creating a column in churn to identify the missing values in the columns c(churn$age, churn$EstimatedSalary)
churn$has_na <- is.na(churn$Age) | is.na(churn$EstimatedSalary)

#Sorting the results to better see the rows with missing values
churn_na_ordered <- churn[order(churn$has_na & churn$Age,decreasing = FALSE),]
tail(churn_na_ordered,n = 9)

#Finding the average salary by age to impute the result for the NA values
average_salary_by_age <- aggregate(EstimatedSalary ~ Age, data = churn, FUN = mean)
print(average_salary_by_age)

#Imputing the missing ages based on average salary by age previously calculated 
churn[is.na(churn$EstimatedSalary)& churn$Age=="34","EstimatedSalary"] <- " 113050.15"
churn[is.na(churn$EstimatedSalary)& churn$Age=="47","EstimatedSalary"] <- " 116341.25"

#Given that there are only 6 missing values left it also makes sense to drop these rows, given that it wont impact the overall data
churn_no_na <- churn[!is.na(churn$Age),]
churn       <- churn_no_na

#Plotting by age to see the spread
hist(churn$Age, main = "Histogram of Age", xlab = "Age", breaks = 10)

#Grouping by decades, because in the dataset there are few values under 20 I grouped them as 20s and for values above 70 I used 70+ that way there are no groups with insufficient data 
churn$Decade = cut(churn$Age,
                breaks = c(0, 29, 39, 49, 59, 69, 100),
                labels = c("20s", "30s", "40s", "50s", "60s", "70s +"),
                right = FALSE)

# Print the updated dataframe to see the new Decade column
print(head(churn, n = 100))

#Dropping the columns that wont be utilized in the model
churn <- churn %>% select(-c(id, CustomerId, Surname, Gender, Age, has_na))

#Changing the values of the numerical variables to logs to increase the accuracy of the model
churn <- churn %>% mutate(log_credit = log(1+CreditScore), balance_log = log(1+Balance), estimatedsalary_log = log(1+as.numeric(EstimatedSalary)))

# Setting a seed for reproducibility 
set.seed(123)

# Defining n as the size of the dataframe
n <- nrow(churn)

# Determine the size of the training set (e.g., 70% of the dataset)
train_size <- floor(0.7 * n)

# Create a random index to select rows for the training set
train_index <- sample(seq_len(n), size = train_size)

# Subset the dataset into training and test sets
train_set <- churn[train_index, ]
test_set  <- churn[-train_index, ]

# Install data.table (if you haven't already)
install.packages("data.table")

# Load the data.table package into your R session
library(data.table)

#Creating the model based on the relevant variables
log.model <- glm(formula=Exited ~ Geography +
                                  Tenure +
                                  NumOfProducts +
                                  HasCrCard +
                                  IsActiveMember +
                                  Decade +
                                  log_credit +
                                  balance_log +
                                  estimatedsalary_log, family = binomial(link='logit'),data = train_set) 
#Looking at the summary of the model
summary(log.model)

#Checking the prediction accuracy
fitted.probabilities <- predict(log.model, newdata=test_set, type='response')
fitted.results       <- ifelse(fitted.probabilities > 0.5,1,0)

misClasificError <- mean(fitted.results != test_set$Exited)
print(paste('Accuracy',1-misClasificError))

#Creating a confusion matrix
confusion_matrix <- table(test_set$Exited, fitted.results)
confusion_matrix
#Creating a heat map for the confusion matrix
cm <- confusionMatrix(factor(fitted.results), factor(test_set$Exited), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))


table(test_set$Exited)
#To prevent "invalid graphic state" error
dev.off()

#Making the plot
ggplot(plt, aes(Prediction,Reference, fill= Freq))+
geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference")

#Changing the values to factors 
test_set$Exited <- factor(test_set$Exited, levels = c(1,0))
fitted.results  <- factor(fitted.results, levels = c(1,0))

test_set$Exited
#Calculating sensitivity and specificity of the model
sensitivity(test_set$Exited, fitted.results)
specificity(test_set$Exited, fitted.results)

################################################################################
#Grouping by decades
newcus$Decade = cut(newcus$Age,
                   breaks = c(0, 29, 39, 49, 59, 69, 100),
                   labels = c("20s", "30s", "40s", "50s", "60s", "70s +"),
                   right = FALSE)

# Print the updated dataframe to see the new Decade column
print(head(newcus, n = 100))

#Dropping the columns that wont be utilized in the model
newcus <- newcus %>% select(-c(id, CustomerId, Surname, Gender, Age))

#Changing the values of the numerical variables to logs to increase the accuracy of the model
newcus <- newcus %>% mutate(log_credit = log(1+CreditScore), balance_log = log(1+Balance), estimatedsalary_log = log(1+as.numeric(EstimatedSalary)))

#Applying the model to the new dataframe "newcus"
fitted.probabilities_newcus <- predict(log.model, newdata=newcus, type='response')
fitted.results_newcus       <- ifelse(fitted.probabilities_newcus > 0.5,1,0)

#Including the results of the model as a new column in newcus named Exited
newcus$Exited <- fitted.results_newcus

#Making a histogram to see how many clients will exit depending on age

#Filtering the dataframe for the clients that exited and not exited
newcus <- newcus %>%
  mutate(ExitStatus = ifelse(Exited == 1, "Exited", "Not Exited"))

# Create the histogram
ggplot(newcus, aes(x = Decade, fill = ExitStatus)) +
  geom_histogram(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("Exited" = "red", "Not Exited" = "#009194")) +
  theme_minimal() +
  labs(title = "Clients by Decades and Exit Status",
       x = "Decades",
       y = "Count",
       fill = "Exit Status")

result <- newcus %>%
  filter(Exited == 1) %>%  # Filter for people who have exited
  group_by(Decade) %>%  # Group by the decades column
  summarise(Count = n())  # Summarize the count of people in each group

# Display the result
print(result)

################################################################################
##################################ANALYSIS######################################
################################################################################

#By looking at the p values in the summary of model, we can see that all variables have a substantial impact in the model, expect for geography spain, this may be because of the lack of data from spanish clients.
#The model has an accuracy of 83% which is pretty good considering the type of model that was used.
#The model has a sensitivity of 71.9% this means that the model predicts correctly 71.9% of the true positive values.
#The model has a specificity of 84.6% this means that the model predicts correctly 84.6% of the true negatives values.
#This tells us that the model is better at predicting the clients that will not exit, more than, the clients that will exit.
#Based on the results of the histogram we can see that most of the clients that will exit are between the ages of 40 and 60.



