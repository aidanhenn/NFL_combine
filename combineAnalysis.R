rm(list = ls())
install.packages('openxlsx')
install.packages('readxl')
install.packages('summarytools')
install.packages('dplyr')
install.packages("corrplot")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("cli")
install.packages("caret", dependencies = "Depends")

library(ggplot2)
library(randomForest)
library(summarytools)
library(dplyr)
library(openxlsx)
library(readxl)
library(corrplot)
library(caret)
library(cli)


file_path = "C://Users//aidan//OneDrive//Desktop//CombineStats.xlsx"

sheet_names <- paste0("Sheet", 1:6)

# Read data from sheets 1 to 6
my_data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})


# Combine the data frames into a single data frame
combine_data <- do.call(rbind, my_data_list)

combine_data %>%
  dfSummary() %>%
  print()

summary(combine_data)

n_rows <- nrow(combine_data)
n_cols <- ncol(combine_data)

cat(n_rows, "rows and", n_cols, "columns\n")

null_counts <- colSums(is.na(combine_data))
print(null_counts)

unique_counts <- sapply(combine_data, function(x) length(unique(x)))
print(unique_counts)


# Change col name so it's easier to work with
names(combine_data)[names(combine_data) == '40yd'] <- 'FourtyTime'

# Set drafted col to 1 if drafted, 0 otherwise
combine_data$Drafted <- ifelse(!is.na(combine_data$`Drafted (tm/rnd/yr)`), 1, 0)

# Convert height to inches
to_inches <- function(height) {
  parts <- strsplit(as.character(height), "-")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- feet * 12 + inches
  return(total_inches)
}

# Apply the function to the Ht column
combine_data$Ht <- sapply(combine_data$Ht, to_inches)

# Check correlations within data
selected_columns <- combine_data[, c("Ht","Wt","FourtyTime", "Vertical","Bench", "Broad Jump", "3Cone", "Shuttle", "Drafted")]
complete_rows <- complete.cases(selected_columns)
cor_matrix <- cor(selected_columns[complete_rows, ])
cor_matrix
corrplot(cor_matrix, method = "color")

#Filter to only get "Speed" positions
RB_WR_CB_S <- combine_data %>% filter(Pos == "RB" | Pos == "WR" | Pos == "CB" | Pos == "S")
selected_columns <- RB_WR_CB_S[, c("Ht","Wt","FourtyTime", "Vertical","Bench", "Broad Jump", "3Cone", "Shuttle", "Drafted")]
complete_rows <- complete.cases(selected_columns)
cor_matrix <- cor(selected_columns[complete_rows, ])
cor_matrix
corrplot(cor_matrix, method = "color")

#Filter to only get Linemen positions
OL_DT_OG_C_DL <- combine_data %>% filter(Pos == "OL" | Pos == "DT" | Pos == "OG" | Pos == "C" | Pos == "DL")
selected_columns <- OL_DT_OG_C_DL[, c("Ht","Wt","FourtyTime", "Vertical","Bench", "Broad Jump", "3Cone", "Shuttle", "Drafted")]
complete_rows <- complete.cases(selected_columns)
cor_matrix <- cor(selected_columns[complete_rows, ])
cor_matrix
corrplot(cor_matrix, method = "color")


#Function to split the Drafted column to team, round, pick and year
split_and_extract <- function(drafted) {
  parts <- strsplit(as.character(drafted), " / ")[[1]]
  team <- parts[1]
  round <- as.numeric(gsub("\\D", "", parts[2]))
  pick <- as.numeric(gsub("\\D", "", parts[3]))
  year <- as.numeric(parts[4])
  return(c(Team = team, Round = round, Pick = pick, Year = year))
}

cbData <- combine_data
# Apply the function to the Drafted column and create the new columns
cbData[c("Team", "Round", "Pick", "Year")] <- t(sapply(cbData$`Drafted (tm/rnd/yr)`, split_and_extract))

# Convert columns to numeric
cbData$Round <- as.numeric(cbData$Round)
cbData$Pick <- as.numeric(cbData$Pick)
cbData$Year <- as.numeric(cbData$Year)

cbData <- cbData %>%
  rename(`Target` = 'Round' )

#Filter to only get WR position
wideReceivers <- cbData %>% filter(Pos == "WR")

# Box plot for wide recievers 40yd times for each draft round
ggplot(wideReceivers, aes(x = as.factor(Target), y = FourtyTime)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Wide Reciever 40yd Dash Time by Draft Round", x = "Target (Draft Round)", y = "40yd Dash Time")

# Box plot for wide receivers 40yd times for drafted/undrafted with average annotations
ggplot(wideReceivers, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = FourtyTime)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red",
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               vjust = -1, position = position_dodge(width = 0.75)) +
  labs(title = "Wide Receiver 40yd Dash Times for drafted/undrafted players", x = "Draft Status", y = "40yd Dash Time") +
  scale_y_continuous(labels = scales::comma_format())

# Box plot for Linemen 40yd times between drafted and undrafted
ggplot(OL_DT_OG_C_DL, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = Vertical)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red",
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               vjust = -1, position = position_dodge(width = 0.75)) +
  labs(title = "Wide Receiver 40yd Dash Times for drafted/undrafted players", x = "Draft Status", y = "40yd Dash Time") +
  scale_y_continuous(labels = scales::comma_format())


# Visualizing 40 times by position

selected_columns <- combine_data[, c("Ht","Wt","FourtyTime", "Drafted")]
complete_rows <- complete.cases(selected_columns)
data <- combine_data[complete_rows,]

# Bar plot with 40yd by position
ggplot(data, aes(x = Pos, y = FourtyTime, color = Pos)) +
  geom_boxplot() +
  labs(title = "40yd Times by Position", x = "Position", y = "40yd Time (Seconds)")

# Bench reps by position
ggplot(data, aes(x = Pos, y = Bench, color = Pos)) +
  geom_boxplot() +
  labs(title = "Bench Press Reps by Position", x = "Position", y = "Bench Press (# of reps)")

#Drafted VS Undrafted Verticals
ggplot() +
  # Boxplot for OL_DT_OG_C_DL dataset
  geom_boxplot(data = OL_DT_OG_C_DL, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = Vertical),
               fill = "lightblue", position = position_dodge(width = 0.75)) +

  # Boxplot for RB_WR_CB_S dataset
  geom_boxplot(data = RB_WR_CB_S, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = Vertical),
               fill = "lightgreen", position = position_dodge(width = 0.75)) +
  labs(title = "Comparison of Drafted vs Undrafted Players Verticals",
       x = "Draft Status", y = "Vertical (Inches)")


# Drafted vs Undrafted 40yd
ggplot() +
  # Boxplot for OL_DT_OG_C_DL dataset
  geom_boxplot(data = OL_DT_OG_C_DL, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = FourtyTime),
               fill = "lightblue", position = position_dodge(width = 0.75)) +

  # Boxplot for RB_WR_CB_S dataset
  geom_boxplot(data = RB_WR_CB_S, aes(x = factor(Drafted, labels = c("Undrafted", "Drafted")), y = FourtyTime),
               fill = "lightgreen", position = position_dodge(width = 0.75)) +
  labs(title = "Comparison of Drafted vs Undrafted Players 40yd Dash Times",
       x = "Draft Status", y = "40yd Time (Seconds)")


# Training the model

# Selected columns
selected_columns <- c('Pos','Ht', 'Wt', 'FourtyTime', 'Vertical','Drafted')
complete <- combine_data[complete.cases(combine_data[, selected_columns]), selected_columns]

complete$Drafted <- factor(complete$Drafted)

# 80% for training, 20% for testing
train_index <- createDataPartition(complete$Drafted, p=0.80, list=FALSE)
test <- complete[-train_index,]
train <- complete[train_index,]


train_model_combine <- function(method) {
  set.seed(42)
  control <- trainControl(method="cv", number=10) # 10-fold cross validation
  metric <- "Accuracy"
  model <- train(Drafted~., data=train, method=method, metric=metric, trControl=control)
  return(model)
}

# LDA 70%
fit.lda <- train_model_combine("lda")
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, test$Drafted)

# CART 69%
fit.cart <- train_model_combine("rpart")
predictions <- predict(fit.cart, test)
confusionMatrix(predictions, test$Drafted)

# kNN 58%
fit.knn <- train_model_combine("knn")
predictions <- predict(fit.knn, test)
confusionMatrix(predictions, test$Drafted)

# SVM #65%
fit.svm <- train_model_combine("svmRadial")
predictions <- predict(fit.svm, test)
confusionMatrix(predictions, test$Drafted)

# Random Forest #69%
fit.rf <- train_model_combine("rf")
predictions <- predict(fit.rf, test)
confusionMatrix(predictions, test$Drafted)


# (New model with highest accuracy)
# GLM 71%
fit.glm <- train_model_combine("glm")
predictions <- predict(fit.glm, test)
confusionMatrix(predictions, test$Drafted)





