library(readxl) # for reading Excel files
library(neuralnet)
library(dplyr)
library(data.table)
library(tidyverse)
library(MLmetrics)
library(ggplot2)

library(plotly)
library(caTools)
library(useful)
library(tictoc)
library(Metrics)

# Normalize function
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# De-normalizing function
denormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

# Function for calculating smape as it is not included in MLmetrics
sMAPE <- function(actual, predicted){
  200/length(actual) * sum(abs(actual - predicted)/(abs(actual) + abs(predicted)))
}


# Loading the dataset
df <- read_excel("./datasets/uow_consumption.xlsx")
df
dim(df)
str(df)
summary(df)

# Change column names
colnames(df)[2] <- "1800H"
colnames(df)[3] <- "1900H"
colnames(df)[4] <- "2000H"
df

# Plot data
ggplot(df, aes(x = date, y = `2000H`)) +
  geom_line() +
  labs(title = "Hourly Electricity Consumption at 2000H",
       x = "Date",
       y = "Electricity Consumption (kWh)")


# Create time-delayed versions of the "2000H" column up to t4 and t7
df_delayed <- df %>% 
  mutate(t1 = lag(`2000H`, n = 1),
         t2 = lag(`2000H`, n = 2),
         t3 = lag(`2000H`, n = 3),
         t4 = lag(`2000H`, n = 4),
         t7 = lag(`2000H`, n = 7))

# Remove those rows with NA due to shift
df_delayed <- slice(df_delayed, 8:nrow(df_delayed))
df_delayed <- df_delayed[complete.cases(df_delayed),]

# Getting the minimum and maximum values for 2000H
min_value <- min(df[4])
max_value <- max(df[4])

# View the minimum and maximum values
print(min_value)
print(max_value)

#Scaling with normalize function
df_scaled <- as.data.frame(lapply(df_delayed[4:9], normalize))

# Add the 2000H column back to the scaled data frame
df_scaled <- cbind(OG_2000H = df_delayed$`2000H`, df_scaled)
dim(df_scaled)
boxplot(df_scaled[-1]) # view boxplot after normalizing without og 2000H column

# Train-test Split
df_scaled_train <- df_scaled[1:380,]
df_scaled_test = df_scaled[380:463,]

# Create the input and output matrices
#train_inputs <- data.matrix(df_scaled_train[, 2:6])
#train_outputs <- data.matrix(df_scaled_train$`X2000H`)

IO_1 <- `X2000H`~t1+t2
IO_2 <- `X2000H`~t2+t3+t4
IO_3 <- `X2000H`~t1+t2+t3+t7
IO_4 <- `X2000H`~t1+t2+t3+t4+t7

time_start <- Sys.time()
model_1 <-
  neuralnet(
    IO_4,
    data = df_scaled_train,
    hidden = c(5,10,20),
    act.fct = "tanh",
    linear.output = F
  )
time_end <- Sys.time()
time_train <- time_end-time_start
print(time_train)

# look at the parameters inside the above code
summary(model_1)
model_1$result.matrix

# plotting nn model
plot(model_1)

# Compute predictions on test dataset
model_1_results <- predict(model_1, df_scaled_test)
predictions <- model_1_results

# Convert predictions back to the original scale
predictions_denormalized <- denormalize(predictions, min_value, max_value)

actuals <- df_scaled_test$`OG_2000H`

comparison = data.frame(predictions_denormalized,actuals)
print(comparison)

# Calculate evaluation metrics for the predictions
rmse <- RMSE(actuals, predictions_denormalized)
mae <- MAE(actuals, predictions_denormalized)
mape <- MAPE(actuals, predictions_denormalized)
smape <- sMAPE(actuals, predictions_denormalized)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape, "\n")

