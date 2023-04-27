library(readxl) # for reading Excel files
library(neuralnet)
library(dplyr)
library(tidyverse)
library(MLmetrics)
library(ggplot2)

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
#df_delayed <- slice(df_delayed, 8:nrow(df_delayed))
df_delayed <- df_delayed[complete.cases(df_delayed),]

# Getting the minimum and maximum values for 2000H
min_value <- min(df[4])
max_value <- max(df[4])

# View the minimum and maximum values
print(min_value)
print(max_value)

#Scaling with normalize function
df_scaled <- as.data.frame(lapply(df_delayed[2:9], normalize))

# Add the 2000H column back to the scaled data frame
df_scaled <- cbind(OG_2000H = df_delayed$`2000H`, df_scaled)
dim(df_scaled)

# Train-test Split
df_scaled_train <- df_scaled[1:380,]
df_scaled_test = df_scaled[381:463,]

# Create the input and output matrices

IO_1 <- `X2000H`~t1+t2+t3+t4+t7
IO_2 <- `X2000H`~t1+t2+t3+t4+t7+`X1900H`
IO_3 <- `X2000H`~t1+t2+t3+t4+t7+`X1800H`
IO_4 <- `X2000H`~t1+t2+t3+t4+t7+`X1900H`+`X1800H`

# Different Internal Structures

hidden_layer_1 <- c(12)
hidden_layer_2 <- c(6,2)
hidden_layer_3 <- c(15)
hidden_layer_4 <- c(8,4)

acti_func_1 <- "logistic"
acti_func_2 <- "tanh"

# Training

set.seed(123) # set the seed for training to be consistent
time_start <- Sys.time()
model_NARX <- neuralnet(
  IO_4,
  data = df_scaled_train,
  hidden = c(6,2),
  act.fct = acti_func_2,
  linear.output = F
)
time_end <- Sys.time()
time_train <- time_end-time_start
print(time_train)

# look at the parameters inside the above code
summary(model_NARX)
model_NARX$result.matrix

# plotting nn model
plot(model_NARX)

# Testing
# Compute predictions on test dataset
model_1_results <- predict(model_NARX, df_scaled_test)
predictions <- model_1_results

# Convert predictions back to the original scale
predictions_denormalized <- denormalize(predictions, min_value, max_value)

actuals <- df_scaled_test$`OG_2000H`

comparison = data.frame(predictions_denormalized,actuals)
print(comparison)

# Evaluating
# Calculate evaluation metrics for the predictions
rmse <- RMSE(actuals, predictions_denormalized)
mae <- MAE(actuals, predictions_denormalized)
mape <- MAPE(actuals, predictions_denormalized)
smape <- sMAPE(actuals, predictions_denormalized)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape, "\n")


# Plot the predicted vs. actual values graph (Scatter plit)
plot(x=predictions_denormalized, y=actuals,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Scatter plot: Predicted vs. Actual Values')
abline(a = 0, b = 1, col = 'red') # adding a red line denoting perfect predictions

# Line chart
ggplot() +
  geom_line(aes(x = seq_along(actuals), y = actuals, color = "True Values")) +
  geom_line(aes(x = seq_along(predictions_denormalized), y = predictions_denormalized, color = "Predictions")) +
  xlab("Sample Index") +
  ylab("Value") +
  scale_color_manual(values = c("True Values" = "blue", "Predictions" = "red")) +
  theme_classic()+
  labs(title = "Line Chart: Predicted vs. Actual Values")

