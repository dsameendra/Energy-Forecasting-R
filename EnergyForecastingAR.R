library(readxl) # for reading Excel files
library(neuralnet) # used to create and train NNs
library(dplyr) # used to created lagged data
library(MLmetrics) # used to evaluate NN models
library(tidyverse) # collection of packages including ggplot

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

# Summary of 20th hour
summary(df$`2000H`)

# Plotting 20th hour data
ggplot(df, aes(x = date, y = `2000H`)) +
  geom_line() +
  labs(title = "Hourly Electricity Consumption at 2000H",
       x = "Date",
       y = "Electricity Consumption (kWh)")

# Part B
# Creating IO Matrix
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

# Part C
#Scaling with normalize function
df_scaled <- as.data.frame(lapply(df_delayed[4:9], normalize)) # only 2000H and delayed values are taken

# Adding the 2000H column back to the scaled data frame
df_scaled <- cbind(OG_2000H = df_delayed$`2000H`, df_scaled)
dim(df_scaled)

# Train-test Split
df_scaled_train <- df_scaled[1:380,]
df_scaled_test = df_scaled[381:nrow(df_scaled),]
dim(df_scaled_train)
dim(df_scaled_test)

# Part D
## Creating Different Input/Output Formulas
IO_1 <- `X2000H`~t1
IO_2 <- `X2000H`~t1+t2+t3
IO_3 <- `X2000H`~t1+t2+t3+t7
IO_4 <- `X2000H`~t1+t2+t3+t4+t7
IO_5 <- `X2000H`~t1+t7
IO_6 <- `X2000H`~t7

# Activation Functions
acti_func_L <- "logistic"
acti_func_T <- "tanh"

# Hidden layers
hid_layer_structute <- c(5,2)

## Training

set.seed(42) # setting a seed for training to be consistent
model_AR <- neuralnet(
  IO_5,
  data = df_scaled_train,
  hidden = hid_layer_structute,
  act.fct = acti_func_T,
  linear.output = F
)

# look at the parameters inside the above code
summary(model_AR)

# Result matrix (information about learned weights)
model_AR$result.matrix

# plotting nn model
plot(model_AR)


## Testing
# Compute predictions on test dataset
model_1_results <- predict(model_AR, df_scaled_test)
predictions <- model_1_results

# Converting predictions back to the original scale
predictions_denormalized <- denormalize(predictions, min_value, max_value)

# Getting the actual values from testset
actuals <- df_scaled_test$`OG_2000H`

comparison = data.frame(predictions_denormalized,actuals)
print(comparison)


## Evaluating
# Calculating required evaluation metrics for the predictions
rmse <- RMSE(actuals, predictions_denormalized)
mae <- MAE(actuals, predictions_denormalized)
mape <- MAPE(actuals, predictions_denormalized)
smape <- sMAPE(actuals, predictions_denormalized)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape, "\n")

# Part I
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

