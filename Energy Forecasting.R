library(readxl) # for reading Excel files
library(neuralnet)
library(dplyr)
library(data.table)
library(tidyverse)

library(plotly)
library(caTools)
library(useful)
library(tictoc)
library(Metrics)
library(ggplot2)
library(MLmetrics)

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

# Create time-delayed versions of the "2000H" column up to t4 and t7
df_delayed <- df %>% 
  mutate(t1 = lag(`2000H`, n = 1),
         t2 = lag(`2000H`, n = 2),
         t3 = lag(`2000H`, n = 3),
         t4 = lag(`2000H`, n = 4),
         t7 = lag(`2000H`, n = 7))

# Drop the first 7 rows as delayed values have NA
df_delayed <- slice(df_delayed, 8:nrow(df_delayed))

#shift and create new columns
df_shifted <- shift.column(data=df, columns="2000H",len = 1, up = FALSE,newNames = sprintf("t1", "2000H"))
df_shifted <- shift.column(data=df_shifted, columns="2000H",len = 2, up = FALSE,newNames = sprintf("t2", "2000H"))
df_shifted <- shift.column(data=df_shifted, columns="2000H",len = 3, up = FALSE,newNames = sprintf("t3", "2000H"))
df_shifted <- shift.column(data=df_shifted, columns="2000H",len = 4, up = FALSE,newNames = sprintf("t4", "2000H"))
df_shifted <- shift.column(data=df_shifted, columns="2000H",len = 7, up = FALSE,newNames = sprintf("t7", "2000H"))

#z-score normalization
df_scaled <- as.data.frame(df_delayed %>% mutate_at(vars(-date), scale, center=T)) # Drop dates Column
df_scaled <- within(df_scaled, rm(date))
df_scaled

# Normalize function
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#Scalling with normalize function
df_scaled <- as.data.frame(lapply(df_delayed[4:9], normalize))
df_scaled <- cbind(df_scaled, df_delayed[c(4)])

# Train-test Split
df_scaled_train = head(df_scaled, n =380)
df_scaled_test = tail(df_scaled, n =73)

# Create the input and output matrices
train_inputs <- data.matrix(df_scaled_train[, 4:8])
train_outputs <- data.matrix(df_scaled_train$`2000H`)
train_inputs
train_outputs


formula_1 <- `2000H`~t1+t2+t3+t4+t7
model_1 <- neuralnet(formula_1, data = df_scaled_train, hidden = c(5), rep = 5, act.fct = "logistic", threshold = 2, linear.output = F)
# look at the parameters inside the above code
summary(model_1)
plot(model_1)
class(model_1)
# Compute predictions on test dataset
model_1.results <- predict(model_1, df_scaled_test)
predictions <- model_1.results
predictions

# Convert predictions back to the original scale
predictions_unscaled <- predictions * sd(df_scaled_test$`2000H`) + mean(df_scaled_test$`2000H`)
predictions_unscaled
  
test_col <- df_scaled_test$`2000H`

# Then, unscale the column using the mean and standard deviation of the training data
test_col_unscaled <- scale(test_col, center = -mean(df$`2000H`), scale = 1/sd(df$`2000H`))
test_col_unscaled

train_col_unscaled <- scale(predictions, center = -mean(df$`2000H`), scale = 1/sd(df$`2000H`))
train_col_unscaled


# Calculate evaluation metrics for the predictions
rmse <- RMSE(train_col_unscaled, test_col_unscaled)
mae <- MAE(train_col_unscaled, test_col_unscaled)
mape <- MAPE(train_col_unscaled, test_col_unscaled)


sMAPE <- function(actual, predicted){
  200/length(actual) * sum(abs(actual - predicted)/(abs(actual) + abs(predicted)))
}
smape <- sMAPE(train_col_unscaled, test_col_unscaled)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape, "\n")

