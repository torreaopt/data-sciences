library(dplyr)
library("ggplot2")

getwd()
setwd("/Users/sethip/Work/PGDS/data-sciences/Datasets/Retail/")
getwd()

Store_df = read.csv("Store_sales.csv", stringsAsFactors = FALSE)
str(Store_df)
Store_df$Date <- as.Date(Store_df$Date, "%d-%m-%Y")
str(Store_df)

# Non-performing Store 38
Store38_df <- Store_df %>%
  filter(Store==38)
Store38_df <- arrange(Store38_df, Date)
View(Store38_df)

# Model 1
Store38_lrm1 <- lm(formula = Weekly_Sales~Date+CPI+Unemployment+Fuel_Price+Holiday_Flag,
                  data = Store38_df)
summary(Store38_lrm1)

# Model 2
Store38_lrm2 <- lm(formula = Weekly_Sales~CPI+Unemployment,
                  data = Store38_df)
summary(Store38_lrm2)

# Model Prediction
Store38_df <- Store38_df %>%
  select(Date, CPI, Unemployment, Weekly_Sales)
Store38_df <- Store38_df %>%
  mutate(Predicted_Weekly_Sales = predict(Store38_lrm2),
         Error = Weekly_Sales-Predicted_Weekly_Sales,
         Absolute_Error_Pct = abs(Error*100/Weekly_Sales))
head(Store38_df)
View(Store38_df)
min(Store38_df$Error)
max(Store38_df$Error)
min(Store38_df$Absolute_Error_Pct)
max(Store38_df$Absolute_Error_Pct)
  
