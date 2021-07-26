getwd()
setwd("/Users/sethip/Work/PGDS/data-sciences/Datasets/Retail/")
getwd()

library(dplyr)
install.packages("ggplot2")
library("ggplot2")
install.packages("ggfortify")
library(ggfortify)
install.packages("xts")
library("xts")
install.packages("forecast")
library(forecast)

Store_df = read.csv("Store_sales.csv", stringsAsFactors = FALSE)
Store_df$Date <- as.Date(Store_df$Date, "%d-%m-%Y")
summary(Store_df)
class(Store_df)
head(Store_df)
str(Store_df)
ggplot(Store_df, aes(x=Date, y=Weekly_Sales)) +
  geom_point() + geom_smooth(method=lm)

# Top performing Store 20
Store20_df <- Store_df %>%
  filter(Store==20)
str(Store20_df)
Store20_df$Date <- as.Date(Store20_df$Date, "%d-%m-%Y")
str(Store20_df)
ggplot(Store20_df, aes(x=Date, y=Weekly_Sales)) +
  ggtitle("Weekly Sales of Store#20") + xlab("Weeks") +
  geom_point() + geom_smooth(method=lm)
Store20_lrm <- lm(formula = Weekly_Sales~Date+CPI+Unemployment+Fuel_Price+Holiday_Flag,
                         data = Store20_df)
summary(Store20_lrm)
cor(Store20_df$CPI, Store20_df$Weekly_Sales)
cor(Store20_df$Fuel_Price, Store20_df$Weekly_Sales)

# Non-performing Store 38
Store38_df <- Store_df %>%
  filter(Store==38)
str(Store38_df)
Store38_df$Date <- as.Date(Store38_df$Date, "%d-%m-%Y")
str(Store38_df)
ggplot(Store38_df, aes(x=Date, y=Weekly_Sales)) +
  ggtitle("Weekly Sales of Store#38") + xlab("Weeks") +
  geom_point() + geom_smooth(method=lm)
Store38_Sales_ts <- xts(Store38_df[,-2],
                       order.by=as.Date(Store38_df$Date))
autoplot(Store38_Sales_ts[,c("CPI", "Unemployment", "Weekly_Sales")],
         title="Multivariate Time Series for Store#38") +
  xlab("Weeks")
cor(Store38_df$CPI, Store38_df$Weekly_Sales)
cor(Store38_df$Unemployment, Store38_df$Weekly_Sales)
Store38_lrm <- lm(formula = Weekly_Sales~Date+CPI+Unemployment+Fuel_Price+Holiday_Flag,
                         data = Store38_df)
summary(Store38_lrm)