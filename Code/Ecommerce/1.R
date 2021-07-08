getwd()
setwd("/Users/sethip/Work/PGDS/data-sciences/Datasets/")
getwd()
store_df = read.csv("Store_sales.csv")
View(store_df)
Gross_Total_Sales <- sum(store_df$Weekly_Sales)
store_summary <- store_df%>%
  group_by(Store) %>%
  summarise(Store_Sales=sum(Weekly_Sales)) %>%
  mutate(Store_Sales_Share = format(Store_Sales*100/Gross_Total_Sales, digits = 2, nsmall = 2))
store_summary <- store_summary[
  order(store_summary$Store_Sales, decreasing = T),]
store_summary <- mutate(store_summary, Cummulative_Share = cumsum(Store_Sales_Share))
View(store_summary)
write.csv(store_summary, file="/Users/sethip/Work/PGDS/data-sciences/Results/store_sales_summary.csv")
