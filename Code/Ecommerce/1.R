getwd()
setwd("/Users/sethip/Work/PGDS/data-sciences/Datasets/")
getwd()
store_df = read.csv("Store_sales.csv")
View(store_df)

#====================== Part I ======================
Gross_Total_Sales <- sum(store_df$Weekly_Sales)
store_summary <- store_df%>%
  group_by(Store) %>%
  summarise(Store_Sales=sum(Weekly_Sales)) %>%
  mutate(Store_Sales_Share = format(Store_Sales*100/Gross_Total_Sales, digits = 2, nsmall = 2))
store_summary <- store_summary[
  order(store_summary$Store_Sales, decreasing = T),]
store_summary <- mutate(store_summary, Cummulative_Share = cumsum(Store_Sales_Share))
View(store_summary)

#====================== Part II ======================
# Store-wise Standard Deviation
store_summary <- summarize(group_by(store_df, Store),
                           Store_Sales=sum(Weekly_Sales),
                           Store_Sales_Share = format(Store_Sales*100/Gross_Total_Sales, digits = 2, nsmall = 2),
                           Deviation=sd(Weekly_Sales),
                           Mean=mean(Weekly_Sales),
                           CV=as.numeric(format(Deviation*100/Mean, digits = 2, nsmall = 2)))
View(store_summary)
format(mean(store_summary$CV), digits = 2, nsmall = 2)
median(store_summary$CV)

# Store 20 Summary
store_df$Date <- as.Date(store_df$Date, "%d-%m-%Y")
top_store_weekly_summary <- store_df%>%
  filter(Store==20)
#top_store_weekly_summary$Id <- seq.int(nrow(top_store_weekly_summary))
arrange(top_store_weekly_summary, Date) %>%
  head

View(top_store_weekly_summary)
plot(top_store_weekly_summary$Date, top_store_weekly_summary$Weekly_Sales,
     type = "o",
     col="blue", main = "Weekly Sales from Feb-2010 to Oct-2012", ylab = "Weekly Sales",
     xlab = "Weeks from Feb-2010 to Oct-2012", ylim=c(min(top_store_weekly_summary$Weekly_Sales), max(top_store_weekly_summary$Weekly_Sales)))

