install.packages("googleAnalyticsR")
library(googleAnalyticsR)




ga_auth()

my_accounts<- google_analytics_account_list()
View(my_accounts)
ga_id<- "179741595"

start_date<- "60daysAgo"
enddate<- "yesterday"

df_session<- google_analytics_4(ga_id,
                                date_range =c(start_date, enddate),
                                metrics = c("sessions"),
                                dimensions =c("date"))

