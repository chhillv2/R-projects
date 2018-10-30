install.packages("googleAnalyticsR")
library(googleAnalyticsR)

client_id<- "546679768566-2ch15n2c89hh6ee2mgritqvk7li9bu9s.apps.googleusercontent.com"
client_secret<- "iPosw-XvzvwkMzx4iK07EBh8"  
ga_auth()
token<- Auth(client_id, client_secret)
