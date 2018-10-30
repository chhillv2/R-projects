auto<-read.csv("/Users/vikaschhillar/Downloads/used-cars-database/autos.csv")
head(x)
names(x)
len(df)
is.null(x)
names(x)
---
  title: "Used cars database"
author: "Donyoe"
output: 
  html_document: 
  fig_height: 7
fig_width: 10
theme: cosmo
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
```

# Reading Data
```{r}
install.packages("lubridate")
library(data.table)
library(ggplot2)
library(lubridate)

auto$dateCrawled <- ymd_hms(auto$dateCrawled) # Format time
auto$dateCreated <- ymd_hms(auto$dateCreated)
auto$lastSeen <- ymd_hms(auto$lastSeen)
auto$nrOfPictures <- NULL #Delete useless columns
auto$seller <- NULL
auto$offerType <- NULL
auto <- auto[price<150000&price>60] # Price between 60 and 150000 dollars
auto <- auto[yearOfRegistration>=1863&yearOfRegistration<2017]
auto <- auto[powerPS>0&powerPS<1100]
nom <- strsplit(as.character(auto$name),split = "_")
auto$model <- as.factor(sapply(nom,"[[",1))
summary(auto)
ggplot(auto,aes(price))+
  stat_density(fill="blue")+scale_x_log10(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(title="Vehicle Price",subtitle="In Euros",caption="Donyoe")

ggplot(auto[powerPS>20],aes(powerPS))+
  stat_density(fill="blue")+scale_x_log10()+
  labs(title="Vehicle PowerPS",subtitle="PowerPS",caption="Donyoe")
ggplot(auto,aes(kilometer))+
  stat_density(fill="coral")+
  labs(title="Vehicle kilometers",subtitle="Kilometers",caption="Donyoe")+xlab(NULL)

ggplot(auto[yearOfRegistration>1989,.N,by=.(monthOfRegistration,yearOfRegistration)],aes(x = monthOfRegistration,y = N,fill=N))+
  geom_bar(stat = "identity")+labs(title="Vehicles by month and year of registration",subtitle="Number of vehicles",caption="Donyoe")+xlab("Month")+ylab(NULL)+facet_wrap(~yearOfRegistration)

dif_dura <- summary(as.numeric(difftime(auto$lastSeen,auto$dateCreated,units = "secs")))
dura1 <- duration(num = as.numeric(dif_dura),units = "seconds")
cbind(dif_dura,as.character(dura1))

ggplot(auto[!vehicleType%in%c("")&!fuelType%in%c("")],aes(y = vehicleType,x=fuelType))+
  geom_tile(aes(fill=log(price)))+
  labs(title="Price by Vehicle Type and Fuel Type",caption="Donyoe")+xlab("Fuel Type")+ylab("Vehicle Type")

ggplot(auto,aes(x = kilometer,y = powerPS))+geom_smooth()+
  labs(title="Kilometer VS horse power",caption="Donyoe")+xlab("Kilometers")+ylab("horse power"),color="red"



2) ##############

library(data.table)
library(ggplot2)
library(lubridate)
auto$dateCrawled <- ymd_hms(auto$dateCrawled) # Format time
auto$dateCreated <- ymd_hms(auto$dateCreated)
auto$lastSeen <- ymd_hms(auto$lastSeen)
auto$nrOfPictures <- NULL #Delete useless columns
auto$seller <- NULL
auto$offerType <- NULL
auto <- auto[price<150000&price>60] # Price between 60 and 150000 dollars
auto <- auto[yearOfRegistration>=1863&yearOfRegistration<2017]
auto <- auto[powerPS>0&powerPS<1100]
nom <- strsplit(as.character(auto$name),split = "_")
auto$model <- as.factor(sapply(nom,"[[",1))
ggplot(auto,aes(price))+
  stat_density(fill="blue")+scale_x_log10(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(title="Vehicle Price",subtitle="In Euros",caption="Donyoe")
ggplot(auto[powerPS>20],aes(powerPS))+
  stat_density(fill="blue")+scale_x_log10()+
  labs(title="Vehicle PowerPS",subtitle="PowerPS",caption="Donyoe")

ggplot(auto,aes(kilometer))+
  stat_density(fill="coral")+
  labs(title="Vehicle kilometers",subtitle="Kilometers",caption="Donyoe")+xlab(NULL)

ggplot(auto[yearOfRegistration>1989,.N,by=.(monthOfRegistration,yearOfRegistration)],aes(x = monthOfRegistration,y = N,fill=N))+
  geom_bar(stat = "identity")+labs(title="Vehicles by month and year of registration",subtitle="Number of vehicles",caption="Donyoe")+xlab("Month")+ylab(NULL)+facet_wrap(~yearOfRegistration)
ggplot(auto[yearOfRegistration>1960,.N,by=yearOfRegistration],aes(x = yearOfRegistration,y = N,fill=N))+
  geom_bar(stat = "identity")+labs(title="Vehicles by year of registration",subtitle="Number of vehicles",caption="Donyoe")+xlab("Year")+ylab(NULL)

ggplot(auto,aes(y = price,x=vehicleType,fill=vehicleType))+
  geom_boxplot()+labs(title="Vehicles by Price",subtitle="Price",caption="Donyoe")+xlab(NULL)+ylab("Price")+scale_y_log10(labels = scales::dollar_format(suffix = "€", prefix = ""))

ggplot(auto,aes(x = price,y = kilometer))+geom_smooth()+
  labs(title="Price VS Kilometer",caption="Donyoe")+xlab(NULL)+ylab("Kilometers")+scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""))

ggplot(auto,aes(x = price,y = powerPS))+geom_smooth()+
  labs(title="Price VS PowerPS",caption="Donyoe")+xlab(NULL)+ylab("PowerPS")+scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""))

ggplot(auto,aes(x = kilometer,y = powerPS))+geom_smooth()+
  labs(title="Kilometer VS PowerPS",caption="Donyoe")+xlab("Kilometers")+ylab("PowerPS")


print('Number of listed cars is: ', len(auto))

ggplot(auto, aes(x =  kilometer, y = price, fill = gearbox, colour = gearbox)) + geom_point(alpha = 0.01) + 
  geom_smooth() + ylim(c(0,median(auto$price))) + ggtitle("Kilometer vs Price") +
  xlab("Kilometer") + ylab("Price")


install.packages("zipcode")
library(lubridate)
summary(auto$price)
summary(auto$powerPS)
library(ggplot2)
p1 <- ggplot(aes(x=vehicleType, y=powerPS), data = auto) + 
  geom_boxplot()
p2 <- ggplot(aes(x=vehicleType, y=powerPS), data = auto) + 
  geom_boxplot() +
  ylim(quantile(auto$powerPS, 0.05), quantile(auto$powerPS, 0.95))

grid.arrange(p1, p2, ncol = 1)

auto[(powerPS < quantile(powerPS, 0.05)) | (powerPS > quantile(powerPS, 0.95)), powerPS := NA]
auto[powerPS  < 40, powerPS := NA]
ggplot(aes(x=vehicleType, y=powerPS), data = auto) + 
  geom_boxplot()
head(auto$)
