library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(tidyverse)
library(reshape2)
library(ggcorrplot)

setwd("C:/Users/Umut/Desktop/dersler/IE 360/HW1/HW1 data")

carproduction<-read.csv(file="Total Automobile Production.csv",header=TRUE)
oilprices<-read.csv(file="Oil Prices.csv",header=TRUE)
searchterms<-read.csv(file="Search Terms.csv",header=TRUE)
priceindex<-read.csv(file="Transportation Price Index.csv",header=TRUE)
creditinterest<-read.csv(file="Vehicle Credit Interest Rates.csv",header=TRUE)
exchangerates<-read.csv(file="Exchange Rates.csv",header=TRUE)


carproduction<-carproduction %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
oilprices<-oilprices %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
searchterms<-searchterms %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
priceindex<-priceindex %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
creditinterest<-creditinterest %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
exchangerates<-exchangerates %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))



ggplot(carproduction, aes(x=Date,y=AutomobileProduction))+geom_line(color="red")+
    labs(x="Date",y="Automobile Production",title="Monthly Automobile Production")
ggplot(carproduction,aes(x=AutomobileProduction))+
    geom_histogram(binwidth=10000)

ggplot(carproduction,aes(x=factor(month(Date)),y=AutomobileProduction,colour = month(Date)))+
  geom_bar(stat="identity", aes(fill=month(Date)),color="black")+
  facet_wrap(~year(Date))+
  labs(x="Months of Each Year",title="Monthly Automobile Production By Year")

ggplot(carproduction,aes(x=factor(year(Date)),y=AutomobileProduction))+
  geom_boxplot(aes(fill=factor(year(Date))))+
  theme(legend.position = "none")+
  labs(x="Year",title="Monthly Automobile Production Deviation Each Year")

ggplot(carproduction,aes(x=factor(month(Date)),y=AutomobileProduction))+
  geom_boxplot(aes(fill=factor(month(Date))))+
  theme(legend.position = "none")+
  labs(x="Month",title="Average Automobile Production Each Month")

ggplot(oilprices, aes(x=Date,y=BRENT.PETROL.EUBP))+geom_line(color="red")+
    labs(x="Date",y="Oil Price EUBP",title="Monthly Oil Price")

ggplot(oilprices,aes(x=factor(month(Date)),y=BRENT.PETROL.EUBP,colour = month(Date)))+ 
geom_bar(stat="identity",aes(fill=month(Date)),color="black")+ 
facet_wrap(~year(Date))+ 
labs(x="Months of Each Year",title="Monthly Oil Price By Year")

ggplot(oilprices,aes(x=factor(year(Date)),y=BRENT.PETROL.EUBP))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Monthly Oil Price Deviation Each Year")

plot(priceindex)

correl_info = cor(priceindex[,c("TP.FG.J07","TP.FG.J071","TP.FG.J072","TP.FG.J073")])

ggcorrplot(correl_info, hc.order=TRUE, type="lower", lab=TRUE)

priceindex <- priceindex[,c('Date','TP.FG.J071')]

ggplot(priceindex,aes(x=factor(year(Date)),y=TP.FG.J071))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Transportation Price Index by Year")

ggplot(creditinterest,aes(x=factor(month(Date)),y=Vehicle.Credit.Interest.Rates,colour = month(Date)))+ 
geom_bar(stat="identity",aes(fill=month(Date)),color="black")+ 
facet_wrap(~year(Date))+ 
labs(x="Months of Each Year",title="Monthly Vehicle Credit Interest Rates By Year")

ggplot(creditinterest,aes(x=factor(year(Date)),y=Vehicle.Credit.Interest.Rates))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Vehicle Credit Interest Rates by Year")

ggplot(exchangerates, aes(x=Date,y=USD.TL))+geom_line(color="red")+
    labs(x="Date",y="USD",title="USD - TL")

ggplot(exchangerates,aes(x=factor(year(Date)),y=USD.TL))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="USD - TL")


plot(searchterms)

correl_info = cor(searchterms[,c("Sifir.Araba","Tasit.Sigortası","Araba")])

ggcorrplot(correl_info, hc.order=TRUE, type="lower", lab=TRUE)

ggplot(searchterms[,c("Date","Sifir.Araba")],aes(x=Date,y=Sifir.Araba))+
geom_line(size = 1,color="red")+
geom_line(data=searchterms[,c("Date","Tasit.Sigortasi")],aes(x=Date,y=Tasit.Sigortasi),size = 1,color="blue")+
geom_line(data=searchterms[,c("Date","Araba")],aes(x=Date,y=Araba),size = 1,color="green")+
labs(x="Year", y="Search Terms (R: Sıfır Araba, B: Taşıt Sigortası, G: Araba)",title="Serch Terms Related To Automobiles")

acf(carproduction[,"AutomobileProduction"])

SifirAraba <-searchterms[,"Sifir.Araba"]
TasitSigortasi <-searchterms[,"Tasit.Sigortasi"]
Araba <-searchterms[,"Araba"]
EUBP <-oilprices[,2]
PriceIndex071 <- priceindex[,"TP.FG.J071"]
Interest <-creditinterest[,2]
USDTL <- exchangerates[,2]

carproductiondata <- cbind(carproduction, SifirAraba,TasitSigortasi,Araba,EUBP,PriceIndex071,Interest,USDTL)
head(carproductiondata)

correl_info = cor(carproductiondata[,2:9])

ggcorrplot(correl_info, hc.order=TRUE, type="lower", lab=TRUE)

plot(carproductiondata)

acf(carproductiondata)
