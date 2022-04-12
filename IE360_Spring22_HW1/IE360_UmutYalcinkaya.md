## 1. Introduction

Automobile manufactoring is among one of the most important industries in Turkey. When its peak in 2017, 1 million 120 tousand and 860 motor vehicles are produced in Turkey in a single year. While this number does not make Turkey a automobile producing giant when compared to much more industrialized markets, it makes Turkey, the largest producer at close geographic area of Balkans and Middle East. Since this industry creates a non-negligable part of Turkish GDP and workforce, it is crutual to see and understand the trends and future of this industry. 

Using data sets obtained from Central Bank of the Republic of Turkey and Google Trends, this study will try to understand "Are monthly production volumes of Automobiles related to the global oil prices, transportation price index, vehicle credit interest rates and USD-TL exchange rates as well as some related keyword searches at google.com?" To see and understand the larger trends occuring in this industry, this study will use data from 09-2009 to 02-2022. Data provided by Central Bank of the Republic of Turkey and Google Trends is used at monthly level.



```R
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
```


```R
carproduction<-carproduction %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
oilprices<-oilprices %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
searchterms<-searchterms %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
priceindex<-priceindex %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
creditinterest<-creditinterest %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))
exchangerates<-exchangerates %>% rename(Date= Tarih)%>% mutate(Date=as.Date(as.yearmon(Date)))


```

## 2. Visualisation of Data

### 2.1. Monthly Automobile Production


```R
ggplot(carproduction, aes(x=Date,y=AutomobileProduction))+geom_line(color="red")+
    labs(x="Date",y="Automobile Production",title="Monthly Automobile Production")
ggplot(carproduction,aes(x=AutomobileProduction))+
    geom_histogram(binwidth=10000)
```


    
![png](output_4_0.png)
    



    
![png](output_4_1.png)
    



```R
ggplot(carproduction,aes(x=factor(month(Date)),y=AutomobileProduction,colour = month(Date)))+
  geom_bar(stat="identity", aes(fill=month(Date)),color="black")+
  facet_wrap(~year(Date))+
  labs(x="Months of Each Year",title="Monthly Automobile Production By Year")
```


    
![png](output_5_0.png)
    



```R
ggplot(carproduction,aes(x=factor(year(Date)),y=AutomobileProduction))+
  geom_boxplot(aes(fill=factor(year(Date))))+
  theme(legend.position = "none")+
  labs(x="Year",title="Monthly Automobile Production Deviation Each Year")
```


    
![png](output_6_0.png)
    



```R
ggplot(carproduction,aes(x=factor(month(Date)),y=AutomobileProduction))+
  geom_boxplot(aes(fill=factor(month(Date))))+
  theme(legend.position = "none")+
  labs(x="Month",title="Average Automobile Production Each Month")
```


    
![png](output_7_0.png)
    


As it can be clearly seen from these graphs Automobile production in Turkey follows simmilar characteristics each year. In every August production numbers nearly drop to half while in months of October and November, ptoduction increases. The graphs also shows that yearly production was increasing up until 2017 but has been decreesing in the years following 2017. Also curiously, start restrictions of Covid-19 pandemic is extremely clear to see from this data. At April of 2020, automobile production nearly dropped to 0 due to the start of restrictions for covid-19 in Turkey.

### 2.2. Oil Price


```R
ggplot(oilprices, aes(x=Date,y=BRENT.PETROL.EUBP))+geom_line(color="red")+
    labs(x="Date",y="Oil Price EUBP",title="Monthly Oil Price")
```


    
![png](output_10_0.png)
    



```R
ggplot(oilprices,aes(x=factor(month(Date)),y=BRENT.PETROL.EUBP,colour = month(Date)))+ 
geom_bar(stat="identity",aes(fill=month(Date)),color="black")+ 
facet_wrap(~year(Date))+ 
labs(x="Months of Each Year",title="Monthly Oil Price By Year")
```


    
![png](output_11_0.png)
    



```R
ggplot(oilprices,aes(x=factor(year(Date)),y=BRENT.PETROL.EUBP))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Monthly Oil Price Deviation Each Year")
```


    
![png](output_12_0.png)
    


Simmilar to the automobile prdoction data effects of Covid-19 pandemic is also extremely easy to see. At the 3rd and 4th months of 2020, global oil prices hit a record low nearly going as low as 0. While in some years such as 2011, 2012 and 2013 oil prices stayed mostly stable. However, there are sudden changes in third quarter of 2014 and first quarter of 2020.  

### 2.3. Transportation Price Index


```R
plot(priceindex)
```


    
![png](output_15_0.png)
    



```R
correl_info = cor(priceindex[,c("TP.FG.J07","TP.FG.J071","TP.FG.J072","TP.FG.J073")])

ggcorrplot(correl_info, hc.order=TRUE, type="lower", lab=TRUE)
```


    
![png](output_16_0.png)
    



```R
priceindex <- priceindex[,c('Date','TP.FG.J071')]
```


```R
ggplot(priceindex,aes(x=factor(year(Date)),y=TP.FG.J071))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Transportation Price Index by Year")
```


    
![png](output_18_0.png)
    


Descriptions of transportation price indexes

TP.FG.J07	07.Transportation

TP.FG.J071	071.Vehicle Purchase

TP.FG.J072	072.Personal Vehicle Management Cost

TP.FG.J073	073.Transportation Services

4 different price indexes that are related to the transportation industry seems to be corralate between each other. Thus only using one of them will be enough. TP.FG.J071 will be used for this purposes since it is the most contextually related one to the production of automobiles. It is also clear that there is and exponential and undisturbed rise in transportation price index between years 2009-2022

### 2.4. Vehicle Credit Interest Rates


```R
ggplot(creditinterest,aes(x=factor(month(Date)),y=Vehicle.Credit.Interest.Rates,colour = month(Date)))+ 
geom_bar(stat="identity",aes(fill=month(Date)),color="black")+ 
facet_wrap(~year(Date))+ 
labs(x="Months of Each Year",title="Monthly Vehicle Credit Interest Rates By Year")

ggplot(creditinterest,aes(x=factor(year(Date)),y=Vehicle.Credit.Interest.Rates))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="Vehicle Credit Interest Rates by Year")
```


    
![png](output_21_0.png)
    



    
![png](output_21_1.png)
    


It can be seen that variation and mean of vehicle interest rates hugely increased in last 5 years. This is also the same period of time that automobile production started to decrease. This might mean that volitile economic situation and following unpredictible interest rates maked buying cars harder. Thus, supply might be reduced to match the reduced demand. Of course this is only a guess that is made by looking the visual data. 

### 2.5. USD-TL Exchange Rates


```R
ggplot(exchangerates, aes(x=Date,y=USD.TL))+geom_line(color="red")+
    labs(x="Date",y="USD",title="USD - TL")
```


    
![png](output_24_0.png)
    



```R
ggplot(exchangerates,aes(x=factor(year(Date)),y=USD.TL))+ 
geom_boxplot(aes(fill=factor(year(Date))))+ theme(legend.position = "none")+ 
labs(x="Year",title="USD - TL")

```


    
![png](output_25_0.png)
    


It is clear that value of USD in terms of Turkish Lira has increased exponentially between years 2009-2022.

### 2.6. Related Search Terms

Monthly data about three different search terms that could be related to the automobile demand was taken from Google Trends. These serch terms are "Sıfır Araba", "Taşıt Sigortası", "Araba"


```R
plot(searchterms)

correl_info = cor(searchterms[,c("Sifir.Araba","Tasit.Sigortası","Araba")])

ggcorrplot(correl_info, hc.order=TRUE, type="lower", lab=TRUE)
```


    
![png](output_28_0.png)
    



    
![png](output_28_1.png)
    


There dos not seem to be a clear correlation between all of these terms. While there seems to be some correlation between search "araba" and others, considering all of them in our model would be beneficial.


```R
ggplot(searchterms[,c("Date","Sifir.Araba")],aes(x=Date,y=Sifir.Araba))+
geom_line(size = 1,color="red")+
geom_line(data=searchterms[,c("Date","Tasit.Sigortasi")],aes(x=Date,y=Tasit.Sigortasi),size = 1,color="blue")+
geom_line(data=searchterms[,c("Date","Araba")],aes(x=Date,y=Araba),size = 1,color="green")+
labs(x="Year", y="Search Terms (R: Sıfır Araba, B: Taşıt Sigortası, G: Araba)",title="Serch Terms Related To Automobiles")
```


    
![png](output_30_0.png)
    


## 3. Correlation


```R
acf(carproduction[,"AutomobileProduction"])
```


    
![png](output_32_0.png)
    


There is a significant correlation for the automobile production data for 12 month lag. This seasonalty should be addressed while creating the linear regression model. Seasonalty could be addressed by adding aditional categorical month variables.


```R
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
```


<table>
<thead><tr><th scope=col>Date</th><th scope=col>AutomobileProduction</th><th scope=col>SifirAraba</th><th scope=col>TasitSigortasi</th><th scope=col>Araba</th><th scope=col>EUBP</th><th scope=col>PriceIndex071</th><th scope=col>Interest</th><th scope=col>USDTL</th></tr></thead>
<tbody>
	<tr><td>2009-09-01</td><td>50320     </td><td>20        </td><td>18        </td><td>11        </td><td>65.82     </td><td>108.13    </td><td>15.67     </td><td>1.49      </td></tr>
	<tr><td>2009-10-01</td><td>45452     </td><td>18        </td><td>19        </td><td>11        </td><td>74.91     </td><td>113.70    </td><td>15.02     </td><td>1.46      </td></tr>
	<tr><td>2009-11-01</td><td>41635     </td><td>16        </td><td>13        </td><td>13        </td><td>77.77     </td><td>115.19    </td><td>14.07     </td><td>1.48      </td></tr>
	<tr><td>2009-12-01</td><td>48917     </td><td>20        </td><td>19        </td><td>12        </td><td>77.91     </td><td>113.19    </td><td>13.13     </td><td>1.50      </td></tr>
	<tr><td>2010-01-01</td><td>46810     </td><td>12        </td><td>23        </td><td>14        </td><td>71.20     </td><td>115.11    </td><td>13.18     </td><td>1.47      </td></tr>
	<tr><td>2010-02-01</td><td>46365     </td><td>16        </td><td>15        </td><td>13        </td><td>76.36     </td><td>115.06    </td><td>13.27     </td><td>1.51      </td></tr>
</tbody>
</table>




    
![png](output_34_1.png)
    



```R
plot(carproductiondata)
```


    
![png](output_35_0.png)
    


Transportation Price index and USD-TL seems to be strongly correlated. Because of this using only one of them is probably enoguh. Also, since both of them seems to be exponentially increasing using log values of these data would be more logical. Unsprusingly, oil price is the only variable that has negative correlation with all other. Roughly, decreases in oil prices coencide with increases in automobile production. There also seems to be some correlation between search term "araba" and USD-TL exchange rate data.


```R
acf(carproductiondata)
```


    
![png](output_37_0.png)
    



    
![png](output_37_1.png)
    



    
![png](output_37_2.png)
    



    
![png](output_37_3.png)
    


## 4. Conclusion

Summarizing all the analysis done, it can be said that,

- The information that is given by USD-TL exchange rates and Transportation Price Index is nearly identical. Thus using both time series will probably wont make a meaningfull difference. Just using one of them seems the most logical answer in further analysis.

- There is positive correlation between price index, exchange rate, oil prices and interest rates. Correlation between nearly all of these variables are significant

- Most of these variables, such as exchange rates, interest rates, and transportation price index has increased in tersm of variance after 2017. Which might be related to the decreasing automobile production numbers due to the volitile economiy and reduced customer demands.

- It is clear that for Automile Production data there is a 12 month seasonalty that should be addressed while creating a prediction model. This montly effect is especially clear for month of August which for whatever reason has a much lower production numbers compared to the other months of the year.

- Effects of Covid-19 pandemic can be seen in oil priceses and automobile production data clearly. For a regression model to be succesful, effects of pandemic should also be considered. This can be done by adding another variables that shows monthly covid cases in Turkey. Another choice could be adding a chategorical variable that marks the months that Turkey experienced covid-19 restrictions. Covid-19 can also be altogether disregarded as it can be seen as a once in a lifetime unprecedented event. In this case only the data from "normal" times can be used.

## 5. References

Data sets are obtained from Central Bank of the Republic of Turkey and Google Trends.
