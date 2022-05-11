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

setwd("C:/Users/Umut/Desktop/dersler/IE 360/HW2")
gasoline<-read.csv(file="IE360_Spring22_HW2_data.csv",header=TRUE)
colnames(gasoline) <- c("Date","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")

str(gasoline)

gasoline$Date<-gsub('_',' ', gasoline$Date)
gasoline$UGS<-gsub(' ','', gasoline$UGS)
gasoline$NLPG<-gsub(' ','', gasoline$NLPG)
gasoline$NUGV<-gsub(' ','', gasoline$NUGV)
gasoline$GNPA<-gsub(' ','', gasoline$GNPA)
gasoline$GNPC<-gsub(' ','', gasoline$GNPC)
gasoline$GNPT<-gsub(' ','', gasoline$GNPT)

gasoline$UGS <- as.numeric(gasoline$UGS) 
gasoline$NLPG <- as.numeric(gasoline$NLPG) 
gasoline$NUGV <- as.numeric(gasoline$NUGV) 
gasoline$GNPA <- as.numeric(gasoline$GNPA) 
gasoline$GNPC <- as.numeric(gasoline$GNPC) 
gasoline$GNPT <- as.numeric(gasoline$GNPT) 

gasoline <- gasoline %>% mutate(Year = case_when(
    grepl("2000", gasoline$Date, fixed = TRUE) ~ 2000,    
    grepl("2001", gasoline$Date, fixed = TRUE) ~ 2001,    
    grepl("2002", gasoline$Date, fixed = TRUE) ~ 2002,    
    grepl("2003", gasoline$Date, fixed = TRUE) ~ 2003,    
    grepl("2004", gasoline$Date, fixed = TRUE) ~ 2004,    
    grepl("2005", gasoline$Date, fixed = TRUE) ~ 2005,    
    grepl("2006", gasoline$Date, fixed = TRUE) ~ 2006,    
    grepl("2007", gasoline$Date, fixed = TRUE) ~ 2007 ))

gasoline <- gasoline %>% mutate(Quarter = case_when(
  grepl("Q1", gasoline$Date, fixed = TRUE) ~ 1,    
  grepl("Q2", gasoline$Date, fixed = TRUE) ~ 2,    
  grepl("Q3", gasoline$Date, fixed = TRUE) ~ 3,    
  grepl("Q4", gasoline$Date, fixed = TRUE) ~ 4 ))

gasoline$Date <- paste(gasoline$Year, gasoline$Quarter, sep="-") %>% yq() %>% as.Date()
head(gasoline)
str(gasoline)

gasolinetraining <- gasoline[-c((nrow(gasoline)-3):nrow(gasoline)), ]

gasolinets <- ts(gasolinetraining[c(2,3,4,5,6,7,8,9,10,11)], freq=4, start=c(2000,1))

plot(gasolinets[,c(1:5)],main="Time Series",xlab="Date")
plot(gasolinets[,c(6:10)],main="Time Series",xlab="Date")

ggplot(gasolinetraining, aes(x=Date,y=UGS))+geom_line(color="red")+
  labs(x="Date",y="UGS",title="Quarterly Gasoline Sales")

ggplot(gasolinetraining,aes(x=factor(Quarter),y=UGS,colour = Quarter))+
  geom_bar(stat="identity", aes(fill=Quarter),color="black")+
  facet_wrap(gasolinetraining$Year)+
  labs(title="Quarterly UGS")

ggplot(gasolinetraining,aes(x=factor(Quarter),y=UGS))+
  geom_boxplot(aes(fill=factor(Quarter)))+
  theme(legend.position = "none")+
  labs(x="Quarter",title="Quarterly Sales Deviation")

correl_info = cor(gasolinetraining[2:13])

ggcorrplot(correl_info, type="lower", lab=TRUE)

acf(gasolinetraining$UGS, main="Autocorrelation of UGS")

addetive<-decompose(gasolinets[,1], type="additive")
plot(addetive)

multiplicative<-decompose(gasolinets[,1], type="multiplicative")
plot(multiplicative)

deseasonalized <- gasolinets[,1]/multiplicative$seasonal
acf(gasolinets[,1])
ts.plot(deseasonalized)
acf(deseasonalized)

detrend<-deseasonalized/multiplicative$trend
ts.plot(detrend)
acf(detrend, na.action = na.pass, lag = 12)

model <- lm(UGS~Year+factor(Quarter), data=gasoline)
summary(model)
#Adjusted R-squared:  0.8966 

model <- lm(UGS~Year+factor(Quarter)+RNUV, data=gasoline)
summary(model)
#Adjusted R-squared:  0.909 Adding just RNUV improved model slightly

model <- lm(UGS~Year+factor(Quarter)+NLPG, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9313 Adding just NLPG improved model significantly

model <- lm(UGS~Year+factor(Quarter)+PU, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9062  Adding just PU improved model sliglty

model <- lm(UGS~Year+factor(Quarter)+PG, data=gasoline)
summary(model)
#Adjusted R-squared:  0.901  Adding just PG did not improved model much

model <- lm(UGS~Year+factor(Quarter)+NUGV, data=gasoline)
summary(model)
#Adjusted R-squared:  0.8933  Adding just NUGV reduced the model accuracy

model <- lm(UGS~Year+factor(Quarter)+NDGV, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9191   Adding just NDGV improved model sliglty

model <- lm(UGS~Year+factor(Quarter)+GNPA, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9083   Adding just GNPA improved model sliglty

model <- lm(UGS~Year+factor(Quarter)+GNPC, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9177    Adding just GNPC improved model significantly

model <- lm(UGS~Year+factor(Quarter)+GNPT, data=gasoline)
summary(model)
#Adjusted R-squared:  0.9149    Adding just GNPT improved model significantly

model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter), data=gasoline)
summary(model)
plot(model)
#Adjusted R-squared:  0.9602 

gasoline$UGSlag5 <- lag(gasoline$UGS, n = 5L, default = NA)
gasoline$UGSlag4 <- lag(gasoline$UGS, n = 4L, default = NA)

model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+UGSlag4, data=gasoline)
summary(model)

model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+UGSlag5, data=gasoline)
summary(model)

gasoline$RNUVlag1 <- lag(gasoline$RNUV, n = 1L, default = NA)
gasoline$NLPGlag1 <- lag(gasoline$NLPG, n = 1L, default = NA)
gasoline$PUlag1 <- lag(gasoline$PU, n = 1L, default = NA)
gasoline$PGlag1 <- lag(gasoline$PG, n = 1L, default = NA)
gasoline$NUGVlag1 <- lag(gasoline$NUGV, n = 1L, default = NA)
gasoline$NDGVlag1 <- lag(gasoline$NDGV, n = 1L, default = NA)
gasoline$GNPAlag1 <- lag(gasoline$GNPA, n = 1L, default = NA)
gasoline$GNPClag1 <- lag(gasoline$GNPC, n = 1L, default = NA)
gasoline$GNPTlag1 <- lag(gasoline$GNPT, n = 1L, default = NA)

model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+RNUVlag1, data=gasoline)
summary(model)
# Adding RNUVlag1 imrpoved the model: Adjusted R-squared = 0.9679 
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+NLPGlag1, data=gasoline)
summary(model)
# Adding NLPGlag1 imrpoved the model: Adjusted R-squared =  0.9726 
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+PUlag1, data=gasoline)
summary(model)
# Adding PUlag1 imrpoved the model: Adjusted R-squared =  0.9658 
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+PGlag1, data=gasoline)
summary(model)
# Adding PGlag1 imrpoved the model: Adjusted R-squared =  0.9662 
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+NUGVlag1, data=gasoline)
summary(model)
# Adding NUGVlag1 imrpoved the model: Adjusted R-squared = 0.9674  
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+NDGVlag1, data=gasoline)
summary(model)
# Adding NDGVlag1 imrpoved the model: Adjusted R-squared = 0.9677  
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+GNPAlag1, data=gasoline)
summary(model)
# Adding GNPAlag1 imrpoved the model: Adjusted R-squared = 0.9658  
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+GNPClag1, data=gasoline)
summary(model)
# Adding GNPClag1 imrpoved the model: Adjusted R-squared =  0.9747  
model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+GNPTlag1, data=gasoline)
summary(model)
# Adding GNPTlag1 imrpoved the model: Adjusted R-squared =  0.971 

model <- lm(UGS~GNPC+NDGV*PG+GNPA*NLPG+Year+factor(Quarter)+GNPClag1+NLPGlag1+RNUVlag1, data=gasoline)
summary(model)
plot(model)

gasoline$predicted <- predict(model, gasoline)
gasoline$residuals <- gasoline$UGS-gasoline$predicted

#Comparing residuals and variables of the model

ggplot(gasoline[2:28,], aes(x=UGS))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs Actual values")

ggplot(gasoline[2:28,], aes(x=predicted))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs Predicted values")

ggplot(gasoline[2:28,], aes(x=GNPC))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs GNPC")
ggplot(gasoline[2:28,], aes(x=NDGV))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs NDGV")
ggplot(gasoline[2:28,], aes(x=PG))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs PG")
ggplot(gasoline[2:28,], aes(x=NLPG))+geom_point(aes(y=residuals),color="red")+
  labs(title="Residuals vs NLPG")

ggplot(gasoline[1:28,], aes(x=predicted))+geom_point(aes(y=UGS),color="red")+
  labs(title="Actual vs predicted values")+
  geom_abline(slope=1)

ggplot(gasoline, aes(x=Date))+geom_line(aes(y=UGS),color="red")+
  geom_line(aes(y=predicted),color="blue")+
  labs(x="Date",y="UGS",title="Quarterly Gasoline Sales (Red) vs Predicted Values (Blue)")

gasoline$predicted[29:32]

ggplot(gasoline[29:32,], aes(x=Date))+
  geom_line(aes(y=predicted),color="blue")+
  labs(x="Date",y="UGS",title="Predicted Values for 2007")
