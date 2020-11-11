#Econ 753
#PS5
#Jesús Lara

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library("rio")
library(matlib)
library(gdata)
library(tinytex)
library(car)
library(scales)
library(ggplot2)
library(foreign)
library(rmarkdown)
library(fastDummies)
library(haven)
library(pmdplyr)
library(plotrix)
library(foreign)
library(stringr)
library(alfred)
library(aTSA)
library(lmtest)  ## For Granger causality


options(scipen=10000)
options(digits=4)

rm(list=ls())

setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS5")

### Part 2

BAA  <- get_fred_series("BAA", series_name="BAA")   
FEDFUNDS  <- get_fred_series("FEDFUNDS", series_name="FEDFUNDS") 


ggplot(BAA, aes(x=date,y=BAA))+geom_line()

ggplot(FEDFUNDS, aes(x=date,y=FEDFUNDS))+geom_line()

#1
series<-merge(BAA, FEDFUNDS, by="date")
series<-series %>% filter(date>="1963-03-01" & date<="2009-06-01")

ggplot(series, aes(x=date))+theme_classic() + 
  geom_line(aes(y=BAA,color="black"),size=1)+
  geom_line(aes(y=FEDFUNDS,color="blue"),size=1)+
  xlab("Date")+
  ylab("Rate")+
  scale_color_discrete(name="Interest Rate", label=c("BAA","FEDFUNDS"))

BAA1<-adf.test(series$BAA)
FEDFUNDS1<-adf.test(series$FEDFUNDS)


## Create a Data Frame with results:

ADF1<-data.frame(BAA1$type1,BAA1$type2[,2:3],BAA1$type3[,2:3],FEDFUNDS1$type1[,2:3], FEDFUNDS1$type2[,2:3], FEDFUNDS1$type3[,2:3])
colnames(ADF1)<-c("Lag",rep(c("ADF", "P-Value"),6))
save(ADF1, file="ADF1.Rdata")

#The series are non-stationary :(


#2 First difference data


series<-series %>% mutate(d.BAA=c(NA,diff(BAA)),d.FEDFUNDS=c(NA,diff(FEDFUNDS))) #Chulada

BAA2<-adf.test(series$d.BAA)
FEDFUNDS2<-adf.test(series$d.FEDFUNDS)


## Create a Data Frame with results:

ADF2<-data.frame(BAA2$type1,BAA2$type2[,2:3],BAA2$type3[,2:3],FEDFUNDS2$type1[,2:3], FEDFUNDS2$type2[,2:3], FEDFUNDS2$type3[,2:3])
colnames(ADF2)<-c("Lag",rep(c("ADF", "P-Value"),6))
save(ADF2, file="ADF2.Rdata")

#### Granger Causality


#Different specifications: number of lags

lags<-c(2, 5, 10)
granger1<-data.frame()
granger2<-data.frame()
granger3<-data.frame()

for (lag in lags){
test1<-grangertest(d.FEDFUNDS ~d.BAA, order=lag, na.action=na.omit, data=series)
granger1<-rbind(granger1,c(lag, test1[2,3],test1[2,4]))
}

for (lag in lags){
  test1<-grangertest(d.BAA~d.FEDFUNDS, order=lag, na.action=na.omit, data=series)
  granger1<-rbind(granger1,c(lag, test1[2,3],test1[2,4]))
}

### THREE DIFFERENT CYCLES

#Before 1982

for (lag in lags){
  test1<-grangertest(d.FEDFUNDS ~d.BAA, order=lag, na.action=na.omit, data=filter(series,date<"1982-01-01"))
  granger2<-rbind(granger2,c(test1[2,3],test1[2,4]))
}

for (lag in lags){
  test1<-grangertest(d.BAA~d.FEDFUNDS, order=lag, na.action=na.omit, data=filter(series,date<"1982-01-01"))
  granger2<-rbind(granger2,c(test1[2,3],test1[2,4]))
}


#After 1882

for (lag in lags){
  test1<-grangertest(d.FEDFUNDS ~d.BAA, order=lag, na.action=na.omit, data=filter(series,date>="1982-01-01"))
  granger3<-rbind(granger3,c(test1[2,3],test1[2,4]))
}

for (lag in lags){
  test1<-grangertest(d.BAA~d.FEDFUNDS, order=lag, na.action=na.omit, data=filter(series,date>="1982-01-01"))
  granger3<-rbind(granger3,c(test1[2,3],test1[2,4]))
}

#Everything together

granger.table<-cbind(granger1,granger2,granger3)
colnames(granger.table)<-c("Lags", rep(c("F", "P-Value"),3))

save(granger.table,file="granger.table.Rdata")
