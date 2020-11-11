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

BAA1<-adf.test(series$BAA)
FEDFUNDS1<-adf.test(series$FEDFUNDS)


## Create a Data Frame with results:

ADF1<-data.frame(BAA1$type1,BAA1$type2[,2:3],BAA1$type3[,2:3],FEDFUNDS1$type1[,2:3], FEDFUNDS1$type2[,2:3], FEDFUNDS1$type3[,2:3])
colnames(ADF1)<-c("Lag",rep(c("ADF", "P-Value"),6))
save(ADF1, file="ADF1.Rdata")

#The series are non-starionary :(


#2 First difference data


series<-series %>% mutate(d.BAA=c(NA,diff(BAA)),d.FEDFUNDS=c(NA,diff(FEDFUNDS))) #Chulada

BAA2<-adf.test(series$d.BAA)
FEDFUNDS2<-adf.test(series$d.FEDFUNDS)


## Create a Data Frame with results:

ADF2<-data.frame(BAA2$type1,BAA2$type2[,2:3],BAA2$type3[,2:3],FEDFUNDS2$type1[,2:3], FEDFUNDS2$type2[,2:3], FEDFUNDS2$type3[,2:3])
colnames(ADF2)<-c("Lag",rep(c("ADF", "P-Value"),6))
save(ADF2, file="ADF2.Rdata")



