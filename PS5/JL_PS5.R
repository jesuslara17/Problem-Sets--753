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
