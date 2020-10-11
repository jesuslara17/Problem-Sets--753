library(dplyr)
library(data.table)
library(ggplot2)
library("rio")
library(matlib)
library(gdata)
library(tinytex)
library(scales)
library(ggplot2)
library(foreign)
library(rmarkdown)
library(fastDummies)

options(scipen=10000)
options(digits=4)

rm(list=ls())

################### PROBLEM 1 ##########################################
########################################################################

#Set my working directory
#setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS2")

chow <- fread('https://courses.umass.edu/econ753/berndt/chap4.dat/chow')

#(a) Construct appropiate variables and get correlation matrices

chow<-chow %>%   mutate(across(where(is.integer), as.numeric))%>%
  mutate(across(c(RENT, MULT, ACCESS, ADD),list(LN=log),.names="{fn}{col}"),
                      MEM=WORDS*BINARY*DIGITS) %>% 
  mutate(LNMEM=log(MEM)) %>%
  mutate(D61=ifelse(YEAR==61,1,0),
                      D62=ifelse(YEAR==62,1,0),
                      D63=ifelse(YEAR==63,1,0),
                      D64=ifelse(YEAR==64,1,0),
                      D65=ifelse(YEAR==65,1,0))

cor_chow<-chow %>% select(c(YEAR,LNRENT:LNMEM)) %>% select(-MEM)

cor_chow_59<-cor(cor_chow %>% filter(YEAR<60) %>% select(-YEAR))


cor_chow_65<-cor(cor_chow %>% filter(YEAR>=60) %>% select(-YEAR))     


#(b)

lm4_1<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,filter(chow, YEAR>=60))
summary(lm4_1)

coefficients<-data.frame(coef(lm4_1))%>% slice(-c(1:4))

coefficients<-coefficients %>% rename("Coefficient"="coef.lm4_1.") %>% 
  mutate("Price Index"=exp(Coefficient))

coeff2<-chow %>%filter(YEAR> 60) %>%  group_by(YEAR) %>% summarise(mLNMULT=mean(LNMULT),mLNMEM=mean(LNMEM),mLNACCESS=mean(LNACCESS)) %>% ungroup()

coefficients<-data.frame(coefficients,coeff2)

coefficients<-coefficients %>% mutate(PI= exp(Coefficient*w1))
                                             