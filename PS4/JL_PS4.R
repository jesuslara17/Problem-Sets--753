#Econ 753
#PS4
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


options(scipen=10000)
options(digits=4)

rm(list=ls())

setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS4")

# PART 2 #

data<-import("FRB_Z1.csv")
data<-data %>% slice(-c(1:5))
data<-data %>% mutate(across(everything(),as.numeric))

names<-colnames(data)
names<-str_replace_all(names, "Nonfinancial corporate business; ","")
names<-str_replace_all(names," ", ".")
names<-str_replace_all(names, ";", "_")
colnames(data)<-names

data<-data %>% rename("Total_internal_funds"="gross.saving.including.foreign.earnings.retained.abroad.less.net.capital.transfers.paid",
                      "FI"="gross.fixed.investment",
                      "Net_acquisition_assets"="total.financial.assets",
                      "Net_lending_borrowing"="net.lending.(+).or.borrowing.(-).(financial.account)",
                      "year"="Series.Description") 

data<-data %>% select(year, Total_internal_funds, FI, Net_acquisition_assets, Net_lending_borrowing)
data<-data %>% mutate(Net_increase_liabilities=Net_acquisition_assets-Net_lending_borrowing)
data<-data %>% mutate(IF.FI.ratio=Total_internal_funds/FI, NIL.FI.ratio=Net_increase_liabilities/FI)

plot1<-ggplot(data, aes(x=year))+ 
  geom_line(aes(y=IF.FI.ratio,color="x"),size=1)+
  geom_line(aes(y=NIL.FI.ratio,color="y"),size=1) + 
  theme_bw() + xlab("Year") +ylab("Ratio") + 
  scale_color_discrete(name="Flow Type",labels=c("Internal Funds/Fixed Investment", "Net Increase in Liabilities/ Fixed Investment"))+
  theme(legend.position = "bottom")
  
plot1<-plot1+scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2020))
  
plot1
ggsave("PS4plot1.png")


plot2<-ggplot(data, aes(x=year))+ theme_bw()+
  geom_line(aes(y=Total_internal_funds,color="wheat"),size=1)+
  geom_line(aes(y=Net_increase_liabilities, color="green4"),size=1)+
  geom_line(aes(y=FI, color="green5"),size=1)+
  xlab("Year")+ylab("Flow")+
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2020))+
  scale_color_discrete(name="Flow Type",labels=c("Net Increase in Liabilities","Fixed Investment", "Internal Funds"))+
  theme(legend.position = "bottom")
plot2
ggsave("PS4plot2.png")

ggplot(data, aes(x=year))+ theme_bw()+
  geom_line(aes(y=Total_internal_funds,color="wheat"),size=1)+
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2020))+
  theme(legend.position = "bottom")


