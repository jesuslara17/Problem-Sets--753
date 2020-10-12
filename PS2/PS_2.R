library(dplyr)
library(tidyr)
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
library(stargazer)


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

lmb<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,filter(chow, YEAR>=60))
summary(lm4_1)

coefficients<-data.frame(coef(lmb))%>% slice(-c(1:4)) 

coefficients<-coefficients %>% rename("Coefficient"="coef.lmb.") %>% 
  mutate("Price Index"=exp(Coefficient))

#(e) Dealing with heteroscedasticity 

### Dividing all variables by sqrt of volume
chow2<-chow %>% 
  mutate(across(c(LNRENT, LNMULT, LNACCESS, LNADD, LNMEM,D61, D62, D63, D64, D65),
                list(w=~./sqrt(VOLUME)),.names="{col}"))

lme<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,filter(chow2, YEAR>=60))
summary(lme)

###Using the embedded weighted least squares command in R

lmew<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,weights=VOLUME,data=filter(chow, YEAR>=60))


stargazer(lmb, lme,lmew, type = "text", style = "default", intercept.bottom = FALSE, column.labels = c("OLS", "WLS Manual","WLS Auto"))

## Problem 6

#(a Estimating adjacent years)

chow3<-chow %>% mutate(D54=ifelse(YEAR==54,1,0),
                       D55=ifelse(YEAR==55,1,0),
                       D56=ifelse(YEAR==56,1,0),
                       D57=ifelse(YEAR==57,1,0),
                       D58=ifelse(YEAR==58,1,0),
                       D59=ifelse(YEAR==59,1,0),
                       D60=ifelse(YEAR==60,1,0),)

beta <- vector()
beta[1] = 0
for(YEAR in 54:64) { 
  assign(paste("CHOWModel",YEAR,YEAR+1,".lm",SEP=""), lm( paste("LNRENT ~ LNMULT + LNACCESS + LNMEM + D", YEAR+1,sep =""), data = chow3 %>% filter(YEAR==YEAR | YEAR==YEAR+1)))
  beta[YEAR-52] <- print(coef(get(paste("CHOWModel",YEAR,YEAR+1,".lm",SEP="")))[5])
} 

xpooled.lm <- lm(LNRENT ~ LNMULT + LNACCESS + LNMEM + factor(YEAR), data =chow)


###############################
####### PART 2 ##############
rm(list=ls())


#Import CPI-U
cpiu<-import("CPI_U.xls",sheet="Monthly") 
cpiu<-cpiu %>% mutate(year=c(rep(1968:2019,each=12),rep(2020, each=8)),
                      month=c(rep(1:12,length(c(1968:2019))),1:8)) %>% 
                      select(-DATE) %>% rename("cpiu"=CPIAUCSL)

# Import and clean C-CPI-U
ccpiu<-import("C_CPI_U.xls") 
colnames(ccpiu)<- ccpiu %>% slice(10)
ccpiu<-ccpiu %>% slice(-c(1:10))

ccpiu<-ccpiu %>% mutate(year=c(1999, rep(2000:2019,each=12),rep(2020, each=8)),
       month=c(12,rep(1:12,length(c(2000:2019))),1:8)) %>% select(-observation_date) %>%
  rename("ccpiu"="SUUR0000SA0")




#Import and clean CPI-U-RS

cpiurs<-import("r-cpi-u-rs-allitems.xlsx") 
colnames(cpiurs)<-cpiurs %>% slice(5)
cpiurs<-cpiurs %>% slice(-c(1:5))
colnames(cpiurs)<-c("year",1:12,"AVG")

cpiurs<-cpiurs %>% select(-AVG)%>% 
  pivot_longer(cols=c("1":"12"),names_to="month",values_to="cpiurs") 


### Merging the 3 indexes
all_index<-merge(cpiu,ccpiu, by=c("year","month"),all=TRUE) 


all_index<-merge(all_index,cpiurs, by=c("year","month"),all=TRUE) 

all_index <- all_index %>%
  mutate(across(where(is.character),as.numeric))

###Take averages and then calculation inflation

inflation<-all_index %>% 
  group_by(year) %>% 
  summarise(across(c(cpiu,ccpiu,cpiurs), mean))
  
  
inflation<-inflation %>% 
  mutate(across(c(cpiu,ccpiu,cpiurs), list(l=lag), .names="{fn}.{col}")) %>% 
  mutate(infl_cpiu=100*(cpiu-l.cpiu)/l.cpiu,
         infl_ccpiu=100*(cpiu-l.cpiu)/l.ccpiu,
         infl_cpiurs=100*(cpiurs-l.cpiurs)/l.cpiurs)

inflation_plot<-inflation %>% select(year,infl_cpiu,infl_ccpiu) %>% 
  pivot_longer(c(infl_cpiu,infl_ccpiu),names_to="index",values_to="inflation")
  
### ggplotting inflation :O


plot1<-inflation_plot %>% 
  ggplot(aes(x=year, y=inflation, color=index))+geom_line(size=1.5) + 
  theme_bw() +ylab("Inflation")+xlab("Date")
plot1
