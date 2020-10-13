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
library(naniar)


options(scipen=10000)
options(digits=4)

rm(list=ls())

################### PROBLEM 1 ##########################################
########################################################################

#Set my working directory
setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS2")

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


#Correlation data
cor_chow_59<-cor(cor_chow %>% filter(YEAR<60) %>% select(-YEAR))
save(cor_chow_59,file="cor59.Rdata")

cor_chow_65<-cor(cor_chow %>% filter(YEAR>=60) %>% select(-YEAR))     
save(cor_chow_65,file="cor65.Rdata")


#(b)

lmb<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,filter(chow, YEAR>=60))
save(lmb,file="lmb.Rdata")

lmb_reg<-stargazer(lmb, type = "text", style = "default", intercept.bottom = FALSE, column.labels = c("OLS"))

reg_index<-data.frame(Year=c(1960:1965),Coefficients=c(NA,coef(lmb)[5:9]))
reg_index<-reg_index %>% mutate(Price_Index=ifelse(Year==1960,1,exp(Coefficients)))


rownames(reg_index)<-c()
save(reg_index,file="reg_index.Rdata")

#(e) Dealing with heteroscedasticity 


### Dividing all variables by sqrt of volume
chow2<-chow %>% 
  mutate(across(c(LNRENT, LNMULT, LNACCESS, LNADD, LNMEM,D61, D62, D63, D64, D65),
                list(w=~.*sqrt(VOLUME)),.names="{col}"))

lme<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,filter(chow2, YEAR>=60))
summary(lme)

###Using the embedded weighted least squares command in R

lmew<-lm(LNRENT~ LNMULT+LNMEM+LNACCESS+D61+D62+D63+D64+D65,weights=sqrt(VOLUME),data=filter(chow, YEAR>=60))


p3<-stargazer(lmb, lme,lmew , type = "latex", style = "default", intercept.bottom = FALSE, column.labels = c("OLS", "WLS Manual","WLS Auto"))
save(p3,file="p3.tex")

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

beta<-c(0.00000000, -0.06745630, -0.13118744, -0.12639924, -0.25751042, -0.20202314, -0.50355965, -0.08546431, -0.28575199, -0.12956105, -0.31576839,-0.21823508)


###Pooled regression
xpooled.lm <- lm(LNRENT ~ D55+ D56+D57+D58+D59+D60+D61+D62+D63+D64+D65+LNMULT + LNACCESS + LNMEM, data =chow3)

reg_index2<-data.frame(Year=1954:1965, Pooled=c(0,coef(xpooled.lm)[2:12]))

reg_index2<-reg_index2 %>% mutate(Pooled_Dif=Pooled-lag(Pooled), chained_coef=beta)

colnames(reg_index2) <- c("Year", "Pooled Cooefficients", "Changes in Pooled Coefficients",
                                "Adjacent Year Coefficients")
rownames(reg_index2) <- c()
priceIndexTable2 <- format(reg_index2, digits=3)
save(reg_index2, file="reg_index2.Rdata")

### Problem 6

sixBtable <-format(mutate(data.frame(pooled_Coef=coef(xpooled.lm)[2:12],coef_Summed=cumsum(priceIndexTable2[,4][2:12])),
                          pooled_Indices=exp(pooled_Coef), summed_Indices=exp(coef_Summed)),digits=2, scientific=8)


sixBtable <- rbind(c(0,0,1,1),sixBtable)
rownames(sixBtable) <- c()
reg_index3 <- data.frame(year=1954:1965, pooled_Indices=sixBtable$pooled_Indices,
                               summed_Indices=sixBtable$summed_Indices)
colnames(reg_index3) <- c("Year", "Pooled CPI Indices", "Chained CPI Indices")
save(reg_index3,file="reg_index3.Rdata")


###############################
###############################
####### PART 2 ################
rm(list=ls())
options(digits=5)


#Import and clean CPI-U 
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

#Import and clean CPI-U-X1
cpiux1<-import("ERP-2012-table62.xls")
cpiux1<-cpiux1 %>% select(1,10:12)  
cpiux1<-cpiux1 %>% slice(-c(1:3))
colnames(cpiux1) <-c("year","cpiux1","cpiurs","ccpiu")
cpiux1<-cpiux1 %>% slice(-c(45:73))
cpiux1<-cpiux1 %>% replace_with_na(replace=list(cpiurs= "......",ccpiu="......")) %>% 
  mutate(across(where(is.character),as.numeric))

cpiux1<-cpiux1 %>% mutate(lcpiux1=lag(cpiux1)) %>% 
  mutate(inf_cpiux1=100*(cpiux1-lcpiux1)/lcpiux1) #Rate of change (inflation) of CPI-U-X1



# Import and clean Minimum Wage Data
min_wage<-import("Min_wage.xlsx",sheet=2)
min_wage<-min_wage %>% slice(-c(1:5)) %>% select(c(1,2))
colnames(min_wage)<-c("year","mwage")
min_wage<-min_wage %>% mutate(across(where(is.character),as.numeric))

# Merging the 3 indexes
all_index<-merge(cpiu,ccpiu, by=c("year","month"),all=TRUE) 
all_index<-merge(all_index,cpiurs, by=c("year","month"),all=TRUE) 
all_index <- all_index %>%
  mutate(across(where(is.character),as.numeric))

#Take averages and then calculation inflation
inflation<-all_index %>% 
  group_by(year) %>% 
  summarise(across(c(cpiu,ccpiu,cpiurs), mean))


cpiu2018<-as.matrix(inflation  %>% filter(year==2018)%>% select(cpiu))[1,1] #Base year=2018
ccpiu2018<-as.matrix(inflation  %>% filter(year==2018)%>% select(ccpiu))[1,1]
cpiurs2018<-as.matrix(inflation  %>% filter(year==2018)%>% select(cpiurs))[1,1]

  

inflation<-inflation %>% mutate(cpiu=cpiu/(0.01*cpiu2018),
                                ccpiu=ccpiu/(0.01*ccpiu2018),
                                cpiurs=cpiurs/(0.01*cpiurs2018)) %>% 
  mutate(across(c(cpiu,ccpiu,cpiurs), list(l=lag), .names="{fn}.{col}")) %>% 
  mutate(infl_cpiu=100*(cpiu-l.cpiu)/l.cpiu,
         infl_ccpiu=100*(cpiu-l.cpiu)/l.ccpiu,
         infl_cpiurs=100*(cpiurs-l.cpiurs)/l.cpiurs) 



inflation<-inflation %>% 
  mutate(infl_cpiurs=ifelse(year<1979,cpiux1$inf_cpiux1, infl_cpiurs)) #Add cpiux1 for the first years

#re-arrange the data from for ggplot purposes
inflation_plot<-inflation %>%  
  pivot_longer(c(infl_cpiu,infl_ccpiu,infl_cpiurs),names_to="index",values_to="inflation")
  

# Plot 1: inflation with CPI-U, C-CPI-U and C-CPI-U-RS
plot1<-inflation_plot %>% 
  ggplot(aes(x=year, y=inflation, color=index))+geom_line(size=1.2) + 
  theme_bw() +ylab("Inflation")+xlab("Date")+
  scale_color_discrete(name="Index", labels=c("C-CPI-U", "CPI-U","C-CPI-URS"))
plot1
ggsave("plot1.png")


#MINIMUM WAGE 
#For plots 2 and 3:


#Plot 2
min_wage_infl<- min_wage %>%mutate(inflation %>% filter(year<2020) %>%  select(cpiu)*0.01)
min_wage_infl<- min_wage_infl %>% mutate(mwage_2018=mwage/cpiu)

plot2<-min_wage_infl %>% ggplot(aes(x=year,y=mwage_2018))+geom_line(color="red",size=1.5) +
theme_bw()+ylab("Real Wage")+xlab("Year")
plot2
ggsave("plot2.png")

#plot3

min_wage_infl<- min_wage %>%mutate(inflation %>% filter(year<2020) %>%  select(infl_cpiurs)*0.01)
min_wage_infl<- min_wage_infl %>% mutate(hype_Wage=vector(mode="double", length=52))
min_wage_infl$hype_Wage[1] <- min_wage_infl$mwage[1]

for (val in 2:52){
  min_wage_infl$hype_Wage[val] <- min_wage_infl$hype_Wage[val-1] * (1 + min_wage_infl$infl_cpiurs[val])
}

plot3<-min_wage_infl %>% ggplot(aes(x=year))+
  geom_line(aes(y=mwage,color="blue"),size=1)+
  geom_line(aes(y=hype_Wage,color="purple"),size=1) +theme_bw()+
  ylab("Wage")+xlab("Year")+  
  scale_color_discrete(name="Legend", labels=c("Hypothetical Wage", "Actual Nominal Wage"))
plot3
ggsave("plot3.png")


