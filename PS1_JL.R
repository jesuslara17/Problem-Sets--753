
# ECON 753 PS1
# Jes�s Lara
#Github version

library(plyr)
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
options(scipen=10000)
options(digits=4)

rm(list=ls())

#### ################################Problem 1
### ############################A Pure replication part
####################################################################
########################################################################

#Set my working directory
setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753") 


#import the IO dataset, year 2009
IO_1<-import("IND_NIOT_row_sep12.xlsx",sheet="2009") 

### eliminate rows 1,2,4,5 and columns 1,4
IO_2<- IO_1 %>% slice(-c(1,2,4,5)) %>% select(-1)

#vector of names (industries) is row 1 of IO_2 minus the first two columns (they don't have name)
names<-IO_2 %>% slice(1) %>% select(-c(1,2,3))

#delete the row of names
IO_3<- IO_2%>% slice(-1)

#set names to all my columns: first two: industry, name, the combine with my vector names
colnames(IO_3)<-c("industry","var","industry_code", names)

#SUM rows (Domestic + Imports to get overall input expenditures)


IO_4<- IO_3 %>% select(-c(industry,var,industry_code)) %>%  
  mutate_if(is.character,as.numeric) %>%
  mutate(industry=IO_3$industry,var=IO_3$var,industry_code=IO_3$industry_code) %>% 
  relocate(industry,var,industry_code)

#I will now sum the expenditure of each industry's domestic and import
IO_5<- IO_4 %>% group_by(industry_code,industry) %>% summarise_if(is.numeric, sum) 

IO_6<-IO_5 %>% mutate(industry_code=sub('.','',industry_code)) %>% 
  mutate(industry_code=as.numeric(industry_code)) %>% 
  arrange(industry_code) %>% 
  mutate(industry_code=as.character(industry_code))


IO_7<-IO_6 %>% 
  select(-c(industry_code,industry,`Final consumption expenditure by households`:`Total output`) ) 


#Get Leontieff inverse


A_1<-as.matrix(IO_7[-c(36:43),-1])

total_output <- as.vector(IO_6[-c(36:43),44])

inv_total_output<-1/total_output



A_q<- A_1 * inv_total_output[col(A_1)]

L_1<-inv(diag(35)-A_q)

names_1<-as.vector(select(names, -c(36:42)))

colnames(L_1)<-names_1

### Import employment data

emp_1<-import("India-Input-Output Analysis--Employment Estimates--09132019.xlsx", sheet="EO Matrix") 
emp_2<-emp_1 %>% select(`Industries (corresponding to I-O sector)`,`All workers (PS+SS)`) %>% slice(-c(1,37:77))
colnames(emp_2)<-c("industry","all_workers")
emp_3<-emp_2 %>% 
  slice(-36)

emp_4<-emp_3 %>% mutate(total_output=total_output$`Total output`) %>% 
  mutate(all_workers=(as.numeric(all_workers))) %>% 
  mutate(EO=all_workers/total_output)

EOv<-emp_4$EO


EM_1<-data.frame(L_1) %>% mutate_all(.funs=list(E=~.*EOv)) #How to replace old variables?

EM_2<-EM_1 %>% select(36:70)
colnames(EM_2)<-names_1 ## This is the employment matrix

EM_T_1<-EM_2 %>% summarise_all(sum) 

#
dir_emp<-as.matrix(EM_2)
dir_emp<-(diag(dir_emp))
tdir_emp<-t(dir_emp)
tdir_emp<-data.frame(tdir_emp)

colnames(tdir_emp)<-names_1

EM_T_2<-EM_T_1 %>% add_row(slice(tdir_emp,1))

EM_T_3<-EM_T_2 %>% add_row(slice(EM_T_2,1)-slice(EM_T_2,2))


######################################################################
######################################################################
### Import weights########################################################

### weight given to each industry within the different ENERGY sectors
weights_1<-import("India-Input-Output Analysis--Employment Estimates--09132019.xlsx", sheet="Green Energy Program") 

weights_2<-weights_1 %>% slice(-c(1:9,20:42))

res<-weights_2$`(industry-by-industry)`

weights_3<-weights_2 %>% select(-c(1:3))

weights_3[is.na(weights_3)]=0

weights_4<-as.matrix(weights_3 %>% mutate_if(is.character,as.numeric))

EM_T_4<-as.matrix(EM_T_3 %>% mutate_if(is.character,as.numeric))

#w4 (10x35)  ||  t(w4) (35x10)
#EM_T_4(3x35)

EBEnergy_1<-EM_T_4 %*% t(weights_4) #Employment by energy sector matrix
 
####
Sweights_1<-weights_1 %>% slice(c(28:30)) %>% select(c(4:13))

Sweights_1[is.na(Sweights_1)]=0

Sweights_2<-as.matrix(Sweights_1 %>% mutate_if(is.character,as.numeric))

EBSector_1<-EBEnergy_1%*%t(Sweights_2) #### Employment by Sector

##############
GFweights_1<-weights_1 %>% slice(c(36,37)) %>% select(c(4:6))

GFweights_1[is.na(GFweights_1)]=0

GFweights_2<-as.matrix(GFweights_1 %>% mutate_if(is.character,as.numeric))

EBgf_1<-EBSector_1%*%t(GFweights_2)







### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10
                         
jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-data.frame(EBEnergy_3,energy_names) 

colnames(EBEnergy_4)<-c(jobs,"energy_names")
 
EBEnergy_4<-EBEnergy_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

############# Include weighted averages

EB_sector_2<-t(EBSector_1)

wav<-c("Weighted Average for Renewables","Weighted Average for Efficiency", "Weighted Average for Fossil Fuels")
  



EBsector_3<-data.frame(EB_sector_2) 


EBsector_4<-EBsector_3%>% mutate(energy_names=wav) 
colnames(EBsector_4)<-c(jobs, "energy_names")

EBsector_5<-EBsector_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

#################Table 10 final

T10_renew<-EBEnergy_4 %>%  slice(1:5) %>% add_row(slice(EBsector_5,1))
T10_effic<-EBEnergy_4 %>%  slice(6:8) %>% add_row(slice(EBsector_5,2))
T10_fossil<-EBEnergy_4 %>% slice(9:10)%>% add_row(slice(EBsector_5,3))

T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) 
rownames(T10)<-c()
T10
############## Table 11
T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

T11_3<-T11_2 %>% select(-1) 

T11_3<-data.frame(T11_3, Source) 

T11_4<-T11_3 %>%relocate(Source,value)


colnames(T11_4)=c("Source", "Jobs per million USD")


####Replicate table A1

Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
colnames(Sweights_1)<-Category
sector<-c("Renewable Energy", "Energy Efficiency", "Fossil Fuel")
Sectors<-Sweights_1 %>% mutate(sector) %>% relocate(sector)
Sectors<-Sectors %>% pivot_longer(c(2:11),names_to="I-O Industry", values_to="Weights") %>% 
  group_by(sector) %>% filter(Weights>0) %>% rename(Category=sector) %>% ungroup()


colnames(weights_3)<-names_1
Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
A1<-weights_3 %>% mutate(Category) %>% relocate(Category)
A1<-A1 %>% pivot_longer(c(2:36),names_to="I-O Industry",values_to="Weights")
A1<-A1 %>% group_by(Category) %>% filter(Weights>0) %>% ungroup()

A1<-A1 %>% add_row(Sectors) %>%mutate(Weights=100*as.numeric(Weights)) %>% mutate_if(is.numeric, round, digits = 2)

save(A1,file="A1.Rdata")

############################ #####
#### PART B Alternative weights###
##################################

######################################################
### Alternative Weights 1: at the (sub)sectoral level
############################################################

######################################################################
######################################################################
### Import weights########################################################

### weight given to each industry within the different ENERGY sectors
weights_1<-import("India-Input-Output Analysis--Employment Estimates--09132019.xlsx", sheet="Green Energy Program AW1") 

weights_2<-weights_1 %>% slice(-c(1:9,20:42))

res<-weights_2$`(industry-by-industry)`

weights_3<-weights_2 %>% select(-c(1:3))

weights_3[is.na(weights_3)]=0

weights_4<-as.matrix(weights_3 %>% mutate_if(is.character,as.numeric))

EM_T_4<-as.matrix(EM_T_3 %>% mutate_if(is.character,as.numeric))

#w4 (10x35)  ||  t(w4) (35x10)
#EM_T_4(3x35)

EBEnergy_1<-EM_T_4 %*% t(weights_4) #Employment by energy sector matrix

####
Sweights_1<-weights_1 %>% slice(c(28:30)) %>% select(c(4:13))

Sweights_1[is.na(Sweights_1)]=0

Sweights_2<-as.matrix(Sweights_1 %>% mutate_if(is.character,as.numeric))

EBSector_1<-EBEnergy_1%*%t(Sweights_2) #### Employment by Sector

##############
GFweights_1<-weights_1 %>% slice(c(36,37)) %>% select(c(4:6))

GFweights_1[is.na(GFweights_1)]=0

GFweights_2<-as.matrix(GFweights_1 %>% mutate_if(is.character,as.numeric))

EBgf_1<-EBSector_1%*%t(GFweights_2)







### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10

jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-data.frame(EBEnergy_3,energy_names) 

colnames(EBEnergy_4)<-c(jobs,"energy_names")

EBEnergy_4<-EBEnergy_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

############# Include weighted averages

EB_sector_2<-t(EBSector_1)

wav<-c("Weighted Average for Renewables","Weighted Average for Efficiency", "Weighted Average for Fossil Fuels")




EBsector_3<-data.frame(EB_sector_2) 


EBsector_4<-EBsector_3%>% mutate(energy_names=wav) 
colnames(EBsector_4)<-c(jobs, "energy_names")

EBsector_5<-EBsector_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

#################Table 10 final

T10_renew<-EBEnergy_4 %>%  slice(1:5) %>% add_row(slice(EBsector_5,1))
T10_effic<-EBEnergy_4 %>%  slice(6:8) %>% add_row(slice(EBsector_5,2))
T10_fossil<-EBEnergy_4 %>% slice(9:10)%>% add_row(slice(EBsector_5,3))

A1_T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) 
rownames(A1_T10)<-c()

############## Table 11
T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

T11_3<-T11_2 %>% select(-1) 

T11_3<-data.frame(T11_3, Source) 

A1_T11_4<-T11_3 %>%relocate(Source,value)


colnames(A1_T11_4)=c("Source", "Jobs per million USD")

####Replicate table A1

Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
colnames(Sweights_1)<-Category
sector<-c("Renewable Energy", "Energy Efficiency", "Fossil Fuel")
Sectors<-Sweights_1 %>% mutate(sector) %>% relocate(sector)
Sectors<-Sectors %>% pivot_longer(c(2:11),names_to="I-O Industry", values_to="Weights") %>% 
  group_by(sector) %>% filter(Weights>0) %>% rename(Category=sector) %>% ungroup()


colnames(weights_3)<-names_1
Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
A1<-weights_3 %>% mutate(Category) %>% relocate(Category)
A1<-A1 %>% pivot_longer(c(2:36),names_to="I-O Industry",values_to="Weights")
A1<-A1 %>% group_by(Category) %>% filter(Weights>0) %>% ungroup()

A1_A1<-A1 %>% add_row(Sectors) %>%mutate(Weights=100*as.numeric(Weights)) %>% mutate_if(is.numeric, round, digits = 2)

save(A1_A1,file="A1_A1.Rdata")

######################################################################
######################################################################
### ALTERNATIVE WEIGHTS 2########################################################

### weight given to each industry within the different ENERGY sectors
weights_1<-import("India-Input-Output Analysis--Employment Estimates--09132019.xlsx", sheet="Green Energy Program AW2") 

weights_2<-weights_1 %>% slice(-c(1:9,20:42))

res<-weights_2$`(industry-by-industry)`

weights_3<-weights_2 %>% select(-c(1:3))

weights_3[is.na(weights_3)]=0

weights_4<-as.matrix(weights_3 %>% mutate_if(is.character,as.numeric))

EM_T_4<-as.matrix(EM_T_3 %>% mutate_if(is.character,as.numeric))

#w4 (10x35)  ||  t(w4) (35x10)
#EM_T_4(3x35)

EBEnergy_1<-EM_T_4 %*% t(weights_4) #Employment by energy sector matrix

####
Sweights_1<-weights_1 %>% slice(c(28:30)) %>% select(c(4:13))

Sweights_1[is.na(Sweights_1)]=0

Sweights_2<-as.matrix(Sweights_1 %>% mutate_if(is.character,as.numeric))

EBSector_1<-EBEnergy_1%*%t(Sweights_2) #### Employment by Sector

##############
GFweights_1<-weights_1 %>% slice(c(36,37)) %>% select(c(4:6))

GFweights_1[is.na(GFweights_1)]=0

GFweights_2<-as.matrix(GFweights_1 %>% mutate_if(is.character,as.numeric))

EBgf_1<-EBSector_1%*%t(GFweights_2)







### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10

jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-data.frame(EBEnergy_3,energy_names) 

colnames(EBEnergy_4)<-c(jobs,"energy_names")

EBEnergy_4<-EBEnergy_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

############# Include weighted averages

EB_sector_2<-t(EBSector_1)

wav<-c("Weighted Average for Renewables","Weighted Average for Efficiency", "Weighted Average for Fossil Fuels")




EBsector_3<-data.frame(EB_sector_2) 


EBsector_4<-EBsector_3%>% mutate(energy_names=wav) 
colnames(EBsector_4)<-c(jobs, "energy_names")

EBsector_5<-EBsector_4 %>%   relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

#################Table 10 final

T10_renew<-EBEnergy_4 %>%  slice(1:5) %>% add_row(slice(EBsector_5,1))
T10_effic<-EBEnergy_4 %>%  slice(6:8) %>% add_row(slice(EBsector_5,2))
T10_fossil<-EBEnergy_4 %>% slice(9:10)%>% add_row(slice(EBsector_5,3))

A2_T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) 
rownames(A2_T10)<-c()

############## Table 11

T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

T11_3<-T11_2 %>% select(-1) 

T11_3<-data.frame(T11_3, Source) 

A2_T11_4<-T11_3 %>%relocate(Source,value)


colnames(A2_T11_4)=c("Source", "Jobs per million USD")



####Replicate table A1

Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
colnames(Sweights_1)<-Category
sector<-c("Renewable Energy", "Energy Efficiency", "Fossil Fuel")
Sectors<-Sweights_1 %>% mutate(sector) %>% relocate(sector)
Sectors<-Sectors %>% pivot_longer(c(2:11),names_to="I-O Industry", values_to="Weights") %>% 
  group_by(sector) %>% filter(Weights>0) %>% rename(Category=sector) %>% ungroup()
  

colnames(weights_3)<-names_1
Category<-c("Bioenergy","Solar","Wind","Geothermal","Hydro","Weatherization and
Building Retrofits","Industrial Energy Efficiency","Grid Upgrades", "Coal", "Oil and Gas")
A1<-weights_3 %>% mutate(Category) %>% relocate(Category)
A1<-A1 %>% pivot_longer(c(2:36),names_to="I-O Industry",values_to="Weights")
A1<-A1 %>% group_by(Category) %>% filter(Weights>0) %>% ungroup()

A2_A1<-A1 %>% add_row(Sectors) %>%mutate(Weights=100*as.numeric(Weights)) %>% mutate_if(is.numeric, round, digits = 2)

save(A2_A1,file="A2_A1.Rdata")

rm(list= ls()[!(ls() %in% c('T10','T11_4','A1_T10','A1_T11_4','A2_T10','A2_T11_4'))])

#####################################
#####################################

#Save my objects for rmarkdown

save(T10, file = "Table_10_Replication.RData")
save(T11_4, file = "Table_11_Replication.RData")

save(A1_T10, file="Table_10_A1.RData")
save(A1_T11_4, file = "Table_11_A1.RData")


save(A2_T10, file="Table_10_A2.RData")
save(A2_T11_4, file = "Table_11_A2.RData")

load("A1.Rdata")
load("A1_A1.Rdata")
load("A2_A1.Rdata")

A1_F<-A1 %>% mutate("My Weights 1"=A1_A1$Weights, "My Weights 2"=A2_A1$Weights)
save(A1_F,file="A1_F.Rdata")




#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
######################## PROBLEM 2 ##########################################
#############################################################################
#############################################################################


rm(list=ls())


## Read the individual country data (dumped from RR.xls with Save All to CSV)
Australia     <- read.csv("RR - Australia.csv") 
Austria       <- read.csv("RR - Austria.csv")   
Belgium       <- read.csv("RR - Belgium.csv")   
Canada        <- read.csv("RR - Canada.csv")    
Denmark       <- read.csv("RR - Denmark.csv")   
Finland       <- read.csv("RR - Finland.csv")   
France        <- read.csv("RR - France.csv")    
Germany       <- read.csv("RR - Germany.csv")   
Greece        <- read.csv("RR - Greece.csv")    
Ireland       <- read.csv("RR - Ireland.csv")   
Italy         <- read.csv("RR - Italy.csv")     
Japan         <- read.csv("RR - Japan.csv")     
Netherlands   <- read.csv("RR - Netherlands.csv")
NewZealand    <- read.csv("RR - New Zealand.csv")
Norway        <- read.csv("RR - Norway.csv")
Portugal      <- read.csv("RR - Portugal.csv")
Spain         <- read.csv("RR - Spain.csv")
Sweden        <- read.csv("RR - Sweden.csv")
UK            <- read.csv("RR - UK.csv")
US            <- read.csv("RR - US.csv")

## Stack the data
RR <- merge(Australia,Austria,all=TRUE)
RR <- merge(RR,Belgium    ,all=TRUE)
RR <- merge(RR,Canada     ,all=TRUE)
RR <- merge(RR,Denmark    ,all=TRUE)
RR <- merge(RR,Finland    ,all=TRUE)
RR <- merge(RR,France     ,all=TRUE)
RR <- merge(RR,Germany    ,all=TRUE)
RR <- merge(RR,Greece     ,all=TRUE)
RR <- merge(RR,Ireland    ,all=TRUE)
RR <- merge(RR,Italy      ,all=TRUE)
RR <- merge(RR,Japan      ,all=TRUE)
RR <- merge(RR,Netherlands,all=TRUE)
RR <- merge(RR,NewZealand ,all=TRUE)
RR <- merge(RR,Norway     ,all=TRUE)
RR <- merge(RR,Portugal   ,all=TRUE)
RR <- merge(RR,Spain      ,all=TRUE)
RR <- merge(RR,Sweden     ,all=TRUE)
RR <- merge(RR,UK         ,all=TRUE)
RR <- merge(RR,US         ,all=TRUE)

with(RR,table(Country))

## Convert public debt/GDP
RR$debtgdp <- RR$debtgdp

write.dta(RR,"RR-basic.dta")

## Follow rules in RR working spreadsheet for calculating public debt/GDP for each country-year
RR <- within(RR, debtgdp <- ifelse(Country=="Australia",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Austria",ifelse(Year<=1979,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Belgium",ifelse(Year<=1979,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Canada",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Denmark",ifelse(Year<=1949,100*Debt1/GDP1,100*Debt1/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Finland",ifelse(Year<=1977,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="France",ifelse(Year<=1948, 100*Debt1 / GDP1, ifelse(Year<=1978, 100*Debt1 / GDP2,100*Debt2/GDP2)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Germany",ifelse(Year<=1950,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Greece",ifelse((Year>=1884 & Year<=1913) | (Year>=1919 & Year<=1939) | (Year>=1970 & Year<=1992),100*Debt/GDP1, ifelse(Year==2009,100,debtgdp)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Ireland",ifelse(Year<=2010,100*Debt/GDP2,debtgdp),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Italy",ifelse(Year<=1913,100*Debt/GDP1,ifelse(Year<=1946,100*Debt/GNI,ifelse(Year<=1998,100*Debt/GDP1,100*Debt/GDP2))),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Japan",ifelse(Year<=1953,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Netherlands",ifelse(Year<=1956,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="New Zealand",ifelse(Year<=1947,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Norway",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Portugal",ifelse(Year<=1999,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Spain",ifelse(Year<=1957,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Sweden",ifelse(Year<=1949,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="UK" , 100*Debt/GDP, debtgdp ))
RR <- within(RR, debtgdp <- ifelse(Country=="US" , 100*Debt/GDP, debtgdp ))

RR$RGDP <- as.numeric(RR$RGDP)
RR$RGDP1 <- as.numeric(RR$RGDP1)
RR$RGDP2 <- as.numeric(RR$RGDP2)

## Calculate real GDP growth using rules per RR spreadsheet 
lg<-function(x)c(NA,x[1:(length(x)-1)])
RR <- ddply( RR, ~Country, transform, lRGDP=lg(RGDP), lRGDP1=lg(RGDP1), lRGDP2=lg(RGDP2)  )
RR <- within(RR, dRGDP <- ifelse( !is.na(dRGDP), dRGDP,
                                  ifelse( !is.na( RGDP / lRGDP - 1 ), 100*(RGDP / lRGDP - 1) ,
                                          ifelse( !is.na( RGDP2 / lRGDP2 - 1 ), 100*(RGDP2 / lRGDP2 - 1) ,
                                                  ifelse( !is.na( RGDP1 / lRGDP1 - 1 ), 100*(RGDP1 / lRGDP1 - 1),dRGDP )))))

write.dta(RR,"RR-200-processed.dta")

## Cut to postwar analysis
RR <- subset(RR,Year>=1946 & Year<=2009)

## Italy uses another data series through 1946 and is excluded from GITD postwar until 1951
RR <- subset(RR, !(Year<1951 & Country=="Italy"))

## Potential data years 1946-2009
avail.data <- ddply(RR, ~Country, summarize,
                    count.year.GDP=sum(!is.na(dRGDP)),count.year.debt=sum(!is.na(debtgdp)), count.year.both=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"count.year.both"]),]

write.dta(RR,"RR-processed.dta")
## Slow
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)
write.xlsx2(subset(RR,TRUE,select=c(Country,Year,debtgdp,dgcat,dRGDP) ),"RR-keycolumns.xlsx",row.names=FALSE)


## Limit to actually available data
RR <- subset(RR,  !is.na(dRGDP) & !is.na(debtgdp))

## Actually available data years 1946-2009
avail.data <- ddply(RR, ~Country, summarize, min.year=min(Year), count.year=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"min.year"]),]

with(RR,table(Year))
with(RR,table(Country))

## Create RR public debt/GDP categories
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)

## Create expanded public debt/GDP categories
RR$dgcat2.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,120,Inf))
RR$dgcat2 <- factor(RR$dgcat2.lm, labels = c("0-30%","30-60%","60-90%","90-120%","Above 120%"),ordered=TRUE)

## Regression analysis of categories
summary(dgcat.lm <- lm(dRGDP ~ dgcat.lm, data=RR))
summary(dgcat2.lm <- lm(dRGDP ~ dgcat2.lm, data=RR))
linearHypothesis(dgcat2.lm,
                 verbose=TRUE,
                 paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]", "dgcat2.lm(30,60]=dgcat2.lm(120,Inf]") ))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(60,90]=dgcat2.lm(90,120]")))
linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("dgcat2.lm(30,60]=dgcat2.lm(90,120]")))

linearHypothesis(dgcat2.lm, verbose=TRUE, paste( c("(Intercept) + dgcat2.lm(90,120] = 3") ))


## Country-Year average by debtgdp ("correct weights")
## Table 3 Corrected
(RR.correct.sd <- with(RR, tapply( dRGDP, dgcat, sd, na.rm=TRUE )))
(RR.correct.mean <- with(RR, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
RR.correct.mean.df <- data.frame(RR.correct.mean, dgcat=names(RR.correct.mean) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
## NYT Appendix input to Table 1 Line 3
(RR.equalwt.median <- with(RR, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Table 3 Country equal weighting
summary(RR.equalwt.mean)

## Country-Year average by debtgdp ("correct weights") expanded categories
(RR.correct.mean.2 <- with(RR, tapply( dRGDP, dgcat2, mean, na.rm=TRUE )))
RR.correct.mean.2.df <- data.frame(RR.correct.mean.2, dgcat=names(RR.correct.mean.2) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.ex.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat2), mean, na.rm=TRUE )))
summary(RR.ex.equalwt.mean)


## Selective treatment of early years
RR.selective <- subset(RR,
                       !((Year<1950 & Country=="New Zealand") | (Year<1951 & Country=="Australia") | (Year<1951 & Country=="Canada") ))
(RR.selective.mean <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
(RR.selective.median <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Equal weights
## Table 3 Weights,Exclusion
summary(RR.selective.mean)
## Correct weights
## Table 3 Selective years exclusion
with(RR.selective, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Spreadsheet error 
RR.spreadsheet <- subset(RR, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
(RR.spreadsheet.mean <- with(RR.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
(RR.spreadsheet.median <- with(RR.spreadsheet, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Table 3 Spreadsheet, Weights
summary(RR.spreadsheet.mean)
## Table 3 Spreadsheet error
with(RR.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Selective treatment of early years and spreadsheet error
RR.selective.spreadsheet <- subset(RR.selective, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
RR.selective.spreadsheet.mean <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
(RR.selective.spreadsheet.eqweight.median <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), median, na.rm=TRUE )))
## Equal weights
## Table 3 Weights,Exclusion,Spreadsheet Error
summary(RR.selective.spreadsheet.mean)
## Correct weights
## Table 3 Exclusion,Spreadsheet Error
with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Actually available data years 1946-2009 with selective exclusion and spreadsheet error
avail.data <- ddply(RR.selective.spreadsheet, ~Country, summarize, min.year=min(Year), count.year=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"min.year"]),]

## And New Zealand transcription error
## selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
RR.selective.spreadsheet.mean.transcription <- RR.selective.spreadsheet.mean
RR.selective.spreadsheet.mean.transcription["New Zealand",4] <- -7.9
summary(RR.selective.spreadsheet.mean.transcription)
## Table 3 Weights,Exclusion,Spreadsheet Error,Transcription
(RR.published.mean <- apply(RR.selective.spreadsheet.mean.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )


## Medians
## NYT Appendix Table 1 Line 4
(RR.correct.median <- with(RR, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.correct.selective.median <- with(RR.selective, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.correct.spreadsheet.median <- with(RR.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
## NYT Appendix Table 1 Line 3 (Use Median line)
(RR.eqweight.median <- summary(RR.equalwt.median))
summary(RR.spreadsheet.median)
## NYT Appendix Table 1 Line 2 (Dataset is "RR.selective" because it EXCLUDES early years but spreadsheet is corrected)
summary(RR.selective.median)
(RR.correct.ex.median <- with(RR, tapply( dRGDP, dgcat2, median, na.rm=TRUE )))
(RR.selective.spreadsheet.mean.median <- with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.published.median <- apply(RR.selective.spreadsheet.eqweight.median,2,median,na.rm=TRUE))


###############################################
########## Replicate Figure 2 here
###############################################
###############################################
###############################################


figure2<-data.frame(RR.published.mean,RR.published.median)
  
figure2<-data.frame(RR.published.mean.df,RR.published.median)
colnames(figure2)<-c ("Average","Debt_GDP_cat","Median")

figure2<-figure2 %>% relocate(Debt_GDP_cat,Average,Median)
rownames(figure2)<-c()  

figure2<-figure2 %>% pivot_longer(c(2,3), names_to="central",values_to="GDP growth") %>% mutate(c(1:8))


F2<- figure2 %>% ggplot(aes(x=Debt_GDP_cat, y=`GDP growth`, fill=central)) + geom_bar(stat="identity",width=0.5, position=position_dodge())
print(F2)
ggsave("F2.png")

############### PREVALENCE OF PUBLIC-DEBT CATEGORIES
######################################################

options(digits=4)
## Counts of years
prev_1<-data.frame(with(RR, table(Country,dgcat)))
prev_1<- prev_1%>% pivot_wider(names_from=dgcat, values_from=Freq)
save(prev_1,file="prev_1.Rdata")



prev_2<-data.frame(apply(with(RR,table( Country,dgcat)),2,sum))

debt_categories<-rownames(prev_2)
rownames(prev_2)<-c()
prev_2<- data.frame(debt_categories, prev_2)
colnames(prev_2)<-c("Debt Categories", "Frequency")

prev_2<-prev_2 %>% pivot_wider(names_from="Debt Categories", values_from=Frequency)

save(prev_2,file="prev_2.Rdata")





#### Prevalence of GDP growth by years


RR$yearcat.lm <- cut(RR$Year, breaks=c(1945,1950,1960,1970,1980,1990,2000,2010))
RR$yearcat <- factor(RR$yearcat.lm, labels = c("1946-1950","1951-1960","1961-1970","1971-1980",
                                               "1981-1990","1991-2000","2000-2010"),ordered=TRUE)

prevGDP<-RR %>% select(Country, yearcat, dRGDP) 

prevGDP<-prevGDP%>% group_by(Country,yearcat) %>% summarise_if(is.numeric,mean)
prevGDP<-prevGDP %>% pivot_wider(names_from="yearcat",values_from=dRGDP)
save(prevGDP,file="prevGDP.Rdata")



### Debt cat by year
prev_A<-data.frame(with(RR, table(yearcat,dgcat))) %>% pivot_wider(names_from=dgcat, values_from=Freq)
save(prev_A,file="prev_A.Rdata")

################################
########Figure 1 Herndon
################################


RR.newzealand.1951 <- subset(RR.selective.spreadsheet,Country=="New Zealand" & Year==1951)

RR.newzealand.1951 <- subset(RR.selective.spreadsheet,Country=="New Zealand" & Year==1951)


## Categorical scatterplot
n <- ggplot(RR, aes(x=dgcat,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
n <- n + geom_point(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean), shape=5,  size=5 ) 
n <- n + geom_text(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean,label=round(RR.published.mean,1)),hjust=-0.7,size=3,color='darkgray')
n <- n + geom_point(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=RR.correct.mean), shape=16, size=4 )  + theme_bw()
n <- n + geom_text(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=round(RR.correct.mean,1)), hjust=1.7,size=3,color='darkgray')
n <- n + geom_point(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP), shape=0, size=3 )
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste(round(dRGDP,1))), hjust=-0.7,size=3,color='darkgray')
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste("NZ",Year)), hjust=1.2,size=3,color='darkgray')
print(n)
ggsave("Figure_1_Herndon.png")

########### End Figure 1 Herndon et al.

## Create legend for categorical scatterplot
plot(3,10,pch=0,ylim=c(0,70),xlim=c(0,5.5))
text(3.2,10,"New Zealand 1951",adj=0)
points(0,15,pch=16)
text(0.2,15,"Correct average real GDP growth",adj=0)
points(0,10,pch=5,cex=1.5)
text(0.2,10,"RR average real GDP growth",adj=0)
points(3,15,pch=3,col='darkgray')
text(3.2,15,"Country-Year real GDP growth",adj=0)

######### FIgure 2 Herndon et al.


## Categorical scatterplot for expanded categories
o <- ggplot(RR, aes(x=dgcat2,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
o <- o + geom_point(RR.correct.mean.2.df,  mapping=aes(x=dgcat,y=RR.correct.mean.2), shape=16, size=4 )  + theme_bw()
o <- o + geom_text(RR.correct.mean.2.df, mapping=aes(x=dgcat,y=RR.correct.mean.2,label=round(RR.correct.mean.2,1)), hjust=1.7, size=3,color='darkgray')
print(o)
ggsave("Figure_2_Herndon.png")

############## End of figure 2       #####

#Figure 4

library(mgcv)
RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=RR)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
m <- RR %>% ggplot(aes(x=debtgdp,y=dRGDP))
m1 <- m + geom_vline(xintercept=90,color='lightgray',size=1.5)
m1 <- m1 + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
## m1 <- m1 + geom_smooth(method='loess',span=1.0,color='black') + geom_smooth(method='loess',span=0.2,color='black')
m1 <- m1 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))  + coord_cartesian(ylim=c(-0,7), xlim=c(0,150))
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1)

ggsave("Figure_4_Herndon.png")


###########
###########
###########
################Data reorganization: Lagged GDP per Capita and year categories 

RR<-RR %>% mutate(L1.dRGDP=lag(dRGDP),L2.dRGDP=lag(L1.dRGDP),neoliberal=ifelse(Year>1979,1,0)) 
RR<-RR %>% mutate(neoliberal=as.character(neoliberal))


gn<-RR%>% ggplot(aes(x=debtgdp,y=dRGDP,color=neoliberal))+geom_point()+geom_smooth(method="lm",se=FALSE)

print(gn)
ggsave("gn.png")


gn<-RR%>% ggplot(aes(x=debtgdp,y=dRGDP,color=neoliberal))+geom_point()+geom_smooth(method="lm",se=FALSE)

print(gn)
ggsave("gn.png")

gnL1<-RR%>% ggplot(aes(x=debtgdp,y=L1.dRGDP,color=neoliberal))+geom_point()+geom_smooth(method="lm",se=FALSE)

print(gn)
ggsave("gnL1.png")


gnL2<-RR%>% ggplot(aes(x=debtgdp,y=L2.dRGDP,color=neoliberal))+geom_point()+geom_smooth(method="lm",se=FALSE)

print(gn)
ggsave("gnL2.png")

gycat<-RR%>% ggplot(aes(x=debtgdp,y=dRGDP,color=yearcat))+geom_point()+geom_smooth(method="lm",se=FALSE)

print(gycat)
ggsave("gycat.png")

### Now I wil focus just on the richest economies

coeff=0.1
myplot<-RR %>% filter(Country=="US"| Country=="Japan") %>% 
  ggplot(aes(x=Year, y=dRGDP, color=Country))+ scale_y_continuous(sec.axis=sec_axis( trans=~./
                                                                            coeff, name="Debt/GDP"))

myplot<-myplot+ geom_line(aes(y=dRGDP),size=1)+geom_point(aes(y=debtgdp*coeff))     
myplot
ggsave("myplot.png")

