
# ECON 753 PS1
# Jesús Lara
#Github version

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library("rio")
library("xlsx")
library(matlib)
library(gdata)


#### ################################Problem 1
### ############################A Pure replication part
####################################################################
########################################################################

#Set my working directory
setwd("C:/Users/User/Documents/Fall 2020 UMass/753/Problem Sets/Problem Set 1") 

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

emp_3<-emp_2 %>% rename("industry"="Industries (corresponding to I-O sector)","all_workers"="All workers (PS+SS)") %>% 
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


rm(list=setdiff(ls(), EBEnergy_1,EBgf_1,EBSector_1))





### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10
                         
jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-EBEnergy_3 %>% mutate(energy_names) %>% 
  relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

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

T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) %>% rename()

############## Table 11
T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

T11_3<-T11_2 %>% select(-1) %>% 
  mutate(Source) %>% relocate(Source,value) %>% rename("Jobs per million USD"=value ) #Final version of Table 11



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


rm(list=setdiff(ls(), EBEnergy_1,EBgf_1,EBSector_1))





### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10

jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-EBEnergy_3 %>% mutate(energy_names) %>% 
  relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

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

A1T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) %>% rename()

############## Table 11
T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

A1T11_3<-T11_2 %>% select(-1) %>% 
  mutate(Source) %>% relocate(Source,value) %>% rename("Jobs per million USD"=value ) #Final version of Table 11


######################################################################
######################################################################
### ALTERNATIVE wEIGHTS 2########################################################

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


rm(list=setdiff(ls(), EBEnergy_1,EBgf_1,EBSector_1))





### 3 main objects: EBEnergy_1 EBgf_1 EBSector_1 and weights
### Replicate table 10

jobs<-c("Direct + Indirect Jobs", "Direct Jobs", "Indirect Jobs" ) 


EBEnergy_2<-t(EBEnergy_1)


energy_names<-t(as.matrix(weights_1 %>% slice(22) %>% select(4:13)))

EBEnergy_3<-data.frame(EBEnergy_2)

colnames(EBEnergy_3)<-jobs

EBEnergy_4<-EBEnergy_3 %>% mutate(energy_names) %>% 
  relocate(energy_names, `Direct Jobs`, `Indirect Jobs`, `Direct + Indirect Jobs`)

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

A2T10<-T10_renew %>% add_row(T10_effic) %>% add_row(T10_fossil) %>% rename()

############## Table 11
T11<-data.frame(t(EBSector_1[1,])) %>% mutate(X4= GFweights_2[1,1]*X1+ GFweights_2[1,2]*X2,X5=100*(X4-X3)/X3)
T11_2<-T11 %>% pivot_longer(1:5)
Source<-c("Renewable Energy","Energy Efficiency","Fossil Fuels","Clean Energy Total", "Clean Energy relative to Fossil Fuels")

A2T11_3<-T11_2 %>% select(-1) %>% 
  mutate(Source) %>% relocate(Source,value) %>% rename("Jobs per million USD"=value ) #Final version of Table 11

rm(list= ls()[!(ls() %in% c('T10','T11_3','A1T10','A1T11_3','A2T10','A2T11_3'))])





#############################################################################
######################## PROBLEM 2 ##########################################
#############################################################################
#############################################################################

