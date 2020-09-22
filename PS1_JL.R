
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

EM_T_2<-EM_T_1 %>% add_row(slice(EM_2,1))

EM_T_3<-EM_T_2 %>% add_row(slice(indirect_employment,1))

indirect_employment<- EM_2 %>% slice(-1) %>% summarise_all(sum)

### Import weights

weights_1<-import("India-Input-Output Analysis--Employment Estimates--09132019.xlsx", sheet="Green Energy Program") 

weights_2<-weights_1 %>% slice(-c(1:9,20:42))

res<-weights_2$`(industry-by-industry)`

weights_3<-weights_2 %>% select(-c(1:3))

weights_3[is.na(weights_3)]=0

weights_4<-as.matrix(weights_3)
EM
