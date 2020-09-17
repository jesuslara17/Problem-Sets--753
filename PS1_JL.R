
# ECON 753 PS1
# Jesús Lara


library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library("rio")
library("xlsx")
library(matlib)

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





A_1<-as.matrix(IO_7[-c(36:43),-1])

total_output <- as.vector(IO_6[-c(36:43),44])

inv_total_output<-1/total_output


### multiply each cell by the inverse of total output
A_q<- A_1 * inv_total_output[col(A_1)]

### Leontief Matrix about to come!!! <3 <3 <3 

LM_1<-inv(diag(35)-A_q) # Leontief Matrix


