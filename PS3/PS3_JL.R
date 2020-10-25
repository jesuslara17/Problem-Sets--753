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
library(haven)
library(pmdplyr)
library(plotrix)


options(scipen=10000)
options(digits=4)

rm(list=ls())
dev.off

################### PART2  ##########################################
########################################################################

#Set my working directory
setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS3")

#Replicate rows 1-3 of the panel "Stores by State" of Table 3 in Card and Krueger, "Minimum Wages and Employment: 
#A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania" (AER 84(4) 1994). The material for Lab 07 via 
#git provides the Card and Krueger data in several formats. Discuss your method and findings in relation to the original.

fastfood<-read_dta("fastfood.dta")
fastfood<- fastfood %>% mutate(state=ifelse(state==1,"NJ","PA"))

fastfood_empft<-fastfood %>% select(sheet, state, empft, empft2 ) 
fastfood_emppt<-fastfood %>% select(sheet, state, emppt, emppt2) 
fastfood_wage<-fastfood %>% select(sheet, state, wage_st, wage_st2) 
fastfood_wage<-fastfood_wage %>%mutate(wage_cat= ifelse(wage_st==4.25,"L",
                                ifelse(wage_st<5,"M",
                                ifelse(wage_st>=5,"H",NA))))

fastfood_man<-fastfood %>% select(sheet, state, nmgrs, nmgrs2) 


#Pivot Longer empft

fastfood_empft<-fastfood_empft %>% pivot_longer(c(empft,empft2), values_to= "empft", names_to="post")
fastfood_empft<-fastfood_empft %>% mutate(post=ifelse(post=="empft", 0,1))

#Pivot Longer emppt
fastfood_emppt<-fastfood_emppt %>% pivot_longer(c(emppt,emppt2), values_to= "emppt", names_to="post")
fastfood_emppt<-fastfood_emppt %>% mutate(post=ifelse(post=="emppt", 0,1))

#Pivot Longer wage

fastfood_wage<-fastfood_wage %>% pivot_longer(c(wage_st,wage_st2), values_to= "wage_st", names_to="post")
fastfood_wage<-fastfood_wage %>% mutate(post=ifelse(post=="wage_st", 0,1))

#Pivot Longer managers and assistants

fastfood_man<-fastfood_man %>% pivot_longer(c(nmgrs,nmgrs2), values_to= "nmgrs", names_to="post")
fastfood_man<-fastfood_man %>% mutate(post=ifelse(post=="nmgrs", 0,1))



# merge everything

fastfood2<-merge(merge(fastfood_empft,fastfood_emppt),merge(fastfood_wage,fastfood_man))


#Panel Means 1
fastfood2<-fastfood2 %>% mutate(fte1=empft+0.5*emppt,
                                fte2=empft+nmgrs+0.5*emppt)
fte<-fastfood2 %>% group_by(post,state) %>% summarise(across(fte2, mean ,na.rm=TRUE)) %>% ungroup()
fte<-fte %>% pivot_wider(names_from=state,values_from=fte2) %>% mutate(NJminusPA=NJ-PA)
fte<-fte %>% rbind(fte[2,]-fte[1,])

#Panel 2 Means



w<-fastfood2 %>% group_by(post, state, wage_cat) %>% summarise(across(fte2, mean, na.rm=TRUE)) %>%
 ungroup() %>% filter(state=="NJ")

w<-w %>% pivot_wider(names_from=wage_cat, values_from=fte2)
w<-w %>% select(-c(post,state,"NA"))
w<-w %>% rbind(w[2,]-w[1,])


#Panel 3 Means
w.between<-w %>% mutate(LminusH=L-H, MminusH=M-H) 
w.between<-w.between %>% select(LminusH, MminusH)




#table3<-table3 %>% rename("Variable"="post", 
#                          "Difference, NJ-PA"="NJminusPA",
#                          "Wage=$4.25"="L",
#                          "Wage=$4.26-$4.99"="M",
#                          "Wage>4.99"="H",
#                          "Low-high"="LminusH",
#                          "Midrange-high"="MminusH")







#### Standard errors!
### Generate individual variables

fteNJ0=filter(fastfood2, state=="NJ",post==0)$fte2
fteNJ1=filter(fastfood2, state=="NJ",post==1)$fte2
ftePA0=filter(fastfood2, state=="PA",post==0)$fte2
ftePA1=filter(fastfood2, state=="PA",post==1)$fte2

fteNJ0L=filter(fastfood2, state=="NJ",post==0, wage_cat=="L")$fte2
fteNJ1L=filter(fastfood2, state=="NJ",post==1, wage_cat=="L")$fte2

fteNJ0M=filter(fastfood2, state=="NJ",post==0, wage_cat=="M")$fte2
fteNJ1M=filter(fastfood2, state=="NJ",post==1, wage_cat=="M")$fte2

fteNJ0H=filter(fastfood2, state=="NJ",post==0, wage_cat=="H")$fte2
fteNJ1H=filter(fastfood2, state=="NJ",post==1, wage_cat=="H")$fte2


fte.std<-fastfood2 %>% group_by(state,post) %>% summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE))

w.std<-fastfood2 %>% group_by(post, state, wage_cat) %>% 
  summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE)) %>% 
  ungroup() %>% filter(state=="NJ")

fte.std<-fte.std %>% pivot_wider(names_from=state,values_from=std.fte2) 
w.std<-w.std%>% pivot_wider(names_from=wage_cat, values_from=std.fte2)


### Add the standard error of the differences

#Panel 1 differences standard errors
diff_within_state<-c(1, t.test(ftePA0,ftePA1)$stderr, t.test(fteNJ0,fteNJ1)$stderr,t.test(fteNJ0-fteNJ1,ftePA0-ftePA1)$stderr)
diff_between_state<-c(t.test(fteNJ0,ftePA0)$stderr,t.test(fteNJ1,ftePA1)$stderr)


#Panel 2 differences standard errors

diff_within_wagecat<-c(t.test(fteNJ0L,fteNJ1L)$stderr,t.test(fteNJ0M,fteNJ1M)$stderr,t.test(fteNJ0H,fteNJ1H)$stderr)



#colnames(diff_within_state)<-colnames(fte.std)
#fte.std<-fte.std %>% mutate(NJminusPA=diff_between_state)
#fte.std<-fte.std %>% rbind(diff_within_state)


#Panel 3 standard errors


diff_between_wagecat0<-c(t.test(fteNJ0L,fteNJ0H)$stderr,t.test(fteNJ0M,fteNJ0H)$stderr)
diff_between_wagecat1<-c(t.test(fteNJ1L,fteNJ1H)$stderr,t.test(fteNJ1M,fteNJ1H)$stderr)
dd_time_wagecat<-c(t.test(fteNJ1L-fteNJ0L,fteNJ1H-fteNJ0H)$stderr, t.test(fteNJ1M-fteNJ0M,fteNJ1H-fteNJ0H)$stderr)
  
### Putting everything together
#Panel1
fte.std<-fastfood2 %>% group_by(state,post) %>% summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE))
fte.std<-fte.std %>% pivot_wider(names_from=state,values_from=std.fte2) 

fte.std<-fte.std %>% mutate(NJminusPA=diff_between_state)
fte.std<-fte.std %>% rbind(diff_within_state)

###Panel 2

w.std<-fastfood2 %>% group_by(post, state, wage_cat) %>% 
  summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE)) %>% 
  ungroup() %>% filter(state=="NJ")

w.std<-w.std %>% pivot_wider(names_from=wage_cat, values_from=std.fte2)
w.std<-w.std %>% select(-c(post, state,"NA"))
w.std<-w.std %>% rbind(diff_within_wagecat)


###Panel 3

w.std.between<-data.frame(t(diff_between_wagecat0)) 
w.std.between<-w.std.between%>% rbind(diff_between_wagecat1,dd_time_wagecat)


#Putting everything together

#Panel 1


format(fte,digits=2)
format(fte.std,digits=2)

x1<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), function(i) paste0(round(fte[1,i],digits=2)," (",round(fte.std[1,i],digits=2),")"))))
x2<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), function(i) paste0(round(fte[2,i],digits=2)," (",round(fte.std[2,i],digits=2),")"))))
x3<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), function(i) paste0(round(fte[3,i],digits=2)," (",round(fte.std[3,i],digits=2),")"))))

panel_1 <-x1 %>% rbind(x2,x3)
rm(x1,x2,x3)

# Format panel2
y1<-as.data.frame(do.call(cbind,lapply(1:ncol(w), function(i) paste0(round(w[1,i],digits=2)," (",round(w.std[1,i],digits=2),")"))))
y2<-as.data.frame(do.call(cbind,lapply(1:ncol(w), function(i) paste0(round(w[2,i],digits=2)," (",round(w.std[2,i],digits=2),")"))))
y3<-as.data.frame(do.call(cbind,lapply(1:ncol(w), function(i) paste0(round(w[3,i],digits=2)," (",round(w.std[3,i],digits=2),")"))))

panel_2<-y1 %>% rbind(y2,y3)
rm(y1,y2,y3)

#Format panel 3

z1<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), function(i) paste0(round(w.between[1,i],digits=2)," (",round(w.std.between[1,i],digits=2),")"))))
z2<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), function(i) paste0(round(w.between[2,i],digits=2)," (",round(w.std.between[2,i],digits=2),")"))))
z3<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), function(i) paste0(round(w.between[3,i],digits=2)," (",round(w.std.between[3,i],digits=2),")"))))

panel_3<-z1 %>% rbind(z2,z3) 
rm(z1,z2,z3)

#table3<-table3 %>% rename("Variable"="post", 
#                          "Difference, NJ-PA"="NJminusPA",
#                          "Wage=$4.25"="L",
#                          "Wage=$4.26-$4.99"="M",
#                          "Wage>4.99"="H",
#                          "Low-high"="LminusH",
#                          "Midrange-high"="MminusH")

#Panel 1 final
colnames(panel_1)<-c("Variable","NJ","PA","NJ-PA")
panel_1<-panel_1 %>% relocate(Variable,PA) %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))

save(panel_1,file="panel_1.Rdata")

#Panel 2 final
colnames(panel_2)<-c("Wage=$4.25","Wage=$4.26-$4.99","Wage>$4.99")
panel_2<-panel_2 %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))
panel_2<-panel_2 %>% relocate(Variable)

save(panel_2,file="panel_2.Rdata")

#Panel 3 final
colnames(panel_3)<-c("Low-high","Midrange-high")
panel_3<-panel_3 %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))
panel_3<-panel_3 %>% relocate(Variable)

save(panel_3,file="panel_3.Rdata")

