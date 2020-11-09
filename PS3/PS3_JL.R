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

options(scipen=10000)
options(digits=4)

rm(list=ls())

setwd("C:/Users/User/Documents/GitHub/Problem-Sets--753/PS3")


##############################################################################
################################ PART 1 ######################################



################
## Question 1 ##
##   PART a   ##
################

## Extract data
cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")

## Filter for years 1978 and 1985
cps78 <- filter(cps, year==1978)
cps85 <- filter(cps, year==1985)

## Exponentiate wage 
cps78 <- mutate(cps78, wage=exp(lnwage))

## Get geometric and arithmetic mean
cps78.geometricMeanlnwage <- exp(mean(cps78$lnwage))
cps78.arithmeticMeanlnwage <- mean(cps78$wage)

## Annual means
cps78.annualGeometricMeanlnwage <- 2000 * cps78.geometricMeanlnwage
cps78.annualArithmeticMeanlnwage <- 2000 * cps78.arithmeticMeanlnwage

## Mean and standard deviation of ED and EX
cps78.meanEd <- mean(cps78$ed)
cps78.sdEd <- sd(cps78$ed)
cps78.meanEx <- mean(cps78$ex)
cps78.sdEx <- sd(cps78$ex)

## Make tables with info
cps78.lnwageMeanTable <- data.frame("Geometric Mean" = c(cps78.geometricMeanlnwage, cps78.annualGeometricMeanlnwage),
                                    "Arithmetic Mean" = c(cps78.arithmeticMeanlnwage, cps78.annualArithmeticMeanlnwage))
rownames(cps78.lnwageMeanTable) <- c("Hourly", "Annual")
colnames(cps78.lnwageMeanTable) <- c("Geometric Mean", "Arithmetic Mean")
save(cps78.lnwageMeanTable, file="cps78.lnwageMeanTable.Rdata")

cps78.edExTable <- data.frame(Mean = c(cps78.meanEd, cps78.meanEx),
                              "Standard Deviation" = c(cps78.sdEd, cps78.sdEx))
rownames(cps78.edExTable) <- c("Education", "Experience")
colnames(cps78.edExTable) <- c("Mean", "Standard Deviation")
save(cps78.edExTable, file="cps78.edExTable.Rdata")

################
## Question 1 ##
##   PART b   ##
################

## Sample size
cps78.sampleSize <- 550

## Means for demographic dummy variables
cps78.nonwh.mean <- mean(cps78$nonwh)
cps78.hisp.mean <- mean(cps78$hisp)
cps78.fe.mean <- mean(cps78$fe)

## Counts for demographic dummy variables
cps78.nonwh.count <- cps78.sampleSize * cps78.nonwh.mean
cps78.hisp.count <- cps78.sampleSize * cps78.hisp.mean
cps78.fe.count <- cps78.sampleSize * cps78.fe.mean

## Make table with info
cps78.demographicsTable <- data.frame(Mean = c(cps78.nonwh.mean, cps78.hisp.mean, cps78.fe.mean),
                                      Count = c(cps78.nonwh.count, cps78.hisp.count, cps78.fe.count))
rownames(cps78.demographicsTable) <- c("nonwh", "hisp", "fe")
save(cps78.demographicsTable, file="cps78.demographicsTable.Rdata")

################
## Question 1 ##
##   PART c   ##
################

## Filter to subgroups
cps78.male <- filter(cps78, fe==0)
cps78.female <- filter(cps78, fe==1)
cps78.wh <- filter(cps78, nonwh==0 & hisp==0)
cps78.nonwh <- filter(cps78, nonwh==1)
cps78.hisp <- filter(cps78, hisp==1)

## Table for Gender 
cps78.genderTable <- data.frame("Mean ed" = c(mean(cps78.male$ed), mean(cps78.female$ed)),
                                "sd ed" = c(sd(cps78.male$ed), sd(cps78.female$ed)),
                                "Mean Hourly Wage" = c(exp(mean(cps78.male$lnwage)), exp(mean(cps78.female$lnwage))),
                                "sd Wage" = c(sd(cps78.male$wage), sd(cps78.female$wage)),
                                "Mean Annual Wage" = c(2000*exp(mean(cps78.male$lnwage)), 
                                                       2000*exp(mean(cps78.female$lnwage))))
rownames(cps78.genderTable) <- c("Male", "Female")
colnames(cps78.genderTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.genderTable, file="cps78.genderTable.Rdata")

## Table for Race
cps78.raceTable <- data.frame("Mean ed" = c(mean(cps78.wh$ed), mean(cps78.nonwh$ed), mean(cps78.hisp$ed)),
                              "sd ed" = c(sd(cps78.wh$ed), sd(cps78.nonwh$ed), sd(cps78.hisp$ed)),
                              "Mean Hourly Wage" = c(exp(mean(cps78.wh$lnwage)), exp(mean(cps78.nonwh$lnwage)),
                                                     exp(mean(cps78.hisp$lnwage))),
                              "sd Wage" = c(sd(cps78.wh$wage), sd(cps78.nonwh$wage), sd(cps78.hisp$wage)),
                              "Mean Annual Wage" = c(2000*exp(mean(cps78.wh$lnwage)), 
                                                     2000*exp(mean(cps78.nonwh$lnwage)),
                                                     2000*exp(mean(cps78.hisp$lnwage))))
rownames(cps78.raceTable) <- c("Whites", "NonWhites", "Hispanic")
colnames(cps78.raceTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.raceTable, file="cps78.raceTable.Rdata")

################
## Question 1 ##
##   PART d   ##
################

## Exponentiate wage 
cps85 <- mutate(cps85, wage=exp(lnwage))

## Deflate wages by deflator
cps85.deflator <- 1.649
cps85 <- mutate(cps85, wage=wage/cps85.deflator)
cps85 <- mutate(cps85, lnwage=log(wage))

## Get geometric mean and standard deviation
cps85.geometricMeanlnwage <- exp(mean(cps85$lnwage))
cps85.sdwage <- sd(cps85$wage)

## Annual mean
cps85.annualGeometricMeanlnwage <- 2000 * cps85.geometricMeanlnwage

## Mean and standard deviation of ED
cps85.meanEd <- mean(cps85$ed)
cps85.sdEd <- sd(cps85$ed)

## Sample size
cps85.sampleSize <- 534

## Means for demographic dummy variables
cps85.nonwh.mean <- mean(cps85$nonwh)
cps85.hisp.mean <- mean(cps85$hisp)
cps85.fe.mean <- mean(cps85$fe)

## Counts for demographic dummy variables
cps85.nonwh.count <- cps85.sampleSize * cps85.nonwh.mean
cps85.hisp.count <- cps85.sampleSize * cps85.hisp.mean
cps85.fe.count <- cps85.sampleSize * cps85.fe.mean

## Make table with info
cps85.demographicsTable <- data.frame(Mean = c(cps85.nonwh.mean, cps85.hisp.mean, cps85.fe.mean),
                                      Count = c(cps85.nonwh.count, cps85.hisp.count, cps85.fe.count))
rownames(cps85.demographicsTable) <- c("nonwh", "hisp", "fe")
save(cps85.demographicsTable, file="cps85.demographicsTable.Rdata")

## Filter to subgroups
cps85.male <- filter(cps85, fe==0)
cps85.female <- filter(cps85, fe==1)
cps85.wh <- filter(cps85, nonwh==0 & hisp==0)
cps85.nonwh <- filter(cps85, nonwh==1)
cps85.hisp <- filter(cps85, hisp==1)

## Table for Gender 
cps85.genderTable <- data.frame("Mean ed" = c(mean(cps85.male$ed), mean(cps85.female$ed)),
                                "sd ed" = c(sd(cps85.male$ed), sd(cps85.female$ed)),
                                "Mean Hourly Wage" = c(exp(mean(cps85.male$lnwage)), exp(mean(cps85.female$lnwage))),
                                "sd Wage" = c(sd(cps85.male$wage), sd(cps85.female$wage)),
                                "Mean Annual Wage" = c(2000*exp(mean(cps85.male$lnwage)), 
                                                       2000*exp(mean(cps85.female$lnwage))))
rownames(cps85.genderTable) <- c("Male", "Female")
colnames(cps85.genderTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.genderTable, file="cps85.genderTable.Rdata")

## Table for Race
cps85.raceTable <- data.frame("Mean ed" = c(mean(cps85.wh$ed), mean(cps85.nonwh$ed), mean(cps85.hisp$ed)),
                              "sd ed" = c(sd(cps85.wh$ed), sd(cps85.nonwh$ed), sd(cps85.hisp$ed)),
                              "Mean Hourly Wage" = c(exp(mean(cps85.wh$lnwage)), exp(mean(cps85.nonwh$lnwage)),
                                                     exp(mean(cps85.hisp$lnwage))),
                              "sd Wage" = c(sd(cps85.wh$wage), sd(cps85.nonwh$wage), sd(cps85.hisp$wage)),
                              "Mean Annual Wage" = c(2000*exp(mean(cps85.wh$lnwage)), 
                                                     2000*exp(mean(cps85.nonwh$lnwage)),
                                                     2000*exp(mean(cps85.hisp$lnwage))))
rownames(cps85.raceTable) <- c("Whites", "NonWhites", "Hispanic")
colnames(cps85.raceTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.raceTable, file="cps85.raceTable.Rdata")

## Table with totals for both years
cps78.totalTable <- data.frame("Mean ed" = cps78.meanEd,
                               "sd ed" = cps78.sdEd,
                               "Mean Hourly Wage" = cps78.geometricMeanlnwage,
                               "sd Wage" = sd(cps78$wage),
                               "Mean Annual Wage" = cps78.annualGeometricMeanlnwage)
rownames(cps78.totalTable) <- "Whole Sample"
colnames(cps78.totalTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.totalTable, file="cps78.totalTable.Rdata")
cps85.totalTable <- data.frame("Mean ed" = cps85.meanEd,
                               "sd ed" = cps85.sdEd,
                               "Mean Hourly Wage" = cps85.geometricMeanlnwage,
                               "sd Wage" = cps85.sdwage,
                               "Mean Annual Wage" = cps85.annualGeometricMeanlnwage)
rownames(cps85.totalTable) <- "Whole Sample"
colnames(cps85.totalTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.totalTable, file="cps85.totalTable.Rdata")

################
## Question 1 ##
##   PART e   ##
################

##################################################
##            First for lnwage                  ##

## Calculate mean and standard deviation of lnwage
cps78.w <- mean(cps78$lnwage)
cps78.s <- sd(cps78$lnwage)

## Separate wage data into categories
cps78$lnwagecat.subgroup <- cut(cps78$lnwage,
                                breaks = c(-Inf, 
                                           cps78.w - 2 * cps78.s,
                                           cps78.w - cps78.s,
                                           cps78.w,
                                           cps78.w + cps78.s,
                                           cps78.w + 2 * cps78.s,
                                           Inf))
cps78$lnwagecat <- factor(cps78$lnwagecat.subgroup,
                          labels = c("wi<=w-2s",
                                     "w-2s<wi<=w-s",
                                     "w-s<wi<=w",
                                     "w<wi<=w+s",
                                     "w+s<wi<=w+2s",
                                     "w+2s<wi"))

## Create table with counts and proportion of sample size
cps78.logNormalTable <- t(rbind(with(cps78, table(lnwagecat)),
                                with(cps78, table(lnwagecat))/cps78.sampleSize))
colnames(cps78.logNormalTable) <- c("Counts", "Proportion")
save(cps78.logNormalTable, file="cps78.logNormalTable.Rdata")

##################################################
##            Next for wage                     ##

## Calculate mean and standard deviation of wage
cps78.w <- mean(cps78$wage)
cps78.s <- sd(cps78$wage)

## Separate wage data into categories
cps78$wagecat.subgroup <- cut(cps78$wage,
                              breaks = c(-Inf, 
                                         cps78.w - 2 * cps78.s,
                                         cps78.w - cps78.s,
                                         cps78.w,
                                         cps78.w + cps78.s,
                                         cps78.w + 2 * cps78.s,
                                         Inf))
cps78$wagecat <- factor(cps78$wagecat.subgroup,
                        levels = c("(-Inf,2.8]",
                                   "(-0.453,2.8]",
                                   "(2.8,6.06]",
                                   "(6.06,9.32]",
                                   "(9.32,12.6]",
                                   "(12.6, Inf]"),
                        labels = c("wi<=w-2s",
                                   "w-2s<wi<=w-s",
                                   "w-s<wi<=w",
                                   "w<wi<=w+s",
                                   "w+s<wi<=w+2s",
                                   "w+2s<wi"))

## Create table with counts and proportion of sample size
cps78.normalTable <- t(rbind(with(cps78, table(wagecat)),
                             with(cps78, table(wagecat))/cps78.sampleSize))
colnames(cps78.normalTable) <- c("Counts", "Proportion")
save(cps78.normalTable, file="cps78.normalTable.Rdata")

## Chi-squared tests
testlnwage <- chisq.test(cps78.logNormalTable[,1],
                         p=c(pnorm(-2),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),
                             pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),1-pnorm(2)))

testwage <- chisq.test(cps78.normalTable[,1],
                       p=c(pnorm(-2),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),
                           pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),1-pnorm(2)))

chiTestTable <- data.frame(Statistic=c(testlnwage$statistic,testwage$statistic),
                           pValue=c(testlnwage$p.value,testwage$p.value))
rownames(chiTestTable) <- c("lnwage", "wage")
save(chiTestTable, file="chiTestTable.Rdata")

## Below is just stuff I was practicing with that I want to keep

# dnorm(0)
# 
# pnorm(0)-pnorm(-1)
# 
# seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
# dnorm(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#       mean=cps78.w,sd=cps78.s)
# 
# 
# 
# plot(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#      dnorm(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#            mean=cps78.w,sd=cps78.s))
# 
# ggplot(cps78, aes(x=lnwage)) + geom_density()
# 
# 
# , y=densityPlot(lnwage)

################
## Question 6 ##
##   PART a   ##
################

cps78.lnwage.lm1 <- lm(lnwage ~ fe + union + nonwh + hisp + ed + ex + exsq, data=cps78)
save(cps78.lnwage.lm1, file="cps78.lnwage.lm1.Rdata")

linearHypothesis(cps78.lnwage.lm1, c("nonwh = hisp"))
str(linearHypothesis)

table6achow<-data.frame(2.46,0.12)
colnames(table6achow)<-c("F statistic", "P-value")

save(table6achow,file="table6achow.Rdata")

################
## Question 6 ##
##   PART b   ##
################

## Counts table
cps78.demographicsTable6b <- data.frame(Mean = c(1-cps78.nonwh.mean-cps78.hisp.mean, 
                                                 cps78.nonwh.mean, 
                                                 cps78.hisp.mean),
                                        Count = c((1-cps78.nonwh.mean-cps78.hisp.mean)*cps78.sampleSize,
                                                  cps78.nonwh.count, 
                                                  cps78.hisp.count))
rownames(cps78.demographicsTable6b) <- c("other", "nonwh", "hisp")
save(cps78.demographicsTable6b, file="cps78.demographicsTable6b.Rdata")


cps78.raceTable6b <- rbind(with(cps78.wh, data.frame(ed=mean(ed),
                                                     diff=mean(ed)-mean(cps78.wh$ed),
                                                     ex=mean(ex),
                                                     diff=mean(ex)-mean(cps78.wh$ex),
                                                     fe=mean(fe),
                                                     diff=mean(fe)-mean(cps78.wh$fe),
                                                     union=mean(union),
                                                     diff=mean(union)-mean(cps78.wh$union),
                                                     lnwage=mean(lnwage),
                                                     diff=mean(lnwage)-mean(cps78.wh$lnwage))),
                           with(cps78.nonwh, data.frame(ed=mean(ed),
                                                        diff=mean(ed)-mean(cps78.wh$ed),
                                                        ex=mean(ex),
                                                        diff=mean(ex)-mean(cps78.wh$ex),
                                                        fe=mean(fe),
                                                        diff=mean(fe)-mean(cps78.wh$fe),
                                                        union=mean(union),
                                                        diff=mean(union)-mean(cps78.wh$union),
                                                        lnwage=mean(lnwage),
                                                        diff=mean(lnwage)-mean(cps78.wh$lnwage))),
                           with(cps78.hisp, data.frame(ed=mean(ed),
                                                       diff=mean(ed)-mean(cps78.wh$ed),
                                                       ex=mean(ex),
                                                       diff=mean(ex)-mean(cps78.wh$ex),
                                                       fe=mean(fe),
                                                       diff=mean(fe)-mean(cps78.wh$fe),
                                                       union=mean(union),
                                                       diff=mean(union)-mean(cps78.wh$union),
                                                       lnwage=mean(lnwage),
                                                       diff=mean(lnwage)-mean(cps78.wh$lnwage))))
rownames(cps78.raceTable6b) <- c("Others", "NonWhites", "Hispanic")
save(cps78.raceTable6b, file="cps78.raceTable6b.Rdata")

################
## Question 6 ##
##   PART c   ##
################

cps78.wh.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.wh)
cps78.nonwh.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.nonwh)
cps78.hisp.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.hisp)
save(cps78.wh.lnwage.lm1, file="cps78.wh.lnwage.lm1.Rdata")
save(cps78.nonwh.lnwage.lm1, file="cps78.nonwh.lnwage.lm1.Rdata")
save(cps78.hisp.lnwage.lm1, file="cps78.hisp.lnwage.lm1.Rdata")


################
## Question 6 ##
##   PART d   ##
################


cps78.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78)
str(cps78.lm1)

Sc<-deviance(cps78.lm1)

Swh<-deviance(cps78.wh.lnwage.lm1)
Snwh<-deviance(cps78.nonwh.lnwage.lm1)
Shisp<-deviance(cps78.hisp.lnwage.lm1)
k<-5

Nwh<-length(cps78.wh)
Nnwh<-length(cps78.nonwh)
Nhisp<-length(cps78.hisp)
  
F_stat<-((Sc-(Swh+Snwh+Shisp))/k)/((Swh+Snwh+Shisp)/(Nwh+Nnwh+Nhisp-2*k))

f_pvalue<-pf(F_stat, k, Nwh+Nnwh+Nhisp-2*k)

#Make table RSS

table6dRSS<- data.frame(c("Complete","White","Hisp","Non-white"),c(Sc,Swh,Shisp,Snwh))
colnames(table6dRSS)<-c("Sample","RSS")

#Make table Chow test

table6dchow<-data.frame(k,Nwh+Nnwh+Nhisp-2*k,F_stat,f_pvalue)
colnames(table6dchow)<-c("DF 1","DF 2","F statistic", "P-Value")

save(table6dRSS,file="table6dRSS.Rdata")
save(table6dchow,file="table6dchow.Rdata")


########################################################################
########################################################################
########################################################################

################### PART2  #############################################

########################################################################
########################################################################
########################################################################
########################################################################

rm(list=ls())


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



#### Standard errors!
### Generate individual variables

fteNJ0<-filter(fastfood2, state=="NJ",post==0)$fte2
fteNJ1<-filter(fastfood2, state=="NJ",post==1)$fte2
ftePA0<-filter(fastfood2, state=="PA",post==0)$fte2
ftePA1<-filter(fastfood2, state=="PA",post==1)$fte2

fteNJ0L<-filter(fastfood2, state=="NJ",post==0, wage_cat=="L")$fte2
fteNJ1L<-filter(fastfood2, state=="NJ",post==1, wage_cat=="L")$fte2

fteNJ0M<-filter(fastfood2, state=="NJ",post==0, wage_cat=="M")$fte2
fteNJ1M=filter(fastfood2, state=="NJ",post==1, wage_cat=="M")$fte2

fteNJ0H<-filter(fastfood2, state=="NJ",post==0, wage_cat=="H")$fte2
fteNJ1H<-filter(fastfood2, state=="NJ",post==1, wage_cat=="H")$fte2



fte.std<-fastfood2 %>% group_by(state,post) %>% summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE))

w.std<-fastfood2 %>% group_by(post, state, wage_cat) %>% 
  summarise(across(fte2, .fns=list(std=std.error),.names="{fn}.{col}", na.rm=TRUE)) %>% 
  ungroup() %>% filter(state=="NJ")

fte.std<-fte.std %>% pivot_wider(names_from=state,values_from=std.fte2) 
w.std<-w.std%>% pivot_wider(names_from=wage_cat, values_from=std.fte2)


### Add the standard error of the differences

#Panel 1 differences standard errors
diff_within_state<-c(1,  t.test(fteNJ0,fteNJ1)$stderr,t.test(ftePA0,ftePA1)$stderr, t.test(fteNJ0-fteNJ1,ftePA0-ftePA1)$stderr)
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


## Putting everything together

## Format Panel 1
## Pasting \n in between means and standard deviations for LaTex
x1<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), 
                                       function(i) paste0(round(fte[1,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(fte.std[1,i],digits=2),
                                                          ")"))))
x2<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), 
                                       function(i) paste0(round(fte[2,i],digits=2), 
                                                          "\n",
                                                          "(",
                                                          round(fte.std[2,i],digits=2),
                                                          ")"))))
x3<-as.data.frame(do.call(cbind,lapply(1:ncol(fte), 
                                       function(i) paste0(round(fte[3,i],digits=2), 
                                                          "\n",
                                                          "(",
                                                          round(fte.std[3,i],digits=2),
                                                          ")"))))
panel_1 <-x1 %>% rbind(x2,x3)
rm(x1,x2,x3)

## Format panel2
## Pasting \n in between means and standard deviations for LaTex
y1<-as.data.frame(do.call(cbind,lapply(1:ncol(w), 
                                       function(i) paste0(round(w[1,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std[1,i],digits=2),
                                                          ")"))))
y2<-as.data.frame(do.call(cbind,lapply(1:ncol(w), 
                                       function(i) paste0(round(w[2,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std[2,i],digits=2),
                                                          ")"))))
y3<-as.data.frame(do.call(cbind,lapply(1:ncol(w), 
                                       function(i) paste0(round(w[3,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std[3,i],digits=2),
                                                          ")"))))

panel_2<-y1 %>% rbind(y2,y3)
rm(y1,y2,y3)

## Format panel 3
## Pasting \n in between means and standard deviations for LaTex
z1<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), 
                                       function(i) paste0(round(w.between[1,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std.between[1,i],
                                                                digits=2),
                                                          ")"))))
z2<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), 
                                       function(i) paste0(round(w.between[2,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std.between[2,i],digits=2),
                                                          ")"))))
z3<-as.data.frame(do.call(cbind,lapply(1:ncol(w.between), 
                                       function(i) paste0(round(w.between[3,i],digits=2),
                                                          "\n",
                                                          "(",
                                                          round(w.std.between[3,i],digits=2),
                                                          ")"))))

panel_3<-z1 %>% rbind(z2,z3) 
rm(z1,z2,z3)

## Panel 1 final
colnames(panel_1)<-c("Variable","NJ","PA","NJ-PA")
panel_1<-panel_1 %>% relocate(Variable,PA) %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))

## Panel 2 final
colnames(panel_2)<-c("Wage>4.99", "Wage=4.25","Wage=4.26-4.99")
panel_2<-panel_2 %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))
panel_2<-panel_2 %>% relocate(Variable, `Wage=4.25`, `Wage=4.26-4.99`, `Wage>4.99`)

## Panel 3 final
colnames(panel_3)<-c("Low-high","Midrange-high")
panel_3<-panel_3 %>% mutate(Variable=c("FTE before","FTE after", "Change in mean FTE"))
panel_3<-panel_3 %>% relocate(Variable)

## Save all tables
save(panel_1,file="panel_1.Rdata")
save(panel_2,file="panel_2.Rdata")
save(panel_3,file="panel_3.Rdata")