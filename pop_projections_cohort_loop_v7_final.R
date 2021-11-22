#################################################
#Population Projections 2016-2040
#Cohort Model
#population by age
#National Model
#Paul Kilgarriff

#all data comes from CSO API
#fully reproducible - using this R script projections can be calculated using R studio

library(csodata)
library(stringr)
library(tidyverse)

windowsFonts("Arial" = windowsFont("Arial"))
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
windowsFonts("Goudy Stout" = windowsFont("Goudy Stout"))

rm(list = ls())

setwd("D:/folder/folder2")

######################################
#set assumptions

#Should net migration be included in future child bearing women totals? 1 = yes, 0 = No
dynamic = 1

#Net migration
#is net migration between an upper and lower bound or constant
#if random_process = 1, upper and lower bound used. 0 constant net migration value used.
#Gauss-Markov use 2
process = 2
lower_bound = 10000
upper_bound = 35000
constant = 30000

################################################################

#CSO Population by single year, county and sex
ages2 <- cso_get_data("E3003")

###################################
#Bring in Data

##################################################
#Step.1 - Female and Male population by age 2016
##################################################

#Male and Female population by age 2016
#females 2016 - subset
female16 <- ages2[ which(ages2$Sex=='Female'),]
female16 <- female16[ which(female16$County.and.City=='State'),]
female16 <- female16[ which(female16$Single.Year.of.Age!='All ages'),]
female16 <- female16[c(2,6)]
#data wrangle - extract age from character string
female16$Single.Year.of.Age <- as.character(female16$Single.Year.of.Age)
female16$age<-str_sub(female16$Single.Year.of.Age, end=-6)
female16$age[female16$age == "Under 1"] <- "0"
female16$age[female16$age == "100 years and"] <- "100"
female16$age <- as.numeric(female16$age)
names(female16)[names(female16) == "2016"] <- "pop"
#final data
female_pop <- female16[c(2,3)]

#males 2016 - subset
male16 <- ages2[ which(ages2$Sex=='Male'),]
male16 <- male16[ which(male16$County.and.City=='State'),]
male16 <- male16[ which(male16$Single.Year.of.Age!='All ages'),]
male16 <- male16[c(2,6)]
#data wrangle - extract age from character string
male16$Single.Year.of.Age <- as.character(male16$Single.Year.of.Age)
male16$age<-str_sub(male16$Single.Year.of.Age, end=-6)
male16$age[male16$age == "Under 1"] <- "0"
male16$age[male16$age == "100 years and"] <- "100"
male16$age <- as.numeric(male16$age)
names(male16)[names(male16) == "2016"] <- "pop"
#final data
male_pop <- male16[c(2,3)]

##################################################
#Step.2 - Mortality and Survival rates by age
##################################################

##Mortality Rates

#import mortality rates
life_tables <- cso_get_data("VSA32")
#subset
#convert to character
life_tables$Age.x <- as.character(life_tables$Age.x )
#data wrangle - extract age from character string
life_tables <- life_tables[ which(life_tables$Statistic=='px'),]
life_tables$Age.x[life_tables$Age.x == "Birth"] <- "Birth_born"
life_tables$age<-str_sub(life_tables$Age.x, end=-6)
life_tables$age[life_tables$age == "Birth"] <- "0"
life_tables$age <- as.numeric(life_tables$age)
life_tables <- life_tables[c(2,7:8)]
#long to wide format
life_tables <- reshape(life_tables, idvar = "age", timevar = "Sex", direction = "wide")
#rename variables
names(life_tables)[names(life_tables) == "2016.Male"] <- "surv_r_m"
names(life_tables)[names(life_tables) == "2016.Female"] <- "surv_r_f"


##################################################
#Step.3 - Births by mothers age
##################################################

#Birth Rates

#births per annum
births <- cso_get_data("VSA04")
#subset data
births <- births[ which(births$Sex.of.Child=='Both sexes'),]
births <- births[ which(births$Statistic=='All Births'),] 
#drop group observations. Keep specific age totals
list_1 <- c("Under 20 years","20 - 24 years","25 - 29 years","30 - 34 years",
            "35 - 39 years","40 - 44 years","Age not stated","All ages")
births = subset(births, !(births$Age.of.Mother %in% list_1))
#data wrangle - extract age from character string
births$age<-str_sub(births$Age.of.Mother, end=-6)
births$age[births$age == "15 years and "] <- "15"
births$age[births$age == "45 years and"] <- "45"
births$age <- as.numeric(births$age)
#calculate average number of births over the period 2013-2016
births$avg_births <- ((births$`2016`+births$`2015`+births$`2014`+births$`2013`)/4)
births <- births[c(18,19)]
births_all = sum(births$avg_births)
#by mothers age, share of total births
births$shr <- births$avg_births/births_all

#fertility rate by age group
fr <- cso_get_data("VSA04")
#subset data
fr <- fr[ which(fr$Sex.of.Child=='Both sexes'),]
fr <- fr[ which(fr$Statistic=='All Births'),] 
#drop group observations. Keep specific age totals
list_1 <- c("Under 20 years","20 - 24 years","25 - 29 years","30 - 34 years",
            "35 - 39 years","40 - 44 years","45 years and over")
fr = subset(fr, (fr$Age.of.Mother %in% list_1))
fr <- fr[c(2,13)]
names(fr)[names(fr) == "2016"] <- "births"
#births by age of mother
b_u20 <- filter(fr, Age.of.Mother=="Under 20 years")$births
b_20_24 <- filter(fr, Age.of.Mother=="20 - 24 years")$births
b_25_29 <- filter(fr, Age.of.Mother=="25 - 29 years")$births
b_30_34 <- filter(fr, Age.of.Mother=="30 - 34 years")$births
b_35_39 <- filter(fr, Age.of.Mother=="35 - 39 years")$births
b_40_44 <- filter(fr, Age.of.Mother=="40 - 44 years")$births
b_o44 <- filter(fr, Age.of.Mother=="45 years and over")$births

#female age groups
female_pop_ba <- female_pop
fem_under_20 <- sum(female_pop_ba[female_pop_ba$age > 15 & female_pop_ba$age < 20,]$pop)
fem_20_24 <- sum(female_pop_ba[female_pop_ba$age >= 20 & female_pop_ba$age < 25,]$pop)
fem_25_29 <- sum(female_pop_ba[female_pop_ba$age >= 25 & female_pop_ba$age < 30,]$pop)
fem_30_34 <- sum(female_pop_ba[female_pop_ba$age >= 30 & female_pop_ba$age < 35,]$pop)
fem_35_39 <- sum(female_pop_ba[female_pop_ba$age >= 35 & female_pop_ba$age < 40,]$pop)
fem_40_44 <- sum(female_pop_ba[female_pop_ba$age >= 40 & female_pop_ba$age < 45,]$pop)
fem_over_44 <- sum(female_pop_ba[female_pop_ba$age >= 45 & female_pop_ba$age < 50,]$pop)

#age specific birth rate
br_u20 <- b_u20 / fem_under_20
br_20_24 <- b_20_24 / fem_20_24
br_25_29 <- b_25_29 / fem_25_29
br_30_34 <- b_30_34 / fem_30_34
br_35_39 <- b_35_39 / fem_35_39
br_40_44 <- b_40_44 / fem_40_44
br_o44 <- b_o44 / fem_over_44

overall_asfr <- (br_u20 + br_20_24 + br_25_29 + br_30_34 + br_35_39 + br_40_44 + br_o44)*5

#crude birth rate by age group
br_u20_asfr <- (b_u20 / fem_under_20)*1000
br_20_24_asfr <- (b_20_24 / fem_20_24)*1000
br_25_29_asfr <- (b_25_29 / fem_25_29)*1000
br_30_34_asfr <- (b_30_34 / fem_30_34)*1000
br_35_39_asfr <- (b_35_39 / fem_35_39)*1000
br_40_44_asfr <- (b_40_44 / fem_40_44)*1000
br_o44_asfr <- (b_o44 / fem_over_44)*1000


##################################################
#Step.4 - Convergence of fertility rate
##################################################

#Fertility Rate

#fertility rate - converges towards 1.8
#fertility rate = (Total_births / Total_women_child_bearing_age)*31
#31 is the number of child bearing years - 14 - 45
#generate asfr
year <- c(2016:2040)
asfr <- c(0)
asfr <- cbind(year,asfr)
asfr <- as.data.frame(asfr)
asfr$asfr[asfr$year==2016]<- 1.97
asfr$asfr[asfr$year>=2030]<- 1.8
#linear interpolate - converge 2016 to 2030
b = (1.8-1.97) / 14 #14 years
for(y in 1:14) {
  year_sel=2016 + y
  asfr_lin <- 1.97 + (y *b)
  asfr$asfr[asfr$year==year_sel] <- (1.97 + (y *b))
}
  

##################################################
#Step.5 - Migration share by age group
##################################################

#import migration rates
migration_total <- cso_get_data("PEA03")
#net migration only - both sexes (little variation between males and females)
migration_total <- migration_total[ which(migration_total$Sex=='Both sexes'),]
migration_total <- migration_total[ which(migration_total$Inward.or.Outward.Flow=='Net migration'),]
migration_total <- migration_total[ which(migration_total$Age.Group!='All ages'),]

migration_total <- migration_total[c(2,34:38)]
migration_total$mean_mig <- (rowSums(migration_total[2:6]))/5
migration_total$shr <- (migration_total$mean / sum(migration_total$mean))

#create dataframe using share of net migration by age
age <- c(0:70)
shr_mig <- c(0)
mig1 <- cbind(age,shr_mig)
mig1 <- as.data.frame(mig1)
#replace age with shares calculated in previous step
#net migration shares by age group
mig1$shr_mig[mig1$age<=14]<- (filter(migration_total, Age.Group=="0 - 14 years")$shr)/15
mig1$shr_mig[mig1$age>14 & mig1$age<=24]<- (filter(migration_total, Age.Group=="15 - 24 years")$shr)/10
mig1$shr_mig[mig1$age>24 & mig1$age<=44]<- (filter(migration_total, Age.Group=="25 - 44 years")$shr)/20
mig1$shr_mig[mig1$age>44 & mig1$age<=64] <- (filter(migration_total, Age.Group=="45 - 64 years")$shr)/20
mig1$shr_mig[mig1$age>=65]<- (filter(migration_total, Age.Group=="65 years and over")$shr)/6
mig_shares <- mig1


#Gauss-Markov Chain - Net Migration
#Possibilities of using medium term economic growth to determine net migration?

starting = 30000
net_mig=0
net_mig_gm=30000
net_mig_add=30000

for(m in 1:24) {
  
  net_mig <- net_mig_add[1]
  if (m==1) {
    net_mig <- starting
  }

q1 = net_mig*.5
q2 = net_mig*.6
q3 = net_mig*.70
q4 = net_mig*.75
q5 = net_mig*.80
q6 = net_mig*.85  
q7 = net_mig*.9
q8 = net_mig*.95
q9 = net_mig
q10 = net_mig*1.05
q11 = net_mig*1.1
q12 = net_mig*1.15
q13 = net_mig*1.2

#highest probability of having same net migration following year.
#lower probablity for very high migration
mynumbers = c(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13)
myprob = c(.45, .5, .55, .6, .65, .7, .75, .8, .9, .7, .5, .4, .2)

net_mig_add = sample(mynumbers,
                    size = 1, #24 times sampled from `mynumbers`
                    replace = T,
                    prob = myprob)

net_mig_gm <- rbind(net_mig_gm,net_mig_add)

}
net_mig_gm <- as.data.frame(net_mig_gm)

##################################################
#Step.6 - Creat dataframes to track progress
##################################################

#save a dataframe to record births per year
birthsm <- c(0)
birthsf <- c(0)
birthsm_crude <- c(0)
birthsf_crude <- c(0)
year <- c(0)
all_b <- cbind(year,birthsm,birthsf,birthsm_crude,birthsf_crude)

#save a dataframe to record the fertility rate
asfr_1 <-c(0)
df_asfr <- cbind(year,asfr_1)

#save a dataframe to record net migration 
net_mig <-c(0)
all_mig <- cbind(year,net_mig)

#save a dataframe to record deaths
age <- c(0:120)
m_deaths <- as.data.frame(age)
f_deaths <- as.data.frame(age)

#save a dataframe to record population
age <- c(0:120)
m_pop_all <- as.data.frame(age)
f_pop_all <- as.data.frame(age)

##################################################
#Step.7 - Iterative procedure
##################################################

#Starting in 2016, iterate each year

for(v in 1:24) {
  
  ########################################
  #Stage 1 - Survived population by sex
  ########################################
  
  #year analyse - iterate year to year
  year = 2016 + v
  var1 <- as.character(paste("pop_",year, sep=""))
  #bring in age survival rates
  male_pop <- merge(male_pop,life_tables,by="age")
  #age population by 1 year
  male_pop$age <- male_pop$age + 1
  male_pop$pop_old <- male_pop$pop
  #calculate population by age that survived the year
  male_pop$pop <- male_pop$pop*male_pop$surv_r_m
  male_pop$pop <- round(male_pop$pop,0)
  #drop survival rates
  male_pop$surv_r_m<- NULL
  male_pop$surv_r_f<- NULL
  male_pop$deaths <- male_pop$pop_old - male_pop$pop
  
  #save deaths
  myvars <- c("age", "deaths")
  male_death <- male_pop[myvars]
  var1 <- as.character(paste("deaths_",year, sep=""))
  names(male_death)[names(male_death) == "deaths"] <- var1 
  male_pop$deaths <- NULL
  male_pop$pop_old <- NULL
  
  #save male pop
  myvars <- c("age", "pop")
  df_male_pop <- male_pop[myvars]
  
  #bring in age survival rates
  female_pop <- merge(female_pop,life_tables,by="age")
  #age population by 1 year
  female_pop$age <- female_pop$age + 1
  #record old population used for calculating new births
  female_pop$pop_old <- female_pop$pop
  #calculate population by age that survived the year
  female_pop$pop <- female_pop$pop*female_pop$surv_r_f
  female_pop$pop <- round(female_pop$pop,0)
  #drop survival rates
  female_pop$surv_r_m<- NULL
  female_pop$surv_r_f<- NULL
  female_pop$deaths <- female_pop$pop_old - female_pop$pop
  female_pop$pop_old <- NULL
  
  #save deaths
  myvars <- c("age", "deaths")
  female_death <- female_pop[myvars]
  var1 <- as.character(paste("deaths_",year, sep=""))
  names(female_death)[names(female_death) == "deaths"] <- var1 
  female_pop$deaths <- NULL
  
  #save female pop
  myvars <- c("age", "pop")
  df_female_pop <- female_pop[myvars]
  
  ########################################
  #Stage 2 - New births
  ########################################
  
  #new births
  #female age group population
  female_pop_ba <- female_pop
  fem_under_20 <- sum(female_pop_ba[female_pop_ba$age > 15 & female_pop_ba$age < 20,]$pop)
  fem_20_24 <- sum(female_pop_ba[female_pop_ba$age >= 20 & female_pop_ba$age < 25,]$pop)
  fem_25_29 <- sum(female_pop_ba[female_pop_ba$age >= 25 & female_pop_ba$age < 30,]$pop)
  fem_30_34 <- sum(female_pop_ba[female_pop_ba$age >= 30 & female_pop_ba$age < 35,]$pop)
  fem_35_39 <- sum(female_pop_ba[female_pop_ba$age >= 35 & female_pop_ba$age < 40,]$pop)
  fem_40_44 <- sum(female_pop_ba[female_pop_ba$age >= 40 & female_pop_ba$age < 45,]$pop)
  fem_over_44 <- sum(female_pop_ba[female_pop_ba$age >= 45 & female_pop_ba$age < 50,]$pop)
  
  #births by age group - using age group crude birth rate
  births_u20 <- (br_u20_asfr * fem_under_20)/1000
  births_20_24 <- (br_20_24_asfr * fem_20_24)/1000
  births_25_29 <- (br_25_29_asfr * fem_25_29)/1000
  births_30_34 <- (br_30_34_asfr * fem_30_34)/1000
  births_35_39 <- (br_35_39_asfr * fem_35_39)/1000
  births_40_44 <- (br_40_44_asfr * fem_40_44)/1000
  births_o44 <- (br_o44_asfr * fem_over_44)/1000
  
  #converging birth rate from 1.81 to 1.80 over 10 years
  if (v<10) {
  asfr_list <- c("br_u20_asfr","br_20_24_asfr","br_25_29_asfr","br_30_34_asfr","br_35_39_asfr","br_40_44_asfr","br_o44_asfr")
  for (p in asfr_list){
  r <- get(p)
  r <- r * 0.99899
  assign(p,r)
  }
  }

  #Sum births by age of mother
  #total births
  ann_births <- births_u20 + births_20_24 + births_25_29 + births_30_34 + births_35_39 + births_40_44 + births_o44
  #survived births - survival rate for babies
  births_adjusted <- round(ann_births*0.996959798,0) #survival rate of people age 0
  
  #new males born and new females born - 51% males - 49% females
  new_males <- round(births_adjusted*.51,0)
  new_females <- round(births_adjusted*.49,0)
  
  #Other method for calculating births
  #calculate total population age 15 to 45. Women of child bearing age
  birth_females <- sum(female_pop_ba[female_pop_ba$age >= 15 & female_pop_ba$age <= 45,]$pop)
  #converged asfr rate
  rate_fer <- asfr[ which(asfr$year==year),]
  asfr_conv <- rate_fer[,2]
  asfr_year_new <- asfr_conv
  #adjust births for year using new converged asfr rate - get new total births for year
  births_adjusted_2 <- (birth_females*asfr_conv)/31
  
  ########################################
  #Stage 3 - new population after births
  ########################################
  
  #add new population to existing population
  female_pop <- female_pop %>% add_row(pop = new_females, age = 0)
  male_pop <- male_pop %>% add_row(pop = new_males, age = 0)
  
  #record ASFR for this year
  #age specific birth rate
  br_u20 <- births_u20 / fem_under_20
  br_20_24 <- births_20_24 / fem_20_24
  br_25_29 <- births_25_29 / fem_25_29
  br_30_34 <- births_30_34 / fem_30_34
  br_35_39 <- births_35_39 / fem_35_39
  br_40_44 <- births_40_44 / fem_40_44
  br_o44 <- births_o44 / fem_over_44
  
  asfr_1 <- (br_u20 + br_20_24 + br_25_29 + br_30_34 + br_35_39 + br_40_44 + br_o44)*5
  
  new_asfr <- cbind(year,asfr_1)
  df_asfr <- rbind(df_asfr,new_asfr)
  
  ########################################
  #Stage 4 - Net migration
  ########################################
  
  #process = 0 constant migration, process = 1 random, process = 2 Gauss-Markov
  
  if (process==0) {
    net_mig <- constant
  }
  
  if (process==1) {
  net_mig <- runif(1, min=lower_bound, max=upper_bound)
  }
  
  if (process==2) {
    net_mig <- net_mig_gm[v,]
  }
  
  sex_mig <- net_mig/2
  new_mig <- mig_shares
  new_mig$persons <- new_mig$shr_mig*sex_mig
  new_mig$persons <- round(new_mig$persons,0)
  new_mig$shr_mig <- NULL
  
  if (dynamic==1) {
  #add net migration
  male_pop <- merge(male_pop,new_mig,by="age",all.x=T)
  female_pop <- merge(female_pop,new_mig,by="age",all.x=T)
  female_pop[is.na(female_pop)] <- 0
  male_pop[is.na(male_pop)] <- 0
  
  #add net migration to gender dataframes
  male_pop$pop <- male_pop$pop + male_pop$persons
  male_pop$persons <- NULL
  female_pop$pop <- female_pop$pop + female_pop$persons
  female_pop$persons <- NULL
  }
  
  #summarise by group
  male_pop <- male_pop %>% 
    group_by(age) %>% 
    summarise(pop = sum(pop))
  
  #summarise by group
  female_pop <- female_pop %>% 
    group_by(age) %>% 
    summarise(pop = sum(pop))
  
  ########################################
  #Stage 5 - Track births, deaths and migration
  ########################################
  
  #record number of births by sex
  birthsm <- new_males
  birthsf <- new_females
  birthsm_crude <- round(births_adjusted_2*.51,0)
  birthsf_crude <- round(births_adjusted_2*.49,0)
  
  new <- cbind(year,birthsm,birthsf,birthsm_crude,birthsf_crude)
  all_b <- rbind(all_b,new)
  
  #record net migration
  mig_year <- cbind(year,net_mig)
  all_mig <- rbind(all_mig,mig_year)
  
  #record deaths
  m_deaths <- merge(m_deaths,male_death,by="age",all.x=T)
  f_deaths <- merge(f_deaths,female_death,by="age",all.x=T)

  #record pop
  all_f_births <- new_females
  all_m_births <- new_males
  df_female_pop[nrow(df_female_pop) + 1,] = c(0,all_f_births)
  df_male_pop[nrow(df_male_pop) + 1,] = c(0,all_m_births)
  
  #rename variable
  var1 <- as.character(paste("mpop_",year, sep=""))
  names(df_male_pop)[names(df_male_pop) == "pop"] <- var1 
  var1 <- as.character(paste("fpop_",year, sep=""))
  names(df_female_pop)[names(df_female_pop) == "pop"] <- var1 
  #merge
  f_pop_all <- merge(f_pop_all,df_female_pop,by="age",all.x=T)
  m_pop_all <- merge(m_pop_all,df_male_pop,by="age",all.x=T)
  
}

#save data as csv
#dynamic
if (dynamic==1) {
  p2 <- "dyn"
}
if (dynamic==0) {
  p2 <- "non_dyn"
}
#type process
if (process==0) {
p1 <- constant/1000
p1 <- as.character(paste(p1,"_const", sep=""))
}
if (process==1) {
  p1 <- "nm_random"
}
if (process==2) {
  p1 <- "nm_gm"
}

#csv filename to save as
filename <- as.character(paste("pop_16_40_",p1,"_",p2,".csv", sep=""))

#save population year to year
#Total Population
#female
tt_pop <- f_pop_all
tt_pop[is.na(tt_pop)] <- 0
tt_pop$age <- NULL
tt_pop <- as.data.frame(colSums(tt_pop))
tt_f_pop <- tt_pop
names(tt_f_pop)[names(tt_f_pop) == "colSums(tt_pop)"] <- "female_pop" 
tt_f_pop$year <- rownames(tt_f_pop)
tt_f_pop$year <- str_sub(tt_f_pop$year, start=6)
#male
tt_pop <- m_pop_all
tt_pop[is.na(tt_pop)] <- 0
tt_pop$age <- NULL
tt_pop <- as.data.frame(colSums(tt_pop))
tt_m_pop <- tt_pop
names(tt_m_pop)[names(tt_m_pop) == "colSums(tt_pop)"] <- "male_pop" 
tt_m_pop$year <- rownames(tt_m_pop)
tt_m_pop$year <- str_sub(tt_m_pop$year, start=6)
#total pop
tt_pop <- merge(tt_f_pop,tt_m_pop,by="year")
tt_pop$all_pop <- (tt_pop$male_pop + tt_pop$female_pop)
tt_pop$year <- as.numeric(tt_pop$year)

#Births
births <- as.data.frame(all_b)
births <- births[-1,]
births$births <- births$birthsm + births$birthsf
births <- births[c(1,6)]

#Deaths
#males
death_m <- m_deaths
death_m[is.na(death_m)] <- 0
death_m$age <- NULL
death_m <- as.data.frame(colSums(death_m))
death_m <- death_m
names(death_m)[names(death_m) == "colSums(death_m)"] <- "male_deaths" 
death_m$year <- rownames(death_m)
death_m$year <- str_sub(death_m$year, start=8)
#females
death_f <- f_deaths
death_f[is.na(death_f)] <- 0
death_f$age <- NULL
death_f <- as.data.frame(colSums(death_f))
death_f <- death_f
names(death_f)[names(death_f) == "colSums(death_f)"] <- "female_deaths" 
death_f$year <- rownames(death_f)
death_f$year <- str_sub(death_f$year, start=8)
#total deaths
tt_deaths <- merge(death_f,death_m,by="year")
tt_deaths$deaths <- (tt_deaths$male_deaths + tt_deaths$female_deaths)
tt_deaths$year <- as.numeric(tt_deaths$year)
tt_deaths$female_deaths <- NULL
tt_deaths$male_deaths <- NULL

#Net migration
all_mig <- as.data.frame(all_mig)
all_mig <- all_mig[-1,]

#Bring it all together and save
tt_pop <- merge(tt_pop,births,by="year",all.x=T)
tt_pop <- merge(tt_pop,tt_deaths,by="year",all.x=T)
tt_pop <- merge(tt_pop,all_mig,by="year",all.x=T)

if (dynamic==0) {
  tt_pop[,"cum_mig"] <- cumsum(tt_pop$net_mig)
  tt_pop$female_pop <- round(tt_pop$all_pop + (tt_pop$cum_mig/2),0)
  tt_pop$male_pop <- round(tt_pop$all_pop + (tt_pop$cum_mig/2),0)
  tt_pop$all_pop <- tt_pop$all_pop + tt_pop$cum_mig
  tt_pop$cum_mig <- NULL
}

#save as csv

setwd("D:/folder1/results/")
#write csv
write.csv(tt_pop,filename, row.names = FALSE)


#keep only specified dataframes
rm(list=setdiff(ls(),c("tt_pop","births","all_mig","tt_deaths")))

#Sensitivity analysis
#migration 0 - closed economy

#migration 15,000 constant - dynamic

#migration 20,000 constant - dynamic

#migration 30,000 constant - dynamic

#migration 10,000 to 35,000 random process - dynamic


#migration 15,000 constant - non-dynamic

#migration 10,000 constant - non-dynamic

#migration 20,000 constant - non-dynamic

#migration 30,000 constant - non-dynamic
