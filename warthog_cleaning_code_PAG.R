#warthog cleaning code
#PICK UP AT LINE 177

#setwd (or do it through the GUI, if you're a sucker :))
setwd("C:/Users/Patrick/Dropbox (Personal)/Personal/Eleanor and Patrick/Mongoose Project/Data analysis/warthog_cleaning/")

#clear data history
rm(list = ls(all.names = TRUE))

#load libraries
library(tidyverse)
library(lme4)
library(multcomp)

#read in data file
clean <- as_tibble(read.csv("Mongoose video annotations_edited_updated7jan2020.csv"))
#select only relevant columns and organize data
clean <- clean[,c("Interaction_ID", "daten", "Mongoose_Num", "Behavior")]
names(clean) <- c("intID", "daten", "indiv", "behavior")
clean$daten[clean$daten=="unrecorded"]<-NA
clean<-clean[!is.na(clean$daten),]
clean$daten <- as.numeric(clean$daten)#this magically worked for Patrick on 8 Jan 
#clean$daten<-as.numeric(levels(clean$daten)[clean$daten]) ##this turns daten into a numeric from a factor without actually screwing up the numbers

#there are instances where behavior = gone, going, near, past. we want these to all = observed, b/c they mean the same thing
clean <- clean%>%
  filter(behavior!="off")%>%
  mutate(behavior = as.factor(ifelse(behavior=="on", "on", "observed")))
str(clean$behavior)

#read in life history data
lh <- as_tibble(read.csv("lifehistory_Dec2020.csv"))
names(lh)<-tolower(names(lh))
#condense life history to only pack 1B
lh <- lh%>%
  filter(pack == "1B")

#match names of mongooses in cleaning dataset w/ those in lh dataset
length(unique(clean$indiv)) #56 unique IDs in cleaning dataset

clean.lh <- lh%>%
  filter(indiv %in% clean$indiv)
length(unique(clean.lh$indiv)) #51 unique IDs match to the lh data. 5 are wrong in the cleaning dataset

not.lh.indiv <- clean%>%
  filter(!indiv %in% clean.lh$indiv)%>%
  dplyr::select(indiv)#these are the missing individuals -- look them up in the videos to try & clarify IDs

#the missing individuals are  all IDs that we could not clarify from the raw videos, so remove them from the dataset
clean <- clean%>%
  filter(!indiv %in% not.lh.indiv$indiv)

#now check again for expected number of IDs
clean%>%
  filter(indiv %in% lh$indiv)%>%
  summarise(length(unique(indiv))) #51 now in clean$indiv that match the lh indiv--which is correct!

#####
# get birth and death dates for all members of 1B
# this borrows from Faye's group composition code

## Start dates 
start.end<-lh[which(lh$start.end=="START" |
                      lh$start.end=="RESTART"),]

start.end<-start.end[start.end$indiv!="UM PUP",] # Remove unmarked pups

start.end<-start.end[,c("date",
                        "daten",
                        "pack",
                        "indiv",
                        "sex",
                        "start.end",
                        "code",
                        "litter")] # Remove unneeded columns

names(start.end)[c(1:2,7:8)]<-paste("start.",names(start.end)[c(1:2,7:8)],sep="") # Add start prefix to start data

## End dates
end<-lh[which(lh$start.end=="STOP" |
                lh$start.end=="END"),]

## Assign each START or RESTART individual its STOP or END date
start.end$end.daten<-NA
start.end$end.code<-NA
start.end$end.comment<-NA

for (i in 1:length(start.end$indiv)) { 
  
  # Individuals with no end date for a particular pack
  if (length(end$daten[which(end$indiv==start.end$indiv[i] &
                             end$pack==start.end$pack[i] &
                             end$daten>=start.end$start.daten[i])])==0) {
    
    start.end$end.daten[i]<-50000 # Give them a very high end date (i.e. are still in the population in that pack)
    
  } else { # Otherwise individuals are assigned end data from the first END/STOP record (after the start date) they have in that pack
    
    end.data<-end[which(end$indiv==start.end$indiv[i] &
                          end$pack==start.end$pack[i] &
                          end$daten>=start.end$start.daten[i]),]
    
    start.end$end.daten[i]<-end.data$daten[end.data$daten==min(end.data$daten)][1] # end daten
    start.end$end.code[i]<-end.data$code[end.data$daten==min(end.data$daten)][1] # end code
    start.end$end.comment[i]<-end.data$comment[end.data$daten==min(end.data$daten)][1] # end comment
    
  } 
}

## Get each individuals birth date
all.indivs<-data.frame(unique(start.end$indiv),stringsAsFactors=FALSE) # List of all individuals in the population
names(all.indivs)[1]<-"indiv"

# Extract their birth dates
# Individual with unknown birth dates not included here
all.indivs<-merge(all.indivs,start.end[start.end$start.code=="BORN" &
                                         start.end$start.end=="START",
                                       c("start.daten","indiv","pack")],
                  by="indiv")

names(all.indivs)[2]<-"birth.daten"
names(all.indivs)[3]<-"birth.pack" 

start.end$birth.daten<-all.indivs$birth.daten[match(start.end$indiv,all.indivs$indiv)] # Add birth dates to start and end records
start.end$birth.pack<-all.indivs$birth.pack[match(start.end$indiv,all.indivs$indiv)] # Add birth packs to start and end records

## Individuals with no known birth date (i.e. immigrants) are given NA
## Change their birth date to 0 so they are assumed to be adults
start.end$birth.daten<-ifelse(is.na(start.end$birth.daten),0,start.end$birth.daten) # Immigrants assumed to be adults

#now combine the life history data on birth dates (above) with the dates of cleaning interactions to get data on age
#pull out the unique dates of each cleaning interaction
clean.dates <- clean%>%
  group_by(intID)%>%
  slice(1)%>%
  dplyr::select(daten)%>%
  filter(!is.na(daten))

length(clean.dates$daten) #35 cleaning dates
length(unique(clean.dates$daten)) #25 unique dates - 10 repeated dates

#make a list where each dataframe will have info on the members of 1B on the day of the cleaning interaction
clean.grp.comp <- list()

#match the dates of each interaction and choose individuals born before that date and who died after that date (i.e. were alive on cleaning date)
for (i in 1:length(clean.dates$intID)){
  data <- start.end%>%
    filter(start.daten<=as.numeric(clean.dates$daten[i]) & end.daten>=as.numeric(clean.dates$daten[i]))%>%
    dplyr::select(birth.daten, indiv, sex)
  clean.grp.comp[[i]] <- cbind(clean.dates$intID[i], clean.dates$daten[i], data)
}

#combine list into a single dataframe
clean.grp.comp <- as_tibble(do.call("rbind", clean.grp.comp))
#rename columns
names(clean.grp.comp) <- c("intID", "daten", "birth.daten", "indiv", "sex")
clean.grp.comp <- clean.grp.comp[,c("intID", "daten", "indiv", "sex", "birth.daten")]

#double check some of the data--do the individuals w/in the group look right? are they they same individuals in each dataset?
testdate <- sample(clean$daten,1)
start.end$indiv[start.end$birth.daten<=testdate & start.end$end.daten>=testdate]
unique(clean.grp.comp$indiv[clean.grp.comp$daten==testdate])#unique here to account for dates on which >1 cleaning were recorded
start.end$indiv[start.end$birth.daten<=testdate & start.end$end.daten>=testdate] == unique(clean.grp.comp$indiv[clean.grp.comp$daten==testdate])#yes, these vectors are totally equal!

#combine datasets and add a column for whether a given individual (row) was in clean$indiv on the date (daten) of the cleaning event (intID)
full.data <- left_join(clean.grp.comp, clean, by = c("intID", "daten", "indiv"))

#this arranges the data by intID, individual, and behavior in reverse alphabetical order (on before observed). It keeps only the first instance for each individual. 
#so if an indiv was both on & observed in one interaction, it's only listed as "on". If it was observed > 1, it only keeps 1 observed.
full.data<- full.data%>%
  group_by(intID, indiv)%>%
  arrange(intID, indiv, desc(behavior))%>%
  slice(1)

#check if there are any duplicated individuals (this should be 0)
full.data%>%
  group_by(intID)%>%
  filter(duplicated(indiv))

#replace NA values in "behavior" with "not.seen"
full.data$behavior <- ifelse(is.na(full.data$behavior), "not.seen", ifelse(full.data$behavior=="on", "on", "observed"))

#make some new variables
full.data <- full.data%>%
  mutate(age = daten - birth.daten)%>% #age on date of cleaning
  mutate(clean.binary = ifelse(behavior=="on", 1, 0))
full.data$sex <- ifelse(full.data$age<365/2, "P", full.data$sex)

#turn character vectors into factors
full.data$sex <- as.factor(full.data$sex)
full.data$indiv <- as.factor(full.data$indiv)

###
# ANALYSIS
# model of clean (y/n) ~ sex * age + (1|indiv)
# plots of (averaged across individuals, so individuals aren't plotted >1x):
## prop. interactions in which individual cleaned ~ sex
## prop. int ~ age (e.g., choose first age or average age for individual across all sightings)
###

hist(full.data$age)
hist(log10(full.data$age))

#note here that I maybe need a different RE structure--e.g., nested? 
mod.1 <- glmer(data = full.data, formula = clean.binary ~ sex * log10(age) + (1|indiv) + (1|intID), family=binomial(link = "logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
plot(mod.1)
summary(mod.1)
#test for significance of fixed effects with post-hoc test
drop1(mod.1, test = "Chisq") #no significant sex:age interaction
#make a reduced model that does not include the 2-way interaction
mod.1.reduced <- glmer(data = full.data, formula = clean.binary ~ sex + log10(age) + (1|indiv) + (1|intID), family=binomial(link = "logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
plot(mod.1.reduced)
summary(mod.1.reduced)
drop1(mod.1.reduced, test = "Chisq") #sex and age are both significant

#use a post-hoc test to get each sex effect.
summary(glht(mod.1.reduced,linfct = mcp(sex = c("M - F = 0",
                                                  "M - P = 0",
                                                  "F - P = 0"))))

#another way to look at it (PAG 3 Feb 2021): a quadratic relationship w/ age
#note here that I maybe need a different RE structure--e.g., nested? 
mod.2 <- glmer(data = full.data, formula = clean.binary ~ log10(age) + I(log10(age)^2) + sex + (1|indiv) + (1|intID), family=binomial(link = "logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
plot(mod.2)
summary(mod.2) #looks like significant effects of age, the quadratic age term, & sex.

#gonna have to refine this model, as I wonder if sex & age should have an interaction term somehwere here. 

###


#to do w/ this plot
#identify the individuals that never clean--are they of a certain age?
no.clean.indiv <- full.data%>%group_by(indiv)%>%summarise(sum.clean = sum(clean.binary))%>%filter(sum.clean==0)%>%dplyr::select(indiv)
full.data$no.clean <- as.factor(ifelse(full.data$indiv%in%no.clean.indiv$indiv, 1, 0))

test <- full.data%>%group_by(daten, indiv, age)%>%summarise(sum.clean = sum(clean.binary))
test <- test%>%mutate(clean.yn = ifelse(sum.clean>0, 1, 0))

#plot binary clean 0/1 v age
par(mfrow = c(2,1))

a <- ggplot(data = test[test$age>30,], aes(x = age, y = clean.yn, col = indiv))+#col = no.clean
  geom_jitter(height = 0.05)+
  scale_x_continuous(trans="log10")
  #geom_smooth(method = "glm", formula = y~poly(x,2,raw=TRUE))#+
  #geom_density(aes(y = age), fill = "light blue", alpha = 0.5)

b <- ggplot(data = test[test$age>30,])+
  geom_density(aes(x = age))+
  scale_x_continuous(trans="log10")

library(ggpubr)
ggarrange(a, b, ncol = 1, nrow = 2)
  

#distribution of sampling dates
ggplot(data = test, aes(x = daten))+
  geom_histogram()


# prepare another plot
# calculate summary values for each individual
# n.ints = number of interactions individual was there for (removed this for now.)
# prop.clean = proportion of interactions individual cleaned for
# mean.age = mean age
full.data2 <- full.data%>%
  group_by(indiv)%>%
  #mutate(n.ints = length(in.int[in.int=="Y"]))%>%
  mutate(prop.clean = sum(clean.binary)/length(clean.binary))%>%
  mutate(mean.age = mean(age))

#check that this worked
testindiv <- sample(full.data$indiv,1)
full.data2[full.data2$indiv==testindiv,]
#length(full.data$in.int[full.data$indiv==testindiv & full.data$in.int=="Y"]) #n.ints looks right
mean(full.data$age[full.data$indiv==testindiv])#looks right for age
sum(full.data$clean.binary[full.data$indiv==testindiv])/length(full.data$clean.binary[full.data$indiv==testindiv])#and for prop.clean

#now simplify the dataset to include one entry for each individual
full.data2 <- full.data2%>%
  group_by(indiv)%>%
  slice(1)%>%
  dplyr::select(indiv, sex, prop.clean, mean.age)

###
# plot the model effects
###
#plot the age effect
ggplot(data = full.data2, aes(x = mean.age, y = prop.clean))+#, col=sex))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x + I(x^2), se = TRUE)+
  #scale_x_continuous(trans="log10")+
  geom_vline(xintercept = 365/2, lty = 2)+
  ylab("proportion cleaning")+
  xlab("age")+
  theme_bw()

#plot age & sex
ggplot(data = full.data2, aes(x = mean.age, y = prop.clean, col=sex))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  scale_x_continuous(trans="log10")+
  ylab("proportion cleaning")+
  xlab("age")+
  theme_bw()
  
#plot the sex effect
ggplot(data = full.data2, aes(x = sex, y = prop.clean))+
  geom_boxplot()+
  geom_jitter(width = 0.05)+
  ylab("proportion cleaning")+
  xlab("sex")

#plot the histogram of # of mongooses & # of interactions
ggplot(data = full.data2, aes(x = prop.clean))+
  geom_histogram(fill = "light gray", col = "black")+
  geom_density(fill = "light blue", alpha = 0.5)+
  theme_classic()+
  ylab("number of mongooses")+
  xlab("proportion of interactions seen cleaning")
  
