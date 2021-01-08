#warhog cleaning code
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

full.data%>%
  group_by(intID)%>%
  filter(duplicated(indiv))

full.data <- full.data%>%
  group_by(intID, indiv)%>%
  slice(1)

######
###COME BACK TO THIS POINT AND GO FROM HERE.
######


#remove any "off" codes--these would be the only instances of duplicates
full.data%>%
  group_by(indiv)%>%
  slice(1)%>%
  group_by(intID)

#TO CHECK--WHEN INTID.X DOES NOT = INTID.Y IS THAT OK?
full.data <- full.data%>%
  mutate(in.int = ifelse(full.data$intID.x!=full.data$intID.y, "N", "Y")) #N = indiv was in another intID, not the indicated one, Y = indiv was in indicated interaction, NA = indiv exists in 1B, but was never seen in interaction
full.data$in.int <- ifelse(is.na(full.data$in.int), "N", full.data$in.int) #make NAs = N
full.data <- full.data%>%
  mutate(clean = ifelse(full.data$in.int=="Y" & full.data$behavior=="on", "Y", "N")) #make a new column for 'clean' -- if behavior = "on" during the correct video.
#NEED TO REMOVE REPEATED INDIVIDUAL IDS--ANY ROW WHERE INTID.X DOES NOT EQUAL INTID.Y IS AN INDIVIDUAL THAT WAS ALIVE BUT FOR THE PURPOSES OF THAT INTERACTION WAS JUST ALIVE, SO ASSIGN THEM A NEW CODE LIKE "ALIVE"
##AND THEN REMOVE DUPLICATES WITHIN AN INTERACTION

#remove unnecessary columns and rename
full.data <- full.data%>%
  select(!intID.y)%>%
  select(!behavior)
names(full.data) <- c("intID", "daten", "indiv", "sex", "birth.daten", "in.int", "clean")

#make some new variables
full.data <- full.data%>%
  mutate(age = daten - birth.daten)%>% #age on date of cleaning
  mutate(clean.binary = ifelse(clean=="Y", 1, 0))%>% #binary clean code: if yes = 1, if no = 0
  mutate(sex2 = as.factor(ifelse(age<365/2, "P", ifelse(full.data$sex=="M", "M", "F")))) #make pups distinct from males & females
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

#note here that I maybe need a different RE structure--e.g., nested? 
mod.1 <- glmer(data = full.data, formula = clean.binary ~ sex2 * scale(age) + (1|indiv) +(1|intID), family = binomial (link = logit))
#test for significance of fixed effects with post-hoc test
drop1(mod.1, test = "Chisq") #no significant sex:age interaction
#make a reduced model that does not include the 2-way interaction
mod.1.reduced <- glmer(data = full.data, formula = clean.binary ~ sex2 + scale(age) + (1|indiv) +(1|intID), family = binomial (link = logit))
drop1(mod.1.reduced, test = "Chisq") #sex and age are both significant
#use a post-hoc test to get each sex effect.
summary(glht(mod.1.reduced,linfct = mcp(sex2 = c("M - F = 0",
                                                  "M - P = 0",
                                                  "F - P = 0"))))

###
# prepare to plot data
# calculate summary values for each individual
# n.ints = number of interactions individual was there for
# prop.clean = proportion of interactions individual cleaned for
# mean.age = mean age
full.data2 <- full.data%>%
  group_by(indiv)%>%
  mutate(n.ints = length(in.int[in.int=="Y"]))%>%
  mutate(prop.clean = sum(clean.binary)/length(clean.binary))%>%
  mutate(mean.age = mean(age))

#check that this worked
testindiv <- sample(full.data$indiv,1)
full.data2[full.data2$indiv==testindiv,]
length(full.data$in.int[full.data$indiv==testindiv & full.data$in.int=="Y"]) #n.ints looks right
mean(full.data$age[full.data$indiv==testindiv])#looks right for age
sum(full.data$clean.binary[full.data$indiv==testindiv])/length(full.data$clean.binary[full.data$indiv==testindiv])#and for prop.clean

#now simplify the dataset to include one entry for each individual
full.data2 <- full.data2%>%
  group_by(indiv)%>%
  slice(1)%>%
  select(indiv, sex2, n.ints, prop.clean, mean.age)

###
# plot the model effects
###
#plot the age effect
ggplot(data = full.data2, aes(x = mean.age, y = prop.clean))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
#plot the sex effect
ggplot(data = full.data2, aes(x = sex2, y = prop.clean))+
  geom_boxplot()+
  geom_jitter(width = 0.05)

#one question to plot -- how much of cleaning is just driven by being near the interaction?
ggplot(data = full.data2, aes(x = n.ints, y = prop.clean))+
  geom_point()
#the more interactions you're in (whether cleaning or no), the more you'll clean
#is this a thing we want to control for? unsure.
ggplot(data = full.data2, aes(x = mean.age, y = n.ints))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
#as you get older, you're in more interactions, but the cleaning plot shows you clean less in those interactions
ggplot(data = full.data2, aes(x = sex2, y = n.ints))+
  geom_boxplot()+
  geom_jitter(width = 0.05)
#similar trend w/ pups as prop.clean, but adult males may clean more than adult females