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
#and filter dates to be only those relevant to our dataset
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

##If you want a list of all observed mongoose ID's for use in BORIS, make a data file of these ID's
#name.data.file<-as.data.frame(unique(clean$indiv))
#write.csv(name.data.file, "Mongoose_Names.csv")

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

#####
#incorporate pregnancy and dominance data into full.data
#####

#####
# pregnancy
#####
lh$code <- as.factor(lh$code)
#pregnancy is ~60 days before birth, so daten of birth - 60 is pregnancy window
#can assume pregnancy window between day of abortion and nearest oestrus w/in 60 days of abortion....
#...or within 28 days of abortion. 
#can assume pregnancy if code=="FPREG" (first pregnant ID) on day of cleaning event

#load in oestrus data
oestrus <- read.csv("oestrus.csv",header=T,stringsAsFactors=F)
names(oestrus) <- tolower(names(oestrus))
#some filtering 
oestrus<-oestrus[which(oestrus$guard.id!="NONE" & !is.na(oestrus$guard.id)),]
oestrus<-oestrus[which(oestrus$strength==1 | oestrus$strength==2 | oestrus$strength==3),]
oestrus<-oestrus[which(oestrus$confidence==1 | oestrus$confidence==2 | oestrus$confidence==3),]
oestrus <- oestrus%>%
  filter(guard.id!="UNK")%>%
  filter(guard.id!="")%>%
  filter(group=="1B")

full.data$pregnant <- NA

# a loop to identify pregnancy
for (i in 1:nrow(full.data)){
  if(full.data$sex[i]=="P"){ #if not an adult
    full.data$pregnant[i] <- "pup" #pregnant status = pup
  } else if(full.data$sex[i]=="M"){ #if sex==M
    full.data$pregnant[i] <- "male" #pregnant status = male
  } else {
    #find FPREG dates
    preg.dates <- lh$daten[lh$indiv%in%full.data$indiv[i] & lh$code=="FPREG"] 
    #find birth dates
    birth.daten <- as.data.frame(lh%>%filter(indiv%in%full.data$indiv[i])%>%filter(code=="BIRTH")%>%dplyr::select(daten)) #simplify lh to that individual's birth codes & select daten
    birth.daten.range <- unlist(lapply(birth.daten[,1]-60, function(x) seq(x, x+60, 1)))#for each birth.daten, make sequence from 60 days before to birth.daten. 
    #abortion & oestrus dates
    abort.daten <- as.data.frame(lh%>%filter(indiv%in%full.data$indiv[i])%>%filter(code=="ABORT")%>%arrange(daten)%>%dplyr::select(daten))#find the dates of the abort codes
    abort.daten.range <- unlist(lapply(abort.daten[,1]-28, function(x) seq(x, x+28, 1)))#for each abort.daten, make sequence from 28 days before to abort.daten (conservative abortion pregnancy window)
    oestrus.daten <- unique(oestrus%>%filter(female.id%in%full.data$indiv[i])%>%arrange(daten)%>%dplyr::select(daten)) #find the dates of the oestrus codes
    preg.windows <- lapply(oestrus.daten[,1], function(x) x-abort.daten[,1]) #find the range of dates between oestrus and abort dates. this is a list where each [[i]] is oestrus date and each value w/in [[i]] is oestrus-abort
    poss.preg.oes.daten <- NULL #vector for oestrus dates
    poss.preg.abort.daten <- NULL #vector for abortion dates
    poss.preg <- as.data.frame(NULL) #empty data frame
    if(length(preg.windows)==0){ #if there are no oestrus-abort dates (e.g., either no abortions or no oestrus observations)
      poss.preg.vec <- NULL #set vector to NULL
    } else{ #if there are oestrus-abort dates
      for (j in 1:length(preg.windows)){ #for each element in the list preg.windows
        if(any(preg.windows[[j]]%in%seq(-60,0,1))==FALSE){
          poss.preg.vec <- NULL
        }else if(any(preg.windows[[j]]%in%seq(-60,0,1))==TRUE){ #if any values of oestrus-abort are between -60 and 0 (i.e. oestrus was w/in 60 days before abortion)
          poss.preg.oes.daten[j] <- oestrus.daten[j,1] #save oestrus datens to vector for oestrus dates
          poss.preg.abort.daten[j] <- abort.daten[which(preg.windows[[j]]%in%seq(-60,0,1)),1] #save corresponding values of abortion dates
          poss.preg <- as.data.frame(cbind(poss.preg.oes.daten, poss.preg.abort.daten)) #cbind these values  
          poss.preg <- poss.preg[complete.cases(poss.preg),] #remove NAs
          fun <-  function(x,y) seq(x,y,by=1) #make a function to do sequence of one value to another
          poss.preg.vec <- unlist(mapply(fun, poss.preg[,1], poss.preg[,2])) #apply the function to make a vector from each oestrus date (col 1) to each abortion date (col 2)
        }
      }
    }
    pregnancy.dates <- c(preg.dates, birth.daten.range, abort.daten.range, poss.preg.vec) #combine all the dates across the vectors 
    if(full.data$daten[i]%in%pregnancy.dates){ #if the daten of the IGI lies in this vector
      full.data$pregnant[i] <- "pregnant" #female is pregnant
    } 
    else{ #if not
      full.data$pregnant[i] <- "not.pregnant" #female is not pregnant
    }
  }
}

full.data$pregnant <- as.factor(full.data$pregnant) #change pregnant to be a factor
summary(full.data$pregnant)

#check a few
check <- sample(which(full.data$pregnant=="not.pregnant"),1) #randomly sample one individual (replace "not.pregnant" w/ "pregnant", "male", "pup" as desired)
full.data[check,] #find the event for this individual
lh%>%filter(indiv%in%full.data$indiv[check])%>%arrange(daten) #check this individual's LH records
i = check
#indiv BF866 daten 43946--gave birth on daten 43950, 14 days after cleaning event (==pregnant)
#indiv BF853 on daten 44055--gave birth on 44130, 75 days after event (==not pregnant)

#####
# dominance status
#####
# a male is dominant if it was mate guarding a female on the MOST RECENT oestrus date to a given event. 

oestrus$guard.id <- as.factor(oestrus$guard.id)

full.data$guard <- NA
full.data$guard_date <- NA

#run a loop to identify dominant v. subordinate males
for(i in 1:length(full.data$daten)){
  if(full.data$sex[i]=="P"){
    full.data$guard[i] <- "pup"
    full.data$guard_date[i] <- NA
  }else if(full.data$sex[i]=="F"){
    full.data$guard[i] <- "female"
    full.data$guard_date[i] <- NA
  }else if(full.data$daten[i]<min(oestrus$daten)){
    full.data$guard[i] <- NA
    full.data$guard_date[i] <- NA
  }else{
    guard_date <-  max(oestrus$daten[which(oestrus$daten<full.data$daten[i])])#find the guard date nearest to the cleaning date
    full.data$guard_date[i] <- guard_date #set this as the guard date
    guard_vector <- as.factor(as.vector(oestrus$guard.id[oestrus$daten==guard_date])) #a vector of which group members were oestrus for that guard date
    full.data$guard[i] <- ifelse(full.data$indiv[i]%in%guard_vector==TRUE, "Y", "N") #if the individual is in the vector, "Y" for guard, otherwise "N"}
  }
}
full.data$guard <- as.factor(full.data$guard)
summary(full.data$guard)

full.data$guard_IGI_delay <- full.data$daten-full.data$guard_date #new column for days between IGI and guard date
summary(full.data$guard_IGI_delay)#no guard values are within 180 days--this is a bit surprising isn't it? 
full.data$guard[which(full.data$guard_IGI_delay>180)] <- NA #any value >180 gets an NA
summary(full.data$guard)
#looking more closely at the oestrus dates vs. the cleaning dates, there just haven't been any oestrus events in the proper window.
View(oestrus%>%arrange(desc(daten)))
View(full.data%>%arrange(desc(daten)))
#e.g., 44114 is the most recent cleaning daten and 43935 is the earliest cleaning daten
#the only relevant oestrus datens are 44207, 44135 (both too late to be considered), and 43589 (too early to be considered)

#####
#escorting status
#####
escort <- as_tibble(read.csv("pup_association_march2021.csv"))
names(escort) <- tolower(names(escort))
escort <- escort%>%
  filter(group=="1B")
## If strength and confidence not =1 then convert escort to NA (i.e. only consider when strength and conf =1)
escort$escort<-with(escort,ifelse(strength==1 & confidence==1,escort,NA))


#we want to know who escorted whom. 
# so link up the ID of the escort with the ID of the pup in our full.data dataset
#don't worry about doing it w/in the full.data structure just yet. make a new dataset
pups.to.link <- full.data%>%filter(behavior=="on", sex=="P")%>%arrange(indiv)#find the pups you want to link w/ escorts
escort.rels <- data.frame(matrix(nrow = nrow(full.data%>%filter(behavior=="on", sex=="P")), ncol = 2))#make an empty dataframe to fill (so far I know the max # escorts = 2)
for (i in 1:nrow(pups.to.link)){#for each row in full data
    escort.ids <- unique(escort$escort[escort$pup%in%pups.to.link$indiv[i] & !is.na(escort$escort)])#find all escort IDs in escort where the pup was the same as indiv[i]
    escort.rels[i,1:length(escort.ids)] <- escort.ids #paste these adult IDs as escort.rels[i]
}
escort.rels$pup.id <- pups.to.link$indiv
escort.rels <- escort.rels[,c(3,1,2)]
names(escort.rels) <- c("pup.id", "escort1", "escort2")
#make sure this matches up w/ escort data
escort%>%filter(pup=="BM927")
#only save unique rows
escort.rels <- escort.rels[!duplicated(escort.rels$pup.id),]
#ok now this is just a data frame w/ each pup ID and the escort IDs for that pup. 

#now we can ask whether these escort IDs were found also to be on behavior=="on" in the cleaning dataset. 
escort.vec <- c(escort.rels$escort1,escort.rels$escort2[!is.na(escort.rels$escort2)])
full.data%>%filter(behavior=="on")%>%filter(indiv%in%escort.vec)#yeah there's 72 individuals here.

#OK so now what's the question? are pups that clean at a given time more likely to be cleaning with their escorts than with any other individual? 
#so this means find a pup ID and see if the escort1 or escort2 for that pup were in the same intID

full.data%>%filter(behavior=="on")%>%filter(intID%in%full.data$intID[full.data$indiv==escort.rels$pup.id[1]])

#is cleaning in pups predicted by the cleaning frequency of their escort?
#i.e. are you more likely to clean as a pup if you're escorted by an adult who cleans a lot?





#old version that I'm not sure I like
full.data$escort.rels <- NA
for (i in 1:nrow(full.data)){#for each row in full data
  if(full.data$sex[i]=="P"){#if it's a pup
    escort.ids <- unique(escort$escort[escort$pup%in%full.data$indiv[i] & !is.na(escort$escort)])#find all escort IDs in escort where the pup was the same as indiv[i]
    full.data$escort.rels[i] <- paste(escort.ids, collapse=".") #paste these adult IDs as escort.rels[i]
  } else{ #if it's not a pup (i.e. is an adult)
    escort.ids <- unique(escort$pup[escort$escort%in%full.data$indiv[i] & !is.na(escort$escort)]) #find all the pup IDs where the escort was the same as indiv[i]
    full.data$escort.rels[i] <- paste(escort.ids, collapse=".")#paste these pup IDs as escort.rels[i]
  }
}
full.data$escort.rels <- ifelse(full.data$escort.rels=="", NA, full.data$escort.rels)
#so now we have a column linking every adult & every pup if they were found cleaning.

#find all the pups that were noted as "on"
full.data%>%filter(behavior=="on", sex=="P")%>%arrange(indiv)%>%dplyr::select(escort.rels)
#now are these escort.rels also found in full.data%>%filter(behavior=="on")??
escort.string <- full.data%>%filter(behavior=="on", sex=="P")%>%arrange(indiv)%>%dplyr::select(escort.rels)
escort.string <- escort.string[,3]


###
#escorting index code here (I don't think we need this?)
###
for(i in 1:length(full.data$indiv)){
  focal.esc<-escort[which(escort$daten<full.data$daten[i]),] #find escort dates before date of interest
  focal.esc$session.id<-with(focal.esc,paste(daten,session)) #note date & time of escorting session
  
  grp.esc.freq<-length(unique(focal.esc$session.id)) #note total # of times the pack was escorting?
  
  esc.obs<-with(focal.esc[which(focal.esc$pup==full.data$indiv[i]),], #note # of times pup was in being escorted
                tapply(daten,escort,length))
  
  if(grp.esc.freq==0){ #if the group did not have observed escorting
    full.data$pup.esc.freq[i]<-0 #set all these to 0
    full.data$grp.esc.freq[i]<-0
    full.data$pup.esc.no[i]<-0
  }
  
  else { #otherwise...
    temp.esc<-focal.esc[which(focal.esc$pup==full.data$indiv[i]),] #find the escort data for the individual of interest
    
    if(sum(is.na(temp.esc$escort))==0){ #if there are no NAs for escort data for that indiv
      full.data$pup.esc.freq[i]<-with(temp.esc,length(unique(session.id))) #esc.frequency is the # of escorting sessions in which the pup was involved
      full.data$grp.esc.freq[i]<-grp.esc.freq #group esc. frequency is same as group above
      full.data$pup.esc.no[i]<-length(esc.obs)} #pup.esc.no is # of times the pup was escorted
    else if(sum(is.na(temp.esc$escort))>0){ #if there are NAs
      full.data$pup.esc.freq[i]<-with(temp.esc[which(!is.na(temp.esc$escort)),],
                               length(unique(session.id))) #just switch this to account for NAs
      full.data$grp.esc.freq[i]<-grp.esc.freq #this is the same as above
      full.data$pup.esc.no[i]<-length(esc.obs)} #this is the same as above
  }
}

## escort index is the proportion of total escorting events in the group for which the pup was observed being escorted
#e.g., escort.index==0.1 when the pup was involved in 1/10 escorting observations in the group
full.data$escort.index<-ifelse(full.data$grp.esc.freq==0,NA,full.data$pup.esc.freq/full.data$grp.esc.freq)
range(full.data$escort.index,na.rm=T) # 0 to 0.01; so the max a pup was being escorted was 1% of the time???
hist(full.data$escort.index)

plot(full.data$grp.esc.freq~full.data$daten)#later dates involve more overall group escorting observations, as expected
plot(full.data$pup.esc.freq[full.data$sex=="P"]~full.data$age[full.data$sex=="P"])#older pups are escorted more, as expected

#it could be that our escorting index is super low b/c we just don't have enough observations for the group overall. 
#or, maybe I need to 

###
# ANALYSIS
# model of clean (y/n) ~ sex * age + (1|indiv)
# plots of (averaged across individuals, so individuals aren't plotted >1x):
## prop. interactions in which individual cleaned ~ sex
## prop. int ~ age (e.g., choose first age or average age for individual across all sightings)
###

#consider just using pregnant instead of sex, as pregnant includes males, pups, pregnant females & non-pregnant females
#or maybe we first test if pregnancy matters, and then if it doesn't we just replace it w/ sex (i.e. lump all females together)

hist(full.data$age)
hist(log10(full.data$age))

#note here that I maybe need a different RE structure--e.g., nested? 
mod.1 <- glmer(data = full.data, formula = clean.binary ~ pregnant * log10(age) + (1|indiv) + (1|intID), family=binomial(link = "logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
plot(mod.1)
summary(mod.1)
#test for significance of fixed effects with post-hoc test
drop1(mod.1, test = "Chisq") #no significant sex:age interaction
#make a reduced model that does not include the 2-way interaction
mod.1.reduced <- glmer(data = full.data, formula = clean.binary ~ pregnant + log10(age) + (1|indiv) + (1|intID), family=binomial(link = "logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
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

##
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
  
