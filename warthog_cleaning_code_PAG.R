#libraries
library(tidyverse)

#read in data file
clean <- as_tibble(read.csv("C:/Users/Patrick/Dropbox (Personal)/Personal/Eleanor and Patrick/Mongoose Project/Data analysis/mongoose_IDs.csv"))
clean
names(clean) <- c("rowID", "indiv")
clean$indiv[clean$indiv=="BM500?"] <- "BM500"

lh <- as_tibble(read.csv("C:/Users/Patrick/Dropbox (Personal)/Personal/Eleanor and Patrick/Mongoose Project/Data analysis/warthog_cleaning/lifehistory_Dec2020.csv"))
names(lh)<-tolower(names(lh))
lh
#condense life history to only pack 1B
lh <- lh%>%
  filter(pack == "1B")

#MATCH NAMES OF MONGOOSES IN D (CLEANING DATA) W/ NAMES IN LH (LIFE HISTORY DATA)
length(unique(clean$indiv)) #60 unique IDs

clean.lh <- lh%>%
  filter(indiv %in% clean$indiv)
length(unique(clean.lh$indiv)) #50 unique IDs. So 10 are missing/wrong?

not.lh.indiv <- clean%>%
  filter(!indiv %in% clean.lh$indiv)%>%
  select(indiv)#these are the missing individuals 

#which videos are these individuals in?
full.clean.data <- as_tibble(read.csv("C:/Users/Patrick/Dropbox (Personal)/Personal/Eleanor and Patrick/Mongoose Project/Data analysis/Mongoose video annotations_edited P and E.csv"))
names(full.clean.data)

ids.to.check <- full.clean.data%>%
  filter(Mongoose_Num %in% not.lh.indiv$indiv)

write.csv(ids.to.check, file = "C:/Users/Patrick/Dropbox (Personal)/Personal/Eleanor and Patrick/Mongoose Project/Data analysis/Mongoose_IDs_to_check_4Jan2020.csv")  

#####
#now use Faye's code to get birth and death dates for all members of 1B
#later, use this to compare those that cleaned to those that did not. 
#####

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

####
# next steps
# make sure daten is recorded in the cleaning data
# use daten as date of interest and calculate age following "Group composition" code from Faye. 
