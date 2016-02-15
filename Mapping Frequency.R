#Reads in the csv containing emails and metadata, and extracts just the bodies of the emails
setwd("GitHub/MappingHRC")
whole<-read.csv("Emails.csv", stringsAsFactors = F)
long<-subset(whole, subset = nchar(whole$ExtractedBodyText) > 10)
simplified<-data.frame(long$ExtractedBodyText, stringsAsFactors = F)
names(simplified)<-"Body"

#Reads in the csv containing the names of states and their capitals
coun_and_cap<-read.csv("countries.csv", stringsAsFactors = F)[-3]
#from https://raw.githubusercontent.com/icyrockcom/country-capitals/master/data/country-list.csv

#Changes the names of some states in order to align them with the names assigned by the mapping software
coun_and_cap$country[234] <- "United Kingdom"
coun_and_cap$country[177] <- "Taiwan"
coun_and_cap$country[64] <- "Timor-Leste"
coun_and_cap$country[208] <- "South Sudan"
coun_and_cap$country[178] <- "Republic of Congo"

#Tallies then umber of times each state and its capital are mentioned in the emails
for(i in 1:nrow(coun_and_cap)){
  coun_and_cap$Count[i] <- length(grep(coun_and_cap[i, 1], simplified$Body)) + length(grep(coun_and_cap[i, 2], simplified$Body))
}
coun_and_cap<-coun_and_cap[,-2]

#Adds the number of times mentioned of some alternate names (or major cities in those states) for some of the states discussed
coun_and_cap$Count[46] <- coun_and_cap$Count[46] + length(grep("PRC", simplified$Body)) + length(grep("People's Republic of China", simplified$Body)) + length(grep("Hong Kong", simplified$Body))

coun_and_cap$Count[123] <- coun_and_cap$Count[123] + length(grep("Benghazi", simplified$Body))

coun_and_cap$Count[145] <- coun_and_cap$Count[145] + length(grep("Burma", simplified$Body))

coun_and_cap$Count[158] <- coun_and_cap$Count[158] + length(grep("DPRK", simplified$Body))

coun_and_cap$Count[166] <- coun_and_cap$Count[166] + length(grep("West Bank", simplified$Body)) + length(grep("Gaza", simplified$Body)) - length(grep("Jerusalem", simplified$Body))

coun_and_cap$Count[234] <- coun_and_cap$Count[234] + length(grep("UK", simplified$Body)) + length(grep("England", simplified$Body)) + length(grep("Britain", simplified$Body)) + length(grep("Northern Ireland", simplified$Body)) + length(grep("Scotland", simplified$Body)) + length(grep("Wales", simplified$Body))

#Imports the libraries for mapping
library(rgeos)
library(maptools)
library(ggplot2)

#Reads in the map
map <- readShapeSpatial("ne_10m_admin_0_sovereignty.shp")
map$NAME_LONG <- as.character(map$NAME_LONG)

#The map doesn't have separate Palestine, so I'm adding the Palestine count to the Israel count
coun_and_cap$Count[134] <- coun_and_cap$Count[134] + coun_and_cap$Count[166]

#Renames some of the states in the map file
map$NAME_LONG[149] <- "North Korea"
map$NAME_LONG[154] <- "Russia"
map$NAME_LONG[99] <- "South Korea"

#Assigns the long names to their respective polygons on the map
fort_map<-fortify(map, region = "NAME_LONG")

#Removes states which are not in the map's list of states
short_coun<-coun_and_cap[which(coun_and_cap$country %in% map$NAME_LONG),]

#Removes states which are not in the list of states and capitals
short_map<-fort_map[which(fort_map$id %in% short_coun$country),]

#Factoring allows for a one to one relationship between the counts and the country names on the map
short_coun$fac_coun<-as.factor(short_coun$country)
short_map$fac_id<-as.factor(short_map$id)

#Second bolivia snuck in somehow?
short_coun<-short_coun[-22,]

#Creates the map and saves it to a png
png(filename=paste("map.png"), width=800)
ggplot() + geom_map(data = short_coun, aes(map_id = fac_coun, fill = Count), map = short_map) +
  scale_fill_continuous(low = "gray86", high = "deepskyblue3", name = "Times\nMentioned") + 
  labs(x = "", y = "", title = "Frequency of State Discussed") +
  expand_limits(x = short_map$long, y = short_map$lat) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.direction="horizontal", legend.position="top", legend.key.width=unit(5, "cicero"), plot.title = element_text(size = rel(2)), plot.background = element_rect(fill="gray86"), legend.background = element_rect(fill="gray86"))
dev.off()