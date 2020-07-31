#format_weekly_data takes the filtered eBird datafile and assigns all records to geographical zones
#and weekly timesteps
#to get the filtered data, first run getData.R

library(ptinpoly)
library(plyr)
library(maps)
library(ggplot2)

library(dggridR)
library(lubridate)
library(tidyverse)
# resolve potential namespace conflicts
map <- maps::map
#---------------------------

basedir <-  getwd() 
scriptdir <- './R_scripts'
datadir <- './Data_files'
plotdir <- './Plots'
outputdir <- './Outputs'
setwd(basedir)

species <- 'far eastern curlew'
speciesdir <- paste(datadir,'/eBird sightings data/', species, sep= "")


###-----------------------------------------
#manually add in polygons to match the images in Supp Info 1 of Tak's paper
#see supporting information S1 for details of node selection boundaries and coordinates

#set region names-- uncomment and edit positions if investigating alternative nodes at Taiwan or Phillipines (see Supp Info S1)
#regionnames <- c("BREED", "YS", "SK-JPN", "MSIA-IND","NWAUS","NAUS","SEAUS","SAUS", "NK-YS", "TAIWAN")
#regionnames <- c("BREED", "SK-JPN", "MSIA-IND","NWAUS","NAUS","SEAUS","SAUS", "NK-YS", "PHILLIPINES")
regionnames <-  c("BREED", "JPN-SK", "MSIA-IND","NWAUS","NAUS","SEAUS","SAUS", "YS-NK")

values <- data.frame(
     id = seq(1,length(regionnames),1),
     value = regionnames
   )

positions <- data.frame(
  id = c( rep(values$id[1], 6), 
          rep(values$id[2], 5),
          rep(values$id[3], 9),
          rep(values$id[4], 4),
          rep(values$id[5], 8),
          rep(values$id[6], 5),
          rep(values$id[7], 4),
          rep(values$id[8], 7)),
          #rep(values$id[9],6)),
  yLat = c(43,65,65, 45.72,45.72, 43,
           29.5, 36,38,38,29.5,
           -12,6.3,6.3,7.9,7.9,3,-2,-8.45,-12,
           -23.5,-13,-13,-23.5,
           -21,-10,-10,0,0,-17,-17,-21,
           -37.5, -21, -17,-17,-37.5,
           -45, -35, -37.5,-45,
           29.5,43,43,38,38,36,29.5),
           #17.5,29.5,29.5,21, 21, 17.5), #Taiwan etc
           #7.9, 21,21,5,5, 7.9), #Phillipines
  xLong = c(120, 120,170,170, 138.5,138.5, #breeding-- northernmost north korean border, japan-russia border at hokkaido, include mongolian IBAs
            124,124,126,142,142,  #s korea border +japan: northward route
            93, 93, 105,105,120, 120,118,115.43, 115.43, #malaysia, w.indonesia to wallace line
            112,112, 129, 129,  #NW Australia: WA border to tropic of capricorn
            129,129,141,141,153,153,145.77,145.77, #N Australia to Cairns +PNG
            150,145.77,145.77,160,160, #SE Australia to Cairns and NSW/Victorian border at coast
            138.5, 138.5,150,150, #Southern Australia: Victorian coastline to Adelaide
            115,115,130,130,126,124,124) #, #North Korea and Yellow Sea: northward route
            #108,108,130,130,115,115) #Taiwan, Okinawa islands, China east coast to Vietnamese border, include Hainan island
            #115,115,130,130,120,120) #Phillipines
)

datapoly <- merge(values, positions, by=c("id"))


## Plot locations of IBAs ------------------------
wldmap <- map("world2", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), asp= 1)

#stick in in GGplot where we can set the coordinates better
wldGGplot <- ggplot() + geom_polygon(data = wldmap, aes(x=long, y = lat, group = group), fill=NA, color= 'black') + 
  coord_fixed(1.3)
#crop to desired region by fixing xlim
wldGGplot2 <- wldGGplot + coord_fixed(xlim = c(85, 205),  ylim = c(-50,70), ratio = 1)

#import locations of IBAs
setwd(datadir)
IBA_locations <- read.csv(file= "EAA_ID_sites.csv", header= TRUE, sep= ",")
setwd(basedir)
plotLongs <- IBA_locations$Longitude
plotLongs[plotLongs < 0] <- plotLongs[plotLongs < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system
IBA_locations$Adj_Longitude <- plotLongs
#points(plotLongs, IBA_locations$Latitude, col = "red", cex = .6)
wldGGplot3 <- wldGGplot2 + 
  geom_point(data = IBA_locations, aes(x = Adj_Longitude, y = Latitude), color = "red", size = 0.75)


#plot IBA locations of species
#read in species location data
filename <- paste(species, '_IBAs.csv', sep="")
species_locations <- read.csv(filename, header= TRUE, sep= ',')
setwd(basedir)
#add in lat/long data to the species locations
sp_locations <- merge(species_locations, IBA_locations, by.x= "EAA_ID", by.y= "SiteID")

#check to make sure the merge picked up all the sites-- if not, need to cross-check using the line of code below
#this line identifies which locations are missing from the dataset
#species_locations[!species_locations$EAA_ID%in%sp_locations$EAA_ID,]

plotLongs_sp <- sp_locations$Longitude
plotLongs_sp[plotLongs_sp <0] <-  plotLongs_sp[plotLongs_sp <0] +360  #adjust AK sites
plotLongs_sp$Adj_Longitude <- plotLongs
sp_locations$BLOCK <- as.factor(sp_locations$BLOCK)  #factor
wldGGplot4 <- wldGGplot3 + 
  geom_point(data = sp_locations, aes(x = Adj_Longitude, y = Latitude, color = BLOCK), size = 2)

#plot the polygons to make sure they cover the correct areas
wldGGplot5 <- wldGGplot4+
  geom_polygon(data= datapoly, aes(x= xLong, y= yLat, group= id), colour= "red", alpha= 0.2)

plot(wldGGplot5)

setwd(plotdir)
filename <- paste(species, "_plotIBAs.png", sep="")
png(filename)
  plot(wldGGplot5)
dev.off()




### Plot species records ---------------------------
setwd(basedir)
setwd(outputdir)
#get the eBird data
eBirddat <- readRDS("eastern_curlew_all_eBird2020Rds")  #could also try read_ebd function here (from auk package)
####
#eBirddat <- eBirddat[-which(eBirddat$observation_count== "X"),] #already done in getData_curlew
eBirddat <- eBirddat[-which(is.na(eBirddat$observation_count)),] 
#eBirddat$observation_count <- as.numeric(eBirddat$observation_count) #already done in getData_curlew
#find any NAs and remove:
#eBirddat <- eBirddat[-which(is.na(eBirddat$observation_count), arr.ind=TRUE),]

#extract year from date and remove very old records
#eBirddat$year <- as.numeric(as.character(substr(eBirddat$observation_date,0,4) )) #already done in getData_curlew
eBirddat$month <- as.numeric(as.character(substr(eBirddat$observation_date,6,7) ))
eBirddat$day <- as.numeric(as.character(substr(eBirddat$observation_date,9,10) ))
eBirddat$week <- as.numeric(format(eBirddat$observation_date, format= "%U"))#"%Y-%U")  #assign the week number

# oldestyear <- 1980  #set oldest year of records that we want to include
# eBirddat <- eBirddat[- which(eBirddat$year < oldestyear),]

#check for NAs in eBird.queries (i.e. unreported locations) 
NA_lat <- which(is.na(eBirddat$latitude))
NA_long <- which(is.na(eBirddat$longitude))
NAs_all <- union(NA_lat, NA_long)
#remove these rows
if (length(NAs_all)>0){
  eBirddat <- eBirddat[-NAs_all,]
}

#------------------------

# #remove all records where species has never been sighted
eBirddat$locality_id <- as.factor(eBirddat$locality_id)
#aggregate observation data by locality id, find zero entries (i.e. species never observed at locality) and remove
y <- aggregate(eBirddat$observation_count, by=list(Locality=eBirddat$locality_id), FUN=sum)
y <- y[y$x==0,]  #this is the list of locations where the species has never been recorded
y$Locality <- as.factor(y$Locality)  #y is a list of locations that we need to drop-- species never historically seen here

#crop the list so that we only keep the records at sites where birds have been observed at least once since observation started
eBirddat2 <-eBirddat[!(eBirddat$locality_id %in% y$Locality),]

#spatial subsampling
#follows methods at https://cornelllabofornithology.github.io/ebird-best-practices/encounter.html 
#-------------------------------------------------

# generate hexagonal grid with ~ 5 km betweeen cells
dggs <- dgconstruct(spacing = 5)
# get hexagonal cell id and week number for each checklist
checklist_cell <- eBirddat2 %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum,
         year = year(observation_date),
         week = week(observation_date))
# sample one checklist per grid cell per week (only affects output if there are more than one checklist in a cell per week)
# sample detection/non-detection independently 
ebird_ss <- checklist_cell %>% 
  group_by(species_observed, year, week, cell) %>% 
  sample_n(size = 1) %>% 
  ungroup()
#---------------------------
#x <- ebird_ss[ebird_ss$cell== 13243645,] #have a look at the sampled data for one cell

eBirddat2 <- ebird_ss

#save eBirddat2, the full filtered dataset relevant to the species
filename1 <- paste(species, "_all_records.csv", sep="")
setwd(basedir)
setwd(outputdir)
  write.csv(eBirddat2, file = filename1)
setwd(basedir)


#assign each record to a block based on position
#create a matrix of lat and long query data points from eBirddat
eBird.queries <- cbind(eBirddat2$latitude, eBirddat2$longitude)

valid.records <- list()
weekly.counts <- list()
for (i in 1:length(regionnames)){
  vertices <- as.matrix(subset(positions, id== i)[,2:3])
  
  #test if the data are in the site, return the row numbers if so
  testif.in <- eBirddat2[pip2d(vertices, eBird.queries)>=0,]
  valid.records[[i]] <- testif.in
  #summarise the results by getting the count of birds each week (and report the number of lists)
  weekly.counts[[i]] <- ddply(valid.records[[i]], .(week), summarize, obsCount=sum(observation_count), nLists= length(week))
  weekly.counts[[i]]$region <- regionnames[i]
}

#if length of weekly.counts is >53 (i.e. a partial week due to weeks starting on Monday), discard the partial data
#week 1 should then still be consistent with the other weeks due to the same sampling filtering protocol (alternative is to add weeks 53 and 1?)
for (i in 1:length(regionnames)){
  if (nrow(weekly.counts[[i]])>53)
    weekly.counts[[i]] <- weekly.counts[[i]][1:53,]
}


#find all records that are not in any of the polygons
invalid.records <- list()
invalid.records <- eBirddat2
for (i in 1:length(regionnames)){
  invalid.records <- invalid.records[!(invalid.records$locality_id %in% valid.records[[i]]$locality_id),]
}

#flatten weekly counts for export to csv--  this is the dataset that gets used in the analysis
weekly.countsMat <- do.call("rbind", weekly.counts)
#save weekly counts data as csv, the full dataset relevant to the species
filename2 <- paste(species, "_weekly_regioncount.csv", sep="")
setwd(basedir)
setwd(outputdir)
  write.csv(weekly.countsMat , file = filename2)
setwd(basedir)



#evaluate the proportion of sightings captured by each of the regions, and the proportion of sightings not in the regions
evaluate.regions <- list(regionID=integer(),
                                regionname =character(),
                                totbirds=integer(),
                                totlists=integer()) 
for (i in 1:length(regionnames)){
  evaluate.regions$regionID[i] <- i
  evaluate.regions$regionname[i] <- regionnames[i]
  evaluate.regions$totbirds[i] <- sum(valid.records[[i]]$observation_count)
  evaluate.regions$totlists[i] <- length(valid.records[[i]]$checklist_id)
}
evaluate.regions$regionID[(length(regionnames)+1)] <- NA
evaluate.regions$regionname[(length(regionnames)+1)] <- NA

evaluate.regions <- as.data.frame(evaluate.regions)

#tot.birdsObs <- cbind(sum(evaluate.regions$totbirds), sum(evaluate.regions$totlists))


############# map the remaining locations
wldmap <- map("world2", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), asp= 1)
#stick in in GGplot where we can set the coordinates better
wldGGplot <- ggplot() + geom_polygon(data = wldmap, aes(x=long, y = lat, group = group), fill=NA, color= 'black') + 
  coord_fixed(1.3)
#crop to desired region by fixing xlim
wldGGplot2 <- wldGGplot + coord_fixed(xlim = c(85, 205),  ylim = c(-50,70), ratio = 1)

eBirddat2$longitude_adjust <- eBirddat2$longitude
eBirddat2$longitude_adjust[eBirddat2$longitude_adjust < 0] <- eBirddat2$longitude_adjust[eBirddat2$longitude_adjust < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system

invalid.records$longitude_adjust <- invalid.records$longitude
invalid.records$longitude_adjust[invalid.records$longitude_adjust < 0] <- invalid.records$longitude_adjust[invalid.records$longitude_adjust < 0]  +360  #adjust Alaskan sites with -ve longitude to coordinate system


wldGGplot3 <- wldGGplot2 + 
  geom_point(data = eBirddat2, aes(x = longitude_adjust, y = latitude), color = "red", size = 0.5)+
  geom_point(data = invalid.records, aes(x = longitude_adjust, y = latitude), color = "blue", size = 0.5)
  

#plot the polygons to make sure they cover the correct areas
wldGGplot4 <- wldGGplot3+
  geom_polygon(data= datapoly, aes(x= xLong, y= yLat, group= id), colour= "red", alpha= 0.2)

plot(wldGGplot4)

setwd(basedir)
setwd(plotdir)
filename <- paste(species, "_nodeMap.png", sep="")
  png(filename)
  plot(wldGGplot4)
dev.off()

############ plot weekly data in each region

nplotrows <- 3 #specify how many rows for the plot (user defined based on number of regions)
nplotcols <- 3 # as above but for columns


setwd(basedir)
setwd(plotdir)

filename <- paste(species, "_total_count.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
  for (n in 1:(length(regionnames))){
    xvals <- weekly.counts[[n]]$week
      plot(xvals, weekly.counts[[n]]$obsCount, pch=n, type= "b", xlab= "week number", ylab= "total count", main= regionnames[n], xlim= c(0,52))
  }
mtext("Total counts", side = 3, line = -1.5, outer = TRUE)
dev.off()

filename <- paste(species, "_total_lists.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
for (n in 1:length(regionnames)){
  xvals <- weekly.counts[[n]]$week
  plot(xvals, weekly.counts[[n]]$nLists, pch=n, type= "b", xlab= "week number", ylab= "number of lists",main= regionnames[n], xlim= c(0,52))
}
mtext("Total Lists", side = 3, line = -1.5, outer = TRUE)
dev.off()

filename <- paste(species, "_avg_count_perList.png", sep="")
png(filename)
par(mfrow= c(nplotrows, nplotcols))
for (n in 1:length(regionnames)){
  xvals <- weekly.counts[[n]]$week
  plot(xvals, weekly.counts[[n]]$obsCount/weekly.counts[[n]]$nLists, pch=n, type= "b", xlab= "week number", ylab= "avg total count per list", main= regionnames[n], xlim= c(0,52))
}
dev.off()

setwd(basedir)

