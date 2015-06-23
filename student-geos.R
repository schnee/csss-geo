# this code block is to ensure that all the required packages are installed. Hopefully
requiredPackages = c("readr","dplyr",
                     "Imap","ggplot2", 
                     "data.table","grid",
                     "rgdal","geosphere",
                     "memoise","rgdal", 
                     "gridExtra","httr")

toInstall <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]

if(length(toInstall)) install.packages(toInstall)

# now that they are installed, load them up
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(Imap))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(rgdal))

# helpers.R pushes some functions / object out of this script. Helps with a narrative, but the
# best way to externalize code in R is via package creation
source("./helpers.R")

# read in the student locations
students = read_csv("./students.csv")

# geoCodedFile is the cached geocoded data. Since this is expensive (http requests), I don't
# to run this everytime. So, if the file exists, just load it into the env (which will create
# the "geocoded" data)

geoCodedFile = "geocoded.RData"
if(file.access(geoCodedFile)){
  
  print("No geocoded data file - generating from scratch. Sit tight")
  
  # geocode all of the students
  geocoded = lapply(students$location, m_geoCode)
  
  save(geocoded, file=geoCodedFile)
  
  # this is FALSED out to not execute it all the time, but it is good to run this
  # code to demo memoization
  if(FALSE){
    sfi <- "The Santa Fe Institute"
    bench = microbenchmark::microbenchmark(geoCode(sfi), 
                                           m_geoCode(sfi), 
                                           times = 5)
    bench
    autoplot(bench)
  }
} else {
  load(geoCodedFile)
}


# augment the student dataframe with the geocoded address, lon and lat
# I hate having to write code like this. "unlist(lapply(...))"???
students$address = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$address} else({NA})))
students$lon = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$lng} else({NA})))
students$lat = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$lat} else({NA})))

#SFI
students$longitude.sfi = -105.9086
students$latitude.sfi = 35.70028

# create the travel Data Frame. First, take the students, then filter by not NA values
# then create a "dist" column then arrange by dist descending
travelToSFI = students %>% 
  filter(!is.na(lon)) %>% filter(!is.na(lat)) %>%
  mutate(dist = gdist(lon, lat, longitude.sfi, latitude.sfi)) %>%
  arrange(desc(dist))

#write this out so we don't have to do it again
save(travelToSFI, file="travelToSFI.Rdata")

# some descriptive stats
fivenum(travelToSFI$dist)
summary(travelToSFI$dist)

# some descriptive visuals, many of which are silly
ggplot(travelToSFI, aes(x=dist)) + geom_bar()
ggplot(travelToSFI, aes(x=1, y=dist)) + geom_boxplot()
ggplot(travelToSFI, aes(x=dist)) + geom_density(adjust=1)


# OK, what if we want an area under that kernel density estimate curve?
# (I don't know why we want the mass - we just do)
kde = density(travelToSFI$dist)
kde_df = data.frame(x=kde$x, y=kde$y)

ggplot(travelToSFI, aes(x=dist)) + geom_density() + 
  geom_ribbon(data=subset(kde_df, x>2000 & x< 5000), aes(x=x, ymax=y), ymin=0, fill="#FF0000")

kde_fun = approxfun(kde$x,kde$y)
integrate(kde_fun, 2000, 5000) # mass!

# moving to geography now
lons = ggplot(travelToSFI, aes(x=lon)) + geom_density()
lons 
lats = ggplot(travelToSFI, aes(x=lat)) + geom_density()
lats

# why not both?
both = ggplot(travelToSFI, aes(x=lon, y=lat)) + geom_density2d()
both

# or even... (note - 'titlePlot' defined in helpers.R)
grid.arrange(lons, titlePlot, both, lats + coord_flip(), 
             ncol=2, nrow=2, widths=c(3, 1), heights=c(1, 3))

#
# OK enough of that, where are the great circle arcs?
#
# nice to give some progress if possible
pb <- txtProgressBar(min = 1, max = nrow(travelToSFI), style = 3)

# get all of the great circle segments
tripsList = lapply(1:nrow(travelToSFI),greatCircles,travelToSFI, pb)

# create a ginormous data frame from the trips list (not really ginormous)
allTrips = rbindlist(tripsList)

# all those parameters in the "theme(...)" method set visual aesthetics - no tick marks,
# black background, no axis titles, etc
ggplot() + 
  geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber), size=.4, colour="#FFFFFF") +
  coord_fixed(ratio=1) +
  theme(plot.background = element_rect(fill="#000000"),
        panel.background = element_rect(fill="#000000"),
        plot.title = element_text(size=18, colour="#FFFFFF"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave("students-front.png", width=6, height=4, dpi=100)

# Let's get a map
# read shapefile. Just land boundaries
wmap <- readOGR(dsn="./shape-files/ne_110m_land", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

baseworld = ggplot() + 
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group=group, fill=hole),size=0.1, fill="#000000", colour="#FFFFFF")+
  #scale_fill_manual(values=c("#333333", "#000000"), guide="none") +     
  coord_fixed(ratio=1) +
  theme(plot.background = element_rect(fill="#000000"), 
        plot.title = element_text(size=18, colour="#FFFFFF"),
        panel.background = element_rect(fill="#000000"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.background =  element_rect(fill = "#000000"),
        legend.key = element_rect(fill = "#000000"),
        legend.key.width = unit(0.1, "npc"),
        legend.text = element_text(colour="#FFFFFF"))

baseworld

baseworld + geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber), size=.5, colour="#FFFFFF") +
  ggtitle("Complex Systems Summer School 2015")

ggsave("students-back.png", width=6, height=4, dpi=100)


# add a gradient to the path
baseworld + geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber, colour=segNumber), size=.5) +
  scale_colour_gradient("", low="#FFFFFF", high="#0000FF", labels=c("Home", "SFI"),breaks=c(1,100)) 

# ok, put the 2d KDE on top of the map
baseworld + geom_density2d(data=travelToSFI, aes(x=lon, y=lat))

# or get "fancy"
baseworld + stat_density2d(data=travelToSFI, aes(x=lon, y=lat, fill=..level..), geom="polygon", alpha=0.5) +
  scale_fill_gradient(low="blue", high="red")

####
# Experimental Crap
###

allTripsShift = allTrips

wmap_shift = spTransform(wmap, CRS("+proj=robin +lon_0=90w"))
wmap_df = fortify(wmap_shift)
ggplot() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group=group, fill=hole),size=0.1, fill="#000000", colour="#FFFFFF")

