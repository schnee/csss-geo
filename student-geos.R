require(readr)

source("./helpers.R")

# read in the student locations
students = read_csv("./students.csv")

# geocode all of the students
geocoded = lapply(students$location, m_geoCode)

save(geocoded, file="geocoded.RData")

#augment the student dataframe with the geocoded address, lon and lat
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

# some descriptive visuals, many of which are silly
ggplot(travelToSFI, aes(x=dist)) + geom_bar()
ggplot(travelToSFI, aes(x=dist)) + geom_density()
ggplot(travelToSFI, aes(x=1, y=dist)) + geom_boxplot()
ggplot(travelToSFI, aes(x=lon)) + geom_density()
ggplot(travelToSFI, aes(x=lat)) + geom_density()

# why not both?
ggplot(travelToSFI, aes(x=lon, y=lat)) + geom_density2d()

# nice to give some progress if possible
pb <- txtProgressBar(min = 1, max = nrow(travelToSFI), style = 3)

# get all of the great circle segments
tripsList = lapply(1:nrow(travelToSFI),greatCircles,travelToSFI, pb)

# create a ginormous data frame from the trips list (not really ginormous)
allTrips = rbindlist(tripsList)

allTripsShift = allTrips

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
        panel.grid.minor = element_blank()) + 
  ggtitle("FRONT Left Breast")

ggsave("students-front.png", width=6, height=4, dpi=100)

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


baseworld + geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber), size=.5, colour="#FFFFFF") +
  ggtitle("BACK")

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
wmap_shift = spTransform(wmap, CRS("+proj=robin +lon_0=90w"))
wmap_df = fortify(wmap_shift)
ggplot() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group=group, fill=hole),size=0.1, fill="#000000", colour="#FFFFFF")

