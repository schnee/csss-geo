require(readr)

source("./helpers.R")

students = read_csv("./students.csv")
geocoded=lapply(students$location, geoCode)
sfi = geoCode("Santa Fe Institue")


save(geocoded, file="geocoded.RData")
students$address = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$address} else({NA})))
students$lon = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$lng} else({NA})))
students$lat = unlist(lapply(geocoded, function(x) if(!is.na(x[[1]])){x$lat} else({NA})))

students$longitude.sfi = -105.9086
students$latitude.sfi = 35.70028

travelToSFI = students %>% 
  filter(!is.na(lon)) %>% filter(!is.na(lat)) %>%
  mutate(dist = gdist(lon, lat, longitude.sfi, latitude.sfi)) %>%
  arrange(desc(dist))

pb <- txtProgressBar(min = 1, max = nrow(travelToSFI), style = 3)
tripsList = lapply(1:nrow(travelToSFI),greatCircles,travelToSFI, pb)
allTrips = rbindlist(tripsList)

allTripsShift = allTrips

ggplot() + 
  geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber), size=.4, colour="#000000") +
  coord_fixed(ratio=1) +
  theme(plot.background = element_rect(fill="#FFFFFF"),
        panel.background = element_rect(fill="#FFFFFF"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# read shapefile
wmap <- readOGR(dsn="./shape-files/ne_110m_land", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

ggplot() + 
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
        legend.text = element_text(colour="#FFFFFF")) +
  geom_path(data=allTrips, aes(x=lon, y=lat, group=tripNumber), size=.5, colour="#FFFFFF") +
  ggtitle("Complex Systems Summer School\n2015")
