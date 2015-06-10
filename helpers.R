suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(RCurl))
suppressPackageStartupMessages(require(Imap))
suppressPackageStartupMessages(require(geosphere))
suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(rgdal))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(memoise))



#' geoCode
#'
#' geoCodes an address.
#'
#' Using this function from 
#' https://gist.github.com/hadley/9537581
#' I was trying to use the built-in geocode from ggmaps, but I could not get
#' ggmaps installed on CENTOS
#'
#' @param address doesn't have to be an actual address, but needs to resolve uniquely for the Google API
#'
#' @return
#' @export
#'
#' @examples
geoCode <- function(address) {
  # sleep for a bit to ensure that we don't flood Google
  Sys.sleep(0.25) 
  base_url <- "https://maps.google.com/maps/api/geocode/json"
  
  r <- GET(base_url, query = list(address = address, sensor = "false"))
  stop_for_status(r)
  
  result <- content(r)
  if (!identical(result$status, "OK")) {
    warning("Request failed.", call. = FALSE)
    return(c(NA,NA,NA, NA))
  }
  
  first <- result$results[[1]]
  list(
    lat = first$geometry$location$lat,
    lng = first$geometry$location$lng,
    type = first$geometry$location_type,
    address = first$formatted_address
  )
}

# memoised version...
m_geoCode = memoise(geoCode)

# calculates the great circle segments for the i'th lat/lon point in the plotData parameter

#' greatCircles
#' 
#' Calculates the segments that comprising a Great Circle Arc
#'
#' @param i 
#' @param myPlot 
#' @param pb 
#'
#' @return
#' @export
#'
#' @examples
greatCircles = function(i, myPlot, pb) {
  inter = gcIntermediate(c(myPlot[[i,"lon"]],myPlot[[i, "lat"]]),
                         c(myPlot[[i,"longitude.sfi"]],myPlot[[i, "latitude.sfi"]]),
                         n=100, addStartEnd=T, breakAtDateLine=T)
  
  # if inter is a list, then we know that we crossed the int'l date line, so we need to
  # assemble the list into a data frame, but with the segmentNumber and tripNumber define
  # to discriminate between the right and left arcs
  if(is.list(inter)){
    inter1 = as.data.frame(inter[[1]])
    inter2 = as.data.frame(inter[[2]])
    
    inter1$segNumber=seq(1:nrow(inter1))
    inter2$segNumber=seq(1:nrow(inter2)) + nrow(inter1)
    
    inter1$tripNumber=paste("trip",i,1,sep=".")
    inter2$tripNumber=paste("trip",i,2,sep=".")
    
    inter=rbind(inter1,inter2)
  }else{
    inter = as.data.frame(inter)
    inter$segNumber = seq(1:nrow(inter))
    inter$tripNumber = paste("trip", i, sep=".") 
  }
  setTxtProgressBar(pb, i)
  return(inter)
}


getWorldView = function(wmap_df) {
  
  
  HomeAwayYellow = "#FED863"
  HomeAwayBlue = "#2A6EBB"
  HomeAwayGreen = "#839E73"
  
  worldView = ggplot() + 
    geom_polygon( data=wmap_df, aes(x=long, y=lat, group=group, fill=hole))+
    scale_fill_manual(values=c("#333333", "#000000"), guide="none") + # change colors & remove legend
    scale_colour_gradient("", low=HomeAwayBlue, high=HomeAwayYellow, labels=c("Home", "HomeAway"),breaks=c(1,50)) +
    coord_fixed(ratio=1) +
    theme(plot.background = element_rect(fill="#000000"), 
          plot.title = element_text(colour = HomeAwayYellow, size=18),
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
}