#get google map coordinators
setwd("C:\\Users\\z00323\\Documents\\other\\DC bike\\cabi-trip-history-data")
# setwd("C:\\Users\\Ran\\OneDrive\\cabi-trip-history-data")

rm(list=ls(all=TRUE))
library(RCurl)
library(RJSONIO)
library(plyr)
load("FinalStation.rdata")

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}



address <- geoCode("The White House, Washington, DC")
# rm[DCBikeAddress]
# rm[DCBikeAddress2]
DCBikeAddress = as.character(FinalStation$StationAddress)
DCBikeAddress2 = paste(DCBikeAddress[2:length(DCBikeAddress)], ", Washington, DC, USA", sep="")
DCBikeAddress[2:length(DCBikeAddress)] = DCBikeAddress2
DCBikeAddress3 = gsub("&", "and", DCBikeAddress)
# address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
locations  <- ldply(DCBikeAddress3, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)
save(locations,file="geoCodeLocation.rdata")
