### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

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
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

library(gdata)
perldir <- Sys.which("perl")
fc <- "crime.xls" 
dat <- read.xls(fc, sheet = 1, header = TRUE, verbose=FALSE, perl=perldir, na.strings = "UNK")

gps <- matrix(nrow=dim(dat)[1], ncol=4)
city <- ", HOUSTON, USA"

load("gps.RData")
dat$BlockRange <- as.character(dat$BlockRange)
dat$StreetName <- as.character(dat$StreetName)
dat$Type <- as.character(dat$Type)
dat$Suffix <- as.character(dat$Suffix)

library(ggmap)

for (i in 1:(dim(dat)[1])) {
  if (!is.na(gps[i,2]))
    next
  else {
    print(i)
    next
  }
  
  if (is.na(dat$BlockRange[i]))
    dat$BlockRange[i] <- ""
  else if (dat$BlockRange[i] != "")
    dat$BlockRange[i] <- strsplit(as.character(dat$BlockRange[i]), "-")[[1]][1]
  
  if (is.na(dat$StreetName[i]) | dat$StreetName[i] == "UNK")
    dat$StreetName[i] <- " "
  
  if (is.na(dat$Type[i]) | dat$Type[i] == "-")
    dat$Type[i] <- " "
  
  if (is.na(dat$Suffix[i]) | dat$Suffix[i] == "-")
    dat$Suffix[i] <- " "
  
  addr <- paste(dat$BlockRange[i], dat$StreetName[i], dat$Type[i], dat$Suffix[i], city)
  ##gps[i,] <- geoCode(addr)
  cur <- geocode(addr, source = "dsk")
  gps[i,1] <- cur$lat
  gps[i,2] <- cur$lon
}

save(gps, file = "gps.RData")
