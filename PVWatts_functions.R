library(plotly)
library(RColorBrewer)
library(kableExtra)
library(lubridate)
library(xts)

library(httr)
library(httr2)
library(jsonlite)

library(splines)

url <- "https://developer.nrel.gov/api/pvwatts/v8.json"
MY_KEY <- "mywwV6K2ujsZXYmHlkfVuOIjL5ZKmN7TBMuDNpX3"
system_capacity <- 4
module_type <- 0
losses <- 14
array_type <- 2
tilt <- seq(0,90,10)
azimuth <- 180
lat <- 39.48083560687681
lon <- -105.06391035422446
timeframe <- "hourly"

system_specifications <- data.frame(rbind(
  c(what= "system_capacity", value=system_capacity, units="kW"),
  c(what= "module_type", value=module_type, units=NA),
  c(what= "losses", value=losses, units="percent"),
  c(what= "array_type", value=array_type, units=NA),
  c(what= "tilt", value="seq(0,90,10)", units="degrees"),
  c(what= "azimuth", value=azimuth, units="degrees"),
  c(what= "latitude", value=lat, units="degrees"),
  c(what= "longitude", value=lon, units="degrees"),
  c(what= "timeframe", value=timeframe, units="hourly")
))

time1 <- as.POSIXct("2024-01-01 00", format = "%Y-%m-%d %H")
time2 <- as.POSIXct("2024-12-31 00", format = "%Y-%m-%d %H")


PVWatts.API <- function() {
  ALL <- list()
  for ( i in 1:length(tilt) ) {
    query <- paste0(url,
                    "?api_key=",MY_KEY,
                    "&system_capacity=", system_capacity,
                    "&module_type=", module_type,
                    "&losses=", losses,
                    "&array_type=", array_type,
                    "&tilt=", tilt[i],
                    "&azimuth=",azimuth,
                    "&lat=", lat,
                    "&lon=", lon,
                    "&timeframe=", timeframe
    )
    response <- GET(query)
    
    body <- content(response, "text")
    obj <- fromJSON(body)
    
    parms    <- data.frame(value=unlist(obj$inputs))
    
    poa.m    <- data.frame(month=month.name, value=obj$outputs$poa_monthly)
    ac.m     <- data.frame(month=month.name, value=obj$outputs$ac_monthly)
    dc.m     <- data.frame(month=month.name, value=obj$outputs$dc_monthly)
    solrad.m <- data.frame(month=month.name, value=obj$outputs$solrad_monthly)
    
    hour <- seq(from=time1, to=time2, length.out=length(obj$outputs$poa))
    poa.h    <- data.frame(date=hour, value=obj$outputs$poa)
    ac.h     <- data.frame(date=hour, value=obj$outputs$ac)
    dc.h     <- data.frame(date=hour, value=obj$outputs$dc)
    dn.h     <- data.frame(date=hour, value=obj$outputs$dn)
    df.h     <- data.frame(date=hour, value=obj$outputs$df)
    alb.h    <- data.frame(date=hour, value=obj$outputs$alb)
    tamb.h   <- data.frame(date=hour, value=obj$outputs$tamb)
    tcell.h  <- data.frame(date=hour, value=obj$outputs$tcell)
    
    ac.a            <- data.frame(value=obj$outputs$ac_annual)                  #
    solrad.a        <- data.frame(value=obj$outputs$solrad_annual)
    capacity_factor <- data.frame(value=obj$outputs$capacity_factor)
    
    ALL[[i]] <- list(parms = parms, 
                     poa.m = poa.m, poa.h = poa.h,
                     ac.m  = ac.m,  ac.h  = ac.h,   ac.a = ac.a,
                     dc.m  = dc.m,  dc.h  = dc.h,
                     solrad.m = solrad.m, solrad.a = solrad.a,
                     dn.h = dn.h, df.h = df.h, alb.h = alb.h,
                     tamb.h = tamb.h, tcell.h=tcell.h,
                     capacity_factor = capacity_factor)
    
    names(ALL)[i] <- paste0("tilt_", parms$value[5]) 
  }
  return(ALL)
}


get_window <- function(ts, day) {
  # 5 day window
    df.xts <- xts(ts, order.by=ts$date)
    start <- day
    end <- day + days(5)
    w <- data.frame(window(df.xts, start=start, end=end))
    w$date <- as.POSIXct(w$date)
    w$value <- as.numeric(w$value)

    return(w)
}
