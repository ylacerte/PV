library(plotly)
library(RColorBrewer)
library(kableExtra)
library(lubridate)
library(xts)

library(httr)
library(httr2)
library(jsonlite)

library(splines)


PVWatts.API <- function(system) {
  # https://developer.nrel.gov/docs/solar/pvwatts/v8/
  
  url <- "https://developer.nrel.gov/api/pvwatts/v8.json"
  MY_KEY <- "mywwV6K2ujsZXYmHlkfVuOIjL5ZKmN7TBMuDNpX3"
  
  query <- paste0(url,
                  "?api_key=", MY_KEY,
                  "&system_capacity=", system['system_capacity',2],
                  "&module_type=", system['module_type',2],
                  "&losses=", system['losses',2],
                  "&array_type=", system['array_type',2],
                  "&lat=", system['latitude',2],
                  "&lon=", system['longitude',2],
                  "&timeframe=", system['timeframe',2],
                  "&tilt=", system['tilt',2],
                  "&azimuth=", system['azimuth',2]
  )
  
  req <- request(query)
  resp <- req_perform(req)
  #    resp_content_type(resp)    # json
  #    resp_encoding(resp)        # UTF-8
  contents <- resp |> resp_body_json()
  
  inputs <- data.frame(value=unlist(contents$inputs))    
  inputs$name <- rownames(inputs)
  rownames(inputs) <- NULL
  
  version <- contents$version
  
  station_info <- data.frame(value=unlist(contents$station_info))    
  station_info$name <- rownames(station_info)
  rownames(station_info) <- NULL
  
  monthly <- data.frame(
    poa=unlist(contents$outputs$poa_monthly),
    ac=unlist(contents$outputs$ac_monthly),
    dc=unlist(contents$outputs$dc_monthly),
    solrad=unlist(contents$outputs$solrad_monthly))
  
  annual <- data.frame(
    ac=contents$outputs$ac_annual,
    solrad=contents$outputs$solrad_annual)
  
  # https://atb.nrel.gov/electricity/2023/pv-ac-dc
  capacity_factor <- data.frame(value=contents$outputs$capacity_factor)
  
  hourly <- data.frame(
    poa = unlist(contents$outputs$poa),
    ac = unlist(contents$outputs$ac),
    dc = unlist(contents$outputs$dc),
    dn = unlist(contents$outputs$dn),
    df = unlist(contents$outputs$df),
    alb = unlist(contents$outputs$alb),
    tamb = unlist(contents$outputs$tamb),
    tcell = unlist(contents$outputs$tcell),
    wspd = unlist(contents$outputs$wspd))
  
  time1 <- as.POSIXct("0000-01-01 00", format = "%Y-%m-%d %H")
  time2 <- as.POSIXct("0000-12-31 00", format = "%Y-%m-%d %H")
  hourly$hour <- seq(from=time1, to=time2, 
                     length.out=length(contents$outputs$poa))
  
  return (list(inputs = inputs, 
               annual  = annual,
               monthly = monthly, 
               hourly = hourly,
               capacity_factor = capacity_factor
  )
  )    
}

case <- function(parms) {
  system <- data.frame(rbind(
    c(what= "system_capacity", value=4, units="kW"),
    c(what= "module_type", value=0, units=""),
    c(what= "losses", value=14, units="percent"),
    c(what= "array_type", value=2, units=""),
    c(what= "timeframe", value="hourly", units="hourly"),
    c(what= "latitude", value=parms$latitude, units="degrees"),
    c(what= "longitude", value=parms$longitude, units="degrees"),
    c(what= "tilt", value=parms$tilt, units="degrees"),
    c(what= "azimuth", value=parms$azimuth, units="degrees")
  ))
  rownames(system) <- system$what
  return(system)  
}


init_parms <- data.frame(
  tilt=33,
  azimuth=111,
  latitude=39.,
  longitude=-105.
)

#out <- PVWatts.API(case(init_parms))
#str(out)    
