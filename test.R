library(geosphere)
library(leaflet)
library(ggplot2)
library(plotly)
library(kableExtra)
library(lubridate)
library(suntools)
library(xts)
library(sf)
library(viridis)




address <- "8204 Mount Kataka St, Littleton, CO 80125"
location <- data.frame(lon=-105.06391035422446, lat=39.48083560687681)
date <- now()  ; hour(date) <- 0  ; minute(date) <- 0  ; second(date) <- 0

spot <- data.frame(
  address=address, 
  lon=location$lon, 
  lat=location$lat,
  date=date
)
spot

# get sun position for selected hours of the day for the year 
  loc <- matrix(c(spot$lon, spot$lat), nrow = 1)

  from <- as.POSIXct(paste0("2024-01-01 00:00:00"))
  to   <- as.POSIXct(paste0("2024-12-31 23:00:00"))
  t <- seq.POSIXt(from, to, by="hour")
  sp <- data.frame(t, solarpos(crds=loc, dateTime=t))
  colnames(sp) <- c("time", "azimuth", "elevation")
  sp$hour <- as.factor(hour(sp$time))
  
# associate with a lat/lon  
  marker <- data.frame(destPoint(p=loc, b=sp$azimuth, d=1000)) 
  sp$lon <- marker$lon
  sp$lat <- marker$lat
  str(sp) ; head(sp) ; tail(sp)
  

# plot a few analemmas
  a.8  <- sp[which(sp$hour == 8) , ]
  a.12 <- sp[which(sp$hour == 12), ]
  a.18 <- sp[which(sp$hour == 18), ]
  loop <- data.frame(rbind(a.8, a.12, a.18))
  
  pal <- colorFactor(viridis_pal(option = "C")(2), domain = loop$hour)
  leaflet() %>% addTiles() %>%  
    setView(lng=spot$lon, lat=spot$lat, zoom = 14) %>%  
    addCircles(lng=spot$lon, lat=spot$lat, 
               weight=2, color='red', radius = 10, opacity = 2,
               highlightOptions = highlightOptions(color = "green", weight = 15),
               popup=spot$address, label=spot$address) %>% 
    addCircles(lng=loop$lon, lat=loop$lat, label=loop$hour,
               weight=2, color=pal(loop$hour), 
               radius = 10, opacity = 2) 

  