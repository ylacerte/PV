library(httr2)
library(readr)
library(stringr)
library(kableExtra)
library(lubridate)
library(plotly)
library(leaflet)
library(geosphere)
library(viridis)

defaults <- function() {
  
  location <- data.frame(rbind(
    lat=c(value=39.48083560687681, desc="latitude"),
    lon=c(value=-105.06391035422446, desc="longitude")
  ))
  location$value <- as.numeric(location$value)

  system <- data.frame(rbind(
    angle=c(value=40, desc="Inclination angle from horizontal plane"),
    aspect=c(value=0, desc="Orientation (azimuth) angle of the (fixed) plane, 0=south, 90=west, -90=east."),
    pvtechchoice=c(value="crystSi", desc="PV technology"),
    mountingplace=c(value="free", desc="Type of mounting of the PV modules"),
    loss=c(value=14, desc="Sum of system losses, in percent"),
    peakpower=c(value=10, desc="Nominal power of the PV system, in kW"),
    trackingtype=c(value=0, desc="Type of sun tracking used, 0=fixed")
  )) 

  off_grid <- data.frame(rbind(
    batterysize=c(value=50, desc="Energy capacity of the battery, watt-hours (Wh)."),
    cutoff=c(value=40, desc="The battery charge cannot go below the cutoff (in %)."),
    consumptionday=c(value=200, desc="Energy consumption during a 24 hour period (Wh)")
  ))

  calculations <- data.frame(rbind(  
    optimalangles=c(value=1, desc="Calculate optimum inclination AND orientation angles"),
    pvcalculation=c(value=1, desc="Outputs solar radiation, AND include estimated hourly PV production"),
    components=c(value=1, desc="Outputs beam, diffuse and reflected radiation components.")
  ))
  return(list(location=location,
              system=system,
              off_grid=off_grid,
              calculations=calculations))  
}


url <- "https://re.jrc.ec.europa.eu/api/"
outputformat <- "json"

