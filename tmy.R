
TMY <- function(q) {

  # typical meteorological year (TMY) is a set of meteorological data
  # for every hour in a year for a given geographical location
  
  req <- request(q)
  resp <- req_perform(req)
#  resp_content_type(resp)    # json
#  resp_encoding(resp)        # UTF-8
  contents <- resp |> resp_body_json()

  input.location <- t(data.frame(contents$inputs$location))
  location <- data.frame(name=rownames(input.location), value=input.location)
  rownames(location) <- NULL
  location$units <- rbind(contents$meta$inputs$location$variables$latitude$units,
                          contents$meta$inputs$location$variables$longitude$units,
                          contents$meta$inputs$location$variables$elevation$units,
                          "")
  location$desc <- ""
  
  meteo_data <- data.frame(name=rbind(
    "radiation_db", "meteo_db", "year_min", "year_max", 
    "use_horizon", "horizon_db"))
  meteo_data$value <- rbind(
    contents$inputs$meteo_data$radiation_db,
    contents$inputs$meteo_data$meteo_db,
    contents$inputs$meteo_data$year_min,
    contents$inputs$meteo_data$year_max,
    contents$inputs$meteo_data$use_horizon,
    "",
    contents$inputs$meteo_data$horizon_data)
  meteo_data$units <- ""
  meteo_data$desc <- rbind(
    contents$meta$inputs$meteo_data$variables$radiation_db$description,
    contents$meta$inputs$meteo_data$variables$meteo_db$description,
    contents$meta$inputs$meteo_data$variables$year_min$description,
    contents$meta$inputs$meteo_data$variables$year_max$description,
    contents$meta$inputs$meteo_data$variables$use_horizon$description,
    contents$meta$inputs$meteo_data$variables$horizon_db$description)
  
  input.df <- rbind(location, meteo_data)
  input.df %>% kbl() %>% kable_styling()
  
  meta.months_selected <- data.frame(
    name=contents$meta$outputs$months_selected$type,
    units=contents$meta$outputs$months_selected$timestamp,
    desc=contents$meta$outputs$months_selected$description)
  
  meta.tmy_hourly <- data.frame(name=rbind(
    "T2m", "RH", "G(h)", "Gb(n)", "Gd(h)", "IR(h)", "WS10m", "WD10m", "SP"
  ))
  meta.tmy_hourly$units <- rbind(
    contents$meta$outputs$tmy_hourly$variables$T2m$units,
    contents$meta$outputs$tmy_hourly$variables$RH$units,
    contents$meta$outputs$tmy_hourly$variables$`G(h`$units,
    contents$meta$outputs$tmy_hourly$variables$`Gb(n`$units,
    contents$meta$outputs$tmy_hourly$variables$`Gd(h`$units,
    contents$meta$outputs$tmy_hourly$variables$`IR(h`$units,
    contents$meta$outputs$tmy_hourly$variables$WS10m$units,
    contents$meta$outputs$tmy_hourly$variables$WD10m$units,
    contents$meta$outputs$tmy_hourly$variables$SP$units)
  meta.tmy_hourly$desc <- rbind(
    contents$meta$outputs$tmy_hourly$variables$T2m$description,
    contents$meta$outputs$tmy_hourly$variables$RH$description,
    contents$meta$outputs$tmy_hourly$variables$`G(h`$description,
    contents$meta$outputs$tmy_hourly$variables$`Gb(n`$description,
    contents$meta$outputs$tmy_hourly$variables$`Gd(h`$description,
    contents$meta$outputs$tmy_hourly$variables$`IR(h`$description,
    contents$meta$outputs$tmy_hourly$variables$WS10m$description,
    contents$meta$outputs$tmy_hourly$variables$WD10m$description,
    contents$meta$outputs$tmy_hourly$variables$SP$description)
  
  meta.df <- rbind(meta.months_selected, meta.tmy_hourly)
  
  months_selected <- data.frame()
  for ( i in 1:length(contents$outputs$months_selected) ) {
    d <- data.frame(contents$outputs$months_selected[[i]])
    months_selected <- rbind(months_selected, d)
  }
  
  tmy_hourly <- data.frame()
  for ( i in 1:length(contents$outputs$tmy_hourly) ) {
    d <- data.frame(contents$outputs$tmy_hourly[[i]])
    tmy_hourly <- rbind(tmy_hourly, d)
  }
  tmy_hourly$time.UTC. <- ymd_hm(tmy_hourly$time.UTC.)
  
  return(list(input=input.df, meta=meta.df, 
              months=months_selected, tmy=tmy_hourly))  
  
}  

