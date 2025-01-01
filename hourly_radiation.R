
Hourly_radiation <- function(q) {

  # solar irradiance and 
  # PV output data for every hour in a multi-year period

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
                          contents$meta$inputs$location$variables$elevation$units)
  location$desc <- ""
  
  meteo_data <- data.frame(name=rbind(
    "radiation_db", "meteo_db", "year_min", "year_max", 
    "use_horizon", "horizon_db", "horizon_data"))
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
    contents$meta$inputs$meteo_data$variables$horizon_db$description,
    "")

  input.mounting_system <- t(data.frame(contents$inputs$mounting_system))
  mounting_system <- data.frame(name=rownames(input.mounting_system), value=input.mounting_system)
  rownames(mounting_system) <- NULL
  mounting_system$units <- rbind(contents$meta$inputs$mounting_system$fields$slope$units,
                                 "",
                                 contents$meta$inputs$mounting_system$fields$azimuth$units,
                                 "", "")
  mounting_system$desc <- rbind(contents$meta$inputs$mounting_system$fields$slope$description,
                                "",
                                contents$meta$inputs$mounting_system$fields$azimuth$description,
                                "", "")
  
  pv_module <- data.frame(name=rbind("technology", "peak_power", "system_loss"))
  pv_module$value <- ""
  pv_module$units <- rbind("",
                           contents$meta$inputs$pv_module$variables$peak_power$units,
                           contents$meta$inputs$pv_module$variables$system_loss$units)
  pv_module$desc <- rbind(contents$meta$inputs$pv_module$variables$technology$description,
                           contents$meta$inputs$pv_module$variables$peak_power$description,
                           contents$meta$inputs$pv_module$variables$system_loss$description)
  
  input.df <- rbind(location, meteo_data, mounting_system, pv_module)

  meta.hourly <- data.frame(name=rbind(
    "P", "Gb(i)", "Gd(i)", "Gr(i)", "H_sun", "T2m", "WS10m", "Int"
  ))
  
  meta.hourly$units <- rbind(
    "",
    contents$meta$outputs$hourly$variables$`Gb(i`$units,
    contents$meta$outputs$hourly$variables$`Gd(i`$units,
    contents$meta$outputs$hourly$variables$`Gr(i`$units,
    contents$meta$outputs$hourly$variables$H_sun$units,
    contents$meta$outputs$hourly$variables$T2m$units,
    contents$meta$outputs$hourly$variables$WS10m$units,
    "")
  meta.hourly$desc <- rbind(
    contents$meta$outputs$hourly$variables$P$description,
    contents$meta$outputs$hourly$variables$`Gb(i`$description,
    contents$meta$outputs$hourly$variables$`Gd(i`$description,
    contents$meta$outputs$hourly$variables$`Gr(i`$description,
    contents$meta$outputs$hourly$variables$H_sun$description,
    contents$meta$outputs$hourly$variables$T2m$description,
    contents$meta$outputs$hourly$variables$WS10m$description,
    contents$meta$outputs$hourly$variables$Int$description)
  
  hourly <- data.frame()
  for ( i in 1:length(contents$outputs$hourly) ) {
    m <- data.frame(contents$outputs$hourly[[i]])
    hourly <- rbind(hourly, m)
  }
  hourly$time <- ymd_hm(hourly$time)
  
  return(list(input=input.df, meta=meta.hourly, hourly=hourly))  
  
}

