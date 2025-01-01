
Grid_connected <- function(q) {
  # Monthly average values of PV system energy output.
  # In-plane irradiation per month and for the full year.
  
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
  
  input.meteo_data <- t(data.frame(contents$inputs$meteo_data))
  meteo_data <- data.frame(name=rownames(input.meteo_data), value=input.meteo_data)
  rownames(meteo_data) <- NULL
  meteo_data$units <- ""
  meteo_data$desc <- rbind(
    contents$meta$inputs$meteo_data$variables$radiation_db$description,
    contents$meta$inputs$meteo_data$variables$meteo_db$description,
    contents$meta$inputs$meteo_data$variables$year_min$description,
    contents$meta$inputs$meteo_data$variables$year_max$description,
    contents$meta$inputs$meteo_data$variables$use_horizon$description,
    contents$meta$inputs$meteo_data$variables$horizon_db$description)
  
  input.mounting_system <- t(data.frame(contents$inputs$mounting_system))
  mounting_system <- data.frame(name=rownames(input.mounting_system), value=input.mounting_system)
  rownames(mounting_system) <- NULL
  mounting_system$units <- rbind(contents$meta$inputs$mounting_system$fields$slope$units,
                                 "",
                                 contents$meta$inputs$mounting_system$fields$azimuth$units,
                                 "",
                                 "")
  mounting_system$desc <- rbind(contents$meta$inputs$mounting_system$fields$slope$description,
                                "",
                                contents$meta$inputs$mounting_system$fields$azimuth$description,
                                "",
                                "")
  
  input.pv_module <- t(data.frame(contents$inputs$pv_module))
  pv_module <- data.frame(name=rownames(input.pv_module), value=input.pv_module)
  rownames(pv_module) <- NULL
  pv_module$units <- rbind("",
                           contents$meta$inputs$pv_module$variables$peak_power$units,
                           contents$meta$inputs$pv_module$variables$system_loss$units)
  pv_module$desc <- rbind(contents$meta$inputs$pv_module$variables$technology$description,
                           contents$meta$inputs$pv_module$variables$peak_power$description,
                           contents$meta$inputs$pv_module$variables$system_loss$description)

  input.df <- rbind(location, meteo_data, mounting_system, pv_module)
  
  meta.monthly <- data.frame(name=rbind(
    "E_d", "E_m","H(i)_d", "H(i)_m", "SD_m"))
  meta.monthly$units <- rbind(
    contents$meta$outputs$monthly$variables$E_d$units,
    contents$meta$outputs$monthly$variables$E_m$units,
    contents$meta$outputs$monthly$variables$`H(i)_d`$units,
    contents$meta$outputs$monthly$variables$`H(i)_m`$units,
    contents$meta$outputs$monthly$variables$SD_m$units)
  meta.monthly$desc <- rbind(
    contents$meta$outputs$monthly$variables$E_d$description,
    contents$meta$outputs$monthly$variables$E_m$description,
    contents$meta$outputs$monthly$variables$`H(i)_d`$description,
    contents$meta$outputs$monthly$variables$`H(i)_m`$description,
    contents$meta$outputs$monthly$variables$SD_m$description)
  
  meta.totals <- data.frame(name=rbind(
    "E_y","H(i)_y","SD_y","l_aoi","l_spec","l_tg","l_total"
  ))
  meta.totals$units <- rbind(
    contents$meta$outputs$totals$variables$E_y$units,
    contents$meta$outputs$totals$variables$`H(i)_y`$units,
    contents$meta$outputs$totals$variables$SD_y$units,
    contents$meta$outputs$totals$variables$l_aoi$units,
    contents$meta$outputs$totals$variables$l_spec$units,
    contents$meta$outputs$totals$variables$l_tg$units,
    contents$meta$outputs$totals$variables$l_total$units
  )
  meta.totals$desc <- rbind(
    contents$meta$outputs$totals$variables$E_y$description,
    contents$meta$outputs$totals$variables$`H(i)_y`$description,
    contents$meta$outputs$totals$variables$SD_y$description,
    contents$meta$outputs$totals$variables$l_aoi$description,
    contents$meta$outputs$totals$variables$l_spec$description,
    contents$meta$outputs$totals$variables$l_tg$description,
    contents$meta$outputs$totals$variables$l_total$description
  )

  meta.df <- data.frame(value=rbind(meta.monthly, meta.totals))
  
  out.df <- data.frame()
  for ( i in 1:length(contents$outputs$monthly$fixed) ) {
    d <- data.frame(contents$outputs$monthly$fixed[[i]])
    out.df <- rbind(out.df, d)
  }

  totals <- data.frame(contents$outputs$totals$fixed)
  
  return(list(input=input.df, meta=meta.df, out=out.df, totals=totals))  
}
