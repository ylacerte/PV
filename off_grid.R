
Off_grid <- function(q) {

  # Monthly average values of PV system energy output.
  # Probability of battery charge reaching full or empty state. 
  # List of 10 values giving a histogram of battery charge state. 
  
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
                                 "")
  mounting_system$desc <- rbind(contents$meta$inputs$mounting_system$fields$slope$description,
                                "",
                                contents$meta$inputs$mounting_system$fields$azimuth$description,
                                "")
  
  input.pv_module <- t(data.frame(contents$inputs$pv_module))
  pv_module <- data.frame(name=rownames(input.pv_module), value=input.pv_module)
  rownames(pv_module) <- NULL
  pv_module$units <- contents$meta$inputs$pv_module$variables$peak_power$units
  pv_module$desc <- contents$meta$inputs$pv_module$variables$peak_power$description
  
  battery <- data.frame(name=rbind("battery capacity", "battery discharge_cutoff_limit"))
  battery$value <- rbind(contents$inputs$battery$capacity,
                         contents$inputs$battery$discharge_cutoff_limit)
  battery$units <- rbind("Wh", "%")
  battery$desc <- rbind("energy capacity, of the battery", 
                        "battery charge cannot go below a certain percentage of full charge")
  
  consumptionday <- data.frame(
    name="consumptionday", 
    value=contents$inputs$consumption$daily, 
    units="Wh", 
    desc="Energy consumption of all the electrical equipment connected to the system during a 24 hour period"
  )
  input.df <- rbind(location, meteo_data, mounting_system, 
                    pv_module, battery, consumptionday)
  
  
  meta.monthly <- data.frame(name=rbind(
    "E_d", "E_lost_d","f_f", "f_e"))
  
  meta.monthly$units <- rbind(
    contents$meta$outputs$monthly$variables$E_d$units,
    contents$meta$outputs$monthly$variables$E_lost_d$units,
    contents$meta$outputs$monthly$variables$f_f$units,
    contents$meta$outputs$monthly$variables$f_e$units
  )

  meta.monthly$desc <- rbind(
    contents$meta$outputs$monthly$variables$E_d$description,
    contents$meta$outputs$monthly$variables$E_lost_d$description,
    contents$meta$outputs$monthly$variables$f_f$description,
    contents$meta$outputs$monthly$variables$f_e$description
    )
  
  meta.totals <- data.frame(name=rbind(
    "d_total", "E_lost", "E_miss"))
  meta.totals$units <- rbind(
    contents$meta$outputs$totals$variables$d_total$units,
    contents$meta$outputs$totals$variables$E_lost$units,
    contents$meta$outputs$totals$variables$E_miss$units
  )  
  meta.totals$desc <- rbind(
    contents$meta$outputs$totals$variables$d_total$description,
    contents$meta$outputs$totals$variables$E_lost$description,
    contents$meta$outputs$totals$variables$E_miss$description
  )  
  
  meta.histogram <- data.frame(name=rbind(
    "CS", "f_CS"
  ))
  meta.histogram$units <- rbind(
    contents$meta$outputs$histogram$variables$CS$units,
    contents$meta$outputs$histogram$variables$f_CS$units
  )
  meta.histogram$desc <- rbind(
    contents$meta$outputs$histogram$variables$CS$description,
    contents$meta$outputs$histogram$variables$f_CS$description
  )
  
  meta.df <- rbind(meta.monthly, meta.totals, meta.histogram  )

  monthly <- data.frame()
  for ( i in 1:length(contents$outputs$monthly) ) {
    m <- data.frame(contents$outputs$monthly[[i]])
    monthly <- rbind(monthly, m)
  }

  totals <- data.frame(contents$outputs$totals)  
  
  
  histogram <- data.frame()
  for ( i in 1:length(contents$outputs$histogram) ) {
    h <- data.frame(contents$outputs$histogram[[i]])  
    histogram <- rbind(histogram, h)
  }

  return(list(input=input.df, meta=meta.df, monthly=monthly, 
              totals=totals, histogram=histogram))  

}
