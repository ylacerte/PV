Horizon_Profile <- function(q) {

  # list of horizon height values for the given location. 
  # list of sun elevation angles at the June and December solstice. 
  # The horizon height is given at 48 equidistant directions starting 
  # from north and moving clockwise towards east, south and then west. 
  # The sun elevation angles are given at half-hour intervals starting at 00:00UTC.
  
  req <- request(q)
  resp <- req_perform(req)
  #  resp_content_type(resp)    # json
  #  resp_encoding(resp)        # UTF-8
  contents <- resp |> resp_body_json() 
  
  input.df <- data.frame(inputs=rbind(
    c(what=contents$meta$inputs$location$variables$latitude$description, 
      units=contents$meta$inputs$location$variables$latitude$units,
      value=contents$inputs$location$latitude),
    c(what=contents$meta$inputs$location$variables$longitude$description, 
      units=contents$meta$inputs$location$variables$longitude$units,
      value=contents$inputs$location$longitude),
    c(what=contents$meta$inputs$location$variables$elevation$description, 
      units=contents$meta$inputs$location$variables$elevation$units,
      value=contents$inputs$location$elevation),
    c(what=contents$meta$inputs$horizon_db$description, 
      units=NA,
      value=contents$inputs$horizon_db)
  ))
  colnames(input.df) <- c("What", "Units", "Value")
  
  output.df <- data.frame(rbind(
    c(what="A", 
      desc=contents$meta$outputs$horizon_profile$variables$A$description,
      units=contents$meta$outputs$horizon_profile$variables$A$units),
    c(what="H_hor", 
      desc=contents$meta$outputs$horizon_profile$variables$H_hor$description,
      units=contents$meta$outputs$horizon_profile$variables$H_hor$units),
    c(what="A_sun(w)", 
      desc=contents$meta$outputs$winter_solstice$variables$`A_sun(w)`$description,
      units=contents$meta$outputs$winter_solstice$variables$`A_sun(w)`$units),
    c(what="H_sun(w)", 
      desc=contents$meta$outputs$winter_solstice$variables$`H_sun(w)`$description,
      units=contents$meta$outputs$winter_solstice$variables$`H_sun(w)`$units),
    c(what="A_sun(s)", 
      desc=contents$meta$outputs$summer_solstice$variables$`A_sun(s)`$description,
      units=contents$meta$outputs$summer_solstice$variables$`A_sun(s)`$units),
    c(what="H_sun(s)", 
      desc=contents$meta$outputs$summer_solstice$variables$`H_sun(s)`$description,
      units=contents$meta$outputs$summer_solstice$variables$`H_sun(s)`$units)
  ))
  
  
  d <- data.frame(v=unlist((contents$outputs$horizon_profile)))
  a <- unlist(contents$outputs$horizon_profile)
  d <- cbind(d, attributes(a))
  d.1 <- d[which(d$names == "A"),]
  d.2 <- d[which(d$names == "H_hor"),]
  horizon_profile <- cbind(A=d.1$v, H_hor=d.2$v)
  
  d <- data.frame(v=unlist((contents$outputs$winter_solstice)))
  a <- unlist(contents$outputs$winter_solstice)
  d <- cbind(d, attributes(a))
  d.1 <- d[which(d$names == "A_sun(w)"),]
  d.2 <- d[which(d$names == "H_sun(w)"),]
  winter_solstice <- cbind(A_sun=d.1$v, H_sun=d.2$v)
  
  d <- data.frame(v=unlist((contents$outputs$summer_solstice)))
  a <- unlist(contents$outputs$summer_solstice)
  d <- cbind(d, attributes(a))
  d.1 <- d[which(d$names == "A_sun(s)"),]
  d.2 <- d[which(d$names == "H_sun(s)"),]
  summer_solstice <- cbind(A_sun=d.1$v, H_sun=d.2$v)
  
  return(list(input=input.df, meta=output.df,
              horizon_profile=data.frame(horizon_profile), 
              winter_solstice=data.frame(winter_solstice),
              summer_solstice=data.frame(summer_solstice)))  
}

