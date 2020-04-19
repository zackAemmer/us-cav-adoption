#Type is Car, Transit, Transit_RS, Solo_RS, Pooled_RS, Walk, Bike
#The travel time argument should be the time corresponding to the mode being tested
#The travel distance argument is always the driving distance
runUtilityModelParallel = function (df) {
  
  #some unfortunate redundancy for the sake of making the actual equations a bit more clear
  names(df) = c("mode", "travel_time", "travel_distance", "hh_income", "work_density", "age", "trsfare", "service_price_srs", "service_price_srs_base", "service_price_prs",
                  "service_price_prs_base", "service_tt_trs", "service_tt_srs", "service_tt_prs", "ridestrstime", "ridesDist", "parking_cost", "service_wait_time", "transit_wait_time", "mpg", "cost_gallon")
  mode = df$mode
  travel_time = df$travel_time
  travel_distance = df$travel_distance
  hh_income = df$hh_income
  work_density = df$work_density
  age = df$age
  trsfare = df$trsfare
  service_price_srs = df$service_price_srs
  service_price_srs_base = df$service_price_srs_base
  service_price_prs = df$service_price_prs
  service_price_prs_base = df$service_price_prs_base
  service_tt_trs = df$service_tt_trs
  service_tt_srs = df$service_tt_srs
  service_tt_prs = df$service_tt_prs
  ridestrstime = df$ridestrstime
  ridesDist = df$ridesDist
  parking_cost = df$parking_cost
  service_wait_time = df$service_wait_time
  transit_wait_time = df$transit_wait_time
  mpg = df$mpg
  cost_gallon = df$cost_gallon

  #Determine binary age variables
  if (age < 30) {
    is_age_18_29 = 1
    is_age_30_39 = 0
    is_age_40_49 = 0
    is_age_50_59 = 0
  } else if (age < 40) {
    is_age_18_29 = 0
    is_age_30_39 = 1
    is_age_40_49 = 0
    is_age_50_59 = 0
  } else if (age < 50) {
    is_age_18_29 = 0
    is_age_30_39 = 0
    is_age_40_49 = 1
    is_age_50_59 = 0
  } else if (age < 60) {
    is_age_18_29 = 0
    is_age_30_39 = 0
    is_age_40_49 = 0
    is_age_50_59 = 1
  } else {
    is_age_18_29 = 0
    is_age_30_39 = 0
    is_age_40_49 = 0
    is_age_50_59 = 0
  }
  
  #Determine Utility and return it depending on mode argument
  if (mode == "car") {
    gas_fee = (travel_distance/mpg)*cost_gallon
    parking_fee = parking_cost
    travel_cost = gas_fee + parking_fee
    return(-.0258*(travel_time)-6.4561*(travel_cost/hh_income))
    
  } else if (mode == "transit") {
    
    travel_cost = trsfare
    waiting_time = transit_wait_time
    return(-1.7161-0.0222*(travel_time)-6.4561*(travel_cost/hh_income)-.0128*(waiting_time)+.00003190*(work_density))
    
  } else if (mode == "transit_rs") {
    
    travel_cost = trsfare + (service_price_srs_base + service_price_srs*ridesDist)
    waiting_time = transit_wait_time + service_wait_time
    travel_time = ridestrstime + service_tt_trs
    return(-3.8843-.0205*(travel_time)-6.4561*(travel_cost/hh_income)-.0381*(waiting_time)+.00002393*(work_density)+.8575*(is_age_18_29)+.5547*(is_age_30_39)+1.2103*(is_age_40_49)+1.1096*(is_age_50_59))
    
  } else if (mode == "solo_rs") {
    
    travel_cost = service_price_srs_base + (service_price_srs * travel_distance)
    waiting_time = service_wait_time
    travel_time = service_tt_srs
    return(-2.8627-.0242*(travel_time)-6.4561*(travel_cost/hh_income)-.0566*(waiting_time)+.00001393*(work_density)+.3538*(is_age_18_29)+.4071*(is_age_30_39)+.1742*(is_age_40_49)+.2461*(is_age_50_59))
    
  } else if (mode == "pooled_rs") {
    
    travel_cost = service_price_prs_base + (service_price_prs * travel_distance)
    waiting_time = service_wait_time
    travel_time = service_tt_prs
    return(-4.6888-.0279*(travel_time)-6.4561*(travel_cost/hh_income)-.0566*(waiting_time)+.00001393*(work_density))
    
  } else if (mode == "walk") {
    
    return(-2.8525-.0321*(travel_time))
    
  } else if (mode == "bike") {
    
    return(-2.8109-.0578*(travel_time))
    
  } else {
    return(NA)
  }
}
