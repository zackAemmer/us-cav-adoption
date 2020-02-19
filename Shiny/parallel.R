

library(foreach)
library(doParallel)
library(doSNOW)
library(profvis)

system.time({
  numCores = detectCores() - 1
  cl = makeCluster(numCores)
  registerDoSNOW(cl)
  foreach (i=1:100000) %dopar% {
    car_utility = runUtilityModel("car", matched$drvtime[i], matched$drvDist[i], matched$hh_income[i]/1000, matched$density[i], matched$age[i], matched$trsfare[i], 5, 5, 5, 5, matched$uber_tt_trs[i], matched$uber_tt_srs[i], matched$uber_tt_prs[i], matched$ridestrstime[i], matched$ridesDist[i], matched$parking_cost[i], 5, 5, 5, 5)
  }
  stopCluster(cl)
})

system.time({
  car_utility = mapply(runUtilityModel, "car", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
})


profvis({
  cat("Calculating Utilities...\n")
  car_utility = mapply(runUtilityModel, "car", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("car done..\n")
  transit_utility = mapply(runUtilityModel, "transit", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("transit done..\n")
  transit_rs_utility = mapply(runUtilityModel, "transit_rs", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("trs done..\n")
  solo_rs_utility = mapply(runUtilityModel, "solo_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("solo done..\n")
  pooled_rs_utility = mapply(runUtilityModel, "pooled_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("pooled done..\n")
  walk_utility = mapply(runUtilityModel, "walk", matched$wlktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("walk done..\n")
  bike_utility = mapply(runUtilityModel, "bike", matched$biktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, 5, 5, 5, 5, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, 5, 5, 5, 5)
  cat("bike done..\n")
  cat("Finished Utility Calculation\n")
  utilities = data.frame(matched, car_utility, transit_utility, transit_rs_utility, solo_rs_utility, pooled_rs_utility, walk_utility, bike_utility)
})

