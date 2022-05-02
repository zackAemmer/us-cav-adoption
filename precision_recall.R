library(ramify)

setwd('./Desktop')

data = read.csv("from_moein.csv")
data$choice_id = paste0(data$scenario, data$id)

runUtilityModel = function (mode, travel_time, travel_cost, hh_income, waiting_time, work_density, age) {
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
  if (mode == "Car") {
    return(-.0258*(travel_time)-6.4561*(travel_cost/hh_income))
  } else if (mode == "Transit") {
    return(-1.7161-0.0222*(travel_time)-6.4561*(travel_cost/hh_income)-.0128*(waiting_time)+.00003190*(work_density))
  } else if (mode == "Transit + Ride-sourcing") {
    return(-3.8843-.0205*(travel_time)-6.4561*(travel_cost/hh_income)-.0381*(waiting_time)+.00002393*(work_density)+.8575*(is_age_18_29)+.5547*(is_age_30_39)+1.2103*(is_age_40_49)+1.1096*(is_age_50_59))
  } else if (mode == "Solo Ride-sourcing") {
    return(-2.8627-.0242*(travel_time)-6.4561*(travel_cost/hh_income)-.0566*(waiting_time)+.00001393*(work_density)+.3538*(is_age_18_29)+.4071*(is_age_30_39)+.1742*(is_age_40_49)+.2461*(is_age_50_59))
  } else if (mode == "Pooled Ride-sourcing") {
    return(-4.6888-.0279*(travel_time)-6.4561*(travel_cost/hh_income)-.0566*(waiting_time)+.00001393*(work_density))
  } else if (mode == "Walk") {
    return(-2.8525-.0321*(travel_time))
  } else if (mode == "Bike") {
    return(-2.8109-.0578*(travel_time))
  } else {
    return(NA)
  }
}

# Run model to get predictions
data$util = mapply(runUtilityModel, data$alt, data$TT, data$TC, data$incmidbin/1000, data$WT, data$w_den, data$age)

# Get predicted mode for each presented choice
choice_sp = c()
choice_pred = c()
choice_ids = unique(data$choice_id)
for (i in 1:length(choice_ids)) {
  choice_data = data[data$choice_id==choice_ids[i],]
  choice_sp = c(choice_sp, choice_data[choice_data$choice==1,]$alt)
  max_util = max(choice_data$util)
  choice_pred = c(choice_pred, choice_data[choice_data$util==max_util,]$alt)
}

# Function to print precision and recall for a single mode
get_prec_recall = function (mode, choice_pred, choice_sp) {
  TP = 0
  FP = 0
  TN = 0
  FN = 0
  for (i in 1:length(choice_pred)) {
    if (choice_pred[i]==mode && choice_sp[i]==mode) {
      TP = TP+1
    } else if (choice_pred[i]==mode && choice_sp[i]!=mode) {
      FP = FP+1
    } else if (choice_pred[i]!=mode && choice_sp[i]==mode) {
      FN = FN+1
    } else if (choice_pred[i]!=mode && choice_sp[i]!=mode) {
      TN = TN+1
    }
  }
  precision = TP / (TP+FP)
  recall = TP / (TP+FN)
  print(paste(mode, precision, recall, sep=" "))
}

# Calculate statistics
accuracy = sum(choice_pred==choice_sp) / length(choice_pred)
get_prec_recall("Car", choice_pred, choice_sp)
get_prec_recall("Transit", choice_pred, choice_sp)
get_prec_recall("Transit + Ride-sourcing", choice_pred, choice_sp)
get_prec_recall("Solo Ride-sourcing", choice_pred, choice_sp)
get_prec_recall("Pooled Ride-sourcing", choice_pred, choice_sp)
# get_prec_recall("Walk", choice_pred, choice_sp) # Not in Moein's data
# get_prec_recall("Bike", choice_pred, choice_sp) # Not in Moein's data
