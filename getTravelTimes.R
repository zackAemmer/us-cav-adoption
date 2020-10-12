library(stringr)
library(MASS)
library(httr)
library(jsonlite)
library(dplyr)


#Small loop to get total population (of 16 older etc. in the study)
pop = c()
for (puma in list_of_pumas) {
  print(puma)
  x = nrow(pums_data_selected[pums_data_selected$sample_geo == puma,])*20
  print(x)
  pop = c(pop,x)
}
puma_pop = data.frame(list_of_pumas,pop)
write.csv(puma_pop, "C:/users/zae5o/desktop/stl/toyota av/data/puma_pop.csv", row.names = FALSE)

#### Google API Stuff Start ####
#Read in PUMS sample data (original and binned)
list_of_pumas = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/list_of_pumas.csv", stringsAsFactors = F)
list_of_pumas = list_of_pumas$x
cleaned_pums = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/cleaned_pums.csv", stringsAsFactors = F)
pums_data_selected = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/pums_data_selected.csv", stringsAsFactors = F)
#names(pums_data_selected) = c("sample_geo","JWTR","JWMNP","PINCP","AGEP","pid","hid","VEH","state_code")

#Walking and Biking speeds from HCM in ft/second
#speed_walking = 4.7
#speed_biking = 18.8

#Google Key for API requests
API_key = ""

#Function to create google API call string
createAPIString = function(origin, destination, key, mode) {
  x = paste0("https://maps.googleapis.com/maps/api/directions/json?origin=",origin,"&destination=",destination,"&key=",key,"&mode=",mode)
  return(x)
}

#Function to call the google API, record the response in Data folder, and update data frame
callAPI = function(X, origin, destination, travel_mode) {
  api_string = createAPIString(origin,destination,API_key,travel_mode)
  response = GET(api_string)
  response_content = content(response, as = "text")
  response_content_clean = content(response, as = "parsed")
  write(response_content, paste0("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/google_calls_1/", X, "_", travel_mode, ".json"))
  
  #handle case where no route is available
  if (response_content_clean$status == "OK") {
    return(response_content_clean$routes[[1]]$legs[[1]]$duration$value)
  } else {
    return(NA)
  }
}

#Set up dataframe to store resulting travel times in seconds
google_ods = data.frame(matrix(ncol = 27))
nametemp = c("bike_times","walk_times")
names(google_ods) = append(names(fulldata_ods), nametemp)

#For each puma and 5 random ODs, make an API call to determine biking and walking times from the Google API
#Save every response from the Google API in a json file
for (puma in x) {
  
  #subset ods by puma
  temp = fulldata_ods[fulldata_ods$puma_code == puma,]
  print(paste0("Current PUMA is ", puma))
  
  #get 5 random ods to sample with google
  random_ods = temp[sample(nrow(temp), 5, replace = F),]
  
  #run google API for each OD
  bike_times = mapply(callAPI, random_ods$X, random_ods$origins, random_ods$destinations, "bicycling")
  walk_times = mapply(callAPI,random_ods$X, random_ods$origins, random_ods$destinations, "walking")
  od_results = cbind(random_ods, bike_times, walk_times)
  google_ods = rbind(google_ods, od_results)
}
rm(temp,response,response_content,response_content_clean,puma,od,nametemp,test_string,od_results,random_ods,walk_times,bike_times)
google_ods = google_ods[2:nrow(google_ods),]
write.csv(google_ods, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/google_calls_1/google_ods_1.csv", row.names = F)
write.csv(pums_data_selected, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/pums_data_selected.csv", row.names = F)

google_ods = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/google_calls_1/google_ods_1.csv", stringsAsFactors = F)

#Train the linear models with independent drive dist and dependent walk/bike time
rlm_dist_walk = rlm(walk_times ~ 0 + drvDist, google_ods)
rlm_dist_bike = rlm(bike_times ~ 0 + drvDist, google_ods)

plot(google_ods$drvDist, google_ods$bike_times/60, col = "black", type = "p", main = "Google API Calls", xlab = "Distance (miles)", ylab = "Time (minutes)")
points(google_ods$drvDist, google_ods$walk_times/60, col = "red", type = "p")
legend("bottomright", c("Biking","Walking"), fill = c("black","red"))

#### Google API Stuff End ####









####################### Below this is the process of matching PUMS to ODs ############################
#Read in ODs that have driving/transit info from the Bing API
#ODs_to_match = read.csv("C:/users/zae5o/desktop/stl/toyota av/data/ODs_to_match_1.csv")

ODs_to_match = readRDS("C:/users/zae5o/desktop/stl/toyota av/seattle/seattle_1_api.rds")
names(ODs_to_match)[3:11] = c('a1e1','a2e1','a3e1','a1e2','a2e2','a3e2','a1e3','a2e3','a3e3')
ODs_to_match$a1e1 = as.integer(ODs_to_match$a1e1)
ODs_to_match$a2e1 = as.integer(ODs_to_match$a2e1)
ODs_to_match$a3e1 = as.integer(ODs_to_match$a3e1)
ODs_to_match$a1e2 = as.integer(ODs_to_match$a1e2)
ODs_to_match$a2e2 = as.integer(ODs_to_match$a2e2)
ODs_to_match$a3e2 = as.integer(ODs_to_match$a3e2)
ODs_to_match$a1e3 = as.integer(ODs_to_match$a1e3)
ODs_to_match$a2e3 = as.integer(ODs_to_match$a2e3)
ODs_to_match$a3e3 = as.integer(ODs_to_match$a3e3)

ODs_to_match$drvDist = as.numeric(paste(ODs_to_match$drvDist))
ODs_to_match$drvtime = as.numeric(paste(ODs_to_match$drvtime))
ODs_to_match$trstime = as.numeric(paste(ODs_to_match$trstime))
google_ods = readRDS("C:/users/zae5o/desktop/stl/toyota av/seattle/google_ods.rds")
pums_data_selected = readRDS("C:/users/zae5o/desktop/stl/toyota av/seattle/pums_data_selected.rds")

#Use the linear models (created/tested above on google API data) to predict walking/biking times for the joint distribution ODs
rlm_dist_walk = rlm(walk_times ~ 0 + drvDist, google_ods)
rlm_dist_bike = rlm(bike_times ~ 0 + drvDist, google_ods)
ODs_to_match$biktime = predict(rlm_dist_bike, ODs_to_match) / 60
ODs_to_match$wlktime = predict(rlm_dist_walk, ODs_to_match) / 60

#Don't use ODs where distance is 0
ODs_to_match = ODs_to_match[ODs_to_match$drvDist != 0,]

#Handle 404
ODs_to_match = ODs_to_match[ODs_to_match$drvDist != 404.000000,]
ODs_to_match[ODs_to_match$trstime == 404.000000,]$trstime = Inf

#Break PUMS into 9 categories for faster selection during OD matching process
a1e1 = pums_data_selected[0<=pums_data_selected$age & pums_data_selected$age<=29 & -999999<=pums_data_selected$income & pums_data_selected$income<=15000,]
a2e1 = pums_data_selected[30<=pums_data_selected$age & pums_data_selected$age<=54 & -999999<=pums_data_selected$income & pums_data_selected$income<=15000,]
a3e1 = pums_data_selected[55<=pums_data_selected$age & pums_data_selected$age<=999 & -999999<=pums_data_selected$income & pums_data_selected$income<=15000,]
a1e2 = pums_data_selected[0<=pums_data_selected$age & pums_data_selected$age<=29 & 15001<=pums_data_selected$income & pums_data_selected$income<=39996,]
a2e2 = pums_data_selected[30<=pums_data_selected$age & pums_data_selected$age<=54 & 15001<=pums_data_selected$income & pums_data_selected$income<=39996,]
a3e2 = pums_data_selected[55<=pums_data_selected$age & pums_data_selected$age<=999 & 15001<=pums_data_selected$income & pums_data_selected$income<=39996,]
a1e3 = pums_data_selected[0<=pums_data_selected$age & pums_data_selected$age<=29 & 39997<=pums_data_selected$income & pums_data_selected$income<=9999999,]
a2e3 = pums_data_selected[30<=pums_data_selected$age & pums_data_selected$age<=54 & 39997<=pums_data_selected$income & pums_data_selected$income<=9999999,]
a3e3 = pums_data_selected[55<=pums_data_selected$age & pums_data_selected$age<=999 & 39997<=pums_data_selected$income & pums_data_selected$income<=9999999,]

#Function to check if x is within percentage difference of y (to be used with travel times)
checkRange = function(x, y, rangeCR) {
  if (y*(1-rangeCR) <= x && x <= y*(1+rangeCR)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Function to check if the given random sample could feasibly complete their commute in the given OD
#1:car; 2,3,4,5,6:transit; 9:bike; 10:walk; 7,8,12:other; 11:workathome. other is not used, workathome is removed from PUMS dataset
checkCommutePlausible = function(mode, commute_time, drvtime, trstime, wlktime, biktime, rangeCP) {
  if (drvtime != Inf && mode == 1 && checkRange(commute_time, drvtime, rangeCP)) {
    return(TRUE)
  } else if (trstime != Inf && mode %in% c(2:6) && checkRange(commute_time, trstime, rangeCP)) {
    return(TRUE)
  } else if (mode == 9 && checkRange(commute_time, biktime, rangeCP+.2)) {
    return(TRUE)
  } else if (mode == 10 && checkRange(commute_time, wlktime, rangeCP+.2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Set up structure to hold the results
final = data.frame(matrix(ncol = 24))
names(final) = c('hid','pid','sample_geo','age','income','mode','commute_time','vehicles','hh_income','h_geocode','w_geocode',names(ODs_to_match)[14:25], 'rangeCount')

#Set up cluster
#numCores = detectCores() - 1
#clust = makeCluster(numCores)
#registerDoSNOW(clust)
#clusterExport(cl = clust, c('a1e1','a1e2','a1e3','a2e1','a2e2','a2e3','a3e1','a3e2','a3e3','final','ODs_to_match','checkCommutePlausible','checkRange'), envir = .GlobalEnv)

#Actual matching process
for (k in seq(1,nrow(ODs_to_match))) {
  od = ODs_to_match[k,]
  #Check which category columns are not 0
  cats = which(od[,3:11] != 0)
  nums = od[,3:11][cats]
  rm(cats)
  
  #For each needed member, of each nonzero column; get a PUM to fill its part of the distribution
  for (i in seq(1,ncol(nums))) {
    for (j in seq(1,nums[1,i])) {
      possible_PUMS = get(names(nums)[i])
      possible_PUMS = possible_PUMS[possible_PUMS$sample_geo == od$puma_code,]
      random_PUM = possible_PUMS[sample(nrow(possible_PUMS), 1, replace = F),]
      rangetop = .1
      rangeCount = 1
      while (!(checkCommutePlausible(random_PUM[["mode"]], random_PUM[["commute_time"]], od[["drvtime"]], od[["trstime"]], od[["biktime"]], od[["wlktime"]], rangetop))) {
        random_PUM = possible_PUMS[sample(nrow(possible_PUMS), 1, replace = F),]
        #If 10 PUMS are passed without selection increase the range of acceptable commute times
        if (rangeCount %% 10 == 0) {
          rangetop = rangetop + .1
        }
        rangeCount = rangeCount + 1
      }
      #Once suitable PUMS is found attach it to the final dataframe with the corresponding OD data
      toAdd = c(random_PUM, od$h_geocode, od$w_geocode, od[,14:25], rangeCount)
      final[nrow(final)+1,] = toAdd
    }
  }
  print(od$tract_code)
}
#stopCluster(clust)
rm(od,nums,random_PUM,possible_PUMS,i,j,k,rangetop,rangeCount,toAdd)

#Write the matched ODs/PUMS to file
final = final[-1,]
saveRDS(final, "C:/users/zae5o/desktop/stl/toyota av/seattle/matched.rds")
x = readRDS('C:/users/zae5o/desktop/stl/toyota av/seattle/matched.rds')


rm(a1e1,a1e2,a1e3,a2e1,a2e2,a2e3,a3e1,a3e2,a3e3)



