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
ods1 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_1_40k.csv", stringsAsFactors = F)[,-1]
ods2 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_40_80k.csv", stringsAsFactors = F)[,-1]
ods3 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_80_120k.csv", stringsAsFactors = F)
ods4 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_120_160k.csv", stringsAsFactors = F)[,-1]
ods5 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_160_all.csv", stringsAsFactors = F)[,-1]
#ods6 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_140_157k.csv", stringsAsFactors = F)
#ods7 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata3_157_all.csv", stringsAsFactors = F)#[,-1]
#ods8 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsample_fulldata/apifulldata2170_all.csv", stringsAsFactors = F)


#Fix column names, remove "+" character on coordinates, and combine
names_ods = names(ods1)
names(ods1) = names_ods
names(ods2) = names_ods
names(ods3) = names_ods
names(ods4) = names_ods
names(ods5) = names_ods
names(ods6) = names_ods
names(ods7) = names_ods
names(ods8) = names_ods
fulldata_ods = rbind(ods1,ods2,ods3,ods4,ods5)
write.csv(fulldata_ods, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/fulldata_ods_3.csv", row.names = F)
fulldata_ods = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/fulldata_ods_3.csv", stringsAsFactors = F)
rm(ods1,ods2,ods3,ods4,ods5,ods6,ods7,ods8,names_ods)

#Load in the ODs that have been sorted to joint distribution
odsz0 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/joint distribution data/z0_3.csv", stringsAsFactors = F)
odsz1 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/joint distribution data/z1_3.csv", stringsAsFactors = F)
odsz2 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/joint distribution data/z2_3.csv", stringsAsFactors = F)
odsz3 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/joint distribution data/z3_3.csv", stringsAsFactors = F)
odsz4 = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/joint distribution data/z4_3.csv", stringsAsFactors = F)
fulldata_odsz = rbind(odsz0,odsz1,odsz2,odsz3,odsz4)
write.csv(fulldata_odsz, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/odsz_3.csv", row.names = F)
fulldata_odsz = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/fulldata_odsz_3.csv", stringsAsFactors = F)
rm(odsz0,odsz1,odsz2,odsz3,odsz4)

#Match the Bing API call info to the joint distribution ODs
ODs_to_match = merge(fulldata_odsz, fulldata_ods, by = c("h_geocode", "w_geocode"))
ODs_to_match = ODs_to_match[,c(1,2,6:14,26:37)]
names(ODs_to_match) = c(names(ODs_to_match)[1:2], 'a1e1','a2e1','a3e1','a1e2','a2e2','a3e2','a1e3','a2e3','a3e3', names(ODs_to_match)[12:13], 'puma_code', names(ODs_to_match)[15:20], 'drvDist','drvtime','trstime')
rm(fulldata_ods,fulldata_odsz)

#Use the linear models (created/tested above on google API data) to predict walking/biking times for the joint distribution ODs
ODs_to_match$biktime = predict(rlm_dist_bike, ODs_to_match) / 60
ODs_to_match$wlktime = predict(rlm_dist_walk, ODs_to_match) / 60

#Don't use ODs where distance is 0
ODs_to_match = ODs_to_match[ODs_to_match$drvDist != 0,]

#Handle 404
ODs_to_match = ODs_to_match[ODs_to_match$drvDist != 404.000000,]
ODs_to_match[ODs_to_match$trstime == 404.000000,]$trstime = Inf
ODs_to_match = ODs_to_match[order(ODs_to_match$puma_code),]
mislabeled = read.csv("C:/users/Zae5o/Desktop/STL/Toyota AV/Data/mislabeled_trs_2.csv", stringsAsFactors = F)
mislabeled$label = 1
ODs_to_match$label = 0
mislabeled = mislabeled[,c(6,7,27)]
x = merge(mislabeled, ODs_to_match, by = c("h_geocode","w_geocode"), all.y = T)
x$label.y[complete.cases(x$label.x)] = 1
ODs_to_match[x$label.y == 1,]$trstime = Inf
rm(mislabeled,x)

#Save the ODs to match
write.csv(ODs_to_match, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/ODs_to_match_3.csv", row.names = F)

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

ODs_to_match = read.csv('C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/ODs_to_match_3.csv', stringsAsFactors = F)

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
  } else if (trstime != Inf && mode %in% c(2:6) && checkRange(commute_time, trstime, rangeCP+.2)) {
    return(TRUE)
  } else if (mode == 9 && checkRange(commute_time, biktime, rangeCP+.2)) {
    return(TRUE)
  } else if (mode == 10 && checkRange(commute_time, wlktime, rangeCP+.2)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

plot(density(ODs_to_match$drvtime), col = "red", type = "l", main = "Commute Times", xlab = "Minutes", ylab = "Probability Density", xlim = c(0,200))
points(density(ODs_to_match$trstime[ODs_to_match$trstime != Inf]), col = "blue", type = "l")
points(density(ODs_to_match$wlktime), col = "green", type="l")
legend("bottomright", c("Drive","Transit","Walk"), fill = c("red","blue","green"))
x = pums_data_selected[pums_data_selected$mode==1,]
y = pums_data_selected[pums_data_selected$mode %in% c(2:6),]
z = pums_data_selected[pums_data_selected$mode==10,]
plot(density(x$commute_time), col = "red", type = "l", main = "Commute Times", xlab = "Minutes", ylab = "Probability Density", xlim = c(0,200))
points(density(y$commute_time), col = "blue", type = "l")
points(density(z$commute_time), col = "green", type="l")
legend("bottomright", c("Drive","Transit","Walk"), fill = c("red","blue","green"))

#This take a few hours, should be rewritten in Python or consult with someone better at R
final = data.frame(matrix(ncol = 24))
#final = data.frame('hid'=numeric(),'pid'=numeric(),'sample_geo'=numeric(),'age'=numeric(),'income'=numeric(),'mode'=numeric(),'commute_time'=numeric(),'vehicles'=numeric(),'hh_income'=numeric(),'h_geocode'=numeric(),'w_geocode'=numeric(),'puma_code'=character(),'w_lat'=numeric(),'w_lon'=numeric(),'h_lat'=numeric(),'h_lon'=numeric(),'origins'=character(),'destinations'=character(),'drvDist'=numeric(),'drvtime'=numeric(),'trstime'=numeric(),'biktime'=numeric(),'wlktime'=numeric(),'rangeCount'=numeric())
names(final) = c('hid','pid','sample_geo','age','income','mode','commute_time','vehicles','hh_income','h_geocode','w_geocode',names(ODs_to_match)[14:25], 'rangeCount')

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
  print(od$puma_code)
}
rm(od,nums,random_PUM,possible_PUMS,i,j,k,rangetop,rangeCount,toAdd)

#Write the matched ODs/PUMS to file
final = final[-1,]
write.csv(final, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/matched_3.csv", row.names = F)




rm(a1e1,a1e2,a1e3,a2e1,a2e2,a2e3,a3e1,a3e2,a3e3)



