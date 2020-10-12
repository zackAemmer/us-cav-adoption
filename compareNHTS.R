library(spatstat)
library(plotrix)

#Read in Data from NHTS and the matched PUMS
matched = readRDS('C:/users/zae5o/desktop/stl/toyota av/seattle/matched.rds')
perpub = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/NHTS/perpub.csv", stringsAsFactors = F)

#Get full persons dataset with only travel time and mode
nhts = data.frame(perpub$HOUSEID, perpub$PERSONID, perpub$R_AGE, perpub$TIMETOWK, perpub$WRKTRANS, perpub$WRK_HOME, perpub$WTPERFIN, perpub$HHFAMINC, perpub$HHVEHCNT)
names(nhts) = c("HOUSEID","PERSONID","R_AGE","TIMETOWK","WRKTRANS","WRK_HOME","WTPERFIN","HHFAMINC","HHVEHCNT")

#Throw out under 16, non-responses to commute time, other modes, and work from home
nhts = nhts[nhts$R_AGE>15,]
nhts = nhts[nhts$TIMETOWK>0,]
nhts = nhts[nhts$WRK_HOME != 1,]
nhts = nhts[!(nhts$WRKTRANS %in% c(-1,-7,-8,-9,7,8,9,10,17,18,19,97)),]

#Test lower sample of matched individuals
#matched = matched_all[sample(nrow(matched_all), 10, replace = FALSE),]

##
#Mode shares
#1:car; 2,3,4,5,6:transit; 9:bike; 10:walk; 7,8,12:other; 11:workathome. other is not used, workathome is removed from PUMS dataset, other modes are removed in matching process
matched[matched$mode %in% c(1),]$mode = "car"
matched[matched$mode %in% c(2,3,4,5,6),]$mode = "transit"
matched[matched$mode %in% c(10),]$mode = "walk"
matched[matched$mode %in% c(9),]$mode = "bike"

#Mode share NHTS labeling
nhts[nhts$WRKTRANS %in% c(3,4,5,6),]$WRKTRANS = "car"
nhts[nhts$WRKTRANS %in% c(10,11,12,13,14,15,16,20),]$WRKTRANS = "transit"
nhts[nhts$WRKTRANS %in% c(1),]$WRKTRANS = "walk"
nhts[nhts$WRKTRANS %in% c(2),]$WRKTRANS = "bike"

#NHTS weighted modeshare calculations
carwtsum = sum(nhts[nhts$WRKTRANS == "car",]$WTPERFIN)
transitwtsum = sum(nhts[nhts$WRKTRANS == "transit",]$WTPERFIN)
bikewtsum = sum(nhts[nhts$WRKTRANS == "bike",]$WTPERFIN)
walkwtsum = sum(nhts[nhts$WRKTRANS == "walk",]$WTPERFIN)

#PUMS modeshare calculations
carsum = nrow(matched[matched$mode == "car",])
transitsum = nrow(matched[matched$mode == "transit",])
bikesum = nrow(matched[matched$mode == "bike",])
walksum = nrow(matched[matched$mode == "walk",])

#Form summary modeshare counts dataframes
nhts_sums = c(carwtsum,transitwtsum,bikewtsum,walkwtsum)
pums_sums = c(carsum, transitsum, bikesum, walksum)

##
#Vehicle availability
zerowtsum = sum(nhts[nhts$HHVEHCNT == 0,]$WTPERFIN)
onewtsum = sum(nhts[nhts$HHVEHCNT == 1,]$WTPERFIN)
twowtsum = sum(nhts[nhts$HHVEHCNT == 2,]$WTPERFIN)
threepluswtsum = sum(nhts[nhts$HHVEHCNT >= 3,]$WTPERFIN)

#PUMS vehicle availability calculations
zerosum = nrow(matched[matched$vehicles == 0,])
onesum = nrow(matched[matched$vehicles == 1,])
twosum = nrow(matched[matched$vehicles == 2,])
threeplussum = nrow(matched[matched$vehicles >= 3,])

#Form summary vehicle counts dataframes, clean workspace
nhts_veh_sums = c(zerowtsum, onewtsum, twowtsum, threepluswtsum)
pums_veh_sums = c(zerosum, onesum, twosum, threeplussum)

#Plot vehicle availability barplots
veh_labels = c("0","1","2","3+")
#par(mfrow=c(1,2))
#barplot(nhts_veh_sums/sum(nhts_veh_sums), names.arg = veh_labels, ylim = c(0,1), main = "NHTS", xlab = "Vehicles per HH")
#barplot(pums_veh_sums/sum(pums_veh_sums), names.arg = veh_labels, ylim = c(0,1), main = "Matched PUMS", xlab = "Vehicles per HH")

##
#Load the original PUMS dataset to compare mode shares, remove PUMS not in Seattle
pums_data_selected = readRDS("C:/Users/Zae5o/Desktop/STL/Toyota AV/Seattle/pums_data_selected.rds")
pums_data_selected = pums_data_selected[pums_data_selected$mode %in% c(1:6,9,10),]
pums_data_selected = pums_data_selected[pums_data_selected$sample_geo %in% c(5311601:5311605),]

#Cleaned PUMS mode share calculations
pums_car_sum = nrow(pums_data_selected[pums_data_selected$mode == 1,])
pums_transit_sum = nrow(pums_data_selected[pums_data_selected$mode %in% c(2,3,4,5,6),])
pums_bike_sum = nrow(pums_data_selected[pums_data_selected$mode == 9,])
pums_walk_sum = nrow(pums_data_selected[pums_data_selected$mode == 10,])
pums_total = sum(pums_car_sum,pums_bike_sum,pums_transit_sum,pums_walk_sum)
pums_data_sums = c(pums_car_sum, pums_transit_sum, pums_bike_sum, pums_walk_sum)
mode_labels = c("car","transit","bike","walk")
par(mfrow=c(1,3))
#png("C:/users/zae5o/desktop/modeshare.png", width = 480, height = 480, units = "px")
#barplot(nhts_sums/sum(nhts_sums), names.arg = mode_labels, ylim = c(0,1), main = "NHTS Modeshare")
barplot(c(0.64,0.22,0.04,0.10), names.arg = mode_labels, ylim = c(0,1), main = "Seattle ACS Modeshare")
barplot(pums_data_sums/sum(pums_data_sums), names.arg = mode_labels, ylim = c(0,1), main = "Pre-Matching PUMS Modeshare")
barplot(pums_sums/sum(pums_sums), names.arg = mode_labels, ylim = c(0,1), main = "Post-Matching PUMS Modeshare")
#cat(dev.off())

rm(pums_car_sum,pums_transit_sum,pums_bike_sum,pums_walk_sum,pums_total,pums_data_sums)
rm(nhts_veh_sums,pums_veh_sums,veh_labels)
rm(zerowtsum,onewtsum,twowtsum,threepluswtsum,zerosum,onesum,twosum,threeplussum)
rm(nhts_sums,pums_sums,mode_labels)
rm(carwtsum,transitwtsum,bikewtsum,walkwtsum,carsum,transitsum,bikesum,walksum)




#### Redoing commute time/age/income plots as overlaid ECDFs ####
nhts_times_ewcdf = ewcdf(nhts$TIMETOWK, nhts$WTPERFIN)
pums_times_ecdf = ewcdf(pums_data_selected$commute_time)
matched_times_ecdf = ewcdf(matched$commute_time)
nhts_ages_ewcdf = ewcdf(nhts$R_AGE, nhts$WTPERFIN)
pums_ages_ecdf = ewcdf(pums_data_selected$age)
matched_ages_ecdf = ewcdf(matched$age)

#Commute Time
time_range = c(0:200)
plot(nhts_times_ewcdf(time_range), col = "red", type = "l", main = "Commute Times", xlab = "Minutes", ylab = "Cumulative %")
points(matched_times_ecdf(time_range), col = "blue", type = "l")
points(pums_times_ecdf(time_range), col = "green", type = "l")
legend("bottomright", c("NHTS","PUMS","Pre-Matching"), fill = c("red","blue","green"))

#Age
age_range = c(0:100)
plot(nhts_ages_ewcdf(age_range), col = "red", type = "l", main = "Ages", xlab = "Years", ylab = "Cumulative %")
points(matched_ages_ecdf(age_range), col = "blue", type = "l")
points(pums_ages_ecdf(age_range), col = "green", type = "l")
legend("bottomright", c("NHTS","PUMS","Pre-Matching"), fill = c("red","blue","green"))

#HH Income
#1:less10, 2:10-149, 3:15-249, 4:25-349, 5:35-499, 6;50-749, 7:75-999, 8:100-1249, 9:125-149, 10:150-199, 11:200+
#income_range = c(1:11)
income_range = c(0:200000)
pums_cut_income = as.numeric(cut(matched$hh_income, breaks = c(-9999999,10000,14999,24999,34999,49999,74999,99999,124999,149999,199999,9999999), right = T, labels = c(1:11)))
nhts_ans_income = nhts[nhts$HHFAMINC >= 1,]

nhts_ans_income[nhts_ans_income$HHFAMINC==1,]$HHFAMINC = 10000
nhts_ans_income[nhts_ans_income$HHFAMINC==2,]$HHFAMINC = 15000
nhts_ans_income[nhts_ans_income$HHFAMINC==3,]$HHFAMINC = 25000
nhts_ans_income[nhts_ans_income$HHFAMINC==4,]$HHFAMINC = 35000
nhts_ans_income[nhts_ans_income$HHFAMINC==5,]$HHFAMINC = 50000
nhts_ans_income[nhts_ans_income$HHFAMINC==6,]$HHFAMINC = 75000
nhts_ans_income[nhts_ans_income$HHFAMINC==7,]$HHFAMINC = 100000
nhts_ans_income[nhts_ans_income$HHFAMINC==8,]$HHFAMINC = 125000
nhts_ans_income[nhts_ans_income$HHFAMINC==9,]$HHFAMINC = 150000
nhts_ans_income[nhts_ans_income$HHFAMINC==10,]$HHFAMINC = 200000
nhts_ans_income[nhts_ans_income$HHFAMINC==11,]$HHFAMINC = 250000
pums_cut_income[pums_cut_income==1] = 10000
pums_cut_income[pums_cut_income==2] = 15000
pums_cut_income[pums_cut_income==3] = 25000
pums_cut_income[pums_cut_income==4] = 35000
pums_cut_income[pums_cut_income==5] = 50000
pums_cut_income[pums_cut_income==6] = 75000
pums_cut_income[pums_cut_income==7] = 100000
pums_cut_income[pums_cut_income==8] = 125000
pums_cut_income[pums_cut_income==9] = 150000
pums_cut_income[pums_cut_income==10] = 200000
pums_cut_income[pums_cut_income==11] = 250000

nhts_incomes_ewcdf = ewcdf(nhts_ans_income$HHFAMINC, nhts_ans_income$WTPERFIN)
pums_incomes_ecdf = ewcdf(pums_data_selected$hh_income)
matched_incomes_ecdf = ewcdf(matched$hh_income)

plot(nhts_incomes_ewcdf(income_range), col = "red", type = "l", main = "HH Incomes", xlab = "USD", ylab = "Cumulative %")
points(matched_incomes_ecdf(income_range), col = "blue", type = "l")
points(pums_incomes_ecdf(income_range), col = "green", type = "l")
legend("bottomright", c("NHTS","PUMS","Pre-Matching"), fill = c("red","blue","green"))

rm(age_range,income_range,time_range)
rm(nhts_ages_ewcdf,nhts_incomes_ewcdf,nhts_times_ewcdf,matched_incomes_ecdf, nhts_traveltime_ewcdf,pums_ages_ecdf,pums_incomes_ecdf,pums_times_ecdf,pums_traveltime_ecdf)
rm(pums_cut_income,nhts_ans_income)


