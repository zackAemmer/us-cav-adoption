library(data.table)
library(stringr)
library(dplyr)


############################################################################################################
#Using fread and column names from data.table library to decrease the amount of data R needs to load
pums_person_names = c("ST", "SERIALNO", "PUMA", "PINCP", "ADJINC", "AGEP", "JWTR", "JWRIP", "JWMNP")
pums_household_names = c("ST", "SERIALNO", "PUMA", "VEH", "TYPE", "NP", "HINCP", "ADJINC")

#Loading and combining PUMS person datasets a-d
pums_p_dataa = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_pus/psam_pusa.csv", select = pums_person_names)
pums_p_datab = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_pus/psam_pusb.csv", select = pums_person_names)
pums_p_datac = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_pus/psam_pusc.csv", select = pums_person_names)
pums_p_datad = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_pus/psam_pusd.csv", select = pums_person_names)
pums_p_data_selected = rbind(pums_p_dataa, pums_p_datab, pums_p_datac, pums_p_datad)

#Loading and combining PUMS household datasets a-d
pums_hh_dataa = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_hus/psam_husa.csv", select = pums_household_names)
pums_hh_datab = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_hus/psam_husb.csv", select = pums_household_names)
pums_hh_datac = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_hus/psam_husc.csv", select = pums_household_names)
pums_hh_datad = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/PUMS/csv_hus/psam_husd.csv", select = pums_household_names)
pums_hh_data_selected = rbind(pums_hh_dataa, pums_hh_datab, pums_hh_datac, pums_hh_datad)

#Clean Environment
rm(pums_p_dataa, pums_p_datab, pums_p_datac, pums_p_datad)
rm(pums_hh_dataa, pums_hh_datab, pums_hh_datac, pums_hh_datad)

#Generate unique ids for persons/households, then join hh and person PUMS on Serial Number
pums_p_data_selected$pid = seq.int(nrow(pums_p_data_selected))
pums_hh_data_selected$hid = seq.int(nrow(pums_hh_data_selected))
pums_data_selected = merge(pums_p_data_selected, pums_hh_data_selected, by = "SERIALNO")

#Sort the variables into bins, intervals are inclusive to the left. 0 is a possible value in PUMS age data. Label the resulting bins.
binned_hh_income = cut(pums_data_selected$HINCP, breaks = c(-30000,10000,15000,25000,35000,50000,75000,150000,200000,9999999), right = FALSE, include.lowest = T, labels = c("less_10000","10000_14999","15000_24999","25000_34999","35000_49999","50000_74999","75000_149999","150000_199999","200000_over"))
binned_age = cut(pums_data_selected$AGEP, breaks = c(0,10,20,30,40,50,55,60,70,80,999), right = FALSE, include.lowest = T, labels = c("under_10","10_19","20_29","30_39","40_49","50_54","55_59","60_69","70_79","80_over"))
binned_income = cut(pums_data_selected$PINCP, breaks = c(-30000,10000,15000,25000,35000,40000,50000,65000,75000,9999999), right = FALSE, include.lowest = T, labels = c("less_10000","10000_14999","15000_24999","25000_34999","35000_39999","40000_49999","50000_64999","65000_74999","75000_over"))
binned_mode = cut(pums_data_selected$JWTR, breaks = c(1,2,7,9,10,11,12,13), right = FALSE, include.lowest = T, labels = c("car_truck_van","transit","taxi_motorcycle","bike","walk","work_at_home","other"))
binned_vehicles = cut(pums_data_selected$VEH, breaks = c(0,1,2,3,99), right = FALSE, include.lowest = T, labels = c("0","1","2","3_or_more"))
binned_tt = cut(pums_data_selected$JWMNP, breaks = c(0,10,15,20,25,30,35,45,60,9999), right = FALSE, include.lowest = T, labels = c("less_10","10_14","15_19","20_24","25_29","30_34","35_44","45_59","60_over"))
binned_carpool = cut(pums_data_selected$JWRIP, breaks = c(0,2,99), right = FALSE, include.lowest = T, labels = c("not_carpool", "carpool"))

#Assemble the final data frames and clean
cleaned_pums = data.frame(pums_data_selected$SERIALNO, pums_data_selected$ST.x, pums_data_selected$hid, pums_data_selected$pid, pums_data_selected$PUMA.x, binned_age, binned_income, binned_mode, binned_tt, binned_vehicles, binned_hh_income)
#cleaned_hh_pums = data.frame(pums_hh_data_selected$SERIALNO, pums_hh_data_selected$PUMA, pums_hh_data_selected$ST, pums_hh_data_selected$hid, binned_hh_income)
rm(binned_hh_income,binned_age,binned_income,binned_vehicles,binned_tt,binned_carpool,binned_mode,pums_household_names,pums_person_names)

#Restore leading 0's to the state and puma codes
pums_data_selected$ST.x = str_pad(pums_data_selected$ST.x, 2, pad = 0)
pums_data_selected$PUMA.x = str_pad(pums_data_selected$PUMA.x, 5, pad = 0)
cleaned_pums$pums_data_selected.ST.x = str_pad(cleaned_pums$pums_data_selected.ST.x, 2, pad = 0)
cleaned_pums$pums_data_selected.PUMA.x = str_pad(cleaned_pums$pums_data_selected.PUMA.x, 5, pad = 0)
#cleaned_hh_pums$pums_hh_data_selected.ST = str_pad(cleaned_hh_pums$pums_hh_data_selected.ST, 2, pad = 0)
#cleaned_hh_pums$pums_hh_data_selected.PUMA = str_pad(cleaned_hh_pums$pums_hh_data_selected.PUMA, 5, pad = 0)

#Add the state code digits to the PUMA digits - this forms a unique id and matches the TIGER ids
pums_data_selected$sample_geo = paste0(pums_data_selected$ST.x,pums_data_selected$PUMA.x)
cleaned_pums$sample_geo = paste0(cleaned_pums$pums_data_selected.ST.x,cleaned_pums$pums_data_selected.PUMA.x)
#cleaned_hh_pums = transform(cleaned_hh_pums, sample_geo = paste0(cleaned_hh_pums$pums_hh_data_selected.ST,cleaned_hh_pums$pums_hh_data_selected.PUMA))

#Change column names and order
pums_data_selected = pums_data_selected[,c(18,10,19,6,4,7,9,13,16)]
names(pums_data_selected) = c('hid','pid','sample_geo','age','income','mode','commute_time','vehicles','hh_income')
cleaned_pums = cleaned_pums[,c(3,4,12,6,7,8,9,10,11)]
names(cleaned_pums) = c('hid','pid','sample_geo','age','income','mode','commute_time','vehicles','hh_income')

#cleaned_hh_pums = cleaned_hh_pums[,4:6]
#cleaned_hh_pums = cleaned_hh_pums[,c(1,3,2)]
#names(cleaned_hh_pums) = c('hid','sample_geo','hh_income')

#Remove all samples that have NA values
cleaned_pums = cleaned_pums[complete.cases(cleaned_pums),]
pums_data_selected = pums_data_selected[complete.cases(pums_data_selected),]

#Remove samples from Alaska and Hawaii
pums_data_selected$state_code = substr(pums_data_selected$sample_geo, 0, 2)
pums_data_selected = pums_data_selected[pums_data_selected$state_code != '02',]
pums_data_selected = pums_data_selected[pums_data_selected$state_code != '15',]
pums_data_selected = pums_data_selected[,1:9]
cleaned_pums$state_code = substr(cleaned_pums$sample_geo, 0, 2)
cleaned_pums = cleaned_pums[cleaned_pums$state_code != '02',]
cleaned_pums = cleaned_pums[cleaned_pums$state_code != '15',]
cleaned_pums = cleaned_pums[,1:9]

#Print PUMS samples to file
write.csv(cleaned_pums, 'C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/cleaned_pums.csv', row.names = F)
write.csv(pums_data_selected, 'C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/pums_data_selected.csv', row.names = F)


################################################################################################
#Make sure that the ACS scripts have been run before executing this code onwards

#Variables for the bootstrapping
#number_of_samples = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)
number_of_samples = c(250)
number_of_pumas_to_check = 100
bootstrap_results = data.frame(c('age','income','mode_choice','commute_time','vehicles','household_income'))

#Combine the ACS dataframes
puma_list = data.frame(s0101_selected[,2:13], s0601_selected[,5:8], s0801_selected[,4:23], s1901_selected[,4:13])
puma_list = puma_list[-1,]
names(puma_list) = c("id", "name", "Total Population", names(s0101_selected)[5:13], names(s0601_selected)[5:8], names(s0801_selected)[4:23], names(s1901_selected)[4:13])

#Run analysis for each number of samples
for (n in number_of_samples) {
  
  #Select n random pumas
  pumas_used_in_bootstrap = puma_list[sample(nrow(puma_list), number_of_pumas_to_check, replace = F),]
  percent_differences = data.frame(pumas_used_in_bootstrap$id)
  pumas_to_discard = pumas_used_in_bootstrap
  
  #Run the bootstrapping procedure
  i = 1
  while(nrow(pumas_to_discard) != 0) {
    
    #Draw n random samples from the current puma
    current_puma = pumas_to_discard[1,]
    current_pums = subset(cleaned_pums, (as.character(cleaned_pums$sample_geo) == as.character(current_puma$id)))
    current_pums = subset(current_pums, current_pums$age != "10_19") #Throw out under 20
    pums_sample = current_pums[sample(nrow(current_pums), n, replace = F),]
    
    #Break up puma into constraint variables
    current_puma_age = current_puma[,4:12]
    current_puma_income = current_puma[,14:16]
    current_puma_mode = current_puma[,18:23]
    current_puma_tt = current_puma[,24:32]
    current_puma_vehicles = current_puma[,33:36]
    current_puma_hh_income = current_puma[,38:46]
    
    #Compare percentage of each category of each variable with the ACS marginal percentage for a given PUMA
    difference_age = as.numeric(abs((table(pums_sample$age) / n)*100 - current_puma_age))
    difference_income = as.numeric(abs((table(pums_sample$income) / n)*100 - current_puma_income))
    difference_mode = as.numeric(abs((table(pums_sample$mode) / n)*100 - current_puma_mode))
    difference_tt = as.numeric(abs((table(pums_sample$commute_time) / n)*100 - current_puma_tt))
    difference_vehicles = as.numeric(abs((table(pums_sample$vehicles) / n)*100 - current_puma_vehicles))
    difference_hh_income = as.numeric(abs((table(pums_sample$hh_income) / n)*100 - current_puma_hh_income))
    
    #Record the average difference among all categories for each variable
    percent_differences$age[i] = round(mean(difference_age), 2)
    percent_differences$income[i] = round(mean(difference_income), 2)
    percent_differences$mode[i] = round(mean(difference_mode), 2)
    percent_differences$commute_time[i] = round(mean(difference_tt), 2)
    percent_differences$vehicles[i] = round(mean(difference_vehicles), 2)
    percent_differences$hh_income[i] = round(mean(difference_hh_income), 2)

    #Set up next iteration
    pumas_to_discard = pumas_to_discard[-1,]
    i = i+1
  }
  
  #Put each result into the bootstrap results dataframe
  bootstrap_results = cbind.data.frame(bootstrap_results, colMeans(percent_differences[2:7])) #To add modes change from 2:6 to 2:7
}

#Clean and summarize
#rm(current_puma_mode)
rm(pums_sample,current_puma,current_pums,current_puma_age,current_puma_income,current_puma_mode,current_puma_tt,current_puma_vehicles,current_puma_hh_income)
names(bootstrap_results) = c('variable', as.character(number_of_samples))
View(bootstrap_results)
summary(bootstrap_results)

#Plot bootstrap results
bootstrap_results_t = transpose(bootstrap_results)
plot(bootstrap_results_t[2:16,1], type = "l", xlab = 'number of samples', ylab = 'difference in percent', main = 'bootstrap_results', ylim = c(0, 15), xlim = c(0,16))
lines(bootstrap_results_t[2:16,2])
lines(bootstrap_results_t[2:16,3])
lines(bootstrap_results_t[2:16,4])
lines(bootstrap_results_t[2:16,5])
lines(bootstrap_results_t[2:16,6])
lines(bootstrap_results_t[2:16,7])
lines(bootstrap_results_t[2:16,8])
lines(bootstrap_results_t[2:16,9])
lines(bootstrap_results_t[2:16,10])
lines(bootstrap_results_t[2:16,11])
lines(bootstrap_results_t[2:16,12])
lines(bootstrap_results_t[2:16,13])
lines(bootstrap_results_t[2:16,14])
lines(bootstrap_results_t[2:16,15])



######################################################################
#Output the cleaned PUMS to be used with QGIS
write.csv(cleaned_pums, "C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/input_data_US_pums_only/cleaned_pums.csv")


