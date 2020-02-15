library(data.table)
library(stringr)

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

#Loading the list of PUMAs used in this analysis
pums_used = fread("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/pums_used.csv", colClasses = c('character'))

#Clean Environment
rm(pums_p_dataa, pums_p_datab, pums_p_datac, pums_p_datad)
rm(pums_hh_dataa, pums_hh_datab, pums_hh_datac, pums_hh_datad)

#Generate unique ids for persons/households, then join hh and person PUMS on Serial Number
pums_p_data_selected$pid = seq.int(nrow(pums_p_data_selected))
pums_hh_data_selected$hid = seq.int(nrow(pums_hh_data_selected))
pums_data_selected = merge(pums_p_data_selected, pums_hh_data_selected, by = "SERIALNO")

#Sort the variables into bins, intervals are inclusive to the left. 0 is a possible value in PUMS age data. Label the resulting bins.
binned_hh_income = cut(pums_hh_data_selected$HINCP, breaks = c(-1,10000,15000,25000,35000,50000,75000,150000,200000,9999999), right = TRUE, labels = c("less_10000","10000_14999","15000_24999","25000_34999","35000_49999","50000_74999","75000_149999","150000_199999","200000_over"))
binned_age = cut(pums_data_selected$AGEP, breaks = c(-1,10,20,30,40,50,60,70,80,999), right = TRUE, labels = c("under_9","10_19","20_29","30_39","40_49","50_59","60_69","70_79","80_over"))
binned_income = cut(pums_data_selected$PINCP, breaks = c(-1,15000,40000,999999), right = TRUE, labels = c("less_15000","15000_39999","40000_more"))
binned_vehicles = cut(pums_data_selected$VEH, breaks = c(-1,1,2,3,99), right = TRUE, labels = c("none","1","2","3_or_more"))
binned_tt = cut(pums_data_selected$JWMNP, breaks = c(-1,10,15,20,25,30,35,45,60,9999), right = TRUE, labels = c("less_10","10_14","15_19","20_24","25_29","30_34","35_44","45_59","60_over"))
binned_carpool = cut(pums_data_selected$JWRIP, breaks = c(0,2,99), right = TRUE, labels = c("not_carpool", "carpool"))
binned_mode = cut(pums_data_selected$JWTR, breaks = c(1,2,7,9,10,11,12,13), right = TRUE, labels = c("car_alone","transit","taxi_motorcycle_other","bike","walk","work_at_home","taxi_motorcycle_other"))

#Assemble the final data frames and clean
cleaned_pums = data.frame(pums_data_selected$SERIALNO, pums_data_selected$ST.x, pums_data_selected$hid, pums_data_selected$pid, pums_data_selected$PUMA.x, binned_age, binned_income, binned_tt, binned_vehicles)
cleaned_hh_pums = data.frame(pums_hh_data_selected$SERIALNO, pums_hh_data_selected$PUMA, pums_hh_data_selected$ST, pums_hh_data_selected$hid, binned_hh_income)
rm(binned_hh_income,binned_age,binned_income,binned_vehicles,binned_tt,binned_carpool,binned_mode,pums_household_names,pums_person_names)

#Restore leading 0's to the state and puma codes
#cleaned_pums$pums_data_selected.ST.x = str_pad(cleaned_pums$pums_data_selected.ST.x, 2, pad = 0)
cleaned_pums$pums_data_selected.PUMA.x = str_pad(cleaned_pums$pums_data_selected.PUMA.x, 5, pad = 0)
#cleaned_hh_pums$pums_hh_data_selected.ST = str_pad(cleaned_hh_pums$pums_hh_data_selected.ST, 2, pad = 0)
cleaned_hh_pums$pums_hh_data_selected.PUMA = str_pad(cleaned_hh_pums$pums_hh_data_selected.PUMA, 5, pad = 0)

#Add the state code digits to the PUMA digits - this forms a unique id and matches the TIGER ids
cleaned_pums = transform(cleaned_pums, sample_geo = paste0(cleaned_pums$pums_data_selected.ST.x,cleaned_pums$pums_data_selected.PUMA.x))
cleaned_hh_pums = transform(cleaned_hh_pums, sample_geo = paste0(cleaned_hh_pums$pums_hh_data_selected.ST,cleaned_hh_pums$pums_hh_data_selected.PUMA))

#Separate Dataframe with removal of all states but WA (unused for US)
cleaned_pums_wa = subset(cleaned_pums, cleaned_pums$pums_data_selected.ST == 53)
cleaned_hh_pums_wa = subset(cleaned_hh_pums, cleaned_hh_pums$pums_hh_data_selected.ST == 53)

#Remove half of the data
toDelete = seq(0, nrow(cleaned_pums), 2)
toDelete_hh = seq(0, nrow(cleaned_hh_pums), 2)
cleaned_pums = cleaned_pums[-toDelete,]
cleaned_hh_pums = cleaned_hh_pums[-toDelete_hh,]

#Change column names and order so that csv can be used directly for Popgen
cleaned_pums = cleaned_pums[,(3:10)]
cleaned_pums = cleaned_pums[,-3]
cleaned_pums = cleaned_pums[,c(1,2,7,3,4,5,6)]
cleaned_pums$rage = cleaned_pums$binned_age
cleaned_pums$rincome = cleaned_pums$binned_income
cleaned_pums$rcommute_time = cleaned_pums$binned_tt
cleaned_pums$rvehicles = cleaned_pums$binned_vehicles
names(cleaned_pums) = c('hid','pid','sample_geo','age','income','commute_time','vehicles','rage','rincome','rcommute_time','rvehicles')

cleaned_hh_pums = cleaned_hh_pums[,4:6]
cleaned_hh_pums = cleaned_hh_pums[,c(1,3,2)]
cleaned_hh_pums$rhh_income = cleaned_hh_pums$binned_hh_income
names(cleaned_hh_pums) = c('hid','sample_geo','hh_income','rhh_income')

#Remove all rows that are not contained in the PUMAs used in the current analysis
cleaned_pums = subset(cleaned_pums, as.character(cleaned_pums$sample_geo) %in% as.character(pums_used$V1))
cleaned_hh_pums = subset(cleaned_hh_pums, as.character(cleaned_hh_pums$sample_geo) %in% as.character(pums_used$V1))

#Remove leading zeros from the sample_geos
cleaned_pums$sample_geo = as.integer(cleaned_pums$sample_geo)
cleaned_hh_pums$sample_geo = as.integer(cleaned_hh_pums$sample_geo)

#Remove all samples that have NA values
cleaned_pums = cleaned_pums[complete.cases(cleaned_pums),]
cleaned_hh_pums = cleaned_hh_pums[complete.cases(cleaned_hh_pums),]

#Write to csv to be copied to Popgen
write.csv(cleaned_pums, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/sample_person.csv", row.names = FALSE)
write.csv(cleaned_hh_pums, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/sample_household.csv", row.names = FALSE)
