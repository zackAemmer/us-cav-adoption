library(stringr)
library(DBI)
library(RODBC)
library(dplyr)

#Connect to the lodes database, make sure it is running, it uses DSN (must set up separately)
lodes_db = odbcConnect('PostgreSQL35W')

#Read in the tract/puma pairings from QGIS output
cleaned_pums = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/cleaned_pums.csv", stringsAsFactors = F)
puma_tract_dict = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/puma_tract_dict.csv", stringsAsFactors = FALSE)


########################################################
#Make sure the bootstrapPUMSData code has run before this to get list of PUMS as cleaned_pums

#Create puma/tract dictionary and a list of all unique pumas appearing in the cleaned_pums data
list_of_pumas = as.character(unique(cleaned_pums$sample_geo))
list_of_pumas = list_of_pumas[order(list_of_pumas)]
puma_tract_dict = data.frame(puma_tract_data$GEOID, puma_tract_data$GEOID10)

names(puma_tract_dict) = c("tract_code","puma_code")
age_group_1 = c('10_19','20_29')
age_group_2 = c('30_39','40_49','50_54')
age_group_3 = c('55_59','60_69','70_79','80_over')
inc_group_1 = c('less_10000','10000_14999')
inc_group_2 = c('15000_24999','25000_34999','35000_39999')
inc_group_3 = c('40000_49999','50000_64999','65000_74999','75000_over')

#Restore leading zeroes in dictionary and list of pumas
puma_tract_dict$tract_code = str_pad(puma_tract_dict$tract_code, 11, pad = 0)
puma_tract_dict$puma_code = str_pad(puma_tract_dict$puma_code, 7, pad = 0)

#Some tracts are across more than one PUMA; here they are assigned to the first corresponding one that appears in the list
puma_tract_dict = puma_tract_dict[!duplicated(puma_tract_dict$tract_code),]

#Write csv to be used by SQL database
write.csv(puma_tract_dict, 'C:/users/zae5o/desktop/stl/toyota av/data/puma_tract_dict.csv', row.names = F)

#clean up cleaned_pums
cleaned_pums$sample_geo = as.character(cleaned_pums$sample_geo)
cleaned_pums$age = as.character(cleaned_pums$age)
cleaned_pums$income = as.character(cleaned_pums$income)
cleaned_pums$mode = as.character(cleaned_pums$mode)
cleaned_pums$commute_time = as.character(cleaned_pums$commute_time)
cleaned_pums$vehicles = as.character(cleaned_pums$vehicles)
cleaned_pums$hh_income = as.character(cleaned_pums$hh_income)




#Load data
list_of_pumas = read.csv("C:/Users/Zae5o/Desktop/STL/Toyota AV/Data/list_of_pumas.csv", stringsAsFactors = F)
list_of_pumas = as.character(list_of_pumas$x)
list_of_pumas = str_pad(list_of_pumas, 7, pad = 0)

#Function to take 80 random OD sample within given puma - takes like 1 hour
selectLODES = function(puma) {
  to_return = c()
  print(paste('Current PUMA is:', puma))
  
  #Select 80 random OD pairs in current puma, from the LODES database
  query = paste(sep ="", "SELECT * FROM non_selected_trips WHERE puma_code = '", puma, "'")
  x = sqlQuery(lodes_db, query)
  random_ods = x[sample(nrow(x), 80, replace = F),]
  #print(paste('The current OD is:', random_od$h_geocode, 'with', random_od$s000, 'people'))
  return(random_ods)
}

#Create data frame for storing the selected ods, then run on all pumas
selected_ods_3 = data.frame(matrix(ncol = 16))
names(selected_ods_3) = c("w_geocode","h_geocode","s000","sa01","sa02","sa03","se01","se02","se03","si01","si02","si03","createdate","tract_code_trips","tract_code","puma_code")

for (puma in list_of_pumas[1:2336]) {
  selected_ods_3 = rbind(selected_ods_3, selectLODES(puma))
}

#Write to csv
selected_ods_3 = selected_ods_3[-1,]
selected_ods_3$puma_code = str_pad(selected_ods_3$puma_code, 7, pad = 0)
selected_ods_3$w_geocode = str_pad(selected_ods_3$w_geocode, 15, pad = 0)
selected_ods_3$h_geocode = str_pad(selected_ods_3$h_geocode, 15, pad = 0)
selected_ods_3 = subset(selected_ods_3, select = -c(tract_code_trips))
write.csv(selected_ods_3, "C:/Users/zae5o/desktop/stl/toyota av/data/selected_ods_3.csv", row.names = F)

#Check if any ods have been repeated
x = rbind(selected_ods, selected_ods_2)
y = duplicated(x)

