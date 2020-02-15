#This script reads data from any level ACS S0801 Table, and writes specific variables to a .csv to be copied to Popgen.
#Change the input and output csv paths when switching areas, everything else can stay

#Read in the ACS .csv file that was downloaded from AFF; this script is specific to table s0801
s0801 = read.csv("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US_pums_only/ACS_17_5YR_s0801_with_ann_puma.csv", stringsAsFactors = FALSE)

#Listing variable names of interest then filtering other variables out of ACS data
names_id = c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01")
names_mode = c("HC01_EST_VC03","HC01_EST_VC10","HC01_EST_VC11","HC01_EST_VC12","HC01_EST_VC13","HC01_EST_VC14")
names_travel_time = c("HC01_EST_VC46","HC01_EST_VC47","HC01_EST_VC48","HC01_EST_VC49","HC01_EST_VC50","HC01_EST_VC51","HC01_EST_VC52","HC01_EST_VC53","HC01_EST_VC54")
names_cars_available = c("HC01_EST_VC59","HC01_EST_VC60","HC01_EST_VC61","HC01_EST_VC62")
cols_to_keep = c(names_id, names_mode, names_travel_time, names_cars_available)
s0801 = s0801[cols_to_keep]

#Naming the columns that will be used to store the final dataset
var_id_categories = c("GEO.id","GEO.id2","GEO.display.label","Total Population Over 16")
var_mode_categories = c("car_truck_van","transit","walk","bike","taxi_motorcycle_other","work_at_home")
var_tt_categories = c("less_10","10_14","15_19","20_24","25_29","30_34","35_44","45_59","60_over")
var_cars_available = c("none","1","2","3_or_more")

#Separating the variables by category, summing to form bins that match final dataset
id_data = s0801[names_id]
mode_data = s0801[names_mode]
mode_data[] = lapply(mode_data, as.numeric)
travel_time_data = s0801[names_travel_time]
travel_time_data[] = lapply(travel_time_data, as.numeric)
cars_available_data = s0801[names_cars_available]
cars_available_data[] = lapply(cars_available_data, as.numeric)

#Creating final data frame with appropriate bins and variable names
s0801_selected = data.frame(id_data, mode_data, travel_time_data, cars_available_data)
names(s0801_selected) = c(var_id_categories, var_mode_categories, var_tt_categories, var_cars_available)

#Clean environment
rm(id_data, mode_data, travel_time_data, cars_available_data)






##########################################################################################
#Multiply percentages to get total counts. For educational attainment the s0801 table categories mean 25years and over population must be used
#This script must be run after s0101 in order to use total population counts data frame
s0801_selected$Total_Population = as.numeric(s0101_selected$`Total Population`)
s0801_selected$`Total Population Over 16` = as.numeric(s0801_selected$`Total Population Over 16`)

s0801_selected$car_alone = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$car_alone) / 100), 0)
s0801_selected$car_carpool = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$car_carpool) / 100), 0)
s0801_selected$transit = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$transit) / 100), 0)
s0801_selected$transit = s0801_selected$transit + (s0801_selected$Total_Population - s0801_selected$`Total Population Over 16`) #Add under 16yr olds
s0801_selected$walk = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$walk) / 100), 0)
s0801_selected$bike = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$bike) / 100), 0)
s0801_selected$taxi_motorcycle_other = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$taxi_motorcycle_other) / 100), 0)
s0801_selected$work_at_home = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$work_at_home) / 100), 0)

s0801_selected$less_10 = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$less_10) / 100), 0)
s0801_selected$less_10 = s0801_selected$less_10 + (s0801_selected$Total_Population - s0801_selected$`Total Population Over 16`) #Add under 16yr olds
s0801_selected$`10_14` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`10_14`) / 100), 0)
s0801_selected$`15_19` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`15_19`) / 100), 0)
s0801_selected$`20_24` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`20_24`) / 100), 0)
s0801_selected$`25_29` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`25_29`) / 100), 0)
s0801_selected$`30_34` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`30_34`) / 100), 0)
s0801_selected$`35_44` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`35_44`) / 100), 0)
s0801_selected$`45_59` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`45_59`) / 100), 0)
s0801_selected$`60_over` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`60_over`) / 100), 0)

s0801_selected$none = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$none) / 100), 0)
s0801_selected$none = s0801_selected$none + (s0801_selected$Total_Population - s0801_selected$`Total Population Over 16`) #Add under 16yr olds
s0801_selected$`1` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`1`) / 100), 0)
s0801_selected$`2` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`2`) / 100), 0)
s0801_selected$`3_or_more` = round(as.numeric(s0801_selected$`Total Population Over 16` ) * (as.numeric(s0801_selected$`3_or_more`) / 100), 0)

#Creating a region dataframe from sums of the individuals
s0801_region = data.frame(colSums(s0801_selected[2:172,5:24]))
sum(s0801_region[1:7,])
sum(s0801_region[7:16,])
sum(s0801_region[17:20,])
sum(s0801_selected[2:172,25])

#Write to a csv; to be copied into Popgen Individual Marginals
write.csv(s0801_selected, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0801_area.csv")
write.csv(s0801_region, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0801_region.csv")

