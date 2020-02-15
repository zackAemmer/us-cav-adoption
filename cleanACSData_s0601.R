library(stringr)

#This script reads data from any level ACS S0601 Table, and writes specific variables to a .csv to be copied to Popgen.
#Change the input and output csv paths when switching areas, everything else can stay

#Read in the ACS .csv file that was downloaded from AFF; this script is specific to table S0601
s0601 = read.csv("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/ACS/msa/ACS_17_5YR_S0601.csv", stringsAsFactors = FALSE)

#Listing variable names of interest then filtering other variables out of ACS data
names_id = c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01")
names_income = c("HC01_EST_VC53","HC01_EST_VC54","HC01_EST_VC55","HC01_EST_VC56","HC01_EST_VC57","HC01_EST_VC58","HC01_EST_VC59","HC01_EST_VC60","HC01_EST_VC61")
cols_to_keep = c(names_id, names_income)
s0601 = s0601[cols_to_keep]

#Naming the columns that will be used to store the final dataset
var_id_categories = c("GEO.id","GEO.id2","GEO.display.label","Total Population")
var_income_categories = c("Population 15 and Over","less_10000","10000_14999","15000_24999","25000_34999","35000_49999","50000_64999","65000_74999","75000_over")

#Separating the variables by category, summing to form bins that match final dataset
id_data = s0601[names_id]
income_data = s0601[names_income]
income_data[] = lapply(income_data, as.numeric)
#i0 = income_data[names_income[1]]
#i1 = rowSums(income_data[names_income[2:3]])
#i2 = rowSums(income_data[names_income[4:5]])
#i3 = rowSums(income_data[names_income[6:9]])

#Creating final data frame with appropriate bins and variable names
s0601_selected = data.frame(id_data, income_data)
names(s0601_selected) = c(var_id_categories, var_income_categories)

s0601_selected$state_id = str_pad(s0601_selected$GEO.id2, 2, pad = 0)
s0601_selected = s0601_selected[,c(5,14)]
names(s0601_selected) = c("population","state_id")

s0601_selected = s0601_selected[,c(1:5)]


#Clean environment
rm(id_data, income_data, i0, i1, i2, i3, cols_to_keep, names_id,names_income,var_id_categories,var_income_categories)

saveRDS(s0601_selected, "C:/users/zae5o/desktop/stl/toyota av/data/s0601_msa.rds")



####################################################################################
#Multiply percentages to get total counts
s0601_selected$`Total Population` = as.numeric(s0601_selected$`Total Population`)
s0601_selected$less_15000 = round(as.numeric(s0601_selected$`Population 15 and Over`) * (as.numeric(s0601_selected$less_15000) / 100), 0)
s0601_selected$less_15000 = s0601_selected$less_15000 + (s0601_selected$`Total Population` - s0601_selected$`Population 15 and Over`)
s0601_selected$'15000_39999' = round(as.numeric(s0601_selected$`Population 15 and Over`) * (as.numeric(s0601_selected$'15000_39999') / 100), 0)
s0601_selected$'40000_more' = round(as.numeric(s0601_selected$`Population 15 and Over`) * (as.numeric(s0601_selected$'40000_more') / 100), 0)

#Creating a region dataframe from sums of the individuals
s0601_region = data.frame(colSums(s0601_selected[2:172,6:8]))
sum(s0601_region)
sum(s0601_selected[2:172,4])

#Write to a csv; to be copied into Popgen Individual Marginals
write.csv(s0601_selected, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0601_area.csv")
write.csv(s0601_region, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0601_region.csv")

