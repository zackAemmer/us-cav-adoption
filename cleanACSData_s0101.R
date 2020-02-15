#This script reads data from any level ACS S0101 Table, and writes specific variables to a .csv to be copied to Popgen.
#Change the input and output csv paths when switching areas, everything else can stay

#Read in the ACS .csv file that was downloaded from AFF
s0101 = read.csv("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US_pums_only/ACS_17_5YR_S0101_with_ann_puma.csv", stringsAsFactors = FALSE)

#Listing variable names of interest then filtering other variables out of ACS data
names_id = c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01")
names_age = c("HC01_EST_VC03","HC01_EST_VC04","HC01_EST_VC05","HC01_EST_VC06","HC01_EST_VC07","HC01_EST_VC08","HC01_EST_VC09","HC01_EST_VC10","HC01_EST_VC11","HC01_EST_VC12","HC01_EST_VC13","HC01_EST_VC14","HC01_EST_VC15","HC01_EST_VC16","HC01_EST_VC17","HC01_EST_VC18","HC01_EST_VC19","HC01_EST_VC20")
cols_to_keep = c(names_id, names_age)
s0101 = s0101[cols_to_keep]

#Naming the columns that will be used to store the final dataset
var_id_categories = c("GEO.id","GEO.id2","GEO.display.label","Total Population")
var_age_categories = c("under_9","10_19","20_29","30_39","40_49","50_59","60_69","70_79","80_over")

#Separating the variables by category, summing to form bins that match final dataset
id_data = s0101[names_id]
age_data = s0101[names_age]
age_data[] = lapply(age_data, as.numeric)
i0 = rowSums(age_data[names_age[1:2]])
i1 = rowSums(age_data[names_age[3:4]])
i2 = rowSums(age_data[names_age[5:6]])
i3 = rowSums(age_data[names_age[7:8]])
i4 = rowSums(age_data[names_age[9:10]])
i5 = rowSums(age_data[names_age[11:12]])
i6 = rowSums(age_data[names_age[13:14]])
i7 = rowSums(age_data[names_age[15:16]])
i8 = rowSums(age_data[names_age[17:18]])

#Creating final data frame with appropriate bins and variable names
s0101_selected = data.frame(id_data, i0, i1, i2, i3, i4, i5, i6, i7, i8)
names(s0101_selected) = c(var_id_categories, var_age_categories)
s0101_selected$`Total Population` = as.numeric(s0101_selected$`Total Population`)

#Clean environment
rm(id_data, age_data, i0,i1,i2,i3,i4,i5,i6,i7,i8)

#Put counts in terms of percentages for each puma
s0101_selected$under_9 = round((s0101_selected$under_9 / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`10_19` = round((s0101_selected$`10_19` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`20_29` = round((s0101_selected$`20_29` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`30_39` = round((s0101_selected$`30_39` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`40_49` = round((s0101_selected$`40_49` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`50_59` = round((s0101_selected$`50_59` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`60_69` = round((s0101_selected$`60_69` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`70_79` = round((s0101_selected$`70_79` / s0101_selected$`Total Population`)*100, 1)
s0101_selected$`80_over` = round((s0101_selected$`80_over` / s0101_selected$`Total Population`)*100, 1)

s0101_selected$under_9 = 0


###############################################################################################
#Creating a region dataframe from sums of the individuals
s0101_region = data.frame(colSums(s0101_selected[2:172,5:13]))
sum(s0101_region)
sum(s0101_selected[2:172,4])

#Write to a csv; to be copied into Popgen Individual Marginals
write.csv(s0101_selected, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0101_area.csv")
write.csv(s0101_region, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s0101_region.csv")
