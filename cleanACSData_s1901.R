#This script reads data from any level ACS S1901 Table, and writes specific variables to a .csv to be copied to Popgen.
#Change the input and output csv paths when switching areas, everything else can stay

#Read in the ACS .csv file that was downloaded from AFF; this script is specific to table s1901
s1901 = read.csv("C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US_pums_only/ACS_17_5YR_S1901_with_ann_puma.csv", stringsAsFactors = FALSE)

#Listing variable names of interest then filtering other variables out of ACS data
names_id = c("GEO.id","GEO.id2","GEO.display.label","HC01_EST_VC01")
names_income = c("HC01_EST_VC02","HC01_EST_VC03","HC01_EST_VC04","HC01_EST_VC05","HC01_EST_VC06","HC01_EST_VC07","HC01_EST_VC08","HC01_EST_VC09","HC01_EST_VC10","HC01_EST_VC11")
cols_to_keep = c(names_id, names_income)
s1901 = s1901[cols_to_keep]

#Naming the columns that will be used to store the final dataset
var_id_categories = c("GEO.id","GEO.id2","GEO.display.label","Total Households")
var_income_categories = c("less_10000","10000_14999","15000_24999","25000_34999","35000_49999","50000_74999","75000_149999","150000_199999","200000_over")

#Separating the variables by category, summing to form bins that match final dataset
id_data = s1901[names_id]
income_data = s1901[names_income]
income_data[] = lapply(income_data, as.numeric)
i0 = income_data[names_income[1:6]]
i1 = rowSums(income_data[names_income[7:8]])
i2 = income_data[names_income[9:10]]

#Creating final data frame with appropriate bins and variable names
s1901_selected = data.frame(id_data, i0, i1, i2)
names(s1901_selected) = c(var_id_categories, var_income_categories)

#Clean environment
rm(id_data, income_data, i0, i1, i2)







################################################################################################
#Multiply percentages to get total counts. For educational attainment the s1901 table categories mean 25years and over population must be used
s1901_selected$less_10000 = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$less_10000) / 100), 0)
s1901_selected$`10000_14999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`10000_14999`) / 100), 0)
s1901_selected$`15000_24999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`15000_24999`) / 100), 0)
s1901_selected$`25000_34999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`25000_34999`) / 100), 0)
s1901_selected$`35000_49999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`35000_49999`) / 100), 0)
s1901_selected$`50000_74999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`50000_74999`) / 100), 0)
s1901_selected$`75000_149999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`75000_149999`) / 100), 0)
s1901_selected$`150000_199999` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`150000_199999`) / 100), 0)
s1901_selected$`200000_over` = round(as.numeric(s1901_selected$`Total Households` ) * (as.numeric(s1901_selected$`200000_over`) / 100), 0)

s1901_selected$`Total Households` = as.numeric(s1901_selected$`Total Households`)

#Creating a region dataframe from sums of the individuals
s1901_region = data.frame(colSums(s1901_selected[2:172,5:13]))
sum(s1901_region)
sum(s1901_selected[2:172,4])

#Write to a csv; to be copied into Popgen Individual Marginals
write.csv(s1901_selected, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s1901_area.csv")
write.csv(s1901_region, "C:/Users/zae5o/Desktop/STL/Toyota AV/Data/input_data_US/formatted_acs_s1901_region.csv")

