library(MASS)
library(stringr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(wesanderson)

matched = readRDS("C:/users/zae5o/desktop/stl/toyota av/data/matched_all_premodel.rds")
transit_rides1 = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/transit_rides_d1.csv", stringsAsFactors = F)
transit_rides2 = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/transit_rides_d2.csv", stringsAsFactors = F)
transit_rides3 = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/transit_rides_d3.csv", stringsAsFactors = F)
transit_rides = rbind(transit_rides1,transit_rides2,transit_rides3)
rm(transit_rides1,transit_rides2,transit_rides3)

blocktransitfare = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/blocktransitfare.csv", stringsAsFactors = F)
blockzippopdensity = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/blockzippopdensity.csv", stringsAsFactors = F)
matched = matched[matched$hh_income > 0, ]

#### Uber Stuff ####
uber = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/uber_api.csv", stringsAsFactors = F)
full_uber = read.csv("C:/users/zae5o/Desktop/STL/Toyota AV/Data/Autonomous_final_4.csv", stringsAsFactors = F)
x = data.frame(full_uber$w_den, full_uber$id.1)
names(x) = c("w_den","id")
x = x[unique(x$id),]
uber = merge(uber, x, by="id")

#Throw out rides over 100 miles, get unique values
uber = uber[uber$car_d <= 100,]
uber = unique(uber[,-1])
uber = uber[uber$w_den < 10000,]

#Analysis for Uber waiting time regression
model = lm(srs_wt ~ uber_inv, uber)
ggplot(uber, aes(uber$w_den, uber$srs_wt)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  labs(x="Population Density (people/sqmi)", y="Uber Wait Time (minutes)", title="Service Waiting Time and Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
par(mfrow=c(2,2))
plot(uber_inv, uber$srs_wt)
abline(model)
summary(model)
plot(uber$w_den, uber$prs_wt)
hist(uber$srs_wt)
hist(uber$prs_wt)

#Linear models for solo and pooled ride sourcing
rlm_dist_srsPrice = lm(srs_tc ~ car_d, uber)
rlm_dist_prsPrice = lm(prs_tc ~ car_d, uber)
rlm_dist_srsTime = lm(srs_tt ~ car_d, uber)
rlm_dist_prsTime = lm(prs_tt ~ car_d, uber)

#Cost and distance
plot(uber$car_d, uber$srs_tc, col="red", main="Uber Travel Cost", xlab = "Distance (miles)", ylab = "USD")
points(uber$car_d, uber$prs_tc, col="black")
abline(rlm_dist_prsPrice, col="black")
abline(rlm_dist_srsPrice, col="red")
legend("bottomright", c("Solo","Pooled"), fill = c("red","black"))

#Time and distance
uber = uber[uber$car_d != 0,]
rlm_log_dist_srsTime = lm(srs_tt ~ log(car_d), uber)
rlm_log_dist_prsTime = lm(prs_tt ~ log(car_d), uber)
uber_log_tt_srs = predict(rlm_log_dist_srsTime, data.frame(car_d = uber$car_d))
uber_log_tt_prs = predict(rlm_log_dist_prsTime, data.frame(car_d = uber$car_d))
uber$log_srs_tt = uber_log_tt_srs
uber$log_prs_tt = uber_log_tt_prs
toPlotP = uber[order(uber$log_prs_tt),]
toPlotS = uber[order(uber$log_srs_tt),]
plot(uber$car_d, uber$srs_tt, col="red", main="Uber Travel Time", xlab = "Distance (miles)", ylab = "Minutes")
points(uber$car_d, uber$prs_tt, col="black")
points(toPlotS$car_d, toPlotS$log_srs_tt, col="red", type = "l")
points(toPlotP$car_d, toPlotP$log_prs_tt, col="black", type = "l")
abline(rlm_dist_prsTime, col="black")
abline(rlm_dist_srsTime, col="red")
legend("bottomright", c("Solo","Pooled"), fill = c("red","black"))

#Ridesourcing data for predictions to speed up the mapply process. Cost only used to determine what base case should be
#matched$uber_cost_trs = predict(rlm_dist_srsPrice, data.frame(car_d = matched$ridesDist))
#matched$uber_cost_srs = predict(rlm_dist_srsPrice, data.frame(car_d = matched$drvDist))
#matched$uber_cost_prs = predict(rlm_dist_prsPrice, data.frame(car_d = matched$drvDist))
matched$uber_tt_trs = predict(rlm_dist_srsTime, data.frame(car_d = matched$ridesDist))
matched$uber_tt_srs = predict(rlm_dist_srsTime, data.frame(car_d = matched$drvDist))
matched$uber_tt_prs = predict(rlm_dist_prsTime, data.frame(car_d = matched$drvDist))
rm(uber,full_uber,model,rlm_dist_prsPrice,rlm_dist_prsTime,rlm_dist_srsPrice,rlm_dist_srsTime,rlm_log_dist_prsTime,rlm_log_dist_srsTime)
rm(toPlotP,toPlotS,x,uber_cost_prs,uber_cost_srs,uber_cost_trs,uber_inv,uber_log,uber_log_tt_prs,uber_log_tt_srs,uber_tt_prs,uber_tt_srs,uber_tt_trs)
saveRDS(matched,"C:/users/zae5o/desktop/stl/toyota av/data/matched_all_premodel.rds")

#### Density, transit fare, trs+rs ####
#Assumptions in the model
mpg = 25
cost_gallon = 2.79
uber_wait = 5
transit_wait = 5
srs_base_cost = 6.80
prs_base_cost = 4.80
srs_cost_per_mile = 1.20
prs_cost_per_mile = 0.85

#Getting population density and transit fare
blocktransitfare = blocktransitfare[,c(2,8)]
names(blocktransitfare) = c("h_geocode", "trsfare")
blockzippopdensity = blockzippopdensity[,c(2,3)]
names(blockzippopdensity) = c("h_geocode", "density")
transit_rides = transit_rides[,c(2,3,4,5)]
names(transit_rides) = c("h_geocode", "w_geocode", "ridestrstime", "ridesDist")

matched$h_geocode = as.character(matched$h_geocode)
blocktransitfare$h_geocode = as.character(blocktransitfare$h_geocode)
matched = merge(matched, blocktransitfare, by = "h_geocode")
matched = merge(matched, blockzippopdensity, by = "h_geocode")
matched = merge(matched, transit_rides, by = c("h_geocode", "w_geocode"))

#Deal with new 404s
matched[matched$trstime == Inf,]$ridestrstime = Inf
matched[matched$ridestrstime == 404.000000,]$ridestrstime = Inf
matched[matched$ridestrstime == Inf,]$ridesDist = Inf

#### Parking costs ####
#Parking costs data and models
parking_data = read.csv("C:/users/zae5o/desktop/stl/toyota av/data/parking_costs.csv")
names(parking_data) = c("city","parking_costs","pop_densities")
lm_density_cost = lm(parking_costs ~ pop_densities, parking_data)
lm_log_density_cost = lm(parking_costs ~ log(pop_densities), parking_data)
parking_data$lm_log = predict(lm_log_density_cost, parking_data)
toPlotD = parking_data[order(parking_data$lm_log),]
plot(parking_data$pop_densities, parking_data$parking_costs, col="black", main="Parking Cost", xlab = "Population Density (persons/sqmi)", ylab = "USD (daily)")
abline(lm_density_cost)
points(toPlotD$pop_densities, toPlotD$lm_log, type = "l")

park_cost = predict(lm_density_cost, data.frame(pop_densities = matched$density))
park_cost[park_cost > 80] = 80
matched$parking_cost = park_cost

matched = data.frame(matched, uber_tt_trs, uber_tt_srs, uber_tt_prs)
write.csv(matched, "C:/Users/zae5o/desktop/stl/toyota av/data/matched_all_premodel.csv", row.names = FALSE)
matched = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/matched_all_premodel.csv", stringsAsFactors = FALSE)
saveRDS(matched, "C:/Users/zae5o/desktop/stl/toyota av/data/matched_all_premodel.rds")

#### Accessibility Analysis ####
accessibility = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/acc_ms_all.csv", stringsAsFactors = FALSE)
incomes = accessibility[,c(13,21,29,37,45,53,54,55)]
quantile(incomes$hh_income)
incomes$hh_income = cut(incomes$hh_income, breaks = c(0,45000,77400,122500,3209000), labels = c("Under 45","45-77","77-123","Over 123"))
ig1 = incomes[incomes$hh_income == "Under 45",]
ig1 = ig1[complete.cases(ig1),]
ig2 = incomes[incomes$hh_income == "45-77",]
ig2 = ig2[complete.cases(ig2),]
ig3 = incomes[incomes$hh_income == "77-123",]
ig3 = ig3[complete.cases(ig3),]
ig4 = incomes[incomes$hh_income == "Over 123",]
ig4 = ig4[complete.cases(ig4),]
s1 = data.frame(mean(ig1$base_all_acc), mean(ig2$base_all_acc), mean(ig3$base_all_acc), mean(ig4$base_all_acc))
s2 = data.frame(mean(ig1$d50_all_acc), mean(ig2$d50_all_acc), mean(ig3$d50_all_acc), mean(ig4$d50_all_acc))
s3 = data.frame(mean(ig1$d75_all_acc), mean(ig2$d75_all_acc), mean(ig3$d75_all_acc), mean(ig4$d75_all_acc))
s4 = data.frame(mean(ig1$td3_all_acc), mean(ig2$td3_all_acc), mean(ig3$td3_all_acc), mean(ig4$td3_all_acc))
s5 = data.frame(mean(ig1$ti3_all_acc), mean(ig2$ti3_all_acc), mean(ig3$ti3_all_acc), mean(ig4$ti3_all_acc))
labels = levels(incomes$hh_income)
names(s1) = labels
names(s2) = labels
names(s3) = labels
names(s4) = labels
names(s5) = labels
data = rbind(s1,s2,s3,s4,s5)
names(data) = labels
data = data.frame(t(data))
data = cbind(labels,data)
data = melt(data, id.vars = "labels")
data$labels = factor(data$labels,levels(data$labels)[c(4,1,2,3)])

ggplot(data, aes(labels, value, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Accessibility", title="Logsum Accessibility by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))


accessibility = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/acc_ms_all.csv", stringsAsFactors = FALSE)
incomes = accessibility[,c(13,21,29,37,45,53,54,55)]
incomes$density = log(incomes$density)
quantile(incomes$density)
incomes$density = cut(incomes$density, breaks = c(-1,4,6,7,11), labels = c("Under 4","4 to 6","6 to 7","Over 7"))
ig1 = incomes[incomes$density == "Under 4",]
ig1 = ig1[complete.cases(ig1),]
ig2 = incomes[incomes$density == "4 to 6",]
ig2 = ig2[complete.cases(ig2),]
ig3 = incomes[incomes$density == "6 to 7",]
ig3 = ig3[complete.cases(ig3),]
ig4 = incomes[incomes$density == "Over 7",]
ig4 = ig4[complete.cases(ig4),]
s1 = data.frame(mean(ig1$base_all_acc), mean(ig2$base_all_acc), mean(ig3$base_all_acc), mean(ig4$base_all_acc))
s2 = data.frame(mean(ig1$d50_all_acc), mean(ig2$d50_all_acc), mean(ig3$d50_all_acc), mean(ig4$d50_all_acc))
s3 = data.frame(mean(ig1$d75_all_acc), mean(ig2$d75_all_acc), mean(ig3$d75_all_acc), mean(ig4$d75_all_acc))
s4 = data.frame(mean(ig1$td3_all_acc), mean(ig2$td3_all_acc), mean(ig3$td3_all_acc), mean(ig4$td3_all_acc))
s5 = data.frame(mean(ig1$ti3_all_acc), mean(ig2$ti3_all_acc), mean(ig3$ti3_all_acc), mean(ig4$ti3_all_acc))
labels = levels(incomes$density)
names(s1) = labels
names(s2) = labels
names(s3) = labels
names(s4) = labels
names(s5) = labels
data = rbind(s1,s2,s3,s4,s5)
names(data) = labels
data = data.frame(t(data))
data = cbind(labels,data)
data = melt(data, id.vars = "labels")
data$labels = factor(data$labels,levels(data$labels)[c(4,1,2,3)])

ggplot(data, aes(labels, value, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x="Pop. Density Level Quartiles Log(people/sqmi)", y="Avg Accessibility", title="Logsum Accessibility by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))





##### MSA Analysis #####
scen1_all = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_base_all.csv", stringsAsFactors = FALSE)
trips = scen1_all[,c(2,22,23,24)]
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number + trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen1_all = trips[1:5,]
scen1_solo_rs = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_base_solo_rs.csv", stringsAsFactors = FALSE)
trips = scen1_solo_rs[,c(2,20,21)]
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen1_solo_rs = trips[1:5,]
scen1_pooled_rs = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_base_pooled_rs.csv", stringsAsFactors = FALSE)
trips = scen1_pooled_rs[,c(2,18)]
trips$all = trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen1_pooled_rs = trips[1:5,]
scen2_all = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_d75_all.csv", stringsAsFactors = FALSE)
trips = scen2_all[,c(2,22,23,24)]
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number + trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen2_all = trips[1:5,]
scen2_solo_rs = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_d75_solo_rs.csv", stringsAsFactors = FALSE)
trips = scen2_solo_rs[,c(2,20,21)]
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen2_solo_rs = trips[1:5,]
scen2_pooled_rs = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_d75_pooled_rs.csv", stringsAsFactors = FALSE)
trips = scen2_pooled_rs[,c(2,18)]
trips$all = trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips = trips[order(trips$all, decreasing = TRUE),]
top5_scen2_pooled_rs = trips[1:5,]
rm(scen1,scen2,scen3,scen4,trips)

#The CBSAs change in Scen 1 pooled but not in scen 2 - Look at same ones across implementations for consistency
top5_scen1_pooled_rs = scen1_pooled_rs[scen1_pooled_rs$CBSA %in% c(top5_scen1_all$CBSA),][,c(2,18)]
top5_scen1_pooled_rs$all = top5_scen1_pooled_rs$pooled_rs_trip_number
top5_scen1_pooled_rs$all = top5_scen1_pooled_rs$all/1000
top5_scen1_pooled_rs = top5_scen1_pooled_rs[order(top5_scen1_pooled_rs$all, decreasing = TRUE),]

data1 = cbind(top5_scen1_all$all, top5_scen1_solo_rs$all, top5_scen1_pooled_rs$all)
data1 = melt(data1)
labels1 = as.factor(c("35620 NYC","31080 LA","16980 Chi","41860 SF","47900 DC"))
labels1 = factor(labels1,levels(labels1)[c(3,2,1,4,5)])
data1$Var1 = labels1
data2 = cbind(top5_scen2_all$all, top5_scen2_solo_rs$all, top5_scen2_pooled_rs$all)
data2 = melt(data2)
labels2 = as.factor(c("35620 NYC","31080 LA","16980 Chi","33100 Miami","37980 Phil"))
labels2 = factor(labels2,levels(labels2)[c(4,2,1,3,5)])
data2$Var1 = labels2

ggplot(data1, aes(as.factor(Var1), value, fill=as.factor(Var2))) +
  geom_bar(position="dodge", stat="identity") +
  labs(x="MSA", y="Total Daily Trips (thousand trips)", title="Top 5 MSAs", subtitle = "Base Scenario") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_discrete(name="Implementation", labels=c("All","Solo/Solo+TRS","Pooled")) +
  geom_text(aes(label=round(value)), vjust=-.5, color="black", size=3.5, position = position_dodge(width=.9)) +
  ylim(0,5000)
ggplot(data2, aes(as.factor(Var1), value, fill=as.factor(Var2))) +
  geom_bar(position="dodge", stat="identity") +
  labs(x="MSA", y="Total Daily Trips (thousand trips)", title="Top 5 MSAs", subtitle = "75% Price Reduction Scenario") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_discrete(name="Implementation", labels=c("All","Solo/Solo+TRS","Pooled")) +
  geom_text(aes(label=round(value)), vjust=-.5, color="black", size=3.5, position = position_dodge(width=.9)) +
  ylim(0,5000)

#Scatters
scen1_all = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_base_all.csv", stringsAsFactors = FALSE)
labels = c("35620\n NYC","31080\n LA","16980\n Chi","41860\n SF","47900\n DC")
trips = scen1_all
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number + trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips$all_density = trips$transit_rs_trip_density + trips$solo_rs_trip_density + trips$pooled_rs_trip_density
trips$all_share = trips$Ptransit_rs + trips$Psolo_rs + trips$Ppooled_rs
trips = trips[,c(2,27:29)]
trips = trips[order(trips$all, decreasing = TRUE),]
plot(trips$all, trips$all_density, xlab="Thousands of Trips", ylab="Trips per Sq Mile", main="Daily MSA Volume and Density - Base Scenario")
points(trips[1:5,]$all, trips[1:5,]$all_density, col="blue")
text(trips$all[1:5], trips$all_density[1:5], label=labels, pos = 1, col="blue")
plot(trips$all, trips$all_share, xlab="Thousands of Trips", ylab="Market Share", main="Daily MSA Volume and Market Share - Base Scenario")
points(trips[1:5,]$all, trips[1:5,]$all_share, col="blue")
text(trips$all[1:5], trips$all_share[1:5], label=labels, pos = 1, col="blue")
plot(trips$all_density, trips$all_share, xlab="Trips per Sq Mile", ylab="Market Share", main="Daily MSA Density and Market Share - Base Scenario")
x = trips[trips$CBSA %in% c(14860,45940,14460,37980),]
labelsx = c("14860 CT", "45940 NJ", "14460 Boston", "37980 Phil")
points(trips[1:5,]$all_density, trips[1:5,]$all_share, col="blue")
points(x$all_density, x$all_share, col="red")
text(trips$all_density[1:5], trips$all_share[1:5], label=labels, pos = 1, col="blue")
text(x$all_density, x$all_share, label=labelsx, pos = 1, col="red")


scen2_all = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/new_scenarios/msa_agg_d75_all.csv", stringsAsFactors = FALSE)
labels = c("35620\n NYC","31080\n LA","16980\n Chi","33100\n Miami","37980\n Phil")
trips = scen2_all
trips$all = trips$transit_rs_trip_number + trips$solo_rs_trip_number + trips$pooled_rs_trip_number
trips$all = trips$all/1000
trips$all_density = trips$transit_rs_trip_density + trips$solo_rs_trip_density + trips$pooled_rs_trip_density
trips$all_share = trips$Ptransit_rs + trips$Psolo_rs + trips$Ppooled_rs
trips = trips[,c(2,27:29)]
trips = trips[order(trips$all, decreasing = TRUE),]
plot(trips$all, trips$all_density, xlab="Thousands of Trips", ylab="Trips per Sq Mile", main="Daily MSA Volume and Density - 75% Price Reduction")
points(trips[1:5,]$all, trips[1:5,]$all_density, col="blue")
text(trips$all[1:5], trips$all_density[1:5], label=labels, pos = 1, col="blue")
plot(trips$all, trips$all_share, xlab="Thousands of Trips", ylab="Market Share", main="Daily MSA Volume and Market Share - 75% Price Reduction")
points(trips[1:5,]$all, trips[1:5,]$all_share, col="blue")
text(trips$all[1:5], trips$all_share[1:5], label=labels, pos = 1, col="blue")
plot(trips$all_density, trips$all_share, xlab="Trips per Sq Mile", ylab="Market Share", main="Daily MSA Density and Market Share - 75% Price Reduction")
points(trips[1:5,]$all_density, trips[1:5,]$all_share, col="blue")
text(trips$all_density[1:5], trips$all_share[1:5], label=labels, pos = 1, col="blue")

#labels1 = c("35620 New York-Neward-Jersey City","31080 Los Angeles-Long Beach-Anaheim","16980 Chicago-Naperville-Elgin","41860 San Francisco-Oakland-Hayward","47900 Washington-Arlington-Alexandria")
#labels2 = c("35620 New York-Neward-Jersey City","31080 Los Angeles-Long Beach-Anaheim","16980 Chicago-Naperville-Elgin","33100 Miami-Fort Lauderdale-West Palm Beach","37980 Philadelphia-Camden-Wilmington")





