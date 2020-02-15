accessibility = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/acc_ms_all.csv", stringsAsFactors = FALSE)
incomes = accessibility[,c(16,17,18,24,25,26,32,33,34,40,41,42,48,49,50,54,55)]
incomes$base = incomes$Ptransit_rs_base_all + incomes$Psolo_rs_base_all + incomes$Ppooled_rs_base_all
incomes$d50 = incomes$Ptransit_rs_d50_all + incomes$Psolo_rs_d50_all + incomes$Ppooled_rs_d50_all
incomes$d75 = incomes$Ptransit_rs_d75_all + incomes$Psolo_rs_d75_all + incomes$Ppooled_rs_d75_all
incomes$td3 = incomes$Ptransit_rs_td3_all + incomes$Psolo_rs_td3_all + incomes$Ppooled_rs_td3_all
incomes$ti3 = incomes$Ptransit_rs_ti3_all + incomes$Psolo_rs_ti3_all + incomes$Ppooled_rs_ti3_all
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
#ig5 = incomes[incomes$hh_income == "160-200",]
#ig5 = ig5[complete.cases(ig5),]
#ig6 = incomes[incomes$hh_income == "Over 200",]
#ig6 = ig6[complete.cases(ig6),]
s1 = data.frame(mean(ig1$base), mean(ig2$base), mean(ig3$base), mean(ig4$base))
s2 = data.frame(mean(ig1$d50), mean(ig2$d50), mean(ig3$d50), mean(ig4$d50))
s3 = data.frame(mean(ig1$d75), mean(ig2$d75), mean(ig3$d75), mean(ig4$d75))
s4 = data.frame(mean(ig1$td3), mean(ig2$td3), mean(ig3$td3), mean(ig4$td3))
s5 = data.frame(mean(ig1$ti3), mean(ig2$ti3), mean(ig3$ti3), mean(ig4$ti3))
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
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Market Share", title="Market Share by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))






accessibility = read.csv("C:/Users/zae5o/desktop/stl/toyota av/data/acc_ms_all.csv", stringsAsFactors = FALSE)
incomes = accessibility[,c(16,17,18,24,25,26,32,33,34,40,41,42,48,49,50,54,55)]
incomes$density = log(incomes$density)
incomes$base = incomes$Ptransit_rs_base_all + incomes$Psolo_rs_base_all + incomes$Ppooled_rs_base_all
incomes$d50 = incomes$Ptransit_rs_d50_all + incomes$Psolo_rs_d50_all + incomes$Ppooled_rs_d50_all
incomes$d75 = incomes$Ptransit_rs_d75_all + incomes$Psolo_rs_d75_all + incomes$Ppooled_rs_d75_all
incomes$td3 = incomes$Ptransit_rs_td3_all + incomes$Psolo_rs_td3_all + incomes$Ppooled_rs_td3_all
incomes$ti3 = incomes$Ptransit_rs_ti3_all + incomes$Psolo_rs_ti3_all + incomes$Ppooled_rs_ti3_all
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
s1 = data.frame(mean(ig1$base), mean(ig2$base), mean(ig3$base), mean(ig4$base))
s2 = data.frame(mean(ig1$d50), mean(ig2$d50), mean(ig3$d50), mean(ig4$d50))
s3 = data.frame(mean(ig1$d75), mean(ig2$d75), mean(ig3$d75), mean(ig4$d75))
s4 = data.frame(mean(ig1$td3), mean(ig2$td3), mean(ig3$td3), mean(ig4$td3))
s5 = data.frame(mean(ig1$ti3), mean(ig2$ti3), mean(ig3$ti3), mean(ig4$ti3))
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
  labs(x="Pop. Density Level Quartiles Log(people/sqmi)", y="Avg Market Share", title="Market Share by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))

