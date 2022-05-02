library(ggplot2)
library(reshape)
library(forcats)


setwd('/Users/zack/Desktop/stl/us-cav-adoption')


#### Market Share Analysis ####
get_ci = function (data) {
  a = mean(data)
  s = sd(data)
  n = length(data)
  error = qt(0.975,df=n-1)*s/sqrt(n)
  left = a-error
  right = a+error
  return(list(left, right))
}
accessibility = readRDS('acc_ms_all.rds')


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
ci1 = data.frame(rbind(c(get_ci(ig1$base)[[1]], get_ci(ig1$base)[[2]]), c(get_ci(ig2$base)[[1]], get_ci(ig2$base)[[2]]), c(get_ci(ig3$base)[[1]], get_ci(ig3$base)[[2]]), c(get_ci(ig4$base)[[1]], get_ci(ig4$base)[[2]])))
ci2 = data.frame(rbind(c(get_ci(ig1$d50)[[1]], get_ci(ig1$d50)[[2]]), c(get_ci(ig2$d50)[[1]], get_ci(ig2$d50)[[2]]), c(get_ci(ig3$d50)[[1]], get_ci(ig3$d50)[[2]]), c(get_ci(ig4$d50)[[1]], get_ci(ig4$d50)[[2]])))
ci3 = data.frame(rbind(c(get_ci(ig1$d75)[[1]], get_ci(ig1$d75)[[2]]), c(get_ci(ig2$d75)[[1]], get_ci(ig2$d75)[[2]]), c(get_ci(ig3$d75)[[1]], get_ci(ig3$d75)[[2]]), c(get_ci(ig4$d75)[[1]], get_ci(ig4$d75)[[2]])))
ci4 = data.frame(rbind(c(get_ci(ig1$td3)[[1]], get_ci(ig1$td3)[[2]]), c(get_ci(ig2$td3)[[1]], get_ci(ig2$td3)[[2]]), c(get_ci(ig3$td3)[[1]], get_ci(ig3$td3)[[2]]), c(get_ci(ig4$td3)[[1]], get_ci(ig4$td3)[[2]])))
ci5 = data.frame(rbind(c(get_ci(ig1$ti3)[[1]], get_ci(ig1$ti3)[[2]]), c(get_ci(ig2$ti3)[[1]], get_ci(ig2$ti3)[[2]]), c(get_ci(ig3$ti3)[[1]], get_ci(ig3$ti3)[[2]]), c(get_ci(ig4$ti3)[[1]], get_ci(ig4$ti3)[[2]])))
names(ci1) = c("lower","upper")
names(ci2) = c("lower","upper")
names(ci3) = c("lower","upper")
names(ci4) = c("lower","upper")
names(ci5) = c("lower","upper")
ci_data = rbind(ci1,ci2,ci3,ci4,ci5)
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
data$labels = fct_relevel(data$labels, "Under 45")
data$lower = ci_data$lower
data$upper = ci_data$upper

ggplot(data, aes(x=labels, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Market Share", title="Market Share by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))



incomes = accessibility[,c(16,17,18,24,25,26,32,33,34,40,41,42,48,49,50,54,55)]
# incomes$density = log(incomes$density)
incomes$base = incomes$Ptransit_rs_base_all + incomes$Psolo_rs_base_all + incomes$Ppooled_rs_base_all
incomes$d50 = incomes$Ptransit_rs_d50_all + incomes$Psolo_rs_d50_all + incomes$Ppooled_rs_d50_all
incomes$d75 = incomes$Ptransit_rs_d75_all + incomes$Psolo_rs_d75_all + incomes$Ppooled_rs_d75_all
incomes$td3 = incomes$Ptransit_rs_td3_all + incomes$Psolo_rs_td3_all + incomes$Ppooled_rs_td3_all
incomes$ti3 = incomes$Ptransit_rs_ti3_all + incomes$Psolo_rs_ti3_all + incomes$Ppooled_rs_ti3_all
quantile(incomes$density)
incomes$density = cut(incomes$density, breaks = c(0,59,356,1284,30000), labels = c("Under 59","59-356","356-1284","1284+"))
ig1 = incomes[incomes$density == "Under 59",]
ig1 = ig1[complete.cases(ig1),]
ig2 = incomes[incomes$density == "59-356",]
ig2 = ig2[complete.cases(ig2),]
ig3 = incomes[incomes$density == "356-1284",]
ig3 = ig3[complete.cases(ig3),]
ig4 = incomes[incomes$density == "1284+",]
ig4 = ig4[complete.cases(ig4),]
s1 = data.frame(mean(ig1$base), mean(ig2$base), mean(ig3$base), mean(ig4$base))
s2 = data.frame(mean(ig1$d50), mean(ig2$d50), mean(ig3$d50), mean(ig4$d50))
s3 = data.frame(mean(ig1$d75), mean(ig2$d75), mean(ig3$d75), mean(ig4$d75))
s4 = data.frame(mean(ig1$td3), mean(ig2$td3), mean(ig3$td3), mean(ig4$td3))
s5 = data.frame(mean(ig1$ti3), mean(ig2$ti3), mean(ig3$ti3), mean(ig4$ti3))
ci1 = data.frame(rbind(c(get_ci(ig1$base)[[1]], get_ci(ig1$base)[[2]]), c(get_ci(ig2$base)[[1]], get_ci(ig2$base)[[2]]), c(get_ci(ig3$base)[[1]], get_ci(ig3$base)[[2]]), c(get_ci(ig4$base)[[1]], get_ci(ig4$base)[[2]])))
ci2 = data.frame(rbind(c(get_ci(ig1$d50)[[1]], get_ci(ig1$d50)[[2]]), c(get_ci(ig2$d50)[[1]], get_ci(ig2$d50)[[2]]), c(get_ci(ig3$d50)[[1]], get_ci(ig3$d50)[[2]]), c(get_ci(ig4$d50)[[1]], get_ci(ig4$d50)[[2]])))
ci3 = data.frame(rbind(c(get_ci(ig1$d75)[[1]], get_ci(ig1$d75)[[2]]), c(get_ci(ig2$d75)[[1]], get_ci(ig2$d75)[[2]]), c(get_ci(ig3$d75)[[1]], get_ci(ig3$d75)[[2]]), c(get_ci(ig4$d75)[[1]], get_ci(ig4$d75)[[2]])))
ci4 = data.frame(rbind(c(get_ci(ig1$td3)[[1]], get_ci(ig1$td3)[[2]]), c(get_ci(ig2$td3)[[1]], get_ci(ig2$td3)[[2]]), c(get_ci(ig3$td3)[[1]], get_ci(ig3$td3)[[2]]), c(get_ci(ig4$td3)[[1]], get_ci(ig4$td3)[[2]])))
ci5 = data.frame(rbind(c(get_ci(ig1$ti3)[[1]], get_ci(ig1$ti3)[[2]]), c(get_ci(ig2$ti3)[[1]], get_ci(ig2$ti3)[[2]]), c(get_ci(ig3$ti3)[[1]], get_ci(ig3$ti3)[[2]]), c(get_ci(ig4$ti3)[[1]], get_ci(ig4$ti3)[[2]])))
names(ci1) = c("lower","upper")
names(ci2) = c("lower","upper")
names(ci3) = c("lower","upper")
names(ci4) = c("lower","upper")
names(ci5) = c("lower","upper")
ci_data = rbind(ci1,ci2,ci3,ci4,ci5)
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
data$labels = fct_rev(data$labels)
data$lower = ci_data$lower
data$upper = ci_data$upper

ggplot(data, aes(x=labels, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Market Share", title="Market Share by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))


# igX (ig is grouped by either income or density above, swap out columns to test diff scenarios)
for (i in 19:22) {
  data = stack(ig1[,c(18,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(18,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(18,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(18,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
}



#### Accessibility Analysis ####
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
ci1 = data.frame(rbind(c(get_ci(ig1$base_all_acc)[[1]], get_ci(ig1$base_all_acc)[[2]]), c(get_ci(ig2$base_all_acc)[[1]], get_ci(ig2$base_all_acc)[[2]]), c(get_ci(ig3$base_all_acc)[[1]], get_ci(ig3$base_all_acc)[[2]]), c(get_ci(ig4$base_all_acc)[[1]], get_ci(ig4$base_all_acc)[[2]])))
ci2 = data.frame(rbind(c(get_ci(ig1$d50_all_acc)[[1]], get_ci(ig1$d50_all_acc)[[2]]), c(get_ci(ig2$d50_all_acc)[[1]], get_ci(ig2$d50_all_acc)[[2]]), c(get_ci(ig3$d50_all_acc)[[1]], get_ci(ig3$d50_all_acc)[[2]]), c(get_ci(ig4$d50_all_acc)[[1]], get_ci(ig4$d50_all_acc)[[2]])))
ci3 = data.frame(rbind(c(get_ci(ig1$d75_all_acc)[[1]], get_ci(ig1$d75_all_acc)[[2]]), c(get_ci(ig2$d75_all_acc)[[1]], get_ci(ig2$d75_all_acc)[[2]]), c(get_ci(ig3$d75_all_acc)[[1]], get_ci(ig3$d75_all_acc)[[2]]), c(get_ci(ig4$d75_all_acc)[[1]], get_ci(ig4$d75_all_acc)[[2]])))
ci4 = data.frame(rbind(c(get_ci(ig1$td3_all_acc)[[1]], get_ci(ig1$td3_all_acc)[[2]]), c(get_ci(ig2$td3_all_acc)[[1]], get_ci(ig2$td3_all_acc)[[2]]), c(get_ci(ig3$td3_all_acc)[[1]], get_ci(ig3$td3_all_acc)[[2]]), c(get_ci(ig4$td3_all_acc)[[1]], get_ci(ig4$td3_all_acc)[[2]])))
ci5 = data.frame(rbind(c(get_ci(ig1$ti3_all_acc)[[1]], get_ci(ig1$ti3_all_acc)[[2]]), c(get_ci(ig2$ti3_all_acc)[[1]], get_ci(ig2$ti3_all_acc)[[2]]), c(get_ci(ig3$ti3_all_acc)[[1]], get_ci(ig3$ti3_all_acc)[[2]]), c(get_ci(ig4$ti3_all_acc)[[1]], get_ci(ig4$ti3_all_acc)[[2]])))
names(ci1) = c("lower","upper")
names(ci2) = c("lower","upper")
names(ci3) = c("lower","upper")
names(ci4) = c("lower","upper")
names(ci5) = c("lower","upper")
ci_data = rbind(ci1,ci2,ci3,ci4,ci5)
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
data$labels = fct_relevel(data$labels, "Under 45")
data$lower = ci_data$lower
data$upper = ci_data$upper

ggplot(data, aes(x=labels, y=value, ymin=lower, ymax=upper, fill=variable, position="dodge")) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Accessibility", title="Logsum Accessibility by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))


incomes = accessibility[,c(13,21,29,37,45,53,54,55)]
#incomes$density = log(incomes$density)
quantile(incomes$density)
incomes$density = cut(incomes$density, breaks = c(0,59,356,1284,30000), labels = c("Under 59","59-356","356-1284","1284+"))
ig1 = incomes[incomes$density == "Under 59",]
ig1 = ig1[complete.cases(ig1),]
ig2 = incomes[incomes$density == "59-356",]
ig2 = ig2[complete.cases(ig2),]
ig3 = incomes[incomes$density == "356-1284",]
ig3 = ig3[complete.cases(ig3),]
ig4 = incomes[incomes$density == "1284+",]
ig4 = ig4[complete.cases(ig4),]
s1 = data.frame(mean(ig1$base_all_acc), mean(ig2$base_all_acc), mean(ig3$base_all_acc), mean(ig4$base_all_acc))
s2 = data.frame(mean(ig1$d50_all_acc), mean(ig2$d50_all_acc), mean(ig3$d50_all_acc), mean(ig4$d50_all_acc))
s3 = data.frame(mean(ig1$d75_all_acc), mean(ig2$d75_all_acc), mean(ig3$d75_all_acc), mean(ig4$d75_all_acc))
s4 = data.frame(mean(ig1$td3_all_acc), mean(ig2$td3_all_acc), mean(ig3$td3_all_acc), mean(ig4$td3_all_acc))
s5 = data.frame(mean(ig1$ti3_all_acc), mean(ig2$ti3_all_acc), mean(ig3$ti3_all_acc), mean(ig4$ti3_all_acc))
ci1 = data.frame(rbind(c(get_ci(ig1$base_all_acc)[[1]], get_ci(ig1$base_all_acc)[[2]]), c(get_ci(ig2$base_all_acc)[[1]], get_ci(ig2$base_all_acc)[[2]]), c(get_ci(ig3$base_all_acc)[[1]], get_ci(ig3$base_all_acc)[[2]]), c(get_ci(ig4$base_all_acc)[[1]], get_ci(ig4$base_all_acc)[[2]])))
ci2 = data.frame(rbind(c(get_ci(ig1$d50_all_acc)[[1]], get_ci(ig1$d50_all_acc)[[2]]), c(get_ci(ig2$d50_all_acc)[[1]], get_ci(ig2$d50_all_acc)[[2]]), c(get_ci(ig3$d50_all_acc)[[1]], get_ci(ig3$d50_all_acc)[[2]]), c(get_ci(ig4$d50_all_acc)[[1]], get_ci(ig4$d50_all_acc)[[2]])))
ci3 = data.frame(rbind(c(get_ci(ig1$d75_all_acc)[[1]], get_ci(ig1$d75_all_acc)[[2]]), c(get_ci(ig2$d75_all_acc)[[1]], get_ci(ig2$d75_all_acc)[[2]]), c(get_ci(ig3$d75_all_acc)[[1]], get_ci(ig3$d75_all_acc)[[2]]), c(get_ci(ig4$d75_all_acc)[[1]], get_ci(ig4$d75_all_acc)[[2]])))
ci4 = data.frame(rbind(c(get_ci(ig1$td3_all_acc)[[1]], get_ci(ig1$td3_all_acc)[[2]]), c(get_ci(ig2$td3_all_acc)[[1]], get_ci(ig2$td3_all_acc)[[2]]), c(get_ci(ig3$td3_all_acc)[[1]], get_ci(ig3$td3_all_acc)[[2]]), c(get_ci(ig4$td3_all_acc)[[1]], get_ci(ig4$td3_all_acc)[[2]])))
ci5 = data.frame(rbind(c(get_ci(ig1$ti3_all_acc)[[1]], get_ci(ig1$ti3_all_acc)[[2]]), c(get_ci(ig2$ti3_all_acc)[[1]], get_ci(ig2$ti3_all_acc)[[2]]), c(get_ci(ig3$ti3_all_acc)[[1]], get_ci(ig3$ti3_all_acc)[[2]]), c(get_ci(ig4$ti3_all_acc)[[1]], get_ci(ig4$ti3_all_acc)[[2]])))
names(ci1) = c("lower","upper")
names(ci2) = c("lower","upper")
names(ci3) = c("lower","upper")
names(ci4) = c("lower","upper")
names(ci5) = c("lower","upper")
ci_data = rbind(ci1,ci2,ci3,ci4,ci5)
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
data$labels = fct_rev(data$labels)
data$lower = ci_data$lower
data$upper = ci_data$upper

ggplot(data, aes(x=labels, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Accessibility", title="Logsum Accessibility by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))



#### ANOVA between base case and reduced cost/wait scenarios ####
incomes$hh_income = as.factor(incomes$hh_income)

# igX (ig is grouped by either income or density above, swap out columns to test diff scenarios)
for (i in 2:6) {
  data = stack(ig1[,c(1,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(1,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(1,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
  data = stack(ig1[,c(1,i)])
  fit_ig = aov(data$values ~ data$ind)
  print(summary(fit_ig))
}
