library(dplyr)
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

num_straps = 10
accessibility = readRDS('acc_ms_all.rds')


# Market Share Plots
print("1")
incomes = accessibility[,c(16,17,18,24,25,26,32,33,34,40,41,42,48,49,50,54,55)]
incomes$base = incomes$Ptransit_rs_base_all + incomes$Psolo_rs_base_all + incomes$Ppooled_rs_base_all
incomes$d50 = incomes$Ptransit_rs_d50_all + incomes$Psolo_rs_d50_all + incomes$Ppooled_rs_d50_all
incomes$d75 = incomes$Ptransit_rs_d75_all + incomes$Psolo_rs_d75_all + incomes$Ppooled_rs_d75_all
incomes$td3 = incomes$Ptransit_rs_td3_all + incomes$Psolo_rs_td3_all + incomes$Ppooled_rs_td3_all
incomes$ti3 = incomes$Ptransit_rs_ti3_all + incomes$Psolo_rs_ti3_all + incomes$Ppooled_rs_ti3_all
quantile(incomes$hh_income)
incomes$hh_income = cut(incomes$hh_income, breaks = c(0,45000,77400,122500,3209000), labels = c("Under 45","45-77","77-123","Over 123"))
incomes_bootstrapped = incomes %>%
  group_by(hh_income) %>%
  summarise(base_mean=mean(base),
            d50_mean=mean(d50),
            d75_mean=mean(d75),
            td3_mean=mean(td3),
            ti3_mean=mean(ti3)
  )
for (i in 1:num_straps) {
  incomes_new = incomes[sample(nrow(incomes), 622190, replace=TRUE),]
  incomes_new = incomes_new %>%
    group_by(hh_income) %>%
    summarise(base_mean=mean(base),
              d50_mean=mean(d50),
              d75_mean=mean(d75),
              td3_mean=mean(td3),
              ti3_mean=mean(ti3)
    )
  incomes_bootstrapped = rbind(incomes_bootstrapped, incomes_new)
}
incomes_plot_mean = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_mean = mean(base_mean),
            d50_mean = mean(d50_mean),
            d75_mean = mean(d75_mean),
            td3_mean = mean(td3_mean),
            ti3_mean = mean(ti3_mean)
  )
incomes_plot_mean = melt(as.data.frame(incomes_plot_mean), id.vars = "hh_income")
incomes_plot_lower = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_ci_lower = quantile(base_mean, probs=c(.025)),
            d50_ci_lower = quantile(d50_mean, probs=c(.025)),
            d75_ci_lower = quantile(d75_mean, probs=c(.025)),
            td3_ci_lower = quantile(td3_mean, probs=c(.025)),
            ti3_ci_lower = quantile(ti3_mean, probs=c(.025))
  )
incomes_plot_lower = melt(as.data.frame(incomes_plot_lower), id.vars = "hh_income")
incomes_plot_upper = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_ci_upper = quantile(base_mean, probs=c(.975)),
            d50_ci_upper = quantile(d50_mean, probs=c(.975)),
            d75_ci_upper = quantile(d75_mean, probs=c(.975)),
            td3_ci_upper = quantile(td3_mean, probs=c(.975)),
            ti3_ci_upper = quantile(ti3_mean, probs=c(.975))
  )
incomes_plot_upper = melt(as.data.frame(incomes_plot_upper), id.vars = "hh_income")
data = incomes_plot_mean
data$lower = incomes_plot_lower$value
data$upper = incomes_plot_upper$value
data1 = data
incomes1 = incomes

ggplot(data, aes(x=hh_income, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Market Share", title="Market Share by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), text=element_text(size=15)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait")) +
  ylim(0, .25)
ggsave("g1_1.png", device="png")
ggplot(data, aes(x=hh_income, y=value, ymin=lower, ymax=upper, color=variable)) + 
  geom_point(shape=15, size=0.3, position=position_dodge(.2), stat="identity") +
  geom_errorbar(position=position_dodge(.2), width=1.5, size=.3) +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Market Share", title="Market Share by HH Income") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), text=element_text(size=15)) +
  labs(color="Scenario") +
  scale_color_discrete(name="Scenario", labels=c("Base","50% Price Decrease","75% Price Increase","2 Minute Wait","8 Minute Wait"))
ggsave("g1_2.png", device="png")

print("2")
incomes = accessibility[,c(16,17,18,24,25,26,32,33,34,40,41,42,48,49,50,54,55)]
incomes$base = incomes$Ptransit_rs_base_all + incomes$Psolo_rs_base_all + incomes$Ppooled_rs_base_all
incomes$d50 = incomes$Ptransit_rs_d50_all + incomes$Psolo_rs_d50_all + incomes$Ppooled_rs_d50_all
incomes$d75 = incomes$Ptransit_rs_d75_all + incomes$Psolo_rs_d75_all + incomes$Ppooled_rs_d75_all
incomes$td3 = incomes$Ptransit_rs_td3_all + incomes$Psolo_rs_td3_all + incomes$Ppooled_rs_td3_all
incomes$ti3 = incomes$Ptransit_rs_ti3_all + incomes$Psolo_rs_ti3_all + incomes$Ppooled_rs_ti3_all
quantile(incomes$density)
incomes$density = cut(incomes$density, breaks = c(0,59,356,1284,30000), labels = c("Under 59","59-356","356-1284","1284+"))
incomes_bootstrapped = incomes %>%
  group_by(density) %>%
  summarise(base_mean=mean(base),
            d50_mean=mean(d50),
            d75_mean=mean(d75),
            td3_mean=mean(td3),
            ti3_mean=mean(ti3)
  )
for (i in 1:num_straps) {
  incomes_new = incomes[sample(nrow(incomes), 622190, replace=TRUE),]
  incomes_new = incomes_new %>%
    group_by(density) %>%
    summarise(base_mean=mean(base),
              d50_mean=mean(d50),
              d75_mean=mean(d75),
              td3_mean=mean(td3),
              ti3_mean=mean(ti3)
    )
  incomes_bootstrapped = rbind(incomes_bootstrapped, incomes_new)
}
incomes_plot_mean = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_mean = mean(base_mean),
            d50_mean = mean(d50_mean),
            d75_mean = mean(d75_mean),
            td3_mean = mean(td3_mean),
            ti3_mean = mean(ti3_mean)
  )
incomes_plot_mean = melt(as.data.frame(incomes_plot_mean), id.vars = "density")
incomes_plot_lower = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_ci_lower = quantile(base_mean, probs=c(.025)),
            d50_ci_lower = quantile(d50_mean, probs=c(.025)),
            d75_ci_lower = quantile(d75_mean, probs=c(.025)),
            td3_ci_lower = quantile(td3_mean, probs=c(.025)),
            ti3_ci_lower = quantile(ti3_mean, probs=c(.025))
  )
incomes_plot_lower = melt(as.data.frame(incomes_plot_lower), id.vars = "density")
incomes_plot_upper = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_ci_upper = quantile(base_mean, probs=c(.975)),
            d50_ci_upper = quantile(d50_mean, probs=c(.975)),
            d75_ci_upper = quantile(d75_mean, probs=c(.975)),
            td3_ci_upper = quantile(td3_mean, probs=c(.975)),
            ti3_ci_upper = quantile(ti3_mean, probs=c(.975))
  )
incomes_plot_upper = melt(as.data.frame(incomes_plot_upper), id.vars = "density")
data = incomes_plot_mean
data$lower = incomes_plot_lower$value
data$upper = incomes_plot_upper$value
data2 = data
incomes2 = incomes

ggplot(data, aes(x=density, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Market Share", title="Market Share by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), text=element_text(size=15)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait"))
ggsave("g2_1.png", device="png")
ggplot(data, aes(x=density, y=value, ymin=lower, ymax=upper, color=variable)) + 
  geom_point(shape=15, size=0.3, position=position_dodge(.2), stat="identity") +
  geom_errorbar(position=position_dodge(.2), width=1.5, size=.3) +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Market Share", title="Market Share by Density") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), text=element_text(size=15)) +
  labs(color="Scenario") +
  scale_color_discrete(name="Scenario", labels=c("Base","50% Price Decrease","75% Price Increase","2 Minute Wait","8 Minute Wait"))
ggsave("g2_2.png", device="png")

# # igX (ig is grouped by either income or density above, swap out columns to test diff scenarios)
# for (i in 19:22) {
#   data = stack(ig1[,c(18,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(18,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(18,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(18,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
# }


#### Accessibility Analysis ####
print("3")
incomes = accessibility[,c(13,21,29,37,45,53,54,55)]
colnames(incomes) = c("base_no","base","d50","d75","td3","ti3","hh_income","density")
quantile(incomes$hh_income)
incomes$hh_income = cut(incomes$hh_income, breaks = c(0,45000,77400,122500,3209000), labels = c("Under 45","45-77","77-123","Over 123"))
incomes_bootstrapped = incomes %>%
  group_by(hh_income) %>%
  summarise(base_mean=mean(base),
            d50_mean=mean(d50),
            d75_mean=mean(d75),
            td3_mean=mean(td3),
            ti3_mean=mean(ti3)
  )
for (i in 1:num_straps) {
  incomes_new = incomes[sample(nrow(incomes), 622190, replace=TRUE),]
  incomes_new = incomes_new %>%
    group_by(hh_income) %>%
    summarise(base_mean=mean(base),
              d50_mean=mean(d50),
              d75_mean=mean(d75),
              td3_mean=mean(td3),
              ti3_mean=mean(ti3)
    )
  incomes_bootstrapped = rbind(incomes_bootstrapped, incomes_new)
}
incomes_plot_mean = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_mean = mean(base_mean),
            d50_mean = mean(d50_mean),
            d75_mean = mean(d75_mean),
            td3_mean = mean(td3_mean),
            ti3_mean = mean(ti3_mean)
  )
incomes_plot_mean = melt(as.data.frame(incomes_plot_mean), id.vars = "hh_income")
incomes_plot_lower = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_ci_lower = quantile(base_mean, probs=c(.025)),
            d50_ci_lower = quantile(d50_mean, probs=c(.025)),
            d75_ci_lower = quantile(d75_mean, probs=c(.025)),
            td3_ci_lower = quantile(td3_mean, probs=c(.025)),
            ti3_ci_lower = quantile(ti3_mean, probs=c(.025))
  )
incomes_plot_lower = melt(as.data.frame(incomes_plot_lower), id.vars = "hh_income")
incomes_plot_upper = incomes_bootstrapped %>%
  group_by(hh_income) %>%
  summarise(base_ci_upper = quantile(base_mean, probs=c(.975)),
            d50_ci_upper = quantile(d50_mean, probs=c(.975)),
            d75_ci_upper = quantile(d75_mean, probs=c(.975)),
            td3_ci_upper = quantile(td3_mean, probs=c(.975)),
            ti3_ci_upper = quantile(ti3_mean, probs=c(.975))
  )
incomes_plot_upper = melt(as.data.frame(incomes_plot_upper), id.vars = "hh_income")
data = incomes_plot_mean
data$lower = incomes_plot_lower$value
data$upper = incomes_plot_upper$value
data3 = data
incomes3 = incomes

ggplot(data, aes(x=hh_income, y=value, ymin=lower, ymax=upper, fill=variable, position="dodge")) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Accessibility", title="Logsum Accessibility by HH Income") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), text=element_text(size=15)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait")) +
  coord_cartesian(ylim=c(-6,-1))
ggsave("g3_1.png", device="png")
ggplot(data, aes(x=hh_income, y=value, ymin=lower, ymax=upper, color=variable)) + 
  geom_point(shape=15, size=0.3, position=position_dodge(.2), stat="identity") +
  geom_errorbar(position=position_dodge(.2), width=1.5, size=.3) +
  labs(x="Income Level Quartiles (thousands USD)", y="Avg Accessibility", title="Logsum Accessibility by HH Income") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), text=element_text(size=15)) +
  labs(color="Scenario") +
  scale_color_discrete(name="Scenario", labels=c("Base","50% Price Decrease","75% Price Increase","2 Minute Wait","8 Minute Wait"))
ggsave("g3_2.png", device="png")

print("4")
incomes = accessibility[,c(13,21,29,37,45,53,54,55)]
colnames(incomes) = c("base_no","base","d50","d75","td3","ti3","hh_income","density")
quantile(incomes$density)
incomes$density = cut(incomes$density, breaks = c(0,59,356,1284,30000), labels = c("Under 59","59-356","356-1284","1284+"))
incomes_bootstrapped = incomes %>%
  group_by(density) %>%
  summarise(base_mean=mean(base),
            d50_mean=mean(d50),
            d75_mean=mean(d75),
            td3_mean=mean(td3),
            ti3_mean=mean(ti3)
  )
for (i in 1:num_straps) {
  incomes_new = incomes[sample(nrow(incomes), 622190, replace=TRUE),]
  incomes_new = incomes_new %>%
    group_by(density) %>%
    summarise(base_mean=mean(base),
              d50_mean=mean(d50),
              d75_mean=mean(d75),
              td3_mean=mean(td3),
              ti3_mean=mean(ti3)
    )
  incomes_bootstrapped = rbind(incomes_bootstrapped, incomes_new)
}
incomes_plot_mean = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_mean = mean(base_mean),
            d50_mean = mean(d50_mean),
            d75_mean = mean(d75_mean),
            td3_mean = mean(td3_mean),
            ti3_mean = mean(ti3_mean)
  )
incomes_plot_mean = melt(as.data.frame(incomes_plot_mean), id.vars = "density")
incomes_plot_lower = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_ci_lower = quantile(base_mean, probs=c(.025)),
            d50_ci_lower = quantile(d50_mean, probs=c(.025)),
            d75_ci_lower = quantile(d75_mean, probs=c(.025)),
            td3_ci_lower = quantile(td3_mean, probs=c(.025)),
            ti3_ci_lower = quantile(ti3_mean, probs=c(.025))
  )
incomes_plot_lower = melt(as.data.frame(incomes_plot_lower), id.vars = "density")
incomes_plot_upper = incomes_bootstrapped %>%
  group_by(density) %>%
  summarise(base_ci_upper = quantile(base_mean, probs=c(.975)),
            d50_ci_upper = quantile(d50_mean, probs=c(.975)),
            d75_ci_upper = quantile(d75_mean, probs=c(.975)),
            td3_ci_upper = quantile(td3_mean, probs=c(.975)),
            ti3_ci_upper = quantile(ti3_mean, probs=c(.975))
  )
incomes_plot_upper = melt(as.data.frame(incomes_plot_upper), id.vars = "density")
data = incomes_plot_mean
data$lower = incomes_plot_lower$value
data$upper = incomes_plot_upper$value
data4 = data
incomes4 = incomes

ggplot(data, aes(x=density, y=value, ymin=lower, ymax=upper, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position="dodge") +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Accessibility", title="Logsum Accessibility by Density") +
  theme(legend.position="right", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), text=element_text(size=15)) +
  scale_fill_brewer(palette="YlOrRd", name="Scenarios", labels=c("Base","50% Price Decrease","75% Price Decrease","2 Minute Wait","8 Minute Wait")) +
  coord_cartesian(ylim=c(-4,-2))
ggsave("g4_1.png", device="png")
ggplot(data, aes(x=density, y=value, ymin=lower, ymax=upper, color=variable)) + 
  geom_point(shape=15, size=0.3, position=position_dodge(.2), stat="identity") +
  geom_errorbar(position=position_dodge(.2), width=1.5, size=.3) +
  labs(x="Pop. Density Level Quartiles (people/sqmi)", y="Avg Accessibility", title="Logsum Accessibility by Density") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), text=element_text(size=15)) +
  labs(color="Scenario") +
  scale_color_discrete(name="Scenario", labels=c("Base","50% Price Decrease","75% Price Increase","2 Minute Wait","8 Minute Wait"))
ggsave("g4_2.png", device="png")

# #### ANOVA between base case and reduced cost/wait scenarios ####
# incomes$hh_income = as.factor(incomes$hh_income)
# 
# # igX (ig is grouped by either income or density above, swap out columns to test diff scenarios)
# for (i in 2:6) {
#   data = stack(ig1[,c(1,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(1,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(1,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
#   data = stack(ig1[,c(1,i)])
#   fit_ig = aov(data$values ~ data$ind)
#   print(summary(fit_ig))
# }

rm(incomes, incomes_bootstrapped, incomes_new, incomes_plot_mean, incomes_plot_lower, incomes_plot_upper, i, data)

# Check one of the bootstrapped intervals against regular CI to make sure differences are small
comparison1 = incomes1 %>%
  group_by(hh_income) %>%
  summarise(variable="base_mean",
            value=mean(base),
            lower=get_ci(base)[[1]],
            upper=get_ci(base)[[2]])
comparison2 = incomes1 %>%
  group_by(hh_income) %>%
  summarise(variable="d50_mean",
            value=mean(d50),
            lower=get_ci(d50)[[1]],
            upper=get_ci(d50)[[2]])
comparison3 = incomes1 %>%
  group_by(hh_income) %>%
  summarise(variable="d75_mean",
            value=mean(d75),
            lower=get_ci(d75)[[1]],
            upper=get_ci(d75)[[2]])
comparison4 = incomes1 %>%
  group_by(hh_income) %>%
  summarise(variable="td3_mean",
            value=mean(td3),
            lower=get_ci(td3)[[1]],
            upper=get_ci(td3)[[2]])
comparison5 = incomes1 %>%
  group_by(hh_income) %>%
  summarise(variable="ti3_mean",
            value=mean(ti3),
            lower=get_ci(ti3)[[1]],
            upper=get_ci(ti3)[[2]])
comparison = rbind(comparison1,comparison2,comparison3,comparison4,comparison5)
diffs = comparison[,c(4,5)] - data1[,c(4,5)]
mean(abs(colMeans(diffs)))




