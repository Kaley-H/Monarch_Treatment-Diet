#12/8/21
library(readxl)
Exp_2_data_08_07_21 <- read_excel("~/PSU/PSU Research/Exp.2_data_08.07.21.xlsx", 
                                    +     sheet = "Respirometry", col_types = c("date", 
                                                                                +         "text", "text", "date", "text", "text", 
                                                                                +         "numeric", "numeric", "numeric", 
                                                                                +         "numeric", "numeric", "numeric", 
                                                                                +         "numeric", "numeric", "numeric", 
                                                                                +         "numeric"))
library(tidyverse)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLm.R')
library(dplyr)
library(rstatix)
Exp2_data <- Exp_2_data_08_07_21[-c(53:55),]
plot(Exp2_data$norm_CO2_avg)
plot(Exp2_data$norm_CO2_peak)
hist(Exp2_data$norm_CO2_avg)
hist(Exp2_data$norm_CO2_peak)
plot(fitted(lm(norm_CO2_avg~Treatment+Plant, data=Exp2_data)),resid(lm(norm_CO2_avg~Treatment+Plant, data=Exp2_data)))
bartlett.test(norm_CO2_avg~Treatment, data = Exp2_data) #p = 0.4727
bartlett.test(norm_CO2_peak~Treatment, data = Exp2_data) #p = 0.1684
bartlett.test(norm_CO2_avg~Plant, data = Exp2_data) #p = 0.6632
bartlett.test(norm_CO2_peak~Plant, data = Exp2_data) #p = 0.6445
bartlett.test(norm_rmr~Treatment, data = Exp2_data) #p = 0.003678
bartlett.test(norm_rmr~Plant, data = Exp2_data) #p = 1.198e-06
mcSummary(lm(norm_CO2_avg~Treatment+Plant, Exp2data))
library(ggplot2)
library(ggpubr)
ggboxplot(Exp2data, x = "Treatment", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Rearing Treatment", add = "jitter", outlier.shape = NA)
Exp2data <- Exp2_data[-c(32),]
remove(Exp2_data)
mcSummary(lm(norm_CO2_avg~Plant, Exp2data))
ggboxplot(Exp2data, x = "Plant", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Plant", add = "jitter", outlier.shape = NA)
SYR_data <- subset(Exp2data, Plant == "SYR")
CUR_data <- subset(Exp2data, Plant == "CUR")
mcSummary(lm(norm_CO2_avg~Treatment, SYR_data))
mcSummary(lm(norm_CO2_avg~Treatment, CUR_data))
ggboxplot(SYR_data, x = "Treatment", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)
ggboxplot(CUR_data, x = "Treatment", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)
sum_data_exp2 <- subset(Exp2data, Treatment == "Summer")
fall_data_exp2 <- subset(Exp2data, Treatment == "Fall")
mcSummary(lm(norm_CO2_avg~Plant, sum_data_exp2))
mcSummary(lm(norm_CO2_avg~Plant, fall_data_exp2))
ggboxplot(sum_data_exp2, x = "Plant", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)
ggboxplot(fall_data_exp2, x = "Plant", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)

#12/9/21
remove(Exp_2_data_08_07_21)
remove(Exp2data)
library(readxl)
#reimport 'Exp_2_data..' with two added flight traces that I forgot before
library(tidyverse)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLm.R')
library(dplyr)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(cowplot)
bartlett.test(norm_CO2_avg~Treatment, data = Exp_2_data_08_07_21) #p = 0.4042
bartlett.test(norm_CO2_peak~Treatment, data = Exp_2_data_08_07_21) #p = 0.1308
bartlett.test(norm_CO2_avg~Plant, data = Exp_2_data_08_07_21) #p = 0.7778
bartlett.test(norm_CO2_peak~Plant, data = Exp_2_data_08_07_21) #p = 0.7727
bartlett.test(norm_rmr~Treatment, data = Exp_2_data_08_07_21) #p = 0.002656
bartlett.test(norm_rmr~Plant, data = Exp_2_data_08_07_21) #p = 2.472e-05
remove(sum_data_exp2)
remove(fall_data_exp2)
remove(SYR_data)
remove(CUR_data)
SYR_data <- subset(Exp_2_data_08_07_21, Plant == "SYR")
CUR_data <- subset(Exp_2_data_08_07_21, Plant == "CUR")
sum_data_exp2 <- subset(Exp_2_data_08_07_21, Treatment == "Summer")
fall_data_exp2 <- subset(Exp_2_data_08_07_21, Treatment == "Fall")
mcSummary(lm(norm_CO2_avg~Treatment, SYR_data))
mcSummary(lm(norm_CO2_avg~Treatment, CUR_data))
mcSummary(lm(norm_CO2_avg~Plant, sum_data_exp2))
mcSummary(lm(norm_CO2_avg~Plant, fall_data_exp2))
ggboxplot(SYR_data, x = "Treatment", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)
ggboxplot(CUR_data, x = "Treatment", y = "norm_CO2_avg", ylab = "FMR (mL/Hr/g)", xlab = "Treatment", add = "jitter", outlier.shape = NA)
ggboxplot(Exp_2_data_08_07_21, x = "Sex", y = "pre_flight_weight", ylab = "Weight (g)", xlab = "Sex", add = "jitter", outlier.shape = NA)

exp2_treatment_stats <- Exp_2_data_08_07_21 %>% t_test(norm_CO2_avg ~ Treatment) %>% add_xy_position(x = "Treatment") %>% add_significance("p")
ggplot(Exp_2_data_08_07_21, aes(x = Treatment, y = norm_CO2_avg)) + geom_boxplot(width = 0.3) + stat_boxplot(geom = "errorbar", width = 0.15) + labs(y = "Average VCO2 (mL/Hr/g)", x = "Rearing Treatment") + geom_jitter(width = 0) + theme_cowplot(12)
ggplot(Exp_2_data_08_07_21, aes(x = Treatment, y = norm_CO2_avg)) + geom_boxplot(width = 0.3) + stat_boxplot(geom = "errorbar", width = 0.15) + labs(y = "Average VCO2 (mL/Hr/g)", x = "Rearing Treatment", subtitle = get_test_label(exp2_treatment_stats, detailed = TRUE)) + theme_cowplot(12) + geom_jitter(width = 0) + stat_pvalue_manual(exp2_treatment_stats, tip.length = 0.02, label = "p.signif", bracket.nudge.y = 1)
mcSummary(lm(pre_flight_weight~Sex, Exp_2_data_08_07_21))
summary(exp2_treatment_stats)

FMR_plant_treatment_stats <- Exp_2_data_08_07_21 %>% group_by(Plant) %>% t_test(norm_CO2_avg~Treatment) %>% adjust_pvalue() %>% add_significance("p.adj") %>% add_xy_position(x = "Treatment")
CUR_treatment_stats <- CUR_data %>% t_test(norm_CO2_avg ~ Treatment) %>% add_xy_position(x = "Treatment")
ggplot(Exp_2_data_08_07_21, aes(x = Treatment, y = norm_CO2_avg)) + geom_boxplot(width = 0.3) + stat_boxplot(geom = "errorbar", width = 0.15) + labs(y = "Normalized Average VCO2 (mL/Hr/g)", x = "Rearing Treatment") + geom_jitter(width = 0.0) + facet_wrap(~Plant) + theme_cowplot(12) + stat_pvalue_manual(FMR_plant_treatment_stats, tip.length = 0.02, label = "p", bracket.nudge.y = 5) + scale_y_continuous(expand=expansion(mult=c(0.075)))
summary(FMR_plant_treatment_stats)
treatment_plant_stats <- Exp_2_data_08_07_21 %>% group_by(Treatment) %>% t_test(norm_CO2_avg~Plant) %>% adjust_pvalue() %>% add_significance("p.adj") %>% add_xy_position(x = "Plant")
ggplot(Exp_2_data_08_07_21, aes(x = Plant, y = norm_CO2_avg)) + geom_boxplot(width = 0.3) + stat_boxplot(geom = "errorbar", width = 0.15) + labs(y = "Normalized Average VCO2 (mL/Hr/g)", x = "Caterpillar Diet") + geom_jitter(width = 0.0) + facet_wrap(~Treatment) + theme_cowplot(12) + stat_pvalue_manual(treatment_plant_stats, tip.length = 0.02, label = "p", bracket.nudge.y = 5) + scale_y_continuous(expand=expansion(mult=c(0.075)))


#import wing data
Exp2_wings <- read_excel("~/PSU/PSU Research/Exp.2_wing morphology_all.xlsx",)
bartlett.test(FWL~Treatment, data = Exp2_wings) #p = 0.2957
t.test(FWL~Treatment, Exp2_wings, var.equal = TRUE) #p = 0.0003654
bartlett.test(HWL~Treatment, data = Exp2_wings) #p = 0.07599
t.test(HWL~Treatment, Exp2_wings, var.equal = TRUE) #p = 0.00021
bartlett.test(HWL~Sex, data = Exp2_wings) #p = 0.01636
t.test(HWL~Sex, Exp2_wings, var.equal = FALSE) #p = 0.5323
bartlett.test(FWL~Sex, data = Exp2_wings) #p = 0.4661
t.test(FWL~Sex, Exp2_wings, var.equal = TRUE) #p = 0.01416
bartlett.test(FWL~Plant, data = Exp2_wings) #p = 0.7544
t.test(FWL~Plant, Exp2_wings, var.equal = TRUE) #p = 0.03513
bartlett.test(HWL~Plant, data = Exp2_wings) #p = 0.1483
t.test(HWL~Plant, Exp2_wings, var.equal = TRUE) #p = 0.01099


mcSummary(lm(FWL~Sex, Exp2_wings)) # p = 0.014
ggboxplot(Exp2_wings, x = "Sex", y = "FWL", ylab = "FWL", xlab = "Sex", add = "jitter", outlier.shape = NA)
summary(lm(FWL~Treatment+Sex, Exp2_wings))
summary(lm(FWL~Treatment+Sex+Plant, Exp2_wings))
ggboxplot(Exp2_wings, x = "Treatment", y = "FWL", ylab = "FWL", xlab = "Treatment", add = "jitter", outlier.shape = NA)
ggboxplot(Exp2_wings, x = "Treatment", y = "HWL", ylab = "HWL", xlab = "Treatment", add = "jitter", outlier.shape = NA)

ggboxplot(Exp2_wings, x = "Treatment", y = "FWL", ylab = "FWL", xlab = "Treatment", add = "jitter", outlier.shape = NA) + facet_grid(Sex~Plant)
summary(lm(HWL~Treatment, Exp2_wings))
ggboxplot(Exp2_wings, x = "Treatment", y = "HWL", ylab = "HWL", xlab = "Treatment", add = "jitter", outlier.shape = NA) + facet_wrap(~Plant)

#import egg count data
Exp2_egg_counts <- read_excel("~/PSU/PSU Research/Exp.2_wing morphology_all.xlsx")
exp2_eggs <- Exp2_egg_counts[-c(21),]
remove(Exp2_egg_counts)
exp2_eggs2 <- exp2_eggs %>% mutate(Treatment_num = recode(exp2_eggs$Treatment, "Fall" = 1, "Summer" = 2))
mature_eggs <- as.numeric(exp2_eggs2$mature_oocyte)
exp2_eggs2 <- cbind(exp2_eggs2, mature_eggs)

bartlett.test(mature_eggs~Treatment_num, data = exp2_eggs2) #p = 3.67e-14
t.test(mature_eggs~Treatment_num, data = exp2_eggs2, var.equal = FALSE)
exp2_eggstats <- exp2_eggs2 %>% t_test(mature_eggs ~ Treatment_num) %>% add_xy_position(x = "Treatment_num") %>% add_significance()
ggplot(data = exp2_eggs2, aes(x = Treatment, y = mature_eggs)) + labs(x = "Rearing Treatment", y = "Number of Mature Oocytes", subtitle = get_test_label(exp2_eggstats, detailed = TRUE)) + stat_summary(fun = mean, geom = "bar", width = 0.5) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) + theme_cowplot(12) + scale_y_continuous(n.breaks=(10)) + stat_pvalue_manual(exp2_eggstats, tip.length = 0) + theme(aspect.ratio = 3/4)

exp2_eggs3 <- exp2_eggs2 %>% filter(Age > 10)
t.test(mature_eggs~Treatment_num, data = exp2_eggs3, var.equal = FALSE)
ggplot(data = exp2_eggs3, aes(x = Treatment, y = mature_eggs)) + labs(x = "Rearing Treatment", y = "Number of Mature Oocytes", subtitle = get_test_label(exp2_eggstats, detailed = TRUE)) + stat_summary(fun = max, geom = "bar", width = 0.5) + theme_cowplot(12) + stat_pvalue_manual(exp2_eggstats, tip.length = 0.02) + theme(aspect.ratio = 3/4)+ scale_y_continuous(n.breaks=(10))

#1/31/22
#lm(rmr~treatment*pre-flight-weight*plant)
summary(lm(rmr_mlHrCO2~Treatment*pre_flight_weight*Plant, data = Exp_2_data_08_07_21))
summary(lm(mlHrCO2_avg~Treatment*pre_flight_weight*Plant, data = Exp_2_data_08_07_21))

#double check rmr rates SS26, FS24, FS38 
#3-way ANOVA with package 'car' -- careful with which one you run cause different packages will run different types of anovas
#post-hoc tests? (Tukey?)
#compare experiment 1 and 2 CUR

#2/10/22
#import EAG data from "Andy_data_copy
library(scales)
ggplot(EAG_data, aes(x = ng_nonanal, y = Dose_response, color = Butterfly)) + geom_point() + scale_y_continuous(trans = 'log10', breaks = trans_breaks('log10', function(x) 10^x), labels = trans_format('log10', math_format(10^.x))) + theme_cowplot(12)
ggplot(EAG_data, aes(x = ng_nonanal, y = Dose_response, color = Butterfly, shape = Butterfly)) + labs(x = "Nonanal (ng)", y = "Log[Response Amplitude]", title = "Fall EAG Trial") + geom_point(size=3) + coord_trans(y='log10') + theme_bw(12)
EAG_data$Butterfly <- as.factor(EAG_data$Butterfly)

#run correlations as in exp. 1
hist(Exp_2_data_08_07_21$pre_flight_weight)
ggplot(Exp_2_data_08_07_21, aes(x = Age, y = pre_flight_weight)) + labs(x = "Age (days)", y = "Butterfly mass (g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot() + stat_compare_means(method = "anova", label.y = 0.8, label.x = 1.5)
#graph saved as "AgeMass_anova.png"
Exp_2_data_08_07_21$Age <- as.factor(Exp_2_data_08_07_21$Age)
Anova(lm(pre_flight_weight~Age, data = Exp_2_data_08_07_21))
Anova(lm(pre_flight_weight~Age*Treatment*Plant, data = Exp_2_data_08_07_21))
Anova(lm(pre_flight_weight~Age+Sex, data = Exp_2_data_08_07_21))
Anova(lm(mlHrCO2_avg~Age, data = Exp_2_data_08_07_21))
Anova(lm(norm_CO2_avg~Age, data = Exp_2_data_08_07_21))
ggplot(Exp_2_data_08_07_21, aes(x = Age, y = norm_CO2_avg)) + labs(x = "Age (days)", y = "Normalized Avg FMR (mL/Hr/g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot() + stat_compare_means(method = "anova", label.x = 1.25)
#graph saved as "AgeAvgNormFMR_anova.png"
ggplot(Exp_2_data_08_07_21, aes(x = Age, y = norm_rmr)) + labs(x = "Age (days)", y = "Normalized RMR (mL/Hr/g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot() + stat_compare_means(method = "anova", label.x = 1.25)
#graph saved as "AgeNormRMR_anova.png"
bartlett.test(pre_flight_weight~Treatment, data = Exp_2_data_08_07_21)
t.test(pre_flight_weight~Treatment, data = Exp_2_data_08_07_21)
ggplot(Exp_2_data_08_07_21, aes(x = Treatment, y = pre_flight_weight)) + labs(x = "Rearing Treatment", y = "Butterfly mass (g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot() + stat_compare_means(method = "t.test", label.x = 0.6, label.y = 0.8)


#FMR correlation with mass - saved graph as "MassFMR_correlation.png"
ggplot(Exp_2_data_08_07_21, aes(x = pre_flight_weight, y = mlHrCO2_avg)) + labs(x = "Butterfly mass (g)", y = "Average flight metabolic rate (mL/Hr)") + geom_point() + geom_smooth(method = lm) + stat_regline_equation() + stat_cor(aes(label = ..rr.label..), label.y = 15) + theme_cowplot(12)
cor.test(~mlHrCO2_avg+pre_flight_weight, data = Exp_2_data_08_07_21, conf.level = 0.95)

#RMR correlation with mass - saved graph as "MassRMR_correlation.png"
ggplot(Exp_2_data_08_07_21, aes(x = pre_flight_weight, y = rmr_mlHrCO2)) + labs(x = "Butterfly mass (g)", y = "Resting metabolic rate (mL/Hr)") + geom_point() + geom_smooth(method = lm) + stat_regline_equation() + stat_cor(aes(label = ..rr.label..), label.y = 1.3) + theme_cowplot(12)
cor.test(~rmr_mlHrCO2+pre_flight_weight, data = Exp_2_data_08_07_21, conf.level = 0.95)


pfw_fs = lm(pre_flight_weight~Treatment, data = Exp_2_data_08_07_21)
res2 = resid(model2)
plot(fitted(model2), res2)
abline(0,0)
qqnorm(res2)
qqline(res2)
plot(density(res2))
remove(model2)
remove(res2)
remove(data4.2)

#Mass versus sex - graph saved as "MassSex_t_test.png"
Exp_2_data_08_07_21$Sex <- as.factor(Exp_2_data_08_07_21$Sex)
ggplot(Exp_2_data_08_07_21, aes(x = Sex, y = pre_flight_weight)) + labs(y = "Butterfly mass (g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot(12) + stat_compare_means(method = "t.test", label.x = 0.55, label.y = 0.9)
Exp_2_data_08_07_21 %>% t_test(pre_flight_weight~Sex)
bartlett.test(pre_flight_weight~Sex, data = Exp_2_data_08_07_21)
t.test(pre_flight_weight~Sex, data = Exp_2_data_08_07_21)
ggplot(Exp_2_data_08_07_21, aes(x = Sex, y = pre_flight_weight)) + labs(y = "Butterfly mass (g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0) + theme_cowplot(12) + stat_compare_means(method = "t.test", label.x = 0.6, label.y = 0.85) + facet_wrap(~Plant)
#graph saved as "MassSex_plant_t_test.png"

#Plant effects
Anova(lm(FWL~Treatment*Plant*Sex, data = Exp2_wings))
Anova(lm(HWL~Treatment*Plant*Sex, data = Exp2_wings))



ggplot(mass_data_female, aes(x = treatment_, y = B_weight, color = Age)) + labs(x = "Rearing Treatment", y = "Butterfly Mass (g)", title = "Flown + Dissected", subtitle = "Females Aged 8-17") + geom_boxplot(width = 0.3) + geom_jitter(width = 0)+ theme_cowplot(12) + stat_pvalue_manual(Fem_stat, tip.length = 0)
Fem_stat <- Exp_2_data_08_07_21 %>% t_test(B_weight~treatment_) %>% adjust_pvalue() %>% add_significance("p.adj") %>% add_xy_position(x = "treatment_")

#2/11/22
#import lipids from exp 2 data - "lipids_exp2"
lipids2_exp2 <- na.omit(lipids_exp2)
remove(lipids2_exp2)
lipids_exp2 <- lipids_exp2 %>% select(-Notes) #remove notes column
#rerun line 215 to select out data points with "N/A"
cor.test(~pre_weight + Age, data = lipids2_exp2, conf.level = 0.95)
ggplot(lipids2_exp2, aes(x = Age, y = pre_weight)) + geom_point() + geom_smooth(method = lm) + theme_cowplot(12)
#data not normally distributed -- don't think this can be analyzed with a linear model

#compare lipids by treatment
ggplot(lipids2_exp2, aes(x = Treatment, y = lipid_content)) + labs(x = "Treatment", y = "Lipid content (g)") + geom_boxplot(width = 0.3) + geom_jitter(width = 0.1) + stat_compare_means(method = "t.test") + theme_cowplot(12)
bartlett.test(lipid_content~Treatment, data = lipids2_exp2)
t.test(lipid_content~Treatment, data = lipids2_exp2)

#2/16/22
#full model effect on lipid content
mcSummary(lm(lipid_content ~ Treatment * pre_weight * Age * Sex * Plant, data = lipids2_exp2))
#error: "aliased coefficients in the model"
class(lipids2_exp2)

mcSummary(lm(lipid_content ~ Treatment * pre_weight, data = lipids2_exp2))
Anova(lm(lipid_content ~ Treatment * pre_weight, data = lipids2_exp2))
