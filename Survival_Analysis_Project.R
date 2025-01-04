# Data
Lungs = read.csv("DCLungStudy_Clinical_Covariates_with_Hgrade.csv")

# Library
library(survival)
library(ggplot2)

# Clean Data (getting rid of missing values)
Lungs2 = Lungs[1:486,]
Lungs3 = Lungs2[c(1:189,197:415,417:486),]

# Bar charts
ggplot(Lungs3, aes(x = factor(VITAL_STATUS))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Vital Status") +
  ggtitle("Barplot of Vital Status")

ggplot(Lungs, aes(x = factor(SMOKING_HISTORY))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Smoking History") +
  ggtitle("Barplot of Smoking History")

ggplot(Lungs3, aes(x = factor(GENDER))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Gender") +
  ggtitle("Barplot of Gender")

ggplot(Lungs, aes(x = factor(SITE))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Hospitals") +
  ggtitle("Barplot of different Hospitals")

ggplot(Lungs3, aes(x = factor(ADJUVANT_CHEMO))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("CHEMO") +
  ggtitle("Barplot of CHEMO")

ggplot(Lungs3, aes(x = factor(ADJUVANT_RT))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("RT") +
  ggtitle("Barplot of RT")

ggplot(Lungs, aes(x = factor(TESTTYPE))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Test Type") +
  ggtitle("Barplot of Test Type")

ggplot(Lungs, aes(x = factor(Histologic.grade))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Histologic.Grade") +
  ggtitle("Barplot of Histologic.Grade")


# Histograms
ggplot(Lungs, aes(x = MONTHS_TO_LAST_CONTACT_OR_DEATH)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  xlab("Last Contact or Death (Months)") +
  ylab("Number of Patients") +
  ggtitle("Histogram of Last Contact or Death")

ggplot(Lungs3, aes(x = AGE_AT_DIAGNOSIS)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  xlab("Age at Diagnosis (Years)") +
  ylab("Number of Patients") +
  ggtitle("Histogram of Age at Diagnosis")

ggplot(Lungs, aes(x = LABORATORY_BATCH)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  xlab("Laboratory Batch") +
  ylab("Number of Patients") +
  ggtitle("Histogram of Laboratory Batch")


#changing vital status into categorical
censor <- as.numeric(Lungs3$VITAL_STATUS == "Dead", "Alive") 

# Survival Object
surv_obj = Surv(Lungs3$MONTHS_TO_LAST_CONTACT_OR_DEATH,censor)

# KM plots
km_fit <- survfit(surv_obj ~ Lungs3$ADJUVANT_CHEMO)
plot(km_fit, xlab="Time", ylab="Survival Probability", main= "KM plots of Adjuvant Chemo", col = c(1,2,3),ylim = c(0,1))
# No is black, Unknown is red, Green is Yes


# Log-Rank
logr_Site <- survdiff(surv_obj ~ ADJUVANT_CHEMO + strata(SITE), data = Lungs3)
logr_Site

# Cox PH
fit = coxph(surv_obj ~ AGE_AT_DIAGNOSIS + ADJUVANT_CHEMO*SITE, data = Lungs3)
summary(fit)

library(survminer)
ggcoxzph(cox.zph(fit))

ggcoxdiagnostics(fit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

cox.zph(fit)

# AFT
aft_exp<-survreg(surv_obj ~ AGE_AT_DIAGNOSIS + ADJUVANT_CHEMO*SITE, data=Lungs3, dist="weibull")
summary(aft_exp)






















