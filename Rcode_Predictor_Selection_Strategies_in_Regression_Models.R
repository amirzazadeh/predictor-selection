############################################################
# Selecting predictors in regression models using conceptual/theory-driven 
# and data-driven strategies in Stata
############################################################

# Set working directory (edit path as needed)
setwd("/Users/alimirzazadeh1/Documents/GitHub/predictor-selection")

# Load data
library(readxl)
N <- read_excel("NHANES2015.xls", sheet = "data")

############################################################
# BASIC CHECKS
############################################################
str(N)
summary(N$age)
table(N$education, useNA = "ifany")
table(N$gender, useNA = "ifany")
table(N$race, useNA = "ifany")
table(N$usborn, useNA = "ifany")
table(N$maritalstatus, useNA = "ifany")
table(N$hpv, useNA = "ifany")

############################################################
# RECODE SPECIAL VALUES TO MISSING
############################################################

N$education[N$education == 4] <- NA
N$usborn[N$usborn == 2] <- NA
N$maritalstatus[N$maritalstatus %in% c(6,7)] <- NA
N$hpv[!(N$hpv %in% c(0,1))] <- NA

############################################################
# FACTOR VARIABLES (VALUE LABELS)
############################################################

N$education <- factor(N$education,
                       levels = c(0,1,2,3),
                       labels = c("Less than 9th",
                                  "9-11th grade",
                                  "HS/GED",
                                  "Some college+"))

N$gender <- factor(N$gender,
                    levels = c(0,1),
                    labels = c("Male","Female"))

N$race <- factor(N$race,
                  levels = c(0,1,2,3,4),
                  labels = c("Mexican American",
                             "Other Hispanic",
                             "NH White",
                             "NH Black",
                             "Other"))

N$usborn <- factor(N$usborn,
                    levels = c(0,1),
                    labels = c("Born in US","Other"))

N$maritalstatus <- factor(N$maritalstatus,
                           levels = c(0,1,2,3,4,5),
                           labels = c("Married","Widowed",
                                      "Divorced","Separated",
                                      "Never married",
                                      "Living with partner"))

N$hpvF <- factor(N$hpv,
                 levels = c(0,1),
                 labels = c("Negative","Positive"))

############################################################
# CREATE GROUPED VARIABLES
############################################################

# Age group
N$age_grp <- cut(N$age,
                  breaks = c(-Inf, 29, 49, Inf),
                  labels = c("<30","30-49","50+"))

# Education grouped
N$edu_grp <- ifelse(N$education %in% c("Less than 9th",
                                         "9-11th grade",
                                         "HS/GED"),
                     "HS or less","Some college+")
N$edu_grp <- factor(N$edu_grp)

# Hispanic indicator
N$hispanic <- ifelse(N$race %in% c("Mexican American","Other Hispanic"),
                      "Hispanic","Non-Hispanic")
N$hispanic <- factor(N$hispanic)

# BMI categories
N$bmi_cat <- cut(N$bmi,
                  breaks = c(-Inf,18.5,25,30,Inf),
                  labels = c("Underweight","Normal",
                             "Overweight","Obese"))

# Low HDL
N$hdl_low <- ifelse(N$hdl < 40, 1, 0)
N$hdl_low <- factor(N$hdl_low,
                     levels = c(0,1),
                     labels = c("Normal/High","Low HDL"))

# Married flag
N$married_flag <- ifelse(N$maritalstatus == "Married",1,0)
N$married_flag <- factor(N$married_flag,
                          levels = c(0,1),
                          labels = c("Not married","Married"))

############################################################
# POST-CLEANING CHECKS
############################################################

table(N$age_grp)
table(N$edu_grp)
table(N$hispanic)
table(N$bmi_cat)
table(N$hdl_low)
table(N$married_flag)
table(N$hpvF, useNA="ifany")



############################################################
# SIMPLE LOGISTIC REGRESSION
############################################################

# Total effect
model1 <- glm(hpvF ~ usborn + hispanic,
              data = N,
              family = binomial)

summary(model1)
exp(coef(model1))   # Odds ratios

############################################################
# Total Effect, Direct Effect, and Collinearity Check
############################################################

# install.packages("MASS")
# install.packages("car")
library(MASS)
library(car)

# --- TOTAL EFFECT ---
# use hpv (0/1) outcome here
m_total <- glm(hpv ~ usborn + hispanic, family = binomial, data = N)
summary(m_total)
exp(cbind(OR = coef(m_total), confint(m_total)))

# Predicted risk by usborn
usborn_lvls <- levels(N$usborn)
pred_risk_total <- sapply(usborn_lvls, function(lv) {
  newd <- N; newd$usborn <- factor(lv, levels = usborn_lvls)
  mean(predict(m_total, newdata = newd, type = "response"), na.rm = TRUE)
})
pred_risk_total
if(length(pred_risk_total) >= 2) print(pred_risk_total[2] - pred_risk_total[1])  # RD

# --- DIRECT EFFECT ---
m_direct <- glm(hpv ~ usborn + hispanic + edu_grp + married_flag + age_grp,
                family = binomial, data = N)
summary(m_direct)
exp(cbind(OR = coef(m_direct), confint(m_direct)))

pred_risk_direct <- sapply(usborn_lvls, function(lv) {
  newd <- N; newd$usborn <- factor(lv, levels = usborn_lvls)
  mean(predict(m_direct, newdata = newd, type = "response"), na.rm = TRUE)
})
pred_risk_direct
if(length(pred_risk_direct) >= 2) print(pred_risk_direct[2] - pred_risk_direct[1])  # RD

# --- VIF (collinearity) ---
# Option 1 (recommended / robust): use a linear model with the predictors
lm_for_vif <- lm(hpv ~ age_grp + gender + hispanic + edu_grp + married_flag, data = N)
vif_lm <- vif(lm_for_vif)
cat("VIF from linear model:\n"); print(vif_lm)

# Option 2: try VIF on the logistic model directly (may fail if singular)
vif_glm <- tryCatch(vif(m_direct), error = function(e) e)
cat("\nVIF on logistic model (if works):\n"); print(vif_glm)

# --- Stepwise keeping usborn locked in ---


library(MASS)
# Keep only rows with NO missing values in any model variable
D2 <- na.omit(N[, c("hpv","usborn","hispanic",
                    "edu_grp","married_flag",
                    "age_grp","gender")])
# Drop unused factor levels
D2[] <- lapply(D2, function(x) if(is.factor(x)) droplevels(x) else x)
# Fit models on SAME dataset
full_mod <- glm(hpv ~ usborn + hispanic + edu_grp +
                  married_flag + age_grp + gender,
                family = binomial, data = D2)
null_mod <- glm(hpv ~ usborn,
                family = binomial, data = D2)
# Stepwise (keeps usborn because it's in null model)
step_mod <- stepAIC(null_mod,
                    scope = list(lower = null_mod, upper = full_mod),
                    direction = "both")

summary(step_mod)


############################################################
# Hosmer-Lemeshow test (optional)
############################################################
# install.packages("ResourceSelection")
library(ResourceSelection)
# Ensure hpv is numeric 0/1 in your dataset before modeling:
model2 <- glm(hpv ~ usborn + hispanic + edu_grp + married_flag + age_grp,
              family = binomial, data = N)
hoslem.test(model.frame(model2)[[1]], fitted(model2), g=10)


############################################################
# End
############################################################