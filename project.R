#ANALYSIS OF USA INSURANCE SECTOR USING REGRESSION ANALYSIS
setwd("C:/Users/VEDIT/Desktop/data")
insurance<-read.csv("insurance.csv")
insurance
View(insurance)
head(insurance)
summary(insurance)
#Diagnostics test for the insurance data
# 1. Residuals vs Fitted
plot(model, which = 1, main = "Residuals vs Fitted")
# 2. Normal Q-Q Plot
plot(model, which = 2, main = "Normal Q-Q")
# 3. Scale-Location Plot (Homoscedasticity)
plot(model, which = 3, main = "Scale-Location")
# 4. Cook's Distance
plot(model, which = 4, main = "Cook's Distance")
#Check for multicollinearity
vif(model)
#multiple linear regression to model the charges (insurance cost) as a function of multiple predictors.
#STEP 1
# Convert categorical variables to factors
insurance$sex    <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
#Fit the multiple linear regression model
model <- lm(charges ~ age + bmi + children + sex + smoker + region, data = insurance)
summary(model)
# Transform the response variable
insurance$log_charges <- log(insurance$charges)

# Fit the model with the transformed response
model_log <- lm(log_charges ~ age + bmi + children + sex + smoker + region, data = insurance)

# View summary
summary(model_log)

# Diagnostic plots
par(mfrow = c(2,2))
plot(model_log)
insurance$log_bmi <- log(insurance$bmi)
model_bmi_log <- lm(charges ~ age + log_bmi + children + sex + smoker + region, data = insurance)
summary(model_bmi_log)
insurance$sqrt_age <- sqrt(insurance$age)
model_sqrt_age <- lm(charges ~ sqrt_age + bmi + children + sex + smoker + region, data = insurance)
summary(model_sqrt_age)
