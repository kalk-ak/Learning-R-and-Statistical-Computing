wine = read.table("wine.txt", header = TRUE)
head(wine)
plot(wine$consumption, wine$mortality, xlab = "Wine Consumption", ylab = "Mortality Rate")

plot(log(wine$consumption), log(wine$mortality), xlab = "log(Wine Consumption)", ylab = "log(Mortality Rate)")

model <- lm(log(mortality) ~ log(consumption), data = wine)
summary(model)
# Intercept Bo = 2.5555
# Slope B1 = -0.35560

plot(log(wine$consumption), log(wine$mortality), xlab = "log(Wine Consumption)", ylab = "log(Mortality Rate)")
abline(model)

new_data = data.frame(consumption = seq(3, 75, length.out = 100))
new_data$mortality_pred = exp(predict(model, newdata = data.frame(consumption = log(new_data$consumption))))
plot(wine$consumption, wine$mortality, xlab = "Wine Consumption", ylab = "Mortality Rate")
lines(new_data$consumption, new_data$mortality_pred,)

predicted_mortality = exp(predict(model, newdata = data.frame(consumption = log(15.5))))
predicted_mortality


rhrdat <- readRDS("RHR.RDS")
head(rhrdat)

rhrdat <- readRDS("RHR.RDS")
X = cbind(1, rhrdat$exercise, rhrdat$age)
y = rhrdat$RHRdec

beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
names(beta_hat) = c("Intercept", "Exercise", "Age")
beta_hat

# Intercept Bo = -0.20816
# regressor 1(Exercise) = -0.03877
# regressor 2(Age) = 0.00435

model = lm(RHRdec ~ exercise + age, data = rhrdat)
qqnorm(residuals(model))
qqline(residuals(model))

summary(model)

## Estimate for age = 0.00436
## Standard Deviation for age = 0.01503
## P Value = 0.0007306
## T value = 2.901

exercise_estimate = coef(model)["exercise"]
exercise_se = summary(model)$coefficients["exercise", "Std. Error"]
t_stat =(exercise_estimate + 0.01) / exercise_se
df = nrow(rhrdat) - length(coef(model))
p_value = pt(t_stat, df, lower.tail = TRUE)
list(Test_Statistic = t_stat, P_Value = p_value)








































