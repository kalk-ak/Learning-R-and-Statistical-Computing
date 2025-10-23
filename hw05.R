arsenic = readRDS("arsenic.RDS")
head(arsenic)
arsenic$gender_numeric = as.numeric(arsenic$gender == "Male")
arsenic$log_arsenic_toenail = log(arsenic$arsenic.toenail)
arsenic$interaction = arsenic$gender_numeric * arsenic$arsenic.water
model_matrix = model.matrix(~ arsenic.water + age + gender_numeric + interaction, arsenic)

beta = solve(t(model_matrix) %*% model_matrix) %*% t(model_matrix) %*% arsenic$log_arsenic_toenail
residuals = arsenic$log_arsenic_toenail - model_matrix %*% beta
error_std_deviation = sqrt(sum(residuals^2) / (nrow(model_matrix) - ncol(model_matrix)))
cat("Coefficients:\n")
print(beta)
cat("Error Standard Deviation:", error_std_deviation, "\n")




sigma_squared = sum(residuals^2) / (nrow(model_matrix) - ncol(model_matrix))
var_cov_matrix = sigma_squared * solve(t(model_matrix) %*% model_matrix)
std_error_age = sqrt(diag(var_cov_matrix)[3]) 
df = nrow(model_matrix) - ncol(model_matrix)
t_critical = qt(0.995, df) 
beta_age = beta[3]
lower_bound = beta_age - t_critical * std_error_age
upper_bound = beta_age + t_critical * std_error_age
cat("99% Confidence Interval for the 'age' coefficient:", lower_bound, "to", upper_bound, "\n")



# Model matrix for the full model
model_matrix_full = model.matrix(~ arsenic.water + age + gender_numeric + gender_numeric:arsenic.water, arsenic)
# Model matrix for the reduced model
model_matrix_reduced = model.matrix(~ arsenic.water + age + gender_numeric, arsenic)

# Solve for coefficients
beta_full = solve(t(model_matrix_full) %*% model_matrix_full) %*% t(model_matrix_full) %*% arsenic$log_arsenic_toenail
beta_reduced = solve(t(model_matrix_reduced) %*% model_matrix_reduced) %*% t(model_matrix_reduced) %*% arsenic$log_arsenic_toenail

# Calculate residuals and RSS
residuals_full = arsenic$log_arsenic_toenail - model_matrix_full %*% beta_full
residuals_reduced = arsenic$log_arsenic_toenail - model_matrix_reduced %*% beta_reduced
RSS_full = sum(residuals_full^2)
RSS_reduced = sum(residuals_reduced^2)

# Calculate F-statistic
n = nrow(arsenic)
k_full = ncol(model_matrix_full)
p = k_full - ncol(model_matrix_reduced)
F_statistic = ((RSS_reduced - RSS_full) / p) / (RSS_full / (n - k_full))
F_statistic
p_value = pf(F_statistic, p, n - k_full, lower.tail = FALSE)
p_value






age = 45
gender_numeric = 1  # Assuming 'Male' is coded as 1
arsenic_water = 0
predicted_log_arsenic_toenail = beta[1] + beta[2] * arsenic_water + beta[3] * age + beta[4] * gender_numeric + beta[5] * gender_numeric * arsenic_water
X_new = c(1, arsenic_water, age, gender_numeric, gender_numeric * arsenic_water)  
var_prediction = t(X_new) %*% var_cov_matrix %*% X_new + sigma_squared 

#  prediction interval
t_critical = qt(0.995, df=21-5)  
prediction_error_margin = t_critical * sqrt(var_prediction)
lower_bound_log = predicted_log_arsenic_toenail - prediction_error_margin
upper_bound_log = predicted_log_arsenic_toenail + prediction_error_margin
lower_bound = exp(lower_bound_log)
upper_bound = exp(upper_bound_log)
cat("99% Prediction Interval for arsenic.toenail:", lower_bound, "to", upper_bound, "\n")

var_estimate = t(X_new) %*% var_cov_matrix %*% X_new
estimate_error_margin = t_critical * sqrt(var_estimate)
lower_bound_log = predicted_log_arsenic_toenail - estimate_error_margin
upper_bound_log = predicted_log_arsenic_toenail + estimate_error_margin
lower_bound = exp(lower_bound_log)
upper_bound = exp(upper_bound_log)
cat("99% Confidence Interval for the expected value of arsenic.toenail:", lower_bound, "to", upper_bound, "\n")






set.seed(3301)
beta = c(intercept = -0.73439544, b1 = 26.48756195, b2 = -0.02293539, b3 = -0.03483843, b4 = -10.45649872)
sigma = 0.4864371
n_sim = 10000

X_new = c(1, 0, 45, 1, 0)

inside_interval = numeric(n_sim)
for (i in 1:n_sim) {
  error = sigma * sqrt(1/12) * (runif(1) - 0.5)
  y_true_log = sum(X_new * beta) + error
  y_true = exp(y_true_log)
  prediction_error_margin = qt(0.975, df=21-5) * sqrt(t(X_new) %*% var_cov_matrix %*% X_new + sigma^2)
  lower_bound_log = y_true_log - prediction_error_margin
  upper_bound_log = y_true_log + prediction_error_margin
  lower_bound = exp(lower_bound_log)
  upper_bound = exp(upper_bound_log)

  inside_interval[i] = y_true >= lower_bound && y_true <= upper_bound
}


coverage_probability = mean(inside_interval)
coverage_ci = qnorm(c(0.005, 0.995), mean = coverage_probability, sd = sqrt(coverage_probability * (1 - coverage_probability) / n_sim))
cat("Estimated coverage probability:", coverage_probability, "\n")
cat("99% CI for the coverage probability:", coverage_ci, "\n")

################################################################################
# 2


rhrdat <- readRDS("RHR.RDS")
head(rhrdat)
y = rhrdat$RHRdec
X = cbind(1, rhrdat$exercise, rhrdat$age)
beta = solve(t(X) %*% X) %*% t(X) %*% y
y_pred = X %*% beta
residuals = y - y_pred
SST = sum((y - mean(y))^2)
SSE = sum(residuals^2)
SSR = SST - SSE
R_squared = 1 - SSE / SST
R_squared


exer15 <- 1 * (rhrdat$exercise == 1.5)
exer25 <- 1 * (rhrdat$exercise == 2.5)

y = rhrdat$RHRdec
X = cbind(1, exer15, exer25, rhrdat$age)  
beta = solve(t(X) %*% X) %*% t(X) %*% y
cat("Coefficients:", beta, "\n")
cat("Coefficient for exer15:", beta[2], "\n")

y_pred = X %*% beta
residuals = y - y_pred
SST = sum((y - mean(y))^2)
SSE = sum(residuals^2)
R_squared = 1 - SSE / SST
cat("R-squared for the model treating exercise as categorical:", R_squared, "\n")


reduced_model = lm(RHRdec ~ exer15 + exer25 + age, data = rhrdat)
full_model = lm(RHRdec ~ exer15 + exer25 + age + exer15:age + exer25:age, data = rhrdat)
f_test_result = anova(reduced_model, full_model)

print(f_test_result)
p_value <- f_test_result$"Pr(>F)"[2]
p_value



set.seed(3301)
train_indices = sample(1:nrow(rhrdat), size = floor(0.5 * nrow(rhrdat)))
trainSet = rhrdat[train_indices, ]
testSet = rhrdat[-train_indices, ]

trainSet$exer15 = as.numeric(trainSet$exercise == 1.5)
trainSet$exer25 = as.numeric(trainSet$exercise == 2.5)
testSet$exer15 = as.numeric(testSet$exercise == 1.5)
testSet$exer25 = as.numeric(testSet$exercise == 2.5)
numeric_model = lm(RHRdec ~ age + exercise, data = trainSet)

categorical_model = lm(RHRdec ~ age + exer15 + exer25, data = trainSet)
numeric_predictions = predict(numeric_model, newdata = testSet)
categorical_predictions = predict(categorical_model, newdata = testSet)
numeric_mse = mean((testSet$RHRdec - numeric_predictions)^2)
categorical_mse = mean((testSet$RHRdec - categorical_predictions)^2)

cat("MSE for Numeric Model:", numeric_mse, "\n")
cat("MSE for Categorical Model:", categorical_mse, "\n")


####################################################################
3

gen.design.matrix <- function(n, mu, sigma.X, rho) {
  X = matrix(nrow = n, ncol = 3)
  for (i in 1:n) {
    X[i, 1] = 1
  }
  A = numeric(n)  
  Z2 = numeric(n) 
  Z3 = numeric(n) 
  
  for (i in 1:n) {
    A[i] = rnorm(1, mean = 0, sd = sqrt(rho) * sigma.X)
    Z2[i] = rnorm(1, mean = 0, sd = sqrt(1 - rho) * sigma.X)
    Z3[i] = rnorm(1, mean = 0, sd = sqrt(1 - rho) * sigma.X)
    
    X[i, 2] = mu + A[i] + Z2[i]
    X[i, 3] = mu + A[i] + Z3[i]
  }
  
  return(X)
}

mu = 68
sigma.X = 2
beta = c(1, 0, -1)  
sigma = 4            


calculate_power = function(n, rho) {
  p_values = numeric(100) 
  for (i in 1:100) { 
    X = gen.design.matrix(n, mu, sigma.X, rho)
    errors = rnorm(n, 0, sigma)
    Y = X %*% beta + errors
    model = lm(Y ~ X - 1) 
    p_values[i] = summary(model)$coefficients[3, 4]  
  }
  return(mean(p_values < 0.05))  
}

desired_power = 0.9
for (rho in c(0.5, 0.98)) {
  for (n in seq(10, 300, by = 10)) {
    power = calculate_power(n, rho)
    if (power >= desired_power) {
      cat("For rho =", rho, ", with n =", n, "subjects, we got a power of", format(power * 100, nsmall = 2), "%\n")
      break
    }
  }
}























