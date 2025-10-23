set.seed(3301)

rrv = function(n, lamda){
  y.list = numeric(n)
  for (i in 1:n){
    pdf_y = runif(1)^(1/(lamda + 1))
    y.list[i] = pdf_y
  }
  return(y.list)
}

lambda_hat_estimator = function(sample) {
  return(- (100 + sum(log(sample))) / sum(log(sample)))
}

counter = 0
for (i in 1:10000){
  y.list = rrv(n=100, lamda = 5/4)
  lambda_hat = lambda_hat_estimator(y.list)
  lambda_hat
  
  std_error = (lambda_hat + 1) / sqrt(n)
  z_alpha = qnorm(1 - alpha/2)
  

  lower = lambda_hat - z_alpha * std_error
  upper = lambda_hat + z_alpha * std_error
  
  if (lambda >= lower && lambda <= upper) {
    counter = counter + 1
  }
}

coverage_probability = counter / 10000
print(coverage_probability)

dat = readRDS("hw2_dat.RDS")
time_plot = plot(dat)
data
plot(dat, seq(1,150),type = 'l')
dat

qqnorm(dat)
qqline(dat)
