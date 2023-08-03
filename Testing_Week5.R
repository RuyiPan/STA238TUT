library(tidyverse)
library(ggplot2)
Weather <- as.data.frame(rnorm(1000, mean=25, sd=3))
colnames(Weather) <- c("temp")
Weather$date <- c(1:1000)

#Assume temperatures in this summer are normally distributed
#Include a histogram and KDE of the temperatures

#We want to compute a 95% confidence interval for the mean of the exam marks.
#Let's use a few different methods and compare.

#hist and KDE
xbar <- mean(Weather$temp)
sn <- sd(Weather$temp)

Weather %>%
  ggplot(aes(x=temp)) +
  theme_bw() +
  geom_density() +
  geom_histogram(aes(y=after_stat(density)), bins = 15, alpha = 0.2) +
  stat_function(fun = dnorm, args = c(mean = xbar, sd = sn), color="green", linetype="dashed") 

#assuming the data generating process for the temperature follows a normal distribution 
#with a known variance 1
#Compute the 95% confidence interval, 

xbar <- mean(Weather$temp)
sigma <- 1
n <- length(Weather$temp)
critval_t <- qnorm(0.025, lower.tail = FALSE)

# Compute the confidence interval
tibble(
  "lower" = xbar - critval_t*sigma/sqrt(n),
  "upper" = xbar + critval_t*sigma/sqrt(n)
)

####Assuming the data generating process for the temperature follows a normal distribution 
####with unknown variance
#Compute the 95% confidence interval, 

xbar <- mean(Weather$temp)
sn <- sd(Weather$temp)
n <- length(Weather$temp)
critval_t <- qt(0.025, df = n-1, lower.tail = FALSE)

# Compute the confidence interval
tibble(
  "lower" = xbar - critval_t*sn/sqrt(n),
  "upper" = xbar + critval_t*sn/sqrt(n)
)

#### Assume n is sufficiently large, Central Limit Theorem applies
###Compute the 95% confidence interval
xbar <- mean(Weather$temp)
sn <- sd(Weather$temp)
n <- length(Weather$temp)
critval_z <- qnorm(0.025, lower.tail = FALSE)

# Compute the confidence interval
tibble(
  "lower" = xbar - critval_z*sn/sqrt(n),
  "upper" = xbar + critval_z*sn/sqrt(n)
)

### No extra assumptions
#Use the bootstrap principle to compute the 95% confidence interval.

set.seed(2023)

B <- 1000

xbar <- mean(Weather$temp)
sn <- sd(Weather$temp)
n <- length(Weather$temp)

# Generate bootstrap statistic
bootTn <- numeric(B)
for (i in 1:B){
  bootsamp <- sample(Weather$temp, n, replace = TRUE)
  bootTn[i] <- (mean(bootsamp) - xbar) / (sd(bootsamp) / sqrt(n))
}

# Find critical values from bootstrap distribution
alpha <- 0.05 # set the confidence level
critvals <- tibble(
  "cl" = quantile(bootTn, probs = alpha/2)[[1]], 
  "cu" = quantile(bootTn, probs = 1 - alpha/2)[[1]]
)

# Compute the confidence interval
tibble(
  "lower" = xbar - critvals$cu * sn/sqrt(n),
  "upper" = xbar - critvals$cl * sn/sqrt(n)
)




set.seed(2023)

Visits<- rpois(40,lambda=3)
## Visits
##Recall the poisson example (#users visting a websites follows poisson distribution) 
  

### Find a 95% confidence interval for the mean, 
### using an appropriate method, and use that to compute a 95% confidence interval for lambda.


set.seed(2023)

B <- 1000
alpha <- 0.05 # set the confidence level

n <- length(Visits)
xbar <- mean(Visits)
sn <- sd(Visits)

# Generate bootstrap statistic
bootTn <- numeric(B)
for (i in 1:B){
  bootsamp <- sample(Visits, n, replace = TRUE)
  bootTn[i] <- (mean(bootsamp) - xbar) / (sd(bootsamp) / sqrt(n))
}

# Find critical values from bootstrap distribution
critvals <- tibble(
  "cl" = quantile(bootTn, probs = alpha/2)[[1]], 
  "cu" = quantile(bootTn, probs = 1 - alpha/2)[[1]]
)

CI_mu <- tibble(
  "lower" = xbar - critvals$cu * sn/sqrt(n),
  "upper" = xbar - critvals$cl * sn/sqrt(n)
)

# The confidence interval for the mean:
CI_mu

# The confidence interval for lambda:
tibble( 
  "lower" = CI_mu$upper,
  "upper" = CI_mu$lower
)

### Testing claims
# Someone is claiming that the true rate of occurrence is 3.5. 

#### Likelihood ratio test
# Use a likelihood ratio test to determine if our data gives any evidence against this claim. 


n <- length(Visits)

# Claim:
lambda0 <- 3.5

# Estimate:
lambda_mle <- mean(Visits)

# Define the log likelihood function
loglikelihood <- function(Visits, lambda) {
  L <- 0
  for (i in 1:length(Visits)) {
    L <- L + dpois(Visits[i], lambda=lambda,log = T)
  } 
  L
}


# Compute the likelihood ratio
LR <- exp(loglikelihood(Visits, lambda0))/exp(loglikelihood(Visits,lambda_mle))


# likelihood ratio test to check if the data supports the claim
lr <- -2*log(LR) #lr <- 2*(loglikelihood(Visits, lambda0)-loglikelihood(Visits,lambda_mle))
1 - pchisq(lr, df = 1)


#### t-test
###Use a bootstrap t-test to determine if our data gives any evidence against this claim.

set.seed(2023)

n <- length(Visits)
B <- 10000

# Claim:
lambda0 <- 3.5
mu0 <- lambda0

# Data values:
xbar <- mean(Visits)
sn <- sd(Visits)

tobs <- (xbar - mu0) / (sn/sqrt(n))

#empirical bootstrap
Tboot <- numeric(B)
for (i in 1:B){
  bootsamp <- sample(Visits, n, replace=TRUE) 
  bootmean <- mean(bootsamp)
  bootse <- sd(bootsamp)/sqrt(n)
  Tboot[i] <- (bootmean - xbar) / bootse
}

sum(abs(Tboot) >= abs(tobs) ) / B

