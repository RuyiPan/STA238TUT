library(tidyverse)
library(ggplot2)
library(patchwork)
data <- rpois(14, 2) #assume this is the data collected in the last year.

#Recall the poisson example: assume the number of user visiting a website follow poisson(\lambda)
#Write a function for the likelihood function and plot it. 
#In the midterm, we derived the *maximum likelihood estimator* for $\lambda$, $\widehat\theta_{MLE}$.
#Calculate a *maximum likelihood estimate* using the data 


likelihood1 <- function(x, lambda) {
  L <- 1
  for (i in 1:length(x)) {
    L <- L * dpois(x[i], lambda=lambda)
  } 
  L
}

tibble(para_values = c(0.1,8)) %>%
  ggplot(aes(x = para_values)) +
  theme_bw() +
  stat_function(fun= likelihood1, args = list(x = data), n=1000) + 
  geom_vline(xintercept = lambdahat,color="blue") + 
  labs( x = "theta", y = "likelihood") +
  theme(plot.caption = element_text(size = 12))

likelihood2 <- function(x, lambda) {
  L <- 1
  for (i in 1:length(x)) {
    L <- L + dpois(x[i], lambda=lambda,log = T)
  } 
  L
}
lambdahat <- sum(data)/length(data) # MLE for theta using all data

tibble(para_values = c(0.1,8)) %>%
  ggplot(aes(x = para_values)) +
  theme_bw() +
  stat_function(fun= likelihood2, args = list(x = data), n=1000) + 
  geom_vline(xintercept = lambdahat,color="blue") + 
  labs( x = "theta", y = "likelihood") +
  theme(plot.caption = element_text(size = 12))



### Bayesian Inference
#Let's put a gamma prior on $\lambda$. In the last tutorial, we derived the posterior distribution for
# a gamma priors and a likelihood from the same family of distributions as we're using here.
# Write a function to plot the prior in blue and posterior in red. 
#Plot the function for 3-4 sets of values for the hyperparmeters that could represent your belief, 
# indicating the values chosen on each plot 

n <- length(data)
sumx <- sum(data)

prior <- function(lambda, alpha, beta) {dgamma(lambda, shape=alpha, rate = beta) } 
posterior <- function(lambda, alpha, beta) {dgamma(lambda, shape = sumx + alpha, rate = n + beta)} 

plot_bayes <- function(alpha,beta) {
  tibble(x = c(1,5)) %>%
    ggplot(aes(x = x)) +
    theme_bw() +
    stat_function(fun = prior, colour = "blue", args = list(alpha, beta), n=1000) +
    stat_function(fun = posterior, colour = "red", args = list(alpha, beta), n=1000) +
    labs(subtitle = str_c("alpha: ", alpha," and beta: ", beta,""),
         x = "lambda",
         y = "density")
}

plot_bayes(1, 2) / 
  plot_bayes(1,20) | 
  plot_bayes(3,2) / 
  plot_bayes(20,1) 

alpha <- 3
beta <-4
bayes_est<-(sumx + alpha) / (n +beta) #posterior mean


tibble(x = c(0,5)) %>%
  ggplot(aes(x = x)) +
  theme_bw() +
  stat_function(fun = prior, colour = "blue", args = list(alpha, beta), n=1000) +
  stat_function(fun = posterior, colour = "red", args = list(alpha, beta), n=1000) +
  geom_vline(xintercept = bayes_est , color = "green", linetype = "dashed") +
  labs(subtitle = str_c("alpha: ", alpha," and beta: ", beta,""),
       x = "lambda",
       y = "density")






## Bootstrap

#boostrap

#If we cannot sampling from population. we only have a sample

#1. bootstrap sample, X_1^{*},...,X_n^{*}: is a sample from a dataset that is
# drawn with replacement

#2. bootstrap statistics (estimate): sample from sampling distribution of \hat\theta

#3. bootstrap distribution: the distribution of the bootstrap statisitcs

#4. parametric / non-parametric bootstrap


#Use an empirical bootstrap to estimate the rate of occurance
set.seed(2023)
n <- length(data)
B <- 1000


bootlambda<- numeric(B)
for (i in 1:B){
  bootsamp <- sample(data, n, replace = TRUE)
  bootlambda[i] <- mean(bootsamp)
}

bootest <- mean(bootlambda)
bootse <- sd(bootlambda)


lambdahat <- mean(data) 
lambdahatse <- sqrt(lambdahat/n)

tibble(x = bootlambda) %>%
  ggplot(aes(x = x)) +
  theme_bw() +
  geom_histogram(aes(y = after_stat(density)), bins = 24, colour = "black", fill = "lightgrey") +
  stat_function(fun = dnorm, colour = "blue", linetype = "dashed", args = list(lambdahat, lambdahatse))




#Use a parametric bootstrap to, again, estimate $\lambda$ and the standard error of $\widehat\lambda$, 
#and plot a histogram of the bootstrap distribution for $\widehat\theta$.

lambdahat <- mean(data) 
lambdahatse <- sqrt(lambdahat/n)

set.seed(2023)
bootlambda<- numeric(B)
for (i in 1:B){
  bootsamp <- rpois(n, lambda = lambdahat) 
  bootlambda[i] <- mean(bootsamp) 
}

bootest <- mean(bootlambda)
bootse <- sd(bootlambda)

stringr::str_c("The parametric bootstrap estimate is ",
               round(bootest, 4), 
               ", with a standard error of ", round(bootse, 4))

tibble(x = bootlambda) %>%
  ggplot(aes(x = x)) +
  theme_bw() +
  geom_histogram(aes(y = after_stat(density)), bins = 24, colour = "black", fill = "lightgrey") +
  stat_function(fun = dnorm, colour = "blue", linetype = "dashed", args = list(lambdahat, lambdahatse))


