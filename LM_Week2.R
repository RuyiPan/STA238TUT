library(tidyverse)
library(patchwork) # For arranging plots
data("iris")

# Plot KDEs for all lengths in iris and in different facet
glimpse(iris)
lengths_long <- iris %>% 
    pivot_longer(cols = c(Sepal.Length, Petal.Length), names_to = "length", values_to = "values")

lengths_long %>% ggplot(aes(x=values, colour = length)) +
  theme_bw() +
  facet_grid(~length) +
  geom_density(show.legend = FALSE)


#Make a scatter plot of length as a function of the width, with a layer showing a linear model.
p1 <- iris %>%
  ggplot(aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(size = 0.3) +
  geom_smooth(method='lm', formula= y~x, linewidth = 0.4) 

p2 <- iris %>%
  ggplot(aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(size = 0.3) +
  geom_smooth(method='lm', formula= y~x, linewidth = 0.4) 

(p1 | p2)


## Limit theorems

# Assume the number of users visiting a website follows poisson distribution with mean 2 (rate of occurance).
# f(x) = \lambda^x exp(-\lambda) / x!  \lambda = 2

# Consider 500 websites, check the average users' number as websites increase
set.seed(2023)
sim_poi <- function(n, lambda) {
  rpois(n, lambda)
}

n <- 500
lambda <- 2
runningaverage <-cumsum(sim_poi(n,lambda)) / 1:n

df <- tibble(x = 1:length(runningaverage), y = runningaverage)

df %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point(shape = 20, size = 0.2) +
  geom_hline(yintercept = lambda,colour = "red",linewidth = .5,linetype = "dotted") +
  labs(title = "Running test average",
       x = "Number of recording day",
       y = "Number of users",
       color = "Realization")




#What the distribution of the average users if you record N times.
#CLT let us it can be estimated by normal distribution.
N <-300
n <- 500
lambda <- 2
user_num <- numeric(N)
for (i in 1:N) {
  sim <- sim_poi(n, lambda)
  user_num[i] <- sum(sim)/ n
}

mu <- lambda
sigmasq <- lambda/n

z <- (user_num - mu) / sqrt(sigmasq)

# calculate sample mean & standard deviation
muhat <- mean(z) 
sigmahat <- sd(z)

# plot
b <- (24*sqrt(pi))^(1/3) * sigmahat * (N^(-1/3))
tibble(x = z) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = b, colour="black", fill="grey", alpha = 0.3) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), colour = "blue") + # layer with standard normal distribution
  geom_vline(xintercept = muhat, colour = "red")









