library(tidyverse)
library(tibble) # (may not need)
library(psych)
# 'iris' is a r built-in dataset (iris flower data set)
data("iris")

####################################
### BASIC STRUCTURE OF DATA SET
####################################

glimpse(iris)

# iris_github <- read.csv("https://gist.githubusercontent.com/curran/a08a1080b88344b0c8a7/raw/0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv",
#                         stringsAsFactors = T)
# glimpse(iris_github)


# Extract the first/last six rows of the iris dataset
head(iris)
tail(iris)

# Names of all the variables in the dataset
names(iris)

# How many observations do we have 
nrow(iris)

# How many variables are included in the dataset 
ncol(iris)

####################################
### MANIPULATE DATA SET
####################################


# If We will need the measurements of the petal width, petal length and flower specie
iris_part1 <- iris %>% select(Petal.Length,Petal.Width,Species)
glimpse(iris_part1)

# Let's create a dataset which only contains measurements, not flower species, and show a glimpse of it
iris_part2 <- iris[,1:4]
# iris_part2 <- iris[, -5]
glimpse(iris_part2)

#Now let's add an id column to label the observations
iris$id <- 1:nrow(iris)

# 1. Which observation have petal width width over 2.3 ?
iris %>% filter(Petal.Width > 2.3)

# 2. Which flowers got 3.5 on petal length?
iris  %>% filter(Petal.Length == 3.5)

# 3. Filter the data for versicolor species and sepal width equal to or greater than 3.1, 
iris  %>% filter(Species == "versicolor", Sepal.Width >= 3.1)

# 4. We also want it arrange the data by sepal length (descending, or increasing)
iris  %>% filter(Species == "versicolor", Sepal.Width >= 3.1) %>%
  arrange(desc(Sepal.Length))  #arrange(Sepal.Length)


####################################
### NUMERICAL SUMMARIES 
####################################


# 5-number summary of all the mesaruments in centimeters of the flowers
summary(iris[,-c(3,4)])

describe(iris$Petal.Length) # (need psych package)

# Mean and median of the petal length. How do they compare? The median is less than the mean?
xbar = mean(iris$Petal.Length)

xmed = median(iris$Petal.Length)

xmed < xbar

# Quantile
quantile(iris$Petal.Length, prob=0.5)
quantile(iris$Petal.Length, prob=1)
quantile(iris$Petal.Length, prob=0.75)
quantile(iris$Petal.Length, prob=0.25)


#Range
min(iris$Petal.Length)
max(iris$Petal.Length)
range <- max(iris$Petal.Length) - min(iris$Petal.Length)
range

# IQR (interquartile range)
iqr <- quantile(iris$Petal.Length, prob=0.75) - quantile(iris$Petal.Length, prob=0.25)
iqr

# MAD (Median of absolute deviation)
median( abs(iris$Petal.Length - median(iris$Petal.Length)) )

# Sample variance
n <- nrow(iris)

sum((iris$Petal.Length-xbar)^2)/(n-1)

#or
var(iris$Petal.Length)


# Standard deviation
sd(iris$Petal.Length)

# The unique values for sepal width are
iris$Petal.Width %>% unique() %>% sort() #%>% length()

# The unique values for sepal width are
iris$Petal.Length %>% unique() %>% sort() #%>% length()


##############################
### GRAPHICAL SUMMARIES 
##############################


# Boxplot overall
boxplot(iris$Sepal.Length)


# Outliers
temp <- c(iris$Sepal.Length, 10)
boxplot(temp)

# Boxplot for each Species
boxplot(iris$Sepal.Length ~ iris$Species)

iris %>% ggplot(aes(y=Sepal.Length, colour = Species)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot() 



# Plot the Petal.Length distributions as bar graphs
iris %>%
  ggplot(aes(x = Petal.Length, colour = Species)) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0,1,0.1)) +
  facet_wrap(vars(Species)) +
  geom_bar(aes(y = after_stat(prop)), 
           width=0.1, alpha = 0.2, 
           show.legend = FALSE)


## Histogram
# Create a (named) plot of the petal width in the iris data. Choose a theme.

# Link for multiple themes: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
iris_setosa <- iris %>% filter(Species == "setosa")
# theme_bw : white background and gray grid lines
# theme_light : light gray lines and axis (more attention towards the data)
# theme_void : Empty theme, useful for plots with non-standard coordinates or for drawings
# and so on ......
ggplot(iris_setosa, aes(x = Petal.Width)) +
  theme_void() +
  geom_histogram(aes(y = after_stat(density)))
  

# Add a histogram layer to `plot` using the optimal binwidth.
n <- nrow(iris_setosa)
sigmahat <- sd(iris_setosa$Petal.Width)
b <- (24*sqrt(pi))^(1/3) * sigmahat * (n^(-1/3))

plot_hist <- ggplot(iris_setosa, aes(x = Petal.Width)) +
  theme_bw() +
  geom_histogram(aes(y = after_stat(density)), binwidth = b, colour = "black",fill = "grey",alpha = .2)

# Add an automatically computed KDE as a layer on the histogram plot. No need to name this plot. 
plot_hist +
  geom_density()

# Alternative method for plotting KDEs that doesn't use ggplot

dens <- density(iris_setosa$Petal.Width)
tibble(x = dens$x,y = dens$y) %>%
  ggplot(aes(x = x,y = y)) +
  theme_classic() + 
  geom_line()

# Compare petal width between different species
iris %>%
  ggplot(aes(x = Petal.Width, colour = Species)) +
  theme_bw() +
  geom_density()

# See different species in each bins
ggplot(iris) +
  aes(Sepal.Length, colour = Species) +
  geom_histogram(bins = 10, fill = "white") +
  scale_color_hue()

# See histograms for each species
# ggplot(iris, aes(Sepal.Width, fill = Species)) +
#   geom_histogram(binwidth = 0.30) +
#   facet_wrap(~ Species)


# Empirical cumulative distribution function
ggplot(iris, aes(x=Sepal.Length)) +
  stat_ecdf()

ggplot(iris, aes(x=Sepal.Length, colour=Species)) +
  stat_ecdf()


# Scatter plots , bivariate data, see the relationship
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(size=2, shape=23)

ggplot(iris, aes(x=Sepal.Length, y=Petal.Length,colour=Species)) +
  geom_point(size=2, shape=23)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(size=2, shape=23)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width,colour=Species)) +
  geom_point(size=2, shape=23)

### A useful link for data analysis of iris dataset: https://rpubs.com/Geestar73/myR

