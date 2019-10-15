# Chapeter 2
# 3, 6, 7, 13, 18, 22

install.packages("readxl")
install.packages("Hmisc")
install.packages("shiny")
install.packages("DBI")
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("ANOVAreplication")
library(readxl)
library(tidyverse)
library(tidycensus)
library(Hmisc)
library(shiny)
library(DBI)
library(MVR)
library("ANOVAreplication")


# ----------------------------------------------------------
# Computational Problems

# 2.13
# Read excel file into dataframe
my_data_13 <- read_excel("data/ex0112.xlsx")

# (a) Find mean and sample standard deviation by group

my_means <- by(my_data_13$BP,my_data_13$Diet,mean)
my_sd <- by(my_data_13$BP,my_data_13$Diet,sd)

oil <- my_data_13 %>%
  group_by(Diet) %>%
  summarise(mean = mean(), sd = sd()) #not sure how to tell the code to pick the factors/columns

# Antother try... try to break the table into two.
fishoil <- my_data_13 %>%
  filter(Diet %in% c("Fishoil"))


# (b) Find the pooled estimate of standard deviation # Function not working... what library do I need?
pooled.sd(my_data_13)

n1 <- 7
n2 <- 7
s1 <- sd(my_data_13$BP[my_data_13$Diet == "FishOil"]) 
s2 <- sd(my_data_13$BP[my_data_13$Diet == "RegularOil"]) # why is this wrong?

Sp <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))

# (c) compute SE(Y2-Y1). This is standard error for the difference... what is the difference?
std.error(my_data_13$BP, na.rm)

SE <- Sp*sqrt((1/n1)+(1/n2))

# (d) what is dof? what is 97th percentile?
datafish <- my_data_13$BP[my_data_13$Diet == "FishOil"]
datareg <- my_data_13$BP[my_data_13$Diet == "RegularOil"]

t.test(datafish, datareg,
       alternative = c("greater"),
       conf.level = 0.97)

# (e) 95% confidence interval
t.test(datafish, datareg,
       alternative = c("two.sided"),
       conf.level = 0.95)

# (f) t statistic

# (g) one-sided p-value

# ---------------------------------------------------
# 2.18
# Read data
my_data_18 <- read_excel("J:/00 Masters/Stats 511/data/ex0218.xlsx")
my_data_18_1 <- read_excel("J:/00 Masters/Stats 511/data/ex0218_1.xlsx")

# (a) Side by side boxplots
boxplot(my_data_18$Depth ~ my_data_18$Year, ylim=c(0,20),
        xlab = "Year", ylab = "Depth of Beak") # This one works!

# (b) run two-sample t test
year1976 <- my_data_18$Depth[my_data_18$Year == 1976]
year1978 <- my_data_18$Depth[my_data_18$Year == 1978]


t.test(year1976, year1978)

# (c) two-sided p-value from the t-test
t.test(year1976, year1978,
       alternative = c("two.sided"),
       mu= 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

# (d) provide an estimate and 95% for amount by 1978 mean exceeds the 1976 mean

# (e) written answer: the finches in 1978 could be the same birds from the 1976 study or their direct offspring
# not necessarily independent samples

# ----------------------------------------------------
# 2.22
intelligence <- read_excel("J:/00 Masters/Stats 511/data/ex0222.xlsx")

# AFQT
male <- intelligence$AFQT[intelligence$Gender == "male"]
male_mean <- mean(male)

female <- intelligence$AFQT[intelligence$Gender == "female"]
female_mean <- mean(female)

mean_diff <- male_mean - female_mean

t.test(male, female)

boxplot(intelligence$AFQT ~ intelligence$Gender,
        xlab = "Gender", ylab = "AFQT Score")

# Arith
malearith <- intelligence$Arith[intelligence$Gender == "male"]
male_meanarith <- mean(malearith)

femalearith <- intelligence$Arith[intelligence$Gender == "female"]
female_meanarith <- mean(femalearith)

mean_diff_arith <- male_meanarith - female_meanarith

t.test(malearith, femalearith)

# Word

# Parag

# Math





