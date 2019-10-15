## Homework Stats 511 chapter 3
# 3, 5, 8, 18, 26, 28, 30

library(readxl)
library(tidyverse)
library(tidycensus)
library(Hmisc)
library(shiny)
library(DBI)
library(MVR)

# ----------------------------------------------------------------
# Problem 3.18


# ---------------------------------------------------------------------
# Problem 3.26

orange <- read_excel("J:/00 Masters/Stats 511/data/case0302.xlsx")

orange$Dioxin <- orange$Dioxin + 0.5

boxplot(log(orange$Dioxin) ~ orange$Veteran,
        xlab = "Veteran Status", ylab = "Dioxin")

Vietnam_log <- log(orange$Dioxin[orange$Veteran == "Vietnam"])
Other <- log(orange$Dioxin[orange$Veteran == "Other"])


t.test(Vietnam_log, Other,
       alternative = "two.sided",
       mu= 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

(conf <- c(exp(-0.05408), exp(0.14115))) # This is the back transformation e^(confidence interval)

# ---------------------------------------------------------------------
# Problem 3.28
# Bumpus's Data. Obtain p-values from the t-test to compare humerus lengths for sparrows that survived and 
# those that perished (Exercise 2.21), with and without the smallest length in the perished group 
# (length D 0:659 inch). Do the conclusions depend on this one observation? What action should be taken if 
# they do?

sparrow <- read_excel("J:/00 Masters/Stats 511/data/ex0221.xlsx")

survived <- sparrow$Humerus[sparrow$Status == "Survived"]
perished <- sparrow$Humerus[sparrow$Status == "Perished"]

t.test(survived, perished,
       alternative = "two.sided",
       var.equal = TRUE,  # assume that variance is equal...
       conf.level = 0.95) # p value is 0.081

sparrowmin <- sparrow[-which.min(sparrow$Humerus),] # creating a new data set without the min.

survivedmin <- sparrowmin$Humerus[sparrowmin$Status == "Survived"]
perishedmin <- sparrowmin$Humerus[sparrowmin$Status == "Perished"]

t.test(survivedmin, perishedmin,
       alternative = "two.sided",
       var.equal = TRUE,  # assume that variance is equal...
       conf.level = 0.95) # p value is 0.18

# ----------------------------------------------------------------
# Problem 3.30
income <- read_excel("J:/00 Masters/Stats 511/data/ex0330.xlsx")

t.test(income$Income2005[income$Educ == "12"], income$Income2005[income$Educ == "16"],
       alternative = "two.sided",
       var.equal = TRUE,
       conf.level = 0.95)
