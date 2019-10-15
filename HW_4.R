## Chapter 4 Homework Stats 511
# 3, 10, 17, 19, 25, 27, 31

# -----------------------------------------------------
# Problem 17


# -------------------------------------------------
# Problem 19         r help # http://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test

birdslegs <- read_excel("data/ex0221.xlsx")

# Rank sum comparison
wilcox.test(Humerus ~ Status, data=birdslegs) # does this show a two sided p value?
# I think this function reports a p value based on a normal approximation and it uses a continuity correction
# P value is 0.17. The two sample test is 0.08 and without outlier was 0.18
# the rank-sum test is robust against outliers because it got a similar p value to the t test without the outlier.

# ---------------------------------------------------
# Problem 25
pigs <- read_excel("data/ex0211.xlsx")

control <- pigs$Lifetime[pigs$Group == "Control"]
bacilli <- pigs$Lifetime[pigs$Group == "Bacilli"]

# Welch t test
t.test(control, bacilli,
       alternative = "two.sided",
       var.equal = FALSE,  # welch test is default = FALSE where variance are NOT assumed to be equal
       conf.level = 0.95)
# the additive treatment effect is a sensible model because the confidence interval does not contain zero.

# -------------------------------------------------------------
# Problem 27
twins <- read_excel("data/case0202.xlsx")

# signed rank test before log transform
wilcox.test(twins$Unaffected, twins$Affected, paired=TRUE,
            alternative = c("two.sided"),
            exact = NULL, correct = TRUE) # this line would specify if we are computing an exact p-value (we are not) 
           # or whether there should be a continuity correction
wilcox.test(log(Unaffected) ~ log(Affected), data=twins) # still stuck...

# ------------------------------------------------------------------
# Problem 31
therapy <- read_excel("data/ex0431.xlsx")

# Rank sum comparison
wilcox.test(Survival ~ Group, data=therapy) # Null: Therapy does not affect life expectancy

cont. <- therapy$Survival[therapy$Group == "Control"]
thera. <- therapy$Survival[therapy$Group == "Therapy"] # small sample size

mean(cont.) #20
median(cont.) #17
mean(thera.) #39.4
median(thera.) #21

summary(cont.)
summary(thera.)

boxplot(therapy$Survival ~ therapy$Group,
        xlab = "Treatment", ylab = "Months")

boxplot(therapy$Survival ~ therapy$Group,
        xlab = "Treatment", ylab = "Months")






