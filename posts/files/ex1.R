library(palmerpenguins)
penguins
penguins_na <- na.omit(penguins)
mean(penguins_na$bill_length_mm)
# H0: mu = 40 Ha: mu =\= 40
t.test(penguins_na$bill_length_mm, mu = 40)
# alpha = 0.05
# p = 2.2*10^-16
# e = *10^
# p < alpha
# Reject H0
# There is a significant difference betweeen 
# the mean bill length and 
# the hypothesized value of 40

# Hypothesis Tests
# t-test
# proportions-test
# z-test
# ANOVA -  Analysis of Variance (F-Test)
# Wilcoxan-Rank Sum/Signed test
# Kruskal Tests
# Monte Carlo Tests
# Chi-square Tests

# Bill length is different between sex the penguin
mean(penguins_na$bill_length_mm[penguins_na$sex=='female'])
mean(penguins_na$bill_length_mm[penguins_na$sex=='male'])

library(tidyverse)
penguins_na %>% 
  group_by(sex) %>% 
  summarise(mean(bill_length_mm))

## H0: mu1 - mu2 = 0 HA: mu1 - mu2 =\= 0  
t.test(bill_length_mm ~ sex, data =  penguins_na)
# p = 1.066e-10
# alpha = 0.05
# p < alpha
# Reject H0
# There is a significant difference between the 
# mean bill length for the different
# sexes of the penguins

# 95% CI: (-4.8, -2.6)
#  0 is in CI?
# No: Reject H0


# If yes: Fail to Reject H0


str(penguins)
str(penguins_na)
class(penguins)


# Assumptions
# Independence
# Normality

qqnorm(penguins_na$bill_length_mm[penguins_na$sex=='female'])
qqline(penguins_na$bill_length_mm[penguins_na$sex=='female'])

qqnorm(penguins_na$bill_length_mm[penguins_na$sex=='male'])
qqline(penguins_na$bill_length_mm[penguins_na$sex=='male'])

## Shapiro-Wilks Test
# H0: Data comes from a normal distribution
# HA: Data does not come from a normal distribution
shapiro.test(penguins_na$bill_length_mm[penguins_na$sex=='male'])
# alpha = 0.05
shapiro.test(penguins_na$bill_length_mm[penguins_na$sex=='female'])

length(penguins_na$bill_length_mm[penguins_na$sex=='male'])
length(penguins_na$bill_length_mm[penguins_na$sex=='female'])
#n1 and n2 > 30 xbar_1,xbar_2 ~ N(mu, sigma^2/n) 
# n1 or n2 < 30
# By CLT


## Equal Variance
penguins_na %>% group_by(sex) %>% 
  summarise(var(bill_length_mm))

## F test 
## H0: sigma^2_1/sigma^2_2 =1 
## Ha: sigma^2_1/sigma^2_2 =\= 1 

var.test(bill_length_mm~sex,  data = penguins_na)
# p = 0.2467
# alpha = 0.05
# variance are the same

# Bartlett and Levene's
# H0: variances are the same
# Ha: variances are no the same
bartlett.test(bill_length_mm~sex, data = penguins_na)

library(car)
leveneTest(bill_length_mm~sex, data = penguins_na)



  