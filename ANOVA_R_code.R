#Begin by importing the relevant libraries

library(dplyr)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(fpp2)

#Import the data we want to analyse --> Life expectancy data

life_exp = read.csv("ANOVA_countries.csv")
life_exp <- life_exp[2:41,]
names(life_exp) <- c("Country","Year","Life_expect","Continent")
life_exp <- life_exp[,c("Life_expect","Continent")]

str(life_exp)
life_exp$Life_expect <- as.numeric(as.character(life_exp$Life_expect))

levels(life_exp$Continent) #Can see that "continent" is one of the levels. Will remove it.

life_exp$Continent <- factor(life_exp$Continent)
levels(life_exp$Continent)

str(life_exp)

#Will perform some visualisation first

ggplot(data = life_exp, aes(x=Continent, y=Life_expect, fill = Continent))+
  geom_boxplot()+
  ylab("Life Expectancy (Years)")+
  ggtitle("Life Expectancy by Continent")+
  theme(legend.title=element_blank())

#The boxplot shows that the groups do not have equal variance so anova most likely won't suit this. Will run it anyway


#One way ANOVA
fit <- aov(Life_expect ~ Continent, data = life_exp)

summary(fit)

#Maybe perform some post-hoc tests and plot some shite

plot(fit,1) #Residual plot

plot(fit,2) #Q-Q plot

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

#Can use Leven's test to check for homogeneity of variances
library(car)

leveneTest(Life_expect ~ Continent, data = life_exp)
#The result is not good. Levene test shows that the homoeneity of variances assumption is not met.
#However, we can still work around this. Can use a Kruskal_Wallis test.


#One way test with no assumption about variance
kruskal.test(Life_expect ~ Continent, data = life_exp)

#Can see that the p-value <0.05 shows that there is a statistically significant difference between the continents.

#Want to see which pairs of groups are different.

#Games-Howell Test (Post hoc)
#install.packages("userfriendlyscience") Offers a function oneway() to perform above test.
library(userfriendlyscience)

one.way <- oneway(life_exp$Life_expect, life_exp$Continent, posthoc = "games-howell")
one.way

#Will need to compute some summary statistics for the continents

summary_stat <- group_by(life_exp,Continent) %>%
  summarise(
    count = n(),
    mean = mean(Life_expect, na.rm=TRUE),
    sd = sd(Life_expect, na.rm = TRUE),
    median = median(Life_expect, na.rm =TRUE),
    IQR = IQR(Life_expect,na.rm=TRUE)
  )
summary_stat


