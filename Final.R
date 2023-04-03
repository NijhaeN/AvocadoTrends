# Importing data and libraries 
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("rstatix")
library(rstatix)

# Step one read in the data
avocados <- read_csv("avocados_with_state.csv")
View(avocados)

# Step Two: exploratory data analysis
summary(avocados)
glimpse(avocados)
sum(is.na.data.frame(avocados))
## 14 columns, 18249 rows
## 2 categorical variables, 12 numeric

# Step Three: Check out our categorical variables
avocados %>% count(type)
avocados %>% count(region)
avocados %>% count(state)
# table(avocados$region) # better view of the regions
## looks like there are only 2 types, 54 different regions, and 46 states

# Step Four: Filtering out desired categories, and cleaning up
## We're going to be taking a look at the cities 'Tampa', 'Columbus', 'Chicago'

# %>% means "and then" in R!
### so avocados data, and then, select columns, and then... 
avocados1 <- avocados %>%
  select(-c('4046','4225','4770',ends_with('Bags'))) %>%
  na.omit() %>%
  filter(region %in% c("Tampa", "Columbus", "Chicago")) %>%
  filter(year %in% c(2015,2016,2017)) %>%
  mutate(region = recode(region,
                         Chicago = 0,
                         Tampa = 1,
                         Columbus = 2)) %>%
  mutate(type = recode(type,
                       conventional = 0,
                       organic = 1))

## Clean but keeping categorical data for visualization
avocados2 <- avocados %>%
  select(-c('4046','4225','4770',ends_with('Bags'))) %>%
  na.omit() %>%
  filter(region %in% c("Tampa", "Columbus", "Chicago"))

###
View(avocados1)
summary(avocados1)
# Exporting avocados2 to create visuals in tableu
write.csv(avocados2, "avocadosClean.csv", row.names=TRUE)


### Question to answer : Is the Average price of Organic avocados more than Conventional?
## For this we will run and independent t-test

# let's create a small data frame for our variables
avo <- avocados1 %>%
  select(AveragePrice, type, cost) 

t.test(AveragePrice ~ type, data = avo, alternative= "two.sided", var.equal = FALSE)
# There is a statistically significant difference in the price of conventional
# and organic avocados
## This is more of a consumer observation, but now that we know there is a difference, I wonder
## if the commercial cost effects the price as well.


#### Question to Answer: Does the season effect price of the avocados?
## Two-Way Anova

# wrangling data
avo2 <- avocados1 %>%
  select(AveragePrice, type, Season)

avo2 %>%
  group_by(type, Season) %>%
  get_summary_stats(AveragePrice, type = "mean_sd")

avo2$type <- as.factor(avo2$type)
avo2$Season <- as.factor(avo2$Season)

# Visualizing data
bxp <- ggboxplot(
  avo2, x = "type", y = "AveragePrice",
  color = "Season", palette = "Set 2"
)
bxp

## Checking assumptions

avo2 %>%
  group_by(type, Season) %>%
  identify_outliers(AveragePrice) %>%
  print(n = 27)

# there are some outliers but let's move forward

# normality assumption via linear model
# Build the linear model
model  <- lm(AveragePrice ~ type*Season,
             data = avo2)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

# Normality can be assumed as most of our data fits does fit
# our model linear model even though we do not have the support 
# of the Shapiro test.

# checking the normality by group just for some extra details
avo2 %>%
  group_by(type, Season) %>%
  shapiro_test(AveragePrice)

ggqqplot(avo2, "AveragePrice", ggtheme = theme_bw()) +
  facet_grid(type ~ Season)

# about half of the data falls along the reference lines
# though we do know that this data is spread across regions this may
# have something to do with it

# Homogneity of variance assumption
avo2 %>% levene_test(AveragePrice ~ type*Season)

# we cannot assume homogeneity of variance as out pvalue is significant

# Computation
res.aov <- avo2 %>% anova_test(AveragePrice ~ type * Season)
res.aov


# Post Hocs
### since our two way ANOVA didn't reveal anything significant 
### let's inspect our variables
res.aov
## It seems there is significance in the season and type on the price but they don't have 
## much significance on there own


# Pairwise Comparison
avo2 %>%
  pairwise_t_test(
    AveragePrice ~ Season, 
    p.adjust.method = "bonferroni"
  )
# there was a statistical difference of price between all seasons
# for both organic and conventional avocados









