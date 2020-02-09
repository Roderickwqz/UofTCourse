library(tidyverse)
library(modelr)
library(lubridate)
url <- "https://raw.githubusercontent.com/TheEconomist/graphic-detail-data/master/data/2018-11-24_tv-ratings/IMDb_Economist_tv_ratings.csv"
tv_data <- read.csv(url)
head(tv_data, n = 5)


## Add decade column: 1990 2000 2010
## Add scifi column: inclue Sci-Fi in genres, marked as scifi, otherwise no
## Group the data by decade
## Add mean column, mean value = mean(av_rating) of each decade
## The infix operator %>% is not part of base R, but is in fact defined by the package
## magrittr (CRAN) and is heavily used by dplyr
## pass the left hand side of the operator to the first argument of the 
## right hand side of the operator.
tv_data_edit <- tv_data %>%
  mutate(decade = lubridate::floor_date(ymd(date), years(10))) %>%
  mutate(decade = as.character(format(decade,"%Y"))) %>%
  mutate(scifi = ifelse(grepl("Sci-Fi", genres), "scifi", "no")) %>%
  group_by(decade) %>%
  mutate(mean = mean(av_rating))


# Run an ANOVA
anova1 <- aov(av_rating~decade, data=tv_data_edit)

# Run a linear model
lm1 <- lm(av_rating~decade, data=tv_data_edit)

summary(anova1)

summary(lm1)


### Very simple example
set.seed(12)

##sample_n: select 3 rows, 3 rows in here means 3 rows for each group
simple <- tv_data_edit %>%
  sample_n(3) %>%
  mutate(mean = mean(av_rating))

p <- simple %>%
  ggplot(aes(x = decade, y = av_rating)) +
  geom_point()
p

c(
  lm(av_rating~1, filter(simple, decade == 1990))$coef,
  lm(av_rating~1, filter(simple, decade == 2000))$coef,
  lm(av_rating~1, filter(simple, decade == 2010))$coef
)

## Simple model: each mean for each decade
simple %>%
  select(decade, mean) %>%
  distinct() %>%
  head()


## Each point can be described by its group mean plus a residual
p +
  geom_errorbar(aes(ymin = mean, ymax = mean), colour = "red") +
  geom_text(aes(label=round(mean,3), y = mean), nudge_x = 0.3)


# Add mean of group one, blue line
p +
  geom_hline(yintercept = lm(av_rating~1, filter(simple, decade == 1990))$coef, color
             = "blue") +
  # Add means of other groups in black
  geom_segment(x = 1.7, xend = 2.3, y = 7.605, yend = 7.605) +
  geom_segment(x = 2.7, xend = 3.3, y = 7.418, yend = 7.418) +
  # Beta 1 and Beta 2 are slopes of these red lines
  geom_segment(x = 1, xend = 2, y = 7.745, yend = 7.605, col = "red") +
  geom_segment(x = 2, xend = 3, y = 7.605, yend = 7.418, col = "red") +
  geom_text(aes(label=round(mean,3), y = mean), nudge_x = 0.3)

# ANOVA design matrix (for how we first made it)
model.matrix(data=simple, ~0+decade)

# Linear model design matrix
model.matrix(data=simple, ~decade)
