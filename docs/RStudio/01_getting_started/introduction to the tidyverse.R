
#load data
library(tidyverse)
diamonds2 <- readRDS("01_getting_started/diamonds2.rds")

# shorten data (make longer)
diamonds2 %>% head(n = 5)

diamonds2_long <-diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)

model <- lm(price ~ ., data = diamonds2_long)
model

# widen data
diamonds3 <- readRDS("01_getting_started/diamonds3.rds")
diamonds3
diamonds3 %>% head(n = 5)
diamonds3_short <- diamonds3 %>% 
  pivot_wider(names_from = "dimension",
              values_from = "measurement") %>%
  head(n=5)
diamonds3_short

#seperate data
diamonds4 <- readRDS("01_getting_started/diamonds4.rds")
diamonds4
diamonds4_sep <- diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)
diamonds4_sep

# unite data
diamonds5 <- readRDS("01_getting_started/diamonds5.rds")
diamonds5_unit <- diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')
diamonds5_unit
# -----------------------------------------------------------------------------
# transform data
library(ggplot2) # To load the diamonds dataset
library(dplyr)

# filter data
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)

#arrange data
diamonds %>% 
  arrange(cut, carat, desc(price))

#select data
diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)

# exclusive select
diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

# select helpers
diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

# rename colums
diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

# mutate (recode) variables
diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

# transmutate (recode and delete old) variables
diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

# group and summarize
diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

# glimpse at data set (fill as much of thedata set as fits the screen)
glimpse(diamonds)
# -----------------------------------------------------------------------------
# date data formats in R
library(lubridate)
ymd(20101215)
## "2010-12-15"
mdy("4/1/17")
## "2017-04-01"

bday <- dmy("14/10/1979")
month(bday)
## 10

year(bday)
## 1979
