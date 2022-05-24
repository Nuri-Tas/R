library('dplyr')
library('ggplot2')

''' we will work on mpg dataset which is the fuel economy data for 38 popular cars
from 1999 to 2008 '''

## filter
mpg

# list the columns in mpg
names(mpg)

# display the structure of mpg
str(mpg)

# show the histogram for city miles per galon
hist(mpg$cty)

""" we could see that the most of the cty falls into between 10 and 25. Now let's filter
      out the cty values that is less than 10 and more than 25. """
mpg_cty <- filter(mpg, cty >= 10 & cty <= 25)
View(mpg_cty)

# display chevrolet cars whose cty is less than 20
filter(mpg, manufacturer == 'chevrolet' & cty < 20)

## select
# show all the manufacturers in the dataset
distinct(select(mpg, manufacturer))

# display the model and year columns
select(mpg, c('model', 'year'))
# or
select(mpg, c(2, 4))

## pipe
# display models whose cty is greater than 25 
mpg %>% 
  filter(cty > 25) %>%
  select(model)

# find manufacturers whose drv is 4
mpg %>%
  filter(drv == 4) %>%
 select(manufacturer) %>%
  distinct

# what model of car gets the worst gas mileage (under 15 mpg) ?
mpg %>%
  filter(cty == min(cty)) %>%
  select(model) %>%
  distinct

## group by
# how many cars are there in each drive train?
mpg %>%
  group_by(drv) %>%
  summarise(n = n())

# what is the average and sd value of cty for each model?
# display it with the total number of cars in the regarding model
mpg %>%
  group_by(model) %>%
  summarise(mean = mean(cty, na.rm = T), std = sd(cty, na.rm = T), n = n())


## mutate
# find the product of average city and the total number of cars in each model
mpg %>%
  group_by(model) %>%
  summarise(n = n(), avg_cty = mean(cty, ra.nm = T)) %>%
  mutate(total = n * avg_cty)

# multiply displ column by 0.16 and sort the table by cty in descending order
mpg %>%
  mutate(conversion = 0.16 * displ) %>%
  arrange(desc(cty))

# show the plot of the cty values per models 
ggplot(mpg, aes(x=cty, y=model)) + geom_point()
