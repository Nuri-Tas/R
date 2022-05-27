library('ggplot2')
library('dplyr')

?msleep
# msleep is the sleep dataset of the mammals having 11 columns including genus, sleep_rem and sleep cycle

## Exploratory Data Analysis
# What is the number of rows and columns of the dataset ?
data <- msleep
dim(data)

# How many unique names are there in data?
unique(data$name) # There are 83 ones

# Find the number of null values in the vore column
sum(is.na(data$vore))

# What is the number of names per vore?
data %>%
  group_by(vore) %>%
  summarise(n = n())

# Display the average total sleep per vore in decreasing order
data %>% 
  group_by(vore) %>%
  summarise(avg_vore = mean(sleep_total, na.rm = T)) %>%
  arrange(desc(avg_vore))
  
# What are the animals that has lowest and highest sleep rem?
data %>%
  filter(sleep_rem == min(sleep_rem, na.rm = T) | sleep_rem == max(sleep_rem, na.rm = T)) %>%
  select(name, sleep_rem) 
# pilot whale has sleep rem of 0.1 while this value for thick-tailed opposum is 6.6

# Find the average brain weight and body weight per vore
data %>%
  group_by(vore) %>%
  summarise_at(c("brainwt", "bodywt"), mean, na.rm = TRUE)

# Find the percentages of sleep rem to total sleep per animal and sort them in descending order
data %>%
  mutate(rem_to_total = sleep_rem / sleep_total * 100, ra.nm = T) %>%
  select(name, rem_to_total) %>%
  arrange(desc(rem_to_total))

# What are the animals whose rem sleep to total sleep percentage is greater than 15%?
data %>%
  mutate(rem_to_total = sleep_rem / sleep_total * 100, ra.nm = T) %>%
  filter(rem_to_total > 15) %>%
  select(name, rem_to_total)

# How many animals sleep more than the average sleep total?
data %>%
  mutate(sleep_over_mean = sleep_total - mean(sleep_total, na.rm = T)) %>%
  filter(sleep_over_mean > 0) %>%
  count()
# It's 38

# Find the average of total sleep and rem sleep per animal
data %>%
  rowwise() %>%
  mutate(avg_rem_total = mean(c(sleep_total, sleep_rem), na.rm = T)) %>%
  select(name, avg_rem_total)

# Add a column that specifies whether an animal's rem sleep is lower or greater than 1
data %>%
  mutate(rem_sleep_under_1 = ifelse(sleep_rem < 1, 'lower', 'greater'))

# Turn all words in data into upper case
data %>%
  mutate_all(toupper)

# add a '/n' symbol to every word in data
data %>%
  mutate_all(~paste(.,'/n'))

# Let's now remo '/n' symbols and remove white spacecs with stringr  

library('stringr')
data %>%
  mutate_all(~str_replace_all(., '/n', '')) %>%
  mutate_all(str_trim)
