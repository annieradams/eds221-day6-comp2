# ----- Section 1 : Filter-----#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

# To look for an exact match, ==

view(penguins)
penguins_biscoe<- penguins %>% filter(island == "Biscoe")

# look for exact match for values
penguins_2007 <- penguins %>%  filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie" & island == "Torgersen")
# or 
adelie_torgersen_2 <- penguins %>% filter(species == "Adelie" , island == "Torgersen")

# create subset from penguins that only contains gentoo penguins observed in 2008
gentoo_2008 <- penguins %>% filter(species == "Gentoo" & year == 2008)


# how to include multiple species
gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

# Create a subset that contains observations where island is dream OR the year is 2009
dream_2009<- penguins %>% filter(island== "Dream" | year == 2009)

ggplot(data = pie_crab, aes(x = water_temp, y = size))+ 
  geom_point()

# Keep observations for sites NIB, ZI,DB, JC   
# pie_crab %>% filter( site == "NIB"| site == "ZI"| site == "DB" | site == "JC")
# we can use the %in% operator to ask: does the value in our column match ANY of the values in this vector

pie_sites <- pie_crab %>% filter( site %in% c("NIB","ZI","DB","JC"))

sites <- c("CC","BB","PIE")

pie_sites_2<- pie_crab %>%  filter(site %in% sites)

# create a subset using the %in% operator that includes sites PIE, ZI, NIB, BB, and CC
sites_2 <- c("CC","BB","PIE","ZI", "NIB")
pie_sites2 <- pie_crab %>% filter( site %in% c("NIB","ZI","BB","CC", "PIE"))

# excluding filter statements

# != ( asks is this NOT equal to that value)
exclude_zi <- pie_crab %>% filter(site!= "ZI")


#what if i want to exclude sites BB CC PIE

exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB","CC","PIE"))

#create a subset from pie crab that only contains obs from sites NIB CC and ZI, for crabs with carapace size exceeding 13

pie_subset<- pie_crab %>% filter(site%in% c("NIB","CC","ZI"), size>13)
# select certain columns
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)
#select a range of columns using a single colon
crabs_subset2 <- pie_crab %>% select(site:air_temp)

# select a range and an individual column
crab_subset3 <- pie_crab %>% select(date:water_temp)

#can re order things with select
pie_crab %>% select(name, water_temp, size)


------# Mutate-----
# explicitly use dplyr mutate to add or update a column while keeping all existing columns

crab_cm <- pie_crab %>% mutate(size_cm = size /10)

## what happens if i use mutate to add a new column containing the mean of of the size column

crab_mean_size <- pie_crab %>% mutate(mean_size = mean(size, na.rm= TRUE))


## this is a warning it writes over the previous column called "name"
crabs_awesome <- pie_crab %>% mutate(name = "Teddy is awesome")


# group by site and then find mean crab aize
mean_size_by_site <- pie_crab %>% group_by(site) %>% 
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))
# group by then mutate
group_mutate <- pie_crab %>% 
  group_by(site) %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))


# create a new variable but what it contains is conditional based on previous data
# create new column in pie chart that contains "giant" if the size is greater than 35, or "not giant" if the size is less than or equal to20
#use dplyr::case_when() to write if else statememts more easily

crabs_bin <- pie_crab %>% 
  mutate(sie_bin = case_when (
    size>20 ~"Giant",
    size<=20 ~"Not giant"
  ))


sites_binned <- pie_crab %>%  
  mutate(region = case_when(
    site %in% c("ZI","CC","PIE")~ "Low",
    site %in% c("BB","NIB")~ " Middle",
    TRUE ~ "High"
  ))
