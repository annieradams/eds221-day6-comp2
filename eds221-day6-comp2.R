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


