## gapminder-analysis.R
## analysis with gapminder data
## Iwen Su isu@bren.ucsb.edu
## http://ohi-science.org/data-science-training/programming.html

## Load libraries 
library(tidyverse)


## Read in gapminder data
gapminder <- readr::read_csv('https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv')


## Create a new directory
dir.create("figures")


## create a list of countries
country_list <- unique(gapminder$country)


## create for loop for list of countries
## for each item in `country_list`, name it cntry and run the code within the bra
for(cntry in country_list){

## filter the country to plot
gap_to_plot <- gapminder %>% 
  filter(country == cntry)


## plot
my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdpPercap)) +
  geom_point() +
  labs(title = cntry)


## expand on plot title using paste()
my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdpPercap)) +
  geom_point() +
  labs(title = paste(cntry, "GDP per capita", sep =" "))


## save the plot
ggsave(filename = paste("figures/",cntry,"_gdpPercap.png", sep = ""), plot = my_plot)

}

