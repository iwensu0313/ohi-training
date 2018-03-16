## gapminder-analysis.R
## analysis with gapminder data
## Iwen Su isu@bren.ucsb.edu
## http://ohi-science.org/data-science-training/programming.html

## Load libraries 
library(tidyverse)

## Read in gapminder data
gapminder <- readr::read_csv('https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv')

## Sidenote: use `source("filename.R")` on an R script to run the entire script in that .R file


#################################################
## for loop
#################################################

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



## Exercise!

## Create subfolder
dir.create("figures/europe")

## Create cumulative mean column 
new_gapminder <- gapminder %>% 
  filter(continent == "Europe") %>% 
  mutate(cumMeanGdpPerCap = cummean(gdpPercap))

## Create list of countries in europe
#europe_cntry <- new_gapminder %>% 
#  select(country) %>% 
#  unique() 
## convert tibble to single vector
#europe_cntry <- europe_cntry$country
europe_cntry <- unique(new_gapminder$country)


## create for loop for list of countries in europe
## for each item in `europe`, name it cntry and run the code within the brackets{}
for(cntry in europe_cntry){
  
  ## filter the country to plot
  gap_to_plot <- new_gapminder %>% 
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = cumMeanGdpPerCap)) +
    geom_point() +
    labs(title = cntry)
  
  ## expand on plot title using paste()
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdpPercap)) +
    geom_point() +
    labs(title = paste(cntry, "GDP per capita", sep =" "))
  
  ## save the plot
  ggsave(filename = paste("figures/europe/",cntry,"_gdpPercap.png", sep = ""), plot = my_plot)
  
}



#################################################
## Part A: Conditional Statements with `if` and `else`
#################################################

## Read in data frame
est <- readr::read_csv('https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/countries_estimated.csv')

## Left join gapminder dataset with est dataset
gapminder_est <- left_join(gapminder, est, by = "country") # added 'by = 'country''

## create a list of countries using the joined dataset
gap_europe <- gapminder_est %>% 
  filter(continent == "Europe") %>% 
  mutate(gdpPerCap_cummean = cummean(gdpPercap)) 

europe_cntry <- unique(gap_europe$country)


for(cntry in europe_cntry){
  
  ## filter the country to plot
  gap_to_plot <- gap_europe %>% 
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdpPercap)) +
    geom_point() +
    labs(title = paste(cntry, "GDP per capita", sep =" "))
  
  ## If estimated, add that as a subtitle!
  if(any(gap_to_plot$estimated == "yes")){ # any checks all values and returns TRUE/FALSE
    
    print(paste(cntry, "data are estimated", sep = " "))
    
    my_plot <- my_plot +
      labs(subtitle = "Estimated data")
    
  }
  
  ## save the plot
  ggsave(filename = paste("figures/europe/",cntry,"_gdpPercap.png", sep = ""), plot = my_plot)
  
}



#################################################
## Part B: Conditional Statements with `if` and `else`
#################################################

## Use `if (){} else if (){}`, if you have more than two conditions to test for
## you can just use `if (){} else {}` if there are only two conditions (e.g. estimated == "yes", estimated == "no")
## You can just use `if(){} if you just want to test and do an action for just one condition`

## Using tidied data frames from Part A:
for(cntry in europe_cntry){
  
  ## filter the country to plot
  gap_to_plot <- gap_europe %>% 
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdpPercap)) +
    geom_point() +
    labs(title = paste(cntry, "GDP per capita", sep =" "))
  
  ## If estimated, add that as a subtitle!
  if (any(gap_to_plot$estimated == "yes")){ # any checks all values and returns TRUE/FALSE
    
    print(paste(cntry, "data are estimated", sep = " "))
    
    my_plot <- my_plot +
      labs(subtitle = "Estimated data")
    
  } else if (any(gap_to_plot$estimated == "no")) {
    
    print(paste(cntry, "data are reported"))
    
    my_plot <- my_plot +
      labs(subtitle = "Reported data")
    
  }
  
  ## save the plot
  ggsave(filename = paste("figures/europe/",cntry,"_gdpPercap.png", sep = ""), plot = my_plot)
  
}



#################################################
## Miscellaneous
#################################################

## Some cool packages
library(readr)
library(readxl)
library(stringr)
library(lubridate)

## To install packages from github:
# devtools::install.github("package_name")

## When organizing, good to have a figure folder, intermediate folder, final outputs folder