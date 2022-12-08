library("tidyverse")
library("dplyr")
library("ggplot2")
library("plotly")
library("maps")
library("mapproj")
library("patchwork")
library("leaflet")
library(usmap)
library(usdata)


df <- read.csv("/Users/Owner/Desktop/info201/data/incarceration-trends/incarceration_trends.csv", nrows=-1)  

# The functions might be useful for A4
source("/Users/Owner/Desktop/info201/assignments/a4-fy0403/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}
# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#Count black proportion in jails in 2007
black_proportion_2007 <- df %>%
  filter(year == 2007,na.rm = TRUE) %>%
  select(state, black_jail_pop, total_jail_pop) %>%
  group_by(state) %>%
  summarise(black_pop = sum(black_jail_pop, na.rm = TRUE),
              total_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(black_proportion = black_pop / total_pop * 100)%>%
  mutate_if(is.numeric, round)

#Count white proportion in jails in 2007
white_proportion_2007 <- df %>%
  filter(year == 2007,na.rm = TRUE) %>%
  select(state, white_jail_pop, total_jail_pop) %>%
  group_by(state) %>%
  summarise(white_pop = sum(white_jail_pop, na.rm = TRUE),
            total_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  mutate(white_proportion = white_pop / total_pop * 100)%>%
  mutate_if(is.numeric, round)

#Look for the state with highest proportion of black people in jails
highest_black_proportion_2007 <- black_proportion_2007 %>%
  filter(black_proportion == max(black_proportion, na.rm = TRUE)) %>%
  pull(state)

#Look for the state with highest proportion of white people in jails
highest_white_proportion_2007 <- white_proportion_2007 %>%
  filter(white_proportion == max(white_proportion, na.rm = TRUE)) %>%
  pull(state)

#----------------------------------------------------------------------------#


## Section 3  ---- 
#----------------------------------------------------------------------------#
# This function returns a data frame of jail population per year
get_year_jail_pop <- function() {
  year_jail_pop <- df %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(pop = sum(total_jail_pop, na.rm = TRUE))
  return(year_jail_pop)   
}

# This function returns a chart
plot_jail_pop_for_us <- function() {
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = pop)) +
    labs(
      x = "Year", 
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Figure 1: US Jail Pop."
    )
  return(plot)
}
#----------------------------------------------------------------------------#



## Section 4  ---- 
#----------------------------------------------------------------------------#
# This function shows the dataframe of jail population by states in year
get_jail_pop_by_states <- function(states) {
  df2 <- df %>% 
    select(year, state, total_jail_pop) %>% 
    group_by(year, state) %>% 
    filter(state %in% states) %>% 
    summarise(state_total_pop = sum(total_jail_pop, na.rm = TRUE), .groups = "Maintain")
  return(df2)
}


# This function shows the chart of jail population by states in year
plot_jail_pop_by_states <- function(states){
  plot2 <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = state_jail_pop, color = state,group = state)) +
    labs(
      x = "Year", 
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Figure 2. Increase of Jail Population in U.S. (1970-2018)"
    )
  return(plot2)
}


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
#Make a dataframe which shows proportion of different races in 2007.
get_racial_by_states_2007 <- function(){
  racial <- filter(df, year == 2007) %>%
    select(state, total_jail_pop, black_jail_pop,white_jail_pop, aapi_jail_pop,latinx_jail_pop) %>%
    group_by(state) %>%
    summarise(white_pop = sum(white_jail_pop, na.rm = TRUE),
              black_pop = sum(black_jail_pop, na.rm = TRUE),
              asian_pop = sum(aapi_jail_pop, na.rm = TRUE),
              latinx_pop = sum(latinx_jail_pop, na.rm = TRUE),
              total_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    mutate(white_jail_proportion = white_pop / total_pop * 100,
           black_jail_proportion = black_pop / total_pop * 100,
           asian_jail_proportion = asian_pop / total_pop * 100,
           latinx_jail_proportion = latinx_pop / total_pop * 100)%>%
    drop_na()
  return(racial)
}

#The chart shows the proportion of black people in the jail in different states in 2007
plot_black_by_states_2007 <- function(){
  plot3 <- ggplot(data = get_racial_by_states_2007()) +
    geom_col(mapping = aes(x = state, y = black_jail_proportion),
             fill = "#000099",
             colour = "red") +
    labs(
      x = "State",
      y = "Black Proportion",
      title = "Proportion of black people in Jail in 2007, classified by State",
      caption = "Figure 3. Proportion of black people in Jail in 2007, classified by State"
    )
  return(plot3)
}
plot_black_by_states_2007()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe used for visualization about black people proportion in jails in 2007
get_black_proportion_jail_2007 <- function() {
  df3 <- df %>%
    filter(year == 2007) %>%
    group_by(state) %>%
    summarize(black_jail_pop_prop = round(sum(black_jail_pop/total_jail_pop, na.rm = TRUE))) %>%
    rename(abbr = state)
  df3 <- merge(df3, statepop, by = "abbr")
  return(df3)
}


# This function returns a map about black people proportion in jails in 2007 in different locations
map_black_proportion_jail_2007 <- function() {
  map <- plot_usmap(data = get_black_jail_pop_2018(), values = "black_jail_pop_prop", color = "black") + 
    scale_fill_continuous(
      name = "Black Proportion in Jail (2007)",
      low = "white",
      high = "red",
      label = scales::comma) + theme(legend.position = "right") +
    labs(
      title = "Black People proportion in Jail in U.S. (2007)",
      caption = "Figure 4. Black People Proportion in Jail in U.S. (2007)"
    )
  return(map)
}

## Load data frame ---- 


