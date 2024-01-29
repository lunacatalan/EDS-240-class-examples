#install.packages("ggalt") # extra coordinate systems, geoms, statistical transformations, scales and fonts for 'ggplot2' (includes `geom_lollipop()`)
#install.packages("ggExtra") # {ggplot2} extension2222 which can be used to add marginal histograms/boxplots/density plots to ggplot2 scatterplots
#install.packages("ggdensity") # {ggplot2} extension providing more interpretable visualizations of density estimates based on highest density regions (HDRs)

# -----------------------------------------------------------------------------------------------------
#                                               Load libraries ----
# -----------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(tidycensus)
library(tidycensus)
library(gghighlight)

# -----------------------------------------------------------------------------------------------------
#                                              Read in LD data ----
# -----------------------------------------------------------------------------------------------------

# load API key
source(here::here("week3", "KEYS.R"))
census_api_key(censusKEY)

lyme <- read_csv(here::here("week3", "data", "LD-Case-Counts-by-County-01-20.csv"))

# lyme data ----
lyme_clean <- lyme %>% 
  clean_names() %>% 
  rename(county = ctyname,
         state = stname,
         status = ststatus) %>% 
  pivot_longer(cols = 6:25,
               names_to = "city_year",
               values_to = "reported_cases") %>% 
  mutate(year = as.factor(str_remove(city_year, "cases")))

# calculate the total cases by state
lyme_by_state <- lyme_clean %>% 
  group_by(year, state) %>% 
  summarize(total_cases = sum(reported_cases))

us_state_pop <- get_estimates(geography = "state",
                              product = "population",
                              year = 2019) %>% 
  filter(variable == "POP") %>% 
  select(state = NAME, population = value)

lyme_pop <- left_join(lyme_by_state, us_state_pop) %>% 
  mutate(pop100k = population/100000) %>% 
  mutate(cases_per100k = total_cases/pop100k)

# -----------------------------------------------------------------------------------------------------
#                                              Plotting ----
# -----------------------------------------------------------------------------------------------------

# Line Plots 
#     - Encoding based on position 
#     - Consider the range based on what is a meaningful change in your data  

# line plot 
lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>% # select only these years 
  ggplot(aes(x = year, y = cases_per100k,
             group = state)) +
  geom_line() +
  gghighlight::gghighlight(state == "New Jersey")


# highlight based on conditional
lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>% # select only these years 
  ggplot(aes(x = year, y = cases_per100k,
             group = state, color = state)) +
  geom_line() +
  gghighlight::gghighlight(max(cases_per100k) > 100) # conditional statement

# -----------------------------------------------------------------------------------------------------
#                                             Aspect Ratio ----
# -----------------------------------------------------------------------------------------------------

# Aspect ratio affects perception of SLOPE

lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>% # select only these years 
  filter(state == "Vermont") %>% 
  ggplot(aes(x = year, y = cases_per100k, group = state)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 190, # breaks on the y axis
                                  by = 1)) + # create break every single unit
  coord_fixed(ratio = 1/20) # makes equal sized grid


lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>% # select only these years 
  filter(state == "Vermont") %>% 
  ggplot(aes(x = year, y = cases_per100k, group = state)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 190, # breaks on the y axis
                                  by = 19)) # create break every single unit


# -----------------------------------------------------------------------------------------------------
#                                             Area Plots ----
# -----------------------------------------------------------------------------------------------------

# Area chart --- 1 Group 
#     - Cons: line graph with everything filled in underneath it 
#     - the only thing that matter is the top line; not the area filled in 

lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>% # select only these years 
  filter(state == "Wisconsin") %>% 
  ggplot(aes(x = year, y = cases_per100k, group = state, fill = state)) +
  geom_area() +
  scale_fill_manual(values = "midnightblue") +
  scale_x_discrete(expand = c(0,0)) + # expand to end of the x-axis
  scale_y_continuous(expand = c(0,0)) + # expand to end of the y-axis
  theme(legend.position = "none")


# Stacked Area chart --- multiple groups
#     - Cons:tricky to understand

lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>%
  filter(state %in% c("New York", "Wisconsin", "Pennsylvania", "Vermont")) %>%
  ggplot(aes(x = year, y = cases_per100k, group = state, fill = state)) +
  geom_area()        

# Filled Area chart --- multiple groups
#     - Whole area is out of 100%, and shows the contributions by state to the trends

lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>%
  filter(state %in% c("New York", "Wisconsin", "Pennsylvania", "Vermont")) %>%
  ggplot(aes(x = year, y = cases_per100k, group = state, fill = state)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) # to make scale 0-100%

# Comapring to line chart
lyme_pop %>% 
  filter(year %in% c(2010:2020)) %>%
  filter(state %in% c("New York", "Wisconsin", "Pennsylvania", "Vermont")) %>%
  ggplot(aes(x = year, y = cases_per100k, group = state, color = state)) +
  geom_line() +
  facet_wrap(~state)

