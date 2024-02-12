# install.packages("monochromeR") # a package for creating monochrome color palettes and easily converting rgba values to hex codes (and also some other useful functions)
# install.packages("showtext") # for using fonts more easily in R graphs
# install.packages("ggtext") # improved text rendering support for ggplot2
# install.packages("ggrepel") # ggplot2 extension to repel overlapping labels
# install.packages("googlesheets4") # provides an R interface to Google Sheets via the Sheets API v4

library(tidyverse)
library(palmerpenguins)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() + 
  labs(title = "This title is serif font",
       subtitle = "This subtitle is mono font",
       x = "This axis label is sans font (default)",
       y = "This axis is also sans font (default)") +
  theme(
    plot.title = element_text(family = "serif", size = 30),
    plot.subtitle = element_text(family = "mono", size = 25),
    axis.title = element_text(family = "sans", size = 22),
    axis.text.x = element_text(family = "serif", face = "bold", size = 18),
    axis.text.y = element_text(family = "mono", face = "italic", size = 18)
  )

# Screen device --> operating systems handle text differently
# File device --> write our plot to a file (ef. pdf, png, jpeg)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(monochromeR)
library(showtext)
library(ggtext)
library(ggrepel)
library(googlesheets4)

# import google fonts 
font_add_google(name = "Josefin Sans",
                family = "josefin") # name we provide ggplot

font_add_google(name = "Sen",
                family = "sen") # name we provide ggplot

# enable show text here that configures font across platforms
showtext_auto()


#..........................import data...........................
jobs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                wrangle data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

jobs_clean <- jobs |>
  
  # add cols (needed for dumbbell plot) ----
mutate(percent_male = 100 - percent_female, # % of females within each industry was already included
       difference_earnings = total_earnings_male - total_earnings_female) |>  # diff in earnings between M & F
  
  # rearrange columns ----
relocate(year, major_category, minor_category, occupation,
         total_workers, workers_male, workers_female,
         percent_male, percent_female,
         total_earnings, total_earnings_male, total_earnings_female, difference_earnings,
         wage_percent_of_male) |>
  
  # drop rows with missing earning data ----
drop_na(total_earnings_male, total_earnings_female) |>
  
  # make occupation a factor ----
mutate(occupation = as.factor(occupation)) |>
  
  # ---- this next step is for creating our dumbbell plots ----

# classify jobs by percentage male or female ----
mutate(group_label = case_when(
  percent_female >= 75 ~ "Occupations that are 75%+ female",
  percent_female >= 45 & percent_female <= 55 ~ "Occupations that are 45-55% female",
  percent_male >= 75 ~ "Occupations that are 75%+ male"
))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              create subset df                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....guarantee the same random samples each time we run code.....
set.seed(0)

#.........get 10 random jobs that are 75%+ female (2016).........
f75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ female") |>
  slice_sample(n = 10)

#..........get 10 random jobs that are 75%+ male (2016)..........
m75 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 75%+ male") |>
  slice_sample(n = 10)

#........get 10 random jobs that are 45-55%+ female (2016).......
f50 <- jobs_clean |>
  filter(year == 2016, group_label == "Occupations that are 45-55% female") |>
  slice_sample(n = 10)

#.......combine dfs & relevel factors (for plotting order).......
subset_jobs <- rbind(f75, m75, f50) |>
  mutate(group_label = fct_relevel(group_label, 
                                   "Occupations that are 75%+ female",
                                   "Occupations that are 45-55% female", 
                                   "Occupations that are 75%+ male"))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Initial plot                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot <- ggplot(subset_jobs) +
  geom_segment(aes(x = total_earnings_female, xend = total_earnings_male,
                   y = fct_reorder(occupation, total_earnings), yend = occupation)) +
  geom_point(aes(x = total_earnings_male, y = occupation),
             color = "#CD93D8", size = 3.25) +
  geom_point(aes(x = total_earnings_female, y = occupation),
             color = "#6A1E99", size = 3.25) +
  facet_wrap(~group_label, nrow = 3, scales = "free_y") +
  scale_x_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k"),
                     breaks = c(25000, 50000, 75000, 100000, 125000))

plot

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Creating Palette                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Make hex codes to reference by name 
earnings_pal <- c("males" = "#2d7787",
                  "females" = "#fc6b4b",
                  dark_text = "#0c1509",
                  light_text = "#4e514d")

# create preview of palette
monochromeR::view_palette(earnings_pal)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Updated Plot                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot <- ggplot(subset_jobs) +
  geom_segment(aes(x = total_earnings_female, xend = total_earnings_male,
                   y = fct_reorder(occupation, total_earnings), yend = occupation)) +
  geom_point(aes(x = total_earnings_male, y = occupation),
             color = earnings_pal["males"], size = 3.25) +
  geom_point(aes(x = total_earnings_female, y = occupation),
             color = earnings_pal["females"], size = 3.25) +
  facet_wrap(~group_label, nrow = 3, scales = "free_y") +
  scale_x_continuous(labels = scales::label_dollar(scale = 0.001, suffix = "k"),
                     breaks = c(25000, 50000, 75000, 100000, 125000))

plot +
  labs(title = "Males earn more than females across most occupations",
       # use / to close out 
       subtitle = "Median earnings of full-time <span style = 'color: #2d7787; font-size: 20pt;'>**male**</span> vs <span style = 'color: #fc6b4b; font-size: 20pt;'>**female**</span> workers by occupation in 2016.",
       caption = "Data Source: TidyTuesday (March 5, 2019)") +
  theme_minimal() +
  # editing within theme()
  theme(
    
    plot.title.position = "plot",
    plot.title = element_text(face = "bold",
                              family = "josefin", # change font
                              size = 25,
                              color = earnings_pal["dark_text"]),
    plot.subtitle = ggtext::element_textbox(size = 17,
                                 family = "sen",
                                 color = earnings_pal["light_text"],
                                 # give breathing room to text
                                 margin = margin(0.5, 0, 1, 0,
                                                 unit = "lines")),
    plot.caption = element_text(family = "sen",
                                color = earnings_pal["light_text"],
                                margin = margin(4, 0, 0, 0,
                                                unit = "lines")),
    # update the facet-wrap header text
    strip.text.x = element_text(face = "bold",
                                family = "josefin",
                                size = 12,
                                hjust = 0),
    panel.spacing.y = unit(1, "lines"),
    axis.text = element_text(color = earnings_pal["light_text"],
                             family = "sen"),
    axis.text.x = element_text(size = 10),
    axis.title = element_blank()
    
  )


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.........................load libraries.........................
library(tidyverse)

#..........................read in data..........................

# read in Google Sheet ----
lobs <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DkDVcl_9rlaqznHfa_v1V1jtZqcuL75Q6wvAHpnCHuk/edit#gid=2143433533") |>
  mutate(temp = as.factor(temp))

1# alternatively, read in csv file ----
lobs <- read_csv(here::here("week6", "data", "metabolism-foraging-data.csv")) |>
  mutate(temp = as.factor(temp))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            create lobster plot                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................create theme..........................
lob_theme <- function(){
  theme_light() +
    theme(
      axis.title.x = ggtext::element_markdown(size = 13,
                                              margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "lines")),
      axis.title.y = ggtext::element_markdown(size = 13,
                                              margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "lines")),
      axis.text = element_text(color = "black", size = 12),
      panel.border = element_rect(colour = "black", size = 0.7),
      panel.grid = element_blank(),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.position = c(0.95, 0.95),
      legend.justification = c(0.95, 0.95),
      legend.box.background = element_rect(color = "black", size = 1.1)
      
    )
}

#..........................create scales.........................
lob_palette <- c("11" = "#7B8698",
                 "16" = "#BAD7E5",
                 "21" = "#DC7E7C",
                 "26" = "#7D3E40")

lob_shapes <-  c("11" = 15,
                 "16" = 16,
                 "21" = 17,
                 "26" = 18)

lob_sizes <- c("11" = 6,
               "16" = 6,
               "21" = 6,
               "26" = 7)

#........................create plot text........................
x_axis_lab <- glue::glue("Resting Metabolic Rate<br>
                         (mg O<sub>2</sub> kg<sup>-1</sup> min<sup>-1</sup>)")

y_axis_lab <- glue::glue("Maximum Consumption Rate<br>
                         (prey consumed predator<sup>-1</sup> 24hr<sup>-1</sup>)")

#............................plot data...........................
lob_plot <- ggplot(lobs, aes(x = SMR, y = avg_eaten,
                             color = temp, shape = temp, size = temp)) +
  geom_point() +
  scale_color_manual(values = lob_palette, name = "Temperature (ºC)") +
  scale_shape_manual(values = lob_shapes, name = "Temperature (ºC)") +
  scale_size_manual(values = lob_sizes, name = "Temperature (ºC)") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x = x_axis_lab,
       y = y_axis_lab) +
  lob_theme()

lob_plot

#..........................add text with geoms.........................
lob_plot +
  geom_text(
    # give coordinate 
    x = 0.1, y = 25,
    label = "Important lobsters",
    size = 4,
    color = "black",
    hjust = "inward"
  ) +
  geom_rect(
    xmin = 0.25, xmax = 0.85,
    ymin = 8.5, ymax = 18,
    alpha = 0.5,
    fill = "gray40",
    color = "black",
    show.legend = F
  )

# However, it is inheritiing the aesthetics of the geoms and it looks bad. Try annotate() instead
#..........................add text with annotate.........................
lob_plot +
  annotate(
    geom = "text",
    # give coordinate 
    x = 0.1, y = 25,
    label = "Important lobsters",
    size = 4,
    color = "black",
    hjust = "inward"
  ) +
  annotate(
    geom = "rect",
    xmin = 0.25, xmax = 0.85,
    ymin = 8.5, ymax = 18,
    alpha = 0.5,
    fill = "gray40",
    color = "black"
  ) +
  annotate(
    geom = "curve",
    x = 0.3, xend = 0.5,
    y = 23.8, yend = 19,
    curvature = -0.13,
    arrow = arrow(length = unit(0.3, "cm")) # add arrow at the end of the curve line
  )
