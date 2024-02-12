# install.packages("paletteer") # a comprehensive collection of color palettes in R using a common interface
# install.packages("viridis") # Colorblind-Friendly Color Maps for R
# install.packages("RColorBrewer") # ColorBrewer Palettes
# install.packages("tigris") # for downloading and using Census TIGER/Line shapefiles in R

library(paletteer)
library(viridis)
library(RColorBrewer)
library(tigris)
library(palmerpenguins)
library(tidyverse)

cat_color_plot <- ggplot(penguins, aes(x = bill_length_mm, 
                                       y = bill_depth_mm, color = species, 
                                       shape = species)) +
  geom_point(size = 4, alpha = 0.8)

cat_color_plot 


cont_color_plot <- ggplot(penguins, aes(x = bill_length_mm, 
                                        y = bill_depth_mm, 
                                        color = body_mass_g)) +
  geom_point(size = 4, alpha = 0.8) 

cont_color_plot 

# Viridis pallete
cat_color_plot +
  scale_color_viridis_d(option = "viridis", direction = -1) 

cont_color_plot +
  scale_color_viridis_c(option = "magma")

# Colors 
RColorBrewer::display.brewer.all()

RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)

RColorBrewer::display.brewer.pal(n = 4, name = 'Dark2')

RColorBrewer::brewer.pal(n = 4, name = 'Dark2')

cat_color_plot +
  scale_color_brewer(palette = "Dark2") 

cont_color_plot +
  scale_color_distiller(palette = "BuPu")

cont_color_plot +
  scale_color_fermenter(palette = "YlGnBu")

# OUtline points
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, fill = body_mass_g)) +
  geom_point(shape = 21, size = 4, alpha = 0.8) +
  scale_fill_distiller(palette = "BuPu")

# Palleteer

cat_color_plot +
  paletteer::scale_color_paletteer_d("calecopal::superbloom3")

# Saving Palette outside of plot 
my_palette <- c("#32DE8A", "#E36414", "#0F4C5C")

cat_color_plot +
  scale_color_manual(values = my_palette)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = my_palette)

penguins |> 
  filter(species != "Chinstrap") |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = my_palette)

my_palette_named <- c("Adelie" = "#32DE8A","Chinstrap" = "#E36414", "Gentoo" = "#0F4C5C")

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = my_palette_named)

penguins |> 
  filter(species != "Chinstrap") |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = my_palette_named)


penguins |> 
  mutate(
    my_color = case_when(
      bill_length_mm < 40 ~ "#D7263D",
      between(bill_length_mm, 40, 50) ~ "#E4BB97",
      bill_length_mm > 50 ~ "#386150"
    )
  ) |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = my_color)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_identity()

# highlight extreme body mass values 
penguins |> 
  mutate(
    my_color = case_when(
      body_mass_g > 6000 ~ "#D7263D",
      TRUE ~ "gray50"
    )
  ) |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = my_color)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_identity(guide = "legend", #specify a legend
                       name = "Body mass (g)", labels = c(">6000", "<= 6000"))

# Simple features in R: Allows different types fo software to specify spatial data in a common way 
#   - Geometry
#   - Attribute data associated with geometries 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    setup                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................
library(tidyverse)
library(tigris)

#.........................get shape data.........................
county_geo <- tigris::counties(class = "sf", 
                               cb = TRUE) |> # cb = TRUE to use cartographic boundary files
  
  # shift US to fit AK, HI, PR (we'll be filtering these out though) and transform CRS to USA Contiguous Albers Equal Area Conic (ESRI:102003) ----
shift_geometry()

#....................import precipitation data...................
precip_data <- read_csv(here::here("week5", "data", "county-jan19-dec23-precip.csv"), 
                        skip = 4)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               data wrangling                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ wrangle geometries  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

county_geo_wrangled <- county_geo |>
  
  # clean up col names ----
janitor::clean_names() |>
  
  # rename county & state cols ----
rename(county = namelsad, state = state_name) |>
  
  # remove states / territories that we don't have precip data for ----
filter(!state %in% c("Alaska", "Hawaii", "District of Columbia",
                     "United States Virgin Islands", "Puerto Rico", "American Samoa",
                     "Commonwealth of the Northern Mariana Islands", "Guam")) |>
  
  # capitalize "city" (VA) ----
mutate(county = str_replace(string = county, pattern = " city", replacement = " City"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ wrangle precipitation data  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

precip_wrangled <- precip_data |>
  
  # clean up col names ----
janitor::clean_names() |>
  
  # rename county col ----
rename(county = name) |>
  
  # filter out DC ----
filter(!county %in% c("Washington, D.C.")) |>
  
  # update name to match that in county_geo df ----
mutate(county = str_replace(string = county, pattern = "Dona Ana County", replacement = "Doña Ana County")) |>
  
  # coerce precip & 20th centruy avg from chr to numeric ----
mutate(value = as.numeric(value),
       x1901_2000_mean = as.numeric(x1901_2000_mean)) |>
  
  # calculate % change ----
mutate(perc_change = ((value - x1901_2000_mean)/x1901_2000_mean)*100) |>
  
  # select, rename, reorder cols ----
select(id, state, county, mean_1901_2000 = x1901_2000_mean, precip = value, perc_change, anomaly_1901_2000_base_period)

##~~~~~~~~~~~~~~~~~~
##  ~ join dfs  ----
##~~~~~~~~~~~~~~~~~~

# join dfs (be sure to join precip TO sf object, not the other way around) -------
joined_precip_geom <- full_join(county_geo_wrangled, precip_wrangled) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ unclassed color scare to show general patterns  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# unclassed map 
base_map <- ggplot(joined_precip_geom) +
  geom_sf(aes(fill = perc_change), linewidth = 0.1) +
  labs(title = "5-year precipitation compared with the 20th century average",
       subtitle = "January 2019 - December 2023",
       caption = "Source: National Centers for Environmental Information") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(face = "italic",
                                margin = margin(t = 2, r = 0.5, b = 0, l = 0, "lines"))
  )

base_map

# define palette ----
my_brew_palette11 <- RColorBrewer::brewer.pal(n = 11, name = 'BrBG')
my_brew_palette11

# attempt 1
base_map + 
  scale_fill_gradientn(colors = my_brew_palette11,
                       labels = scales::label_percent(scale = 1), # number that it will multiply the values by: keep it at 1
                       breaks = scales::breaks_width(width = 10)) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.75))

# attempt 2: set the 0 value so that its more intuitive
base_map + 
  scale_fill_gradientn(colors = my_brew_palette11,
                       labels = scales::label_percent(scale = 1),
                       breaks = scales::breaks_width(width = 10),
                       values = scales::rescale(x = c(
                         min(na.omit(joined_precip_geom)$perc_change),
                         0,
                         max(na.omit(joined_precip_geom)$perc_change)))) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.75))

##~~~~~~~~~~~~~~~~~~
##  ~ classed color scare to communicate statistical brackets  ----
##~~~~~~~~~~~~~~~~~~

# classed map
my_brew_palette10 <- RColorBrewer::brewer.pal(n = 10, name = 'BrBG')
my_brew_palette10

base_map + 
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1)) +
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))

# add more classes 
base_map + 
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1),
                    breaks = scales::breaks_width(width = 10)) + # set breaks to 10's
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))

base_map + 
  scale_fill_stepsn(colors = my_brew_palette10,
                    labels = scales::label_percent(scale = 1),
                    
                    # set the min and max values with the center at 0 of the legend
                    values = scales::rescale(x = c(
                      min(na.omit(joined_precip_geom)$perc_change),
                      0,
                      max(na.omit(joined_precip_geom)$perc_change))), 
                    
                    # set the breaks in the legend (going by 5%)
                    breaks = scales::breaks_width(width = 5)) +
  guides(fill = guide_colorsteps(barwidth = 25, barheight = 0.75))

