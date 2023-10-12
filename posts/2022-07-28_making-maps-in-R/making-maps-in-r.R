#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(tidyverse)
library(ggmap)
library(highcharter)


# ---- load-sources ---------------------------------------------------



# ---- declare-globals ----------------------------------------------------



# ---- load-data ------------------------------------------------------

# load data from Tidy Tuesday 3//01/22
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md

ev_stations <- tidytuesdayR::tt_load(2022, week = 9) %>% .$stations

ds1 <- ev_stations %>%
  filter(STATE == "NC") %>%
  select(LATITUDE, LONGITUDE, STATION_NAME, FUEL_TYPE_CODE, STATE, CITY) %>%
  rename(lat = LATITUDE, lon = LONGITUDE, name = STATION_NAME)


# ggmap -------------------------------------------------------------------

# Using GGMAP to create map of NC EV Staions

nc_boundries <- c(-85, 33.5, -75, 37)

get_stamenmap(bbox = nc_boundries, zoom = 7, map = "toner") %>% ggmap() +
  geom_point(data = ds1, aes(x = lon, y = lat, fill = FUEL_TYPE_CODE), shape = 21, size = 3) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void() +
  theme(legend.position = "bottom"
        ,plot.title = element_text(hjust = 0.5, margin=margin(0,0,30,0)) #center title and add space due to theme
        ) +
  guides(  #use to move legend title to the top, default is to the side
    fill = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = "Alternate Fuel Stations in North Carolina"
    ,fill = "Station Type"
  )








# create map --------------------------------------------------------------

hcmap("countries/us/us-nc-all", showInLegend = FALSE)

colors <- c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')



ds1 <- ev_stations %>%
  filter(STATE == "NC") %>%
  select(LATITUDE, LONGITUDE, STATION_NAME, FUEL_TYPE_CODE) %>%
  rename(lat = LATITUDE, lon = LONGITUDE, name = STATION_NAME) %>%
  mutate(color = colorize(FUEL_TYPE_CODE, colors))



hc1 <- hcmap("countries/us/us-nc-all", showInLegend = FALSE
             ,nullColor = "black" #use to set map background color
             ,borderColor = "pink" #change the colors of the borders!
             ) %>%
  hc_add_series(
    data = ds1
    ,type = "mappoint"
    ,dataLabels = list(enabled = FALSE)
    ,tooltip = list(pointFormat = "{point.name}")
    ,marker = list(lineWidth = 0, radius = 3, symbol = 'circle')
    ,hcaes(color = color
           ,group = FUEL_TYPE_CODE
           )  #use this to add the color and group to each point
  ,color =  colors  # use this to set the color of the series in the legend
  ) %>%
  hc_title(text = "Alternate Fuel Stations in North Carolina")
#note no legend title.  No default way to center legend title and I don't like the look of it


hc1
