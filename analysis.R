# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
eviction <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)
# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(eviction)
num_features <- ncol(eviction)
# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- eviction %>%
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count() %>%
  arrange(-n) %>%
  ungroup() %>%
  top_n(10, wt = n)
# Create a plot of the number of evictions each month in the dataset
by_month <- eviction %>%
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(month) %>%
  count()
# Store plot in a variable
month_plot <- ggplot(data = by_month) +
  geom_line(mapping = aes(x = month, y = n)) +
  labs(x = "Date", y = "Number of Evictions", title = "Evictions over time in SF")
# Map evictions in 2017 
eviction_2017 <- eviction %>%
  mutate(date = as.Date(File.Date, formate = "%m/%d/%y")) %>%
  filter(format(date, "%Y" == "2017")) %>%
  separate(Location, c("lat", "long"), ", ") %>%
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),
    long = as.numeric(gsub("\\)", "", long))
  )
# Format the lat/long variables, filter to 2017

# Create a maptile background
base_plot <- qmplot(
  data = eviction_2017,
  x = long,
  y = lat,
  geom = "blank",
  maptype = "toner-backgroup",
  darken = .7,
  legend = "topleft"
)
# Add a layer of points on top of the map tiles
eviction_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm"))
