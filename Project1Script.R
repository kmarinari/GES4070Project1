################################################################################
# Load Libraries
library(sf)
library(readr)
library(dplyr)
library(tmap)
library(stringr)
library(tidyverse)
library(ggplot2)
library(scales) 


################################################################################
# Load Data
unzip("data/summitstage.zip", files = "stops.txt", exdir = "data/gtfs_temp")

# Load stops.txt
stops <- read_csv("data/gtfs_temp/stops.txt")

# Keep only coordinates and name columns
stops <- stops %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  filter(!is.na(stop_lat) & !is.na(stop_lon))

# Convert to spatial data
stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

# Make a 400 m buffer
stops_proj <- st_transform(stops_sf, 3857)
stop_buf <- st_union(st_buffer(stops_proj, 400)) %>% st_transform(4326)

# Quick plot check
tmap_mode("view")
tm_shape(stops_sf) + tm_dots(size = 0.05, col = "blue") +
  tm_shape(stop_buf) + tm_borders(col = "red")

# Check data
ls()

# Read in tract boundaries .shp
colorado <- st_read("data/Colorado_Census_Tract_Boundaries/Colorado_Census_Tract_Boundaries.shp")

# Check what it looks like
glimpse(colorado)

# Filter for Summit County
# Summit County FIPS code = 08117 (08 = Colorado, 117 = Summit)
summit <- colorado %>%
  filter(startsWith(FIPS, "08117"))

# Transform both to same CRS (for accurate area)
summit <- st_transform(summit, 3857)
stop_buf <- st_transform(stop_buf, 3857)

# Intersect tracts with buffer (area within walking distance)
intersected <- st_intersection(summit, stop_buf)

# Calculate total and accessible area for each tract
tract_access <- intersected %>%
  mutate(access_area = st_area(geometry)) %>%
  group_by(FIPS) %>%
  summarise(access_area = sum(access_area)) %>%
  right_join(
    summit %>% mutate(total_area = st_area(geometry)) %>% st_drop_geometry(),
    by = "FIPS"
  ) %>%
  mutate(
    prop_accessible = as.numeric(access_area) / as.numeric(total_area)
  )

# Add geometry back
summit_access <- summit %>%
  left_join(st_drop_geometry(tract_access) %>% select(FIPS, prop_accessible),
            by = "FIPS")


################################################################################
# Bus Stops Map
library(tmap)
tmap_mode("plot")

tm_shape(summit_access) +
  tm_polygons("prop_accessible",
              palette = "Blues",
              title = "Proportion of Tract within 400m of a Bus Stop") +
  tm_layout(title = "Transit Accessibility in Summit County, CO") +
  tm_shape(stops_sf) +
  tm_dots(size = 0.04, col = "black")


################################################################################
# Load Income Data
income_data <- read_csv("data/Colorado_Counties_Data.csv")

# Rename, Clean, and Filter Data
comparison_data <- income_data %>%
  rename(
    County_Name = geoname,
    Median_Household_Income = med_hh_inc
  ) %>%
  mutate(
    # Clean the income column
    Median_Household_Income = str_remove_all(Median_Household_Income, pattern = "[,$]"),
    Median_Household_Income = as.numeric(Median_Household_Income)
  ) %>%
  filter(grepl("Summit|Eagle|Pitkin|Denver", County_Name)) %>%
  mutate(
    # Create the clean "County" column used for plotting
    County = str_remove(County_Name, " County")
  ) %>%
  filter(!is.na(Median_Household_Income)) %>%
  arrange(desc(Median_Household_Income)) %>%
  # Ensure the order is locked for the plot
  mutate(County = factor(County, levels = County))


################################################################################
# Bar Chart
income_plot <- ggplot(comparison_data, 
                      # Map County to X, Income to Y, and County to fill color
                      aes(x = County, y = Median_Household_Income, fill = County)) +
  
  geom_col(width = 0.7) +
  
  # Add the dollar amount text labels above the bars
  geom_text(aes(label = dollar(Median_Household_Income, accuracy = 1)),
            vjust = -0.5, size = 4) +
  
  # Ensure the names exactly match the levels in the data ("Summit", "Eagle", etc.)
  scale_fill_manual(
    values = c("Summit" = "#D5FFFF",   
               "Eagle" = "#ADD8E6",    
               "Pitkin" = "#4C72B0",   
               "Denver" = "#000080")  
  ) +
  
  # Format the Y-axis as dollars
  scale_y_continuous(labels = dollar_format()) +
  
  # Add Labels and Titles
  labs(
    title = "Median Household Income Comparison (from Colorado Data)",
    x = "County",
    y = "Median Household Income ($)"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" # Hide the legend
  )

# Display plot
print(income_plot)

# Save plot
ggsave("county_income_comparison.png", plot = income_plot, width = 8, height = 6, units = "in")


################################################################################
# Calculate Average Access to Transit Stop
avg_access <- mean(summit_access$prop_accessible, na.rm = TRUE)
avg_access_percent <- round(avg_access * 100, 1)
avg_access_percent


################################################################################
# Transit Accessibility Map:
tmap_mode("plot")  

tm_shape(summit_access) +
  tm_polygons("prop_accessible",
              palette = "Blues",
              style = "quantile",
              title = "Proportion within 400 m of Bus Stop") +
  tm_shape(stops_sf) +
  tm_dots(size = 0.04, col = "black", legend.show = FALSE) +
  tm_layout(title = "Transit Accessibility in Summit County") +
  tm_scale_bar(position = c("right", "bottom")) +   # add scale bar
  tm_compass(position = c("left", "bottom"))        # add north arrow


################################################################################
# Pie Chart
df <- data.frame(
  Category = c("Within 400m of Bus Stop", "Not Within 400m"),
  Value = c(avg_access_percent, 100 - avg_access_percent)
)

ggplot(df, aes(x = "", y = Value, fill = Category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "County Area Transit Coverage") +
  theme_void() +
  scale_fill_manual(values = c("navyblue", "lightblue"))



