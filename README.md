🚲 Capital Bikeshare Data Analysis

An end-to-end data analysis project exploring ride behavior, commute patterns, and fleet utilization for Capital Bikeshare (Washington, D.C.) over a dataset of 1.98M+ rides.

📌 Executive Summary

This project analyzes user demographics (Casual vs. Annual Members), peak usage patterns, distance metrics, and bike preference within D.C.'s urban transit ecosystem.

Key Findings & Strategic Recommendations:

Evening Commute Dominance: Member usage peaks heavily between 5:00 PM and 6:00 PM, indicating high reliance on bikeshare for evening commutes.

E-Bike Preference for Long Trips: Electric bikes cover an average trip distance of ~2.5 km, outperforming classic (~1.8 km) and docked bikes (~1.8 km).

Weekday Mobility: Over 75% of total rides occur on weekdays, proving that bikes serve primarily as functional urban transit rather than pure weekend recreation.

📁 Dataset Overview

The analysis is based on rides_2020_2021_extract.csv, containing 1,986,564 ride records with 16 features:

Column Name

Data Type

Description

ride_id

Character

Unique identifier for each trip

member_casual

Character

User type (member or casual)

rideable_type

Character

Bike category (classic_bike, docked_bike, electric_bike)

start_date / end_date

POSIXct

Start and end timestamps

total_time

Integer

Ride duration (seconds)

start_lat, start_lng

Double

Origin coordinates

end_lat, end_lng

Double

Destination coordinates

🧼 Data Cleaning & Wrangling Pipeline

Data pre-processing was conducted in R using tidyverse, janitor, and lubridate:

Standardized Column Names: Converted headers to snake_case using janitor::clean_names().

Timestamp Parsing: Converted string timestamps into POSIXct objects with UTC timezone formatting.

Geospatial Clean-up: Filtered out null values in geographic coordinate columns (start_lat, start_lng, end_lat, end_lng) prior to Haversine distance calculations.

library(tidyverse)
library(here)
library(janitor)
library(geosphere)
library(lubridate)

# Load and clean dataset
bike_data <- read_csv(here("data", "rides_2020_2021_extract.csv"), na = "N/A") %>%
  rename(
    total_time = Duration,
    start_date = `Start date`,
    end_date = `End date`,
    start_station_name = `Start station name`,
    end_station_name = `End station name`
  ) %>%
  mutate(
    total_time = as.integer(total_time),
    start_date = as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"),
    end_date = as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"),
    member_casual = tolower(member_casual),
    start_lng = as.double(start_lng),
    start_lat = as.double(start_lat),
    end_lng = as.double(end_lng),
    end_lat = as.double(end_lat)
  ) %>%
  clean_names()


📊 Deep-Dive Exploratory Analysis & Findings

1. Hourly Usage: Member vs. Casual Riders

Question: What are the peak hours of operation for casual vs. annual member riders?

Insight: Annual members exhibit a dramatic spike during peak commute hours, reaching maximum volume at 6:00 PM (18:00). Casual users show a smoother bell-curve distribution peaking around 5:00 PM (17:00).

Business Takeaway: Targeted marketing for membership conversion should target casual riders during afternoon hours (12:00 PM – 4:00 PM).

# Hourly volume aggregation
total_trips <- bike_data %>%
  mutate(start_hours = hour(start_date)) %>%
  group_by(member_casual, start_hours) %>%
  summarize(total_rides = n(), .groups = 'drop')

# Visualization
ggplot(total_trips, aes(x = start_hours, y = total_rides, fill = member_casual)) +
  geom_col() +
  facet_wrap(~ member_casual) +
  labs(
    title = "Peak Riding Hours: Members vs. Casual Users",
    x = "Hour of Day (0-23)",
    y = "Total Trips"
  ) +
  theme_minimal()


2. Distance Traveled by Bike Type (Haversine Analysis)

Question: Which bike type is preferred for longer-distance travel?

Insight: Electric bikes register the highest average travel distance (~2.5 km per trip), whereas classic bikes and docked bikes average ~1.8 km.

Business Takeaway: Fleet expansion should prioritize electric bike availability at suburban/outer stations to facilitate longer commutes.

# Distance calculation using the Haversine formula
bike_distance <- bike_data %>%
  filter(!is.na(start_lng) & !is.na(start_lat) & !is.na(end_lng) & !is.na(end_lat)) %>%
  mutate(
    distance_km = distHaversine(
      matrix(c(start_lng, start_lat), ncol = 2),
      matrix(c(end_lng, end_lat), ncol = 2)
    ) / 1000
  ) %>%
  group_by(rideable_type) %>%
  summarise(mean_distance_km = mean(distance_km, na.rm = TRUE), .groups = "drop")

# Visualization
ggplot(bike_distance, aes(x = rideable_type, y = mean_distance_km, fill = rideable_type)) +
  geom_col() +
  labs(
    title = "Average Trip Distance by Bike Type",
    x = "Bike Type",
    y = "Average Distance (km)"
  ) +
  theme_minimal()


3. Weekday vs. Weekend Ride Distribution

Question: What percentage of total rides occur during weekdays versus weekends?

Insight: 75% of total trip volume occurs Monday through Friday, while 25% occurs over the weekend.

Business Takeaway: Station rebalancing and maintenance schedules should be prioritized on weekend mornings to avoid disrupting weekday commute capacity.

# Categorize rides by Day of Week
ride_categories <- bike_data %>%
  mutate(day_of_week = wday(start_date, label = TRUE, abbr = FALSE)) %>%
  mutate(ride_type = ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
  group_by(ride_type) %>%
  summarize(total_rides = n(), .groups = 'drop') %>%
  mutate(percentage = (total_rides / sum(total_rides)) * 100)

# Pie chart visualization
ggplot(ride_categories, aes(x = "", y = percentage, fill = ride_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Rides: Weekday vs. Weekend", fill = "Ride Type") +
  scale_fill_manual(values = c("Weekday" = "skyblue", "Weekend" = "orange")) +
  theme_minimal()


🛠️ Tools & Libraries

Language: R

Data Manipulation: tidyverse, dplyr, janitor, lubridate

Geospatial Processing: geosphere (distHaversine)

Data Visualization: ggplot2

🏷️ Future Scope & Next Steps

Integrate weather data API (temperature, precipitation) to evaluate environmental impact on daily ride counts.

Run spatial clustering (K-Means/DBSCAN) on station coordinates to optimize rebalancing routes.
