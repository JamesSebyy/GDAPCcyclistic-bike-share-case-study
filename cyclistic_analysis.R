# lib runs

install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library(lubridate)


# File number validation

csv_files <- list.files(path = "raw_data", pattern = "\\.csv$", full.names = TRUE) 

length(csv_files) #to confirm the number of files detected


# testing summary for memory issues to confirm with Rstudio servers for a small clean; first file first

test_summary <- read_csv(csv_files[1], show_col_types = FALSE) %>%
  mutate(
    ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")), #creating new column - trip/ride length
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE), 
    hour_of_day = hour(started_at),
    month = floor_date(started_at, "month")) %>%
  filter(
    ride_length_min > 0, # filter out those pesky errors resulting from a somehow higher star time compared to end time or those weirdly short HQ QR travels
    ride_length_min <= 1440, # filters out ride lengths that are more than 24 hours; any ride longer than that is unrealistic or represents a bike that is not docked properly
    member_casual %in% c("member", "casual")) %>%
  group_by(member_casual) %>%
  summarise(
    total_rides = n(), # to count how many each rider type has in terms of rides
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    median_ride_length = median(ride_length_min, na.rm = TRUE),
    .groups = "drop")

test_summary # tibble view summary


# FINALLY it works omg, fifth time's a charm
# ACTUAL analysis, but note, in the previous five trials of analysis, RStudio (on the free account) ends up crashing or forcing a session restart BECAUSe it runs out of memory, therefore, the best case for me is to make the program temporarily store the summary tables of each month instead and then analysing each of those summaries rather than forcing it to store thousands (?) of rows: 

monthly_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month")) %>%
    filter(
      ride_length_min > 0, # again, deleting those weird errors that resulted from higher starting times compared to trip end times or those weirdly short HQ-QR trip durations
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(month, member_casual) %>%
    summarise(
      total_rides = n(),
      avg_ride_length = mean(ride_length_min, na.rm = TRUE),
      median_ride_length = median(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

monthly_summary # so clearly one year does not have 48 months

nrow(monthly_summary)

monthly_summary %>% 
  distinct(month) %>%
  arrange(month) %>%
  print(n = Inf) # month detector, so wheres the detection?

monthly_summary %>%
  count(month, member_casual) %>%
  filter(n > 1) # so duplicates are found

View(monthly_summary)


# Trying a new corrected monthly summary code 

monthly_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month")) %>%
    filter(
      ride_length_min > 0,
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(month, member_casual) %>%
    summarise(
      total_rides = n(),
      total_ride_minutes = sum(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

monthly_summary_final <- monthly_summary %>%
  group_by(month, member_casual) %>%
  summarise(
    total_rides = sum(total_rides),
    total_ride_minutes = sum(total_ride_minutes),
    avg_ride_length = total_ride_minutes / total_rides,
    .groups = "drop") %>%
  arrange(month, member_casual)

monthly_summary_final

nrow(monthly_summary_final) # yes, so NOW it works, 13 x 2 (for the two rider types) which also includes January.

# time to ONLY extract 2025 data.

monthly_summary_2025 <- monthly_summary_final %>%
  filter(month >= ymd("2025-01-01"),
         month <= ymd("2025-12-01"))

monthly_summary_2025


# FINALL okay so just to write it

write_csv(monthly_summary_2025, "monthly_summary_2025.csv")

nrow(monthly_summary_2025)


# Days of the week summary

day_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month"),
      day_of_week_num = wday(started_at, week_start = 1),
      day_of_week = wday(started_at, label = TRUE, abbr = FALSE, week_start = 1)) %>%
    filter(
      month >= ymd("2025-01-01"),
      month <= ymd("2025-12-01"),
      ride_length_min > 0,
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(day_of_week_num, day_of_week, member_casual) %>%
    summarise(
      total_rides = n(),
      total_ride_minutes = sum(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

day_summary_2025 <- day_summary %>%
  group_by(day_of_week_num, day_of_week, member_casual) %>%
  summarise(
    total_rides = sum(total_rides),
    total_ride_minutes = sum(total_ride_minutes),
    avg_ride_length = total_ride_minutes / total_rides,
    .groups = "drop") %>%
  arrange(day_of_week_num, member_casual)

day_summary_2025

write_csv(day_summary_2025, "day_summary_2025.csv")

# Hour summary / Time

hour_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month"),
      hour_of_day = hour(started_at)) %>%
    filter(
      month >= ymd("2025-01-01"),
      month <= ymd("2025-12-01"),
      ride_length_min > 0,
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(hour_of_day, member_casual) %>%
    summarise(
      total_rides = n(),
      total_ride_minutes = sum(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

hour_summary_2025 <- hour_summary %>%
  group_by(hour_of_day, member_casual) %>%
  summarise(
    total_rides = sum(total_rides),
    total_ride_minutes = sum(total_ride_minutes),
    avg_ride_length = total_ride_minutes / total_rides,
    .groups = "drop") %>%
  arrange(hour_of_day, member_casual)

hour_summary_2025 # yes, 48 rows considering 24 hours for each rider type

write_csv(hour_summary_2025, "hour_summary_2025.csv")

# Bike type summary

bike_type_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month")) %>%
    filter(
      month >= ymd("2025-01-01"),
      month <= ymd("2025-12-01"),
      ride_length_min > 0,
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(rideable_type, member_casual) %>%
    summarise(
      total_rides = n(),
      total_ride_minutes = sum(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

bike_type_summary_2025 <- bike_type_summary %>%
  group_by(rideable_type, member_casual) %>%
  summarise(
    total_rides = sum(total_rides),
    total_ride_minutes = sum(total_ride_minutes),
    avg_ride_length = total_ride_minutes / total_rides,
    .groups = "drop") %>%
  arrange(rideable_type, member_casual)

bike_type_summary_2025

write_csv(bike_type_summary_2025, "bike_type_summary_2025.csv")

# So the last would probably be a summary of the data of overall riders regarding rider type and their ride lengths

overall_summary <- map_dfr(csv_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      month = floor_date(started_at, "month")) %>%
    filter(
      month >= ymd("2025-01-01"),
      month <= ymd("2025-12-01"),
      ride_length_min > 0,
      ride_length_min <= 1440,
      member_casual %in% c("member", "casual")) %>%
    group_by(member_casual) %>%
    summarise(
      total_rides = n(),
      total_ride_minutes = sum(ride_length_min, na.rm = TRUE),
      .groups = "drop")
})

overall_summary_2025 <- overall_summary %>%
  group_by(member_casual) %>%
  summarise(
    total_rides = sum(total_rides),
    total_ride_minutes = sum(total_ride_minutes),
    avg_ride_length = total_ride_minutes / total_rides,
    .groups = "drop") %>%
  arrange(member_casual)

overall_summary_2025

write_csv(overall_summary_2025, "overall_summary_2025.csv")



#visualisations:

chart_monthly_rides <- monthly_summary_2025 %>%
  mutate(
    month = ymd(month),
    member_casual = str_to_title(member_casual)) %>%
  ggplot(aes(x = month, y = total_rides, colour = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Rides by Rider Type in 2025",
    subtitle = "Ride volume across the year for casual riders and annual members",
    x = "Month",
    y = "Total rides",
    colour = "Rider type") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + scale_y_continuous(labels = scales::comma) + theme_minimal()
#scales, especially continous scales, were godsends because they made sure ggplot read and presented my datapoints correctly, for example, with commas or determining something to be a date.


chart_monthly_rides





chart_day_rides <- day_summary_2025 %>%
  mutate(
    member_casual = str_to_title(member_casual),
    day_of_week = factor(
      day_of_week,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = day_of_week, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Rides by Day of Week in 2025",
    subtitle = "Comparison of weekly usage patterns between casual riders and annual members",
    x = "Day of week",
    y = "Total rides",
    fill = "Rider type") +
  scale_y_continuous(labels = scales::comma) + theme_minimal() +theme(axis.text.x = element_text(angle = 30, hjust = 1))

chart_day_rides





chart_hour_rides <- hour_summary_2025 %>%
  mutate(member_casual = str_to_title(member_casual)) %>%
  ggplot(aes(x = hour_of_day, y = total_rides, colour = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Rides by Hour of Day in 2025",
    subtitle = "Hourly usage patterns for casual riders and annual members",
    x = "Hour of day (24h format)",
    y = "Total rides",
    colour = "Rider type") + theme_minimal()

chart_hour_rides







chart_bike_type <- bike_type_summary_2025 %>%
  mutate(
    member_casual = str_to_title(member_casual),
    rideable_type = str_replace_all(rideable_type, "_", " "),
    rideable_type = str_to_title(rideable_type)) %>%
  ggplot(aes(x = rideable_type, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Bike Type Usage by Rider Type in 2025",
    subtitle = "Comparison of preferred bike types between casual riders and annual members",
    x = "Bike type",
    y = "Total rides",
    fill = "Rider type") +
  scale_y_continuous(labels = scales::comma) + theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))

chart_bike_type






chart_total_rides <- overall_summary_2025 %>%
  mutate(member_casual = str_to_title(member_casual)) %>%
  ggplot(aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_col(width = 0.65) +
  labs(
    title = "Total Rides by Rider Type in 2025",
    subtitle = "Comparison of total ride volume between casual riders and annual members",
    x = "Rider type",
    y = "Total rides",
    fill = "Rider type") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

chart_total_rides




chart_avg_ride_length <- overall_summary_2025 %>%
  mutate(member_casual = str_to_title(member_casual)) %>%
  ggplot(aes(x = member_casual, y = avg_ride_length, fill = member_casual)) +
  geom_col(width = 0.65) +
  labs(
    title = "Average Ride Length by Rider Type in 2025",
    subtitle = "Average trip duration measured in minutes",
    x = "Rider type",
    y = "Average ride length, minutes",
    fill = "Rider type") + theme_minimal()

chart_avg_ride_length


