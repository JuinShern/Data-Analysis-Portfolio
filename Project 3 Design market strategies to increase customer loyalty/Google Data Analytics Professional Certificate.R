library(tidyverse)

jan2021_data = read.csv("202101-divvy-tripdata.csv")
feb2021_data = read.csv("202102-divvy-tripdata.csv")
mar2021_data = read.csv("202103-divvy-tripdata.csv")
apr2021_data = read.csv("202104-divvy-tripdata.csv")
may2021_data = read.csv("202105-divvy-tripdata.csv")
jun2021_data = read.csv("202106-divvy-tripdata.csv")
jul2021_data = read.csv("202107-divvy-tripdata.csv")
aug2021_data = read.csv("202108-divvy-tripdata.csv")
sep2021_data = read.csv("202109-divvy-tripdata.csv")
oct2021_data = read.csv("202110-divvy-tripdata.csv")
nov2021_data = read.csv("202111-divvy-tripdata.csv")
dec2021_data = read.csv("202112-divvy-tripdata.csv")

annual_data <- rbind(jan2021_data, feb2021_data, mar2021_data, apr2021_data,
                     may2021_data, jun2021_data, jul2021_data, aug2021_data,
                     sep2021_data, oct2021_data, nov2021_data, dec2021_data) %>% 
  mutate(ride_length = as.numeric(difftime(ended_at, started_at)),
         rideable_type = as.factor(rideable_type),
         member_casual = as.factor(member_casual),
         ) %>% 
  separate(started_at, c("start_date", "start_time"), sep = " ") %>% 
  mutate(day_of_week = as.factor(weekdays(as.Date(start_date))),
         start_date = as.Date(start_date))

annual_data <- annual_data %>% 
  mutate(day_of_week = ordered(day_of_week, levels = c("Monday","Tuesday","Wednesday",
                                                         "Thursday","Friday","Saturday",
                                                         "Sunday")))

annual_data %>% 
  filter(ride_length > 0) %>% 
  summary()

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(rideable_type) %>% 
  summarise(avg_ride_length = mean(ride_length)) %>% 
  arrange(-avg_ride_length)

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(rideable_type) %>% 
  summarise(max_ride_length = max(ride_length)) %>% 
  arrange(-max_ride_length)

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(rideable_type) %>% 
  summarise(median_ride_length = median(ride_length)) %>% 
  arrange(-median_ride_length)

### Findings
- The mean, maximum and median of ride length for Docked bike are higher than 
the mean, maximum and median of ride length for classical and electric bikes.

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length)) %>% 
  arrange(-avg_ride_length)

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(member_casual) %>% 
  summarise(max_ride_length = max(ride_length)) %>% 
  arrange(-max_ride_length)

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(member_casual) %>% 
  summarise(median_ride_length = median(ride_length)) %>% 
  arrange(-median_ride_length)

### Findings
- The mean, maximum and median of reide length for casual members are higher than
other type of bikes.

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(day_of_week, member_casual) %>% 
  summarise(mean_ride_length = mean(ride_length)) %>% 
  arrange(-mean_ride_length) %>% 
  pivot_wider(id_cols = day_of_week,
              names_from = member_casual,
              values_from = mean_ride_length)

annual_data %>% 
  filter(ride_length > 0) %>%
  group_by(day_of_week) %>% 
  tally() %>% 
  arrange(-n)

### Findings
- The average riding length on Sunday for both casual and member are the highest.
- The number of rides peaks in Saturday.

annual_data %>% 
  group_by(months.Date(start_date)) %>% 
  tally() %>% 
  arrange(-n)

### Findings
- The number of rides peak in July and trough in February.

## Data visualisation
annual_data %>% 
  filter(ride_length > 0) %>% 
  group_by(day_of_week) %>% 
  summarise(mean_ride_length = mean(ride_length)) %>% 
  ggplot(.,) +
  geom_col(aes(x = day_of_week, y = mean_ride_length))

           