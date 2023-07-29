library(maps)
library(mapproj)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)

#Part 1: Data Description

incarceration_df <- read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"), stringsAsFactors = FALSE)

num_incarceration_obs <- nrow(incarceration_df)

incarceration_year_range <- range(incarceration_df$year)

recent_year <- filter(incarceration_df, !is.na(total_pop) & !is.na(total_jail_pop) & !is.na(total_prison_pop)) %>%
  pull(year) %>%
  max(.)

recent_num_jail <- filter(incarceration_df, year == recent_year) %>%
  summarize(sum = sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(sum)

recent_num_prison <- filter(incarceration_df, year == recent_year) %>%
  summarize(sum = sum(total_prison_pop, na.rm = TRUE)) %>%
  pull(sum)

recent_population <- filter(incarceration_df, year == recent_year) %>%
  summarize(sum = sum(total_pop, na.rm = TRUE)) %>%
  pull(sum)

data_description <- list(num_incarceration_obs, incarceration_year_range, recent_year, recent_num_jail, recent_num_prison, recent_population)

#Part 2: Incarceration Trends Over Time

incarceration_time_series_df <- incarceration_df %>%
  replace(is.na(.), 0) %>%
  filter(year >= 1980, year <= 2016) %>%
  mutate(total_incarcerated = total_jail_pop + total_prison_pop) %>%
  mutate(white_total = white_jail_pop + white_prison_pop) %>%
  mutate(black_total = black_jail_pop +black_prison_pop) %>%
  mutate(latinx_total = latinx_jail_pop, latinx_prison_pop) %>%
  mutate(aapi_total = aapi_jail_pop + aapi_prison_pop) %>%
  mutate(native_total = native_jail_pop + native_prison_pop) %>%
  group_by(year) %>%
  summarize(across(total_incarcerated:native_total, sum)) %>%
  pivot_longer(cols = total_incarcerated:native_total, names_to = "Race")

incarceration_over_time_plot <- ggplot(data = incarceration_time_series_df) + geom_line(mapping = aes(x = year, y = value, color = Race)) + geom_point(mapping = aes(x = year, y = value, color = Race)) +
  scale_color_brewer(palette = "Set1", labels = c("AAPI", "Black", "Latinx", "Native", "Total", "White")) +
  labs(title = "Incarcerated Population Over Time", x = "Year", y = "Population (# of People)")

#Part 3: Highest Black Incarceration Rates

top_10_black_incarceration_states_df <- incarceration_df %>%
  replace(is.na(.), 0) %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  mutate(total_incarceration = total_jail_pop + total_prison_pop, black_incarceration = black_jail_pop + black_prison_pop) %>%
  summarize(total_pop = sum(total_pop_15to64), black_pop = sum(black_pop_15to64), total_incarceration = sum(total_incarceration), black_incarceration = sum(black_incarceration)) %>%
  summarize(total_rate = total_incarceration/total_pop, black_rate = black_incarceration/black_pop, state = state) %>%
  slice_max(black_rate, n = 10) %>%
  arrange(black_rate)


pivot_top_black_states <- top_10_black_incarceration_states_df %>%
  pivot_longer(cols = total_rate:black_rate, names_to = "both_rates") %>%
  mutate(col = factor(state, levels = top_10_black_incarceration_states_df$state))


top_10_black_incarceration_plot <- ggplot(data = pivot_top_black_states) +
  geom_col(mapping = aes(x = col, y = value, fill = both_rates), position = 'dodge') +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_manual(values = c("black", "red"), labels = c("Black", "Total")) +
  labs(title = "States with Highest Rate of Black Incarceration", y = "Percent Incarcerated", x = "State", fill = "")

#Part 4: Racial Incarceration Discrepancy Map

racial_discrepancy <- incarceration_df %>%
  replace(is.na(.), 0) %>%
  filter(year == 2016, state == "WA") %>%
  group_by(county_name) %>%
  summarize(white_pop = sum(white_pop_15to64), white_incarcerated = sum(white_jail_pop) + sum(white_prison_pop), black_pop = sum(black_pop_15to64), black_incarcerated = sum(black_jail_pop) + sum(black_prison_pop), white_ratio = white_incarcerated/white_pop, black_ratio = black_incarcerated/black_pop, fips = fips) %>%
  select(fips, white_ratio, black_ratio) %>%
  summarize(fips = fips, discrepancy_ratio = black_ratio/white_ratio)
  

fips_county <- county.fips %>%
  filter(str_detect(polyname, "washington,")) %>%
  mutate(subregion = str_replace(polyname,"washington,", "")) %>%
  select(fips, subregion) %>%
  mutate(subregion = gsub(":.*","", subregion))

WA_county_shape <- map_data("county") %>%
  filter(region == "washington") %>%
  left_join(., fips_county, by = "subregion") %>%
  left_join(., racial_discrepancy, by = "fips")

racial_discrepancy_map <- ggplot(data = WA_county_shape) + geom_polygon(mapping =  aes(x = long, y = lat, group = group, fill = discrepancy_ratio)) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  coord_map() +
  theme_void() +
  labs(title = "Discrepancies between racial incarceration rates in WA", fill = "ratio of black:white",
       caption = "Shows the discrepancies in incarceration rates between black and white individuals in Washington. The ratio is how many times more commonly black people are incarcerated")
  
#Part 5. Your Own Visualization

prisoner_to_area_df <- incarceration_df %>%
  replace(is.na(.), 0) %>%
  group_by(state) %>%
  filter(year == 2016) %>%
  summarize(male = male_prison_pop, female = female_prison_pop, urbanicity = urbanicity)

prisoner_to_area_plot <- ggplot(data = prisoner_to_area_df) +
  geom_line(mapping = aes(x = male, y = female,  color = urbanicity)) +
  geom_point(mapping = aes(x = male, y = female,  color = urbanicity)) +
  xlim(0, 5000) + ylim(0, 500) +
  scale_color_brewer(palette = "Set2", labels = c("Not Listed", "Rural", "Small/Mid", "Suburban", "Urban")) + 
  labs(title = "Correlation between Men and Women Imprisonment by County Size", x = "Male Prison Population", y = "Female Prison Population")
  


  










