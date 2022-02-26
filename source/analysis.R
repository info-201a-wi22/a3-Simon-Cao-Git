# Setting up foundation for data analysis

library(dplyr)
library(ggplot2)
library(tidyverse)
library(maps)
library(mapproj)

data <- read.csv("../data/incarceration_trends.csv")

# Introduction Information

var_num <- ncol(data)
obs_num <- nrow(data)

# Summary Information

data_2011 <- data %>%
  filter(year == 2011)

data_2016 <- data %>%
  filter(year == 2016)

data_recent <- data %>%
  filter(year >= 2005, year <= 2016)

avg_prison_admission_rate <- mean(data_2016$total_prison_adm_rate, na.rm = T)
avg_black_prison_adm_rate <- mean(data_2016$black_prison_adm_rate,
  na.rm = T
)
avg_white_prison_adm_rate <- mean(data_2016$white_prison_adm_rate,
  na.rm = T
)
max_black_prison_adm_rate <- max(data_2016$black_prison_adm_rate, na.rm = T)
black_prison_adm_rate_diff <- avg_black_prison_adm_rate -
  mean(data_2011$black_prison_adm_rate, na.rm = T)

summary_table <- data %>%
  filter(year >= 2011, year <= 2016) %>%
  group_by(year) %>%
  summarize(
    avg_prison_admission_rate = mean(total_prison_adm_rate, na.rm = T),
    avg_white_prison_admission_rate =
      mean(white_prison_adm_rate, na.rm = T),
    avg_black_prison_admission_rate =
      mean(black_prison_adm_rate, na.rm = T),
    avg_latinx_prison_admission_rate =
      mean(latinx_prison_adm_rate, na.rm = T),
    avg_appi_prison_admission_rate =
      mean(aapi_prison_adm_rate, na.rm = T),
    avg_native_prison_admission_rate =
      mean(native_prison_adm_rate, na.rm = T),
  )

# time trend plot

time_trend_data <- data_recent %>%
  group_by(year) %>%
  summarize(
    avg_county_prison_admission_rate = mean(total_prison_adm_rate, na.rm = T),
    avg_white_prison_admission_rate =
      mean(white_prison_adm_rate, na.rm = T),
    avg_black_prison_admission_rate =
      mean(black_prison_adm_rate, na.rm = T),
    avg_latinx_prison_admission_rate =
      mean(latinx_prison_adm_rate, na.rm = T),
    avg_appi_prison_admission_rate =
      mean(aapi_prison_adm_rate, na.rm = T),
    avg_native_prison_admission_rate =
      mean(native_prison_adm_rate, na.rm = T),
  )

time_trend_plot <- ggplot(time_trend_data, aes(x = year)) +
  geom_line(aes(y = avg_county_prison_admission_rate, colour = "Overall", ),
    size = 2
  ) +
  geom_line(aes(y = avg_white_prison_admission_rate, colour = "White")) +
  geom_line(aes(y = avg_black_prison_admission_rate, colour = "Black")) +
  geom_line(aes(y = avg_latinx_prison_admission_rate, colour = "Latinx")) +
  geom_line(aes(
    y = avg_appi_prison_admission_rate,
    colour = "Asian American / Pacific Islander"
  )) +
  geom_line(aes(
    y = avg_native_prison_admission_rate,
    colour = "Native American"
  )) +
  scale_colour_manual("",
    values = c(
      "Overall" = "purple", "White" = "steelblue",
      "Black" = "black", "Latinx" = "darkred",
      "Asian American / Pacific Islander" = "darkorange",
      "Native American" = "green"
    )
  ) +
  ggtitle("Prison Admission Rate by Race from 2005 to 2016") +
  labs(y = "Prison Admission Rate", x = "Year")

# comparison plot

variable_comparison_data <- data_recent %>%
  filter(year == 2016) %>%
  select(black_prison_adm_rate, black_pop_15to64, state) %>%
  group_by(state) %>%
  summarize(
    avg_black_prison_admission_rate =
      mean(black_prison_adm_rate, na.rm = T),
    total_black_pop_15to64 =
      sum(black_pop_15to64)
  )

variable_comparison_plot <- ggplot(
  variable_comparison_data,
  aes(
    x = total_black_pop_15to64,
    y = avg_black_prison_admission_rate
  )
) +
  geom_point(
    mapping = aes(color = state)
  ) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  ggtitle("Relationship Between States' Black Population and Prison Admission
       Rate") +
  labs(
    y = "Average Prison Admission Rate of Black People in 2016",
    x = "Black Population"
  )

# map plot

map_2016 <- data_recent %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarize(
    avg_black_prison_admission_rate =
      mean(black_prison_adm_rate, na.rm = T)
  )

state_shapes <- map_data("state") %>%
  unite(polyname, region, sep = ",") %>%
  left_join(state.fips, by = "polyname") %>%
  rename(state = abb)

map_data <- state_shapes %>%
  left_join(map_2016, by = "state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

map_plot <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(
      x = long, y = lat, group = group,
      fill = avg_black_prison_admission_rate
    ),
  ) +
  coord_map() +
  scale_fill_continuous(type = "gradient") +
  labs(fill = "Prison Admission Rate of Black People in 2016") +
  blank_theme +
  ggtitle("Map of Prison Admission Rate of Black People by State in 2016")