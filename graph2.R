
# #################################
#
# Program: presentation_graphs.R
#
# Author: Waveley Qiu
# 
# Date: 2023-04-10
#
# Description: graphs projecting T & E prices
#
# ###################################

eggs <- 
  readxl::read_excel("data/egg_prices_jan2013_feb2023.xlsx", range = "A10:M21") %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = jan:dec,
    names_to = "mon"
  ) %>%
  mutate(
    mon = str_to_title(mon),
    year = as.numeric(year),
  ) %>%
  rename(egg_price = value)

tom_and_egg <- 
  readxl::read_excel("data/tomato_prices_jan2021_feb2023.xlsx", range = "A11:D36") %>%
  janitor::clean_names() %>%
  separate(
    label,
    c("year", "mon")
  ) %>%
  rename(tomato_price = observation_value) %>%
  mutate(
    year = as.numeric(year)
  ) %>%
  left_join(
    eggs, 
    by = c("year", "mon")
  ) %>% 
  mutate(
    date = as.Date(paste0(mon, " 01, ", year), "%B %d, %Y")
  ) %>%
  select(
    date, egg_price, tomato_price
  ) %>%
  pivot_longer(
    cols = c(egg_price, tomato_price)
  )

tom_and_egg %>%
  ggplot(
    aes(x = date, y = value, col = name)
  ) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Date",
    y = "Price",
    title = "Price of Tomatoes and Eggs",
    subtitle = "2021-2023",
    caption = "Data from Bureau of Labor Statistics",
    col = "Item"
  ) +
  scale_color_manual(values = c("yellow2", "tomato"),
                     labels = c("Eggs", "Tomatoes")) +
  scale_y_continuous(labels = scales::dollar_format(), 
                     breaks = seq(0, 5, by = 0.5))

                 