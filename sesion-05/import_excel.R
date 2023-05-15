library(tidyverse)
library(janitor)
library(readxl)


# https://foresightbi.com.ng/microsoft-power-bi/dirty-data-samples-to-practice-on/

# Example 1

segments <- c("consumer", "corporate", "home_office")
ship_modes <- c("first_class", "same_day", "second_class", "standard_class")


group_shipping_names <- paste0(
  rep(segments, each = length(ship_modes)),
  "_",
  rep(ship_modes, length(segments))
)


read_xlsx("sesion-05/raw-data/example1.xlsx",
  sheet = 1,
  skip = 1,
  .name_repair = janitor::make_clean_names
) |>
  select(!starts_with("x")) |>
  rename(order_id = "ship_mode") |>
  slice(-1) |>
  rename_with(
    ~ c("order_id", group_shipping_names)
  ) |>
  pivot_longer(
    cols = -1,
    names_pattern = glue::glue(
      '({paste0(segments, collapse = "|")})_(.*)'
    ),
    names_to = c("segment", "shipping_mode"),
    values_drop_na = TRUE
  ) |>
  arrange(segment, shipping_mode, order_id)


# Example 2

read_xlsx(
  "sesion-05/raw-data/example2.xlsx",
  sheet = 1,
  skip = 1,
  .name_repair = janitor::make_clean_names
) |>
  rename(order_date = "segment") |>
  slice(-1) |>
  rename_with(
    ~ c("order_date", group_shipping_names)
  ) |>
  pivot_longer(
    cols = -1,
    names_pattern = glue::glue(
      '({paste0(segments, collapse = "|")})_(.*)'
    ),
    names_to = c("segment", "shipping_mode"),
    values_drop_na = TRUE
  ) |>
  arrange(segment, shipping_mode, order_date) |>
  mutate(
    order_date = as.numeric(order_date),
    order_date = janitor::excel_numeric_to_date(order_date)
  )
