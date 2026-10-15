# Part 3: Reading the Data
library(tidyquant)
library(tidyverse)
library(knitr)
# unemp_raw = tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
# 
# # Only save if the download has the expected columns
# stopifnot(all(c("symbol", "date", "price") %in% names(unemp_raw)))
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")


# 4.1: Renaming entries

unemp = unemp_raw |>
  rename(unemployment_rate = price) |>
    mutate(
      region = case_when(
        symbol == "UNRATE" ~ "United States",
        symbol == "ORUR" ~ "Oregon",
        symbol == "CAUR" ~ "California",
        symbol == "TXUR" ~ "Texas",
        symbol== "NYUR" ~ "New York"
      )
    )


# 4.2 Wrangle the Data
# Filling in missing October data for 2025 with data in the previous month 
# from each region
unemp = unemp |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(
    unemployment_rate = replace(
      unemployment_rate, 
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )) |> ungroup()

# Create column that lags the data to find the unemployment rate change 
# Arrange data by region and date, group by region, then add a column called 
# unemployment rate change by taking the difference of:
# unemployment rate - lagged value of unemployment rate

unemp = unemp |> 
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)
  ) |> ungroup()
  
# Use filter to create tibble of observation Jan 2015 forward
unemp_recent = unemp |> 
  filter(date >= '2015-01-01')

# Group by summary of each region
unemp_summary = unemp |> 
  group_by(region) |> 
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

# Group for recent summary
unemp_recent_summary = unemp_recent |> 
  group_by(region) |> 
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )


# 4.3 Find the Extremes
# Largest Month to Month Increase within each region
unemp_top_increases = unemp |> 
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

# Smallest month to month increase within each region
unemp_top_decreases = unemp |> 
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)


# 4.4. Find the Extremes Excluding 2020 

# Filter data set that excludes 2020 data set (Use pipe for OR)
# use the filtered no COVID data set and filter for the max by ordering by 
# the unemployment rate change and take the top 5

unemp_recent_no_covid = unemp |> 
  filter(date < "2020-01-01" | date >= "2021-01-01")

unemp_top_increases_no_covid = unemp_recent_no_covid |> 
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid = unemp_recent_no_covid |> 
  tidyr::drop_na(unemployment_rate_change) |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)


# Part 5 – Visualize the Data with ggplot2

# 5.1 Line Plot

# Line plot Overlaid
line_plot = ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line()+       # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rate by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

# Line plot by region
line_region_plot = ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() + facet_wrap(~ region)+ 
  labs(title = "Unemployment Rate by Region", 
       x = "Date", y = "Unemployment Rate") + theme_minimal() 

# 5.3. Produce a Bar Chart
# Sorted Bar Chart
bar_plot_region = ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), 
           y = mean_rate, fill = region)) +
           geom_col() + 
           labs(title = "Average Unemployment Rate by Region", 
                x = "Region", y = "Unemployment Rate")

# 5.4 Unemployment Rate of Jan 2025 and Present: Rise of AI

# This is the unemployment data for Jan 2022 to present, representing the unemployment coming out of COVID, and the rise
# of the AI boom. What's interesting about this data is the high unemployment rate in California, as casual inference
# can be made as to why. Texas and New York see a similar rise in unemployment relative to the US as a whole.

unemp_recent_2025 = unemp |> 
  filter(date >= '2022-01-01')
head(unemp_recent)
unemp_2025 = ggplot(unemp_recent_2025, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line()+ labs(title = "Unemployment Rate by Region", 
                    x = "Date", y = "Unemployment Rate") + theme_minimal()




