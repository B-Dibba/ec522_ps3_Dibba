
#2.2. Read the .csv File and Inspect the Date Colum
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

#2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)


#2.3. Reshape Your Data to Long Format

#2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

#Compare the original and long formats
head(retail_raw)
head(retail_raw_long)

#2.4. Build Your tsibble

#2.4a
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

#2.4b
head(retail_ts)
has_gaps(retail_ts)

#2.5. Canadian Gas Production
#2.5a
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b
#2.2. Read the .csv File and Inspect the Date Column
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

#2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

#2.3. Reshape Your Data to Long Format

#2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw)
head(retail_raw_long)

#2.4. Build Your tsibble

#2.4a
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

#2.4b
head(retail_ts)
has_gaps(retail_ts)

#2.5. Canadian Gas Production

#2.5a
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b
can_gas <- canadian_gas |>        
  select(Month, Volume)

has_gaps(can_gas)                 

head(can_gas)                    

#2.6. Working with Dates in a tsibble

#2.6a. Filtering by date
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

#2.6b. Extracting parts of a date.
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

#Compute average volume in each calendar month since 1990:
can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

#Which calendar month has the highest average production, and which has the lowest?
#Ans:Highest = Dec, Lowest = Jun

#2.6c. Date arithmetic
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

#2.6d. Changing the frequency of a series
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

#Why sum() instead of mean()?
#We use sum() because gas production volume is a flow variable, measuring the total amount produced during a period, so the quarterly total is the meaningful quantity (three months of production added together).
#A mean() would give average monthly production, which misrepresents the quarterly output

#Does the quarterly series still appear to be seasonal?
#Yes. Looking at both the head() output and the autoplot, a clear seasonal pattern persists: Q1 and Q4 consistently show higher production than Q2 and Q3, which mirrors the monthly pattern found in 2.6b (December highest, June lowest)

#2.7. Plotting with autoplot()

#2.7a. Add the following to plot your U.S. retail sales data with just one line:

retail_ts |> autoplot(sales)

#2.7b. Now layer ordinary ggplot2 code on top of the autoplot() call, the same way you’d add layers to any ggplot():

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

#What repeating pattern within each year do you see in the NSA series (for example, a spike or a drop at the same time every year)? Does the SA series have this pattern?
#Do the two series follow the same path over the long run, or does seasonal adjustment also change the overall trend and the larger ups and downs (such as recessions)?

#Ans:
#The NSA series (RSAFSNA, teal line) shows a sharp spike every December, followed by a steep drop in January/February, and this jagged pattern repeats every single year. The SA series (RSAFS, red line) is notably smoother, with this repeating within-year oscillation largely removed.

#The two series follow essentially the same long-run upward trend and experience the same large cyclical swings together (like the drops visible around 2008–2009 and 2020 are present in both lines). This shows that seasonal adjustment only removes the predictable, repeating within-year pattern which does not alter the overall trend or business-cycle fluctuations.

#2.7c. Plot Canadian gas production with title, axis labels, and theme_minimal()

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    x     = NULL,
    y     = "Volume (billions of cubic meters)"
  ) +
  theme_minimal()


#2.7d. Adding geom_smooth() on top of the can_gas autoplot

can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
  labs(
    title = "Canadian Natural Gas Production with Trend Smoother",
    x     = NULL,
    y     = "Volume (billions of cubic meters)"
  ) +
  theme_minimal()

#Include one sentence on what you added and why:
  #I added geom_smooth(method = "loess") to can_gas because the strong seasonal oscillations make the underlying long-run trend difficult to see in the raw series, and the smoother traces that trend clearly in red without obscuring the actual data.








