
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
#The NSA series (RSAFSNA, teal line) shows a sharp spike every December, followed by a steep drop in January/February,http://127.0.0.1:20281/graphics/34a0c3dc-675f-4e8c-ad14-4df0e885298c.png and this jagged pattern repeats every single year. The SA series (RSAFS, red line) is notably smoother, with this repeating within-year oscillation largely removed.

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

#Part 3 – Data Transformations (Logs and Growth Rates)

#3.1. Log Transformation
#3.1a. Look back at the plot of can_gas (from 2.7c) and in particular at the seasonal swings in the data. Recall that if the seasonal swings grow with the level of the series, the series is a good candidate for a log transformation. 

#Ans:
#Do the swings get larger as the series grows?
  #Yes, broadly speaking — the seasonal swings do grow in absolute size as the level of the series rises. In the early 1960s, when production was around 1.5–4 billion cubic meters, the peaks and troughs within each year are narrow and close together. By the mid-1970s to early 1980s, as the level climbs toward ~8–10 BCM, the swings are noticeably wider in absolute terms.
#Is that true over the whole sample?
  #Not uniformly. There are two notable departures:
  #1.	Around 1980–1985, the series actually dips — production temporarily falls back — and during this period the swings shrink, which is consistent with proportional seasonality (smaller level → smaller swings).
#2.	Post-1990, once the series surges past ~12–15 BCM, the swings become very large and increasingly erratic/irregular. They don't grow smoothly in proportion to the level; they become jagged and volatile, suggesting that something beyond a simple proportional relationship is occurring in the later part of the sample.

# 3.1b. Add log_volume to can_gas and plot both level and log series

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

# Plot the level series (for direct comparison with the log series below)
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production (Level)",
    x     = NULL,
    y     = "Volume (billions of cubic meters)"
  ) +
  theme_minimal()

# Plot the logged series
can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Canadian Natural Gas Production (Log Scale)",
    x     = NULL,
    y     = "log(Volume)"
  ) +
  theme_minimal()
# Written answer (3.1b):
# After taking logs, the upward trend becomes more linear and the seasonal
# swings are more uniform in absolute size across the sample, rather than
# appearing to grow or shrink with the level of the series.


# 3.1c. Log transformation for the NSA retail series

retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

# Plot the level series
retail_nsa |>
  autoplot(sales) +
  labs(
    title = "NSA Retail Sales (Level)",
    x     = NULL,
    y     = "Millions of dollars"
  ) +
  theme_minimal()

# Plot the logged series
retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "NSA Retail Sales (Log Scale)",
    x     = NULL,
    y     = "log(Sales)"
  ) +
  theme_minimal()

# Written answer (3.1c):
# The case for logging RSAFSNA is stronger than it was for can_gas. Retail
# sales have grown roughly exponentially over the full sample, and the
# December seasonal spike grows visibly larger in dollar terms as the series
# rises, which is exactly the pattern logs are designed to stabilize. After
# logging, the seasonal swings become more constant in size, whereas for
# can_gas the relationship between the level and swing size was less
# consistent across the whole sample.

#3.2. Growth Rates
#3.2a. Using can_gas, add code to your script to compute the month-over-month percentage growth rate in two different ways

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

# Plot growth_direct with autoplot()
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-Month Growth Rate: Canadian Gas Production (Direct)",
    x     = NULL,
    y     = "Growth rate (%)"
  ) +
  theme_minimal()

# Add growth_gap (absolute difference between the two methods) and plot it
can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Gap Between Direct and Log-Difference Growth Rate",
    subtitle = "Absolute difference between growth_direct and growth_logdiff",
    x     = NULL,
    y     = "Absolute difference (percentage points)"
  ) +
  theme_minimal()

#The log-difference approximation is reasonably good for most of the sample as the gap is typically well below 0.5 percentage points but it noticeably breaks down in the early 1960s.



#3.3. SA vs. NSA Growth Rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

#Ans:
#The NSA series (RSAFSNA) is far noisier in growth-rate terms, with month-over-month swings regularly exceeding ±20 percentage points driven by a strong, repeating seasonal pattern (a large positive jump every December followed by a sharp negative reading every January). The SA series (RSAFS) is much smoother, with fluctuations typically staying well within ±5 percentage points, because the predictable seasonal component has already been stripped out before the growth rate is computed. This connects directly to Part 2.7b: the pronounced December spike and steep January drop visible in the NSA level series translate one-for-one into those large oscillating growth rates, whereas the SA series, which removed exactly that repeating within-year pattern, produces a growth rate series that reflects only trend momentum and irregular shocks.

#Part 4 – Seeing, Decomposing, and Modeling Seasonality

#4.1. gg_season() – A Season Plot

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b. Now make the same plot for the SA series (RSAFS). 

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y     = "Millions of dollars"
  ) +
  theme_minimal()

#In a sentence or two, contrast the two season plots. This should make concrete, visually, exactly what “seasonally adjusted” means: the SA series is the NSA series with the repeating seasonal pattern estimated and removed.
#Ans:
#In the NSA season plot, every year's line traces the same distinctive wave,dipping in February, climbing through spring, softening briefly mid-summer, and then surging into December so the lines are stacked nearly on top of one another in shape, which is the visual signature of a strong, repeating seasonal pattern. In the SA season plot, that shared wave is gone: the lines run nearly flat and horizontal across all twelve months, confirming that seasonal adjustment has stripped out exactly that predictable within-year rhythm while leaving the year-to-year level differences (i.e., the long-run growth) intact.

# 4.1c. Season plot for Canadian gas production (logged series)
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y     = "log(Volume)"
  ) +
  theme_minimal()

#Describe the seasonal pattern you see in a sentence. Which months tend to be high, which tend to be low, and does that match your intuition about natural gas production in Canada? (Hint: think about what natural gas is used for.)

#Ans:
#Production is highest in the winter months (January to March), with a clear peak around March, and lowest in the summer months (Juneto September), with the trough falling around June/July, and this pattern repeats consistently across all years, as shown by every year's line tracing nearly the same shape. 
#This matches intuition that natural gas in Canada is used primarily for space heating, so demand and production both surge in the cold winter months and fall sharply when heating is no longer needed in summer.

#4.2. gg_subseries() – A Subseries Plot

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

#4.3. Seasonally Adjusting a Series with X-13ARIMA-SEATS

# 4.3a. Run X-13ARIMA-SEATS on the NSA series and plot components
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

#In a sentence or two: in which month is the seasonal factor highest, and roughly how far above trend are sales in that month? Does the size of the seasonal swings appear to change over time?

#Ans:
#The seasonal factor is highest in December, reaching approximately 1.20, meaning retail sales in December are typically about 20% above the trend-cycle, driven by holiday shopping. The seasonal factor is lowest in January/February, dipping to roughly 0.85–0.87 (about 13–15% below trend), reflecting the post-holiday spending drop. The size of the seasonal swings appears remarkably stable over time.

#4.3b 
#The season_adjust column of retail_nsa_x13 is your seasonally adjusted series.
#Compare it to the official seasonally adjusted series by computing the percent difference between them (using the log-difference approximation) and plotting it

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()



#My X-13 adjustment tracks the Census Bureau's official series very closely for the vast majority of the sample — from 1992 through roughly 2018
#The two adjustments diverge most dramatically around 2020–2022, where the difference spikes to nearly +1.5% before swinging down to approximately −1%, reflecting the fact that the Census Bureau applied custom outlier corrections
#while my default X-13 run had no such tailored settings and therefore estimated a different seasonal factor for those anomalous months. There is also a modest, smaller widening around 2008–2010.

#4.4. A Simple Model – Linear Trend + Seasonal Dummies

#4.4a. Add code to your script to fit a linear trend plus seasonal dummy variables model to the log of the can_gas “Volume” variable:
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

#Report the estimated coefficients from the model in a table (useful for write up)
can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#What is the estimated average monthly growth rate of gas production? Which month has the most significant seasonal effect (largest t-statistic in absolute value)?
#Ans:
#The trend() coefficient is 3.973 × 10⁻³ (≈ 0.004). Multiplied by 100, this gives an estimated average monthly growth rate of approximately 0.40% per month.
#June has the most significant seasonal effect (|t| = 4.777), with a coefficient of −0.251


#4.4b. Now plot the fitted values from this model against the actual data:
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#In a sentence, describe how well the fitted line tracks the actual series — where does it do well, and where does it visibly miss?
#Ans:
#The fitted line tracks the overall upward trend and the repeating seasonal oscillations reasonably well through the middle of the sample (roughly 1975–1995), where the two lines run close together
#it visibly overestimates actual production in the early period (1960s–early 1970s, where the fitted line runs consistently above the actual) and underestimates it in the later period (post-2000, where the actual series surges well above the fitted).

#4.4c. 
can_gas_fit |> gg_tsresiduals()

#Ljung–Box test
can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

#Ans:
#The residuals are clearly not white noise. The time plot reveals a prominent slow-moving, hump-shaped pattern — residuals drift from strongly negative (~−1.0) in the early 1960s, peak near +0.5 around 1970, and then gradually decline through 2005 — indicating that the linear trend badly mischaracterizes the true shape of the trend in the data.
#The ACF confirms this dramatically: autocorrelations remain very large (above 0.50) at virtually every lag out to lag 27, far exceeding the blue significance bands, which is a hallmark of highly persistent, unmodeled structure.

#The Ljung–Box test, with a test statistic of approximately 7,774 and a p-value of essentially 0, strongly rejects the null hypothesis of white noise at the 5% level
#confirming that there is substantial forecastable structure remaining in the residuals that the simple linear trend + seasonal dummies model has entirely failed to capture


#Part 5 – A First Forecast Evaluation

#5.1. An Expanding-Window TS-CV

#5.1a
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#What are the dates of the first and last forecasts that are evaluated? Report the MAE and RMSE of each model in a table. Which model forecasts more accurately? Do the two metrics agree?

#Ans:
#The data begins in January 1960, and the first training window spans 240 months (20 years), covering January 1960 – December 1979. Therefore, the first forecast evaluated is for January 1980.
#As noted in the question, the very last training sample ends in February 2005 (the final data point) and produces an unobservable forecast. So the last forecast evaluated is for February 2005, produced from a training window ending in January 2005.

#The SNAIVE model is far more accurate, with an MAE of 0.562 vs. 4.01 and an RMSE of 0.702 vs. 4.36 for the TSLM. This is consistent with the finding in 4.4c: the linear trend + seasonal dummies model leaves substantial unmodeled structure, so when back-transformed to original units its forecasts can be quite poor.

#Do the two metrics agree? Yes, both MAE and RMSE unanimously favour SNAIVE over TSLM by a wide margin.


#5.2. Is the Difference Significant?

#5.2a.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#Report the DM statistic and p-value. In a sentence or two, state your conclusion in words


#5.3. A Rolling Window

#5.3a.













