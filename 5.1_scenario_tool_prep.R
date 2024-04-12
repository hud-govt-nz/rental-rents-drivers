# Drivers of rents predictive tool using the linear models from the Rents Paper
# Mariona, 2024

rm(list = ls())

library(DBI)
library(tidyverse)
library(lubridate)
library(devtools)
library(readxl)
library(zoo)
library(psych) # for geometric.mean function
library(stats)
library(dbplyr)
library(questionr)
library(ggpubr)
library(lmtest)
library(cowplot)
library(ggrepel)
library(openxlsx)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(aTSA)
library(seasonal)
library(forecast)
library(corrplot)
library(margins)
library(texreg)
library(broom)
library(ShapleyValue)
library(urca)
library(vars)

# Nam's legacy patches of theme code
# con <- dbConnect(odbc::odbc(), "HUD", database = "property",
#                  uid = "Property_ReadOnly", pwd = "88fs97TaQbvU", timeout = 1000)

col <- c("#003E52", "#FFC04A",  "#62B6F3", "#A4343E", "#00826E","#DEDEDE")

source("utility_functions.R")

options(scipen=999)

theme1 = function(base_size=12) {
  theme_minimal() +
    theme(text = element_text(size = base_size, color="grey30"),
          plot.title = element_text(size=18),
          plot.subtitle = element_text(size=12),
          plot.caption = element_text(size=10),
          legend.text = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.title = element_text(size=12),
          strip.text = element_text (color="grey30", size=10, margin = margin(0,.5,0,.5, "cm")),
          panel.spacing = unit(1, "lines"))
}

# #### 1. Load data ----
# 
# combined_nz <- readRDS("data/combined_nz_20230308.RDS") %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(vacant_time = ifelse(year(date)<=2000, NA, vacant_time)) 
# 
# ### Do all the seasonal adjustments of the time series
# 
# # Seasonally adjusted RPI
# # Create a time series object (rpi_ts) for the RPI (Retail Price Index) data:
# # The RPI data is filtered to include only the rows where the month of the date is in the set {3, 6, 9, 12}. The frequency of the time series is set to 4 (quarterly data), and the start time is set to the first date in the dataset.
# rpi_ts <- ts(filter(combined_nz, month(date) %in% c(3,6,9,12))$rpi, freq = 4 , start = as.yearqtr(combined_nz$date[1]))
# 
# # Seasonally adjusted
# # Use the X-11 method which works by decomposing the data into three components: trend-cycle, seasonal, and irregular. The seasonal component is then removed to obtain the seasonally adjusted data.
# rpi_ts_sa <- seas(rpi_ts, x11 = "")
# 
# rpi_sa <- data.frame(rpi_ts_sa$data,  date = as.Date(time(trend(rpi_ts_sa)))) %>%
#   dplyr::select(date, seasonaladj) %>%
#   rename(rpi_sa = seasonaladj) %>%
#   mutate(date = date_to_quarter(date))
# 
# # Join the seasonally adjusted series to the main df
# combined_nz <- combined_nz %>%
#   left_join(rpi_sa) 
# 
# # Add real rents and real earnings measure
# combined_nz <- combined_nz %>%
#   mutate(rpi = as.numeric(rpi)) %>%
#   arrange(desc(date)) %>%
#   fill(cpi) %>% # propagate the last non-missing value forward
#   fill(cpi_exRent) %>% # propagate the last non-missing value forward
#   arrange(date) %>%
#   mutate(rpi_real = rpi/cpi_exRent*1000, # Calculate real values of RPI HPI and earnings by divving them up by CPI
#          earnings_real = earnings/cpi_exRent*1000,
#          hpi_real = hpi/cpi_exRent*1000) %>%
#   mutate(rpi_real = rpi/cpi*1000,
#          earnings_real = earnings/cpi*1000,
#          hpi_real = hpi/cpi*1000)
# 
# # Seasonally adjust the variable vacant time
# # Put into timeseries
# vacant_time_ts <- ts(combined_nz$vacant_time, freq = 12 , start = as.yearmon("1993-01-31"))
# # Seasonally adjusted
# vacant_time_ts_sa <- seas(vacant_time_ts, x11 = "")
# 
# vacant_time_sa <- data.frame(vacant_time_ts_sa$data,  date = as.Date(time(trend(vacant_time_ts_sa)))) %>%
#   dplyr::select(date, seasonaladj) %>%
#   rename(vacant_time_sa = seasonaladj) %>%
#   mutate(date = date_to_month(date))
# 
# combined_nz <- combined_nz %>%
#   left_join(vacant_time_sa)
# 
# # Calculate changes (annual - 12 or quarterly - 3)
# # # Quarterly:
# # t = 3
# # i = 1
# 
# # Annual
# t = 12
# i = 4
# 
# # This pipe arrange the dataset in ascending order based on the date column.
# # Create new variables for the population to dwellings ratio (ppd) and adult population to dwellings ratio (adult_ppd).
# # Calculate the percentage change for various variables in the dataset, such as RPI, earnings, population, HPI, and others, using the chosen time period (t). The change is calculated as (current_value / previous_value - 1) * 100, where the previous value is obtained using the lag() function with the lag value set to t.
# # For some variables, demeaned values and demeaned changes are also calculated by subtracting the mean of the respective variable from the original value or change. This process removes the average effect across the entire dataset, allowing for the analysis of deviations from the mean.
# 
# # Create dummy variables to represent the COVID-19 pandemic (covid_dummy), the Global Financial Crisis (gfc_dummy), and the post-2016 and post-2020 periods (post2016 and post2020).
# # Calculate changes for additional variables, such as the unemployment rate (unemp.change) and the rent-wage ratio (rent_wage_ratio.change).
# # Filter the dataset to create a new dataset called combined_nz_quarterly, which includes only the data for the end of each quarter (March, June, September, and December). It also filters the data to start from "2003-12-31".
# # In the combined_nz_quarterly dataset, calculate the rolling average change in earnings (earnings.change.rolling) and its demeaned value (earnings.change.rolling.demeaned).
# 
# combined_nz <- combined_nz %>%
#   arrange(date) %>%
#   # using seasonally adjusted RPI
#   # mutate(rpi = rpi_sa) %>%
#   # using quarter average rpi rather than end-of-quarter RPI
#   # mutate(rpi_test = (rpi + lag(rpi,t) + lag(rpi,t+3))/3) %>%
#   # using special 2 year interest rate instead of floating rate
#   #mutate(mrate= mrate_2y) %>%
#   # seasonally adjusted vacant time
#   #mutate(vacant_time = vacant_time_sa) %>%
#   # Use stats estimates for longer time-series
#   mutate(ppd = pop/dwells_nz_stats) %>%
#   mutate(adult_ppd = adult_pop/dwells_nz_stats) %>%
#   mutate(rpi.log = log(rpi)) %>%
#   mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
#   mutate(rpi_flow.change = (rpi_stats_flow/lag(rpi_stats_flow,t)-1)*100) %>%
#   mutate(rpi_stock.change = (rpi_stats_stock/lag(rpi_stats_stock,t)-1)*100) %>%
#   mutate(rent_geomean.change = (rent_geomean/lag(rent_geomean,t)-1)*100) %>%
#   mutate(cpi_rent.change = (cpi_rent/lag(cpi_rent,t)-1)*100) %>%
#   mutate(rpi_real.change = (rpi_real/lag(rpi_real,t)-1)*100) %>%
#   mutate(income.change = (income/lag(income,t)-1)*100) %>%
#   mutate(earnings.change = (earnings/lag(earnings,t)-1)*100) %>%
#   mutate(earnings.change.demeaned = earnings.change - mean(earnings.change,na.rm = T)) %>%
#   mutate(earnings_leed.change = (earnings_leed/lag(earnings_leed,t)-1)*100) %>%
#   mutate(earnings_real.change = (earnings_real/lag(earnings_real,t)-1)*100) %>%
#   mutate(earnings_sa.change = (earnings_sa/lag(earnings_sa,t)-1)*100) %>%
#   mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
#   mutate(dwells_nz_stats.change = (dwells_nz_stats/lag(dwells_nz_stats,t)-1)*100) %>%
#   mutate(dwells_nz_stats.change.demeaned = dwells_nz_stats.change - mean(dwells_nz_stats.change,na.rm = T)) %>%
#   
#   mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
#   mutate(pop.change.demeaned = pop.change - mean(pop.change,na.rm = T)) %>%
#   
#   mutate(adult_pop.change = (adult_pop/lag(adult_pop,t)-1)*100) %>%
#   mutate(hpi.change = (hpi/lag(hpi,t)-1)*100) %>%
#   mutate(hpi_real.change = (hpi_real/lag(hpi_real,t)-1)*100) %>%
#   mutate(cpi_exRent.change = (cpi_exRent/lag(cpi_exRent,t)-1)*100) %>%
#   mutate(cpi.change = (cpi/lag(cpi,t)-1)*100) %>%
#   mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
#   mutate(ppd.change = (ppd/lag(ppd,t)-1)*100) %>%
#   mutate(ppd.change.demeaned = ppd.change - mean(ppd.change,na.rm = T)) %>%
#   mutate(adult_ppd.change = (adult_ppd/lag(adult_ppd,t)-1)*100) %>%
#   mutate(earnings_leed.change.demeaned = earnings_leed.change - mean(earnings_leed.change,na.rm = T)) %>%
#   mutate(earnings_real.change.demeaned = earnings_real.change - mean(earnings_real.change,na.rm = T)) %>%
#   mutate(income.change.demeaned = income.change - mean(income.change,na.rm = T)) %>%
#   mutate(vacancy.demeaned = vacancy - mean(vacancy,na.rm = T)) %>%
#   mutate(vacancy.change = vacancy - lag(vacancy,t)) %>%
#   mutate(vacant_time.demeaned = vacant_time - mean(vacant_time,na.rm = T)) %>%
#   # mutate(vacant_time.change = vacant_time-lag(vacant_time,t)) %>%
#   mutate(vacant_time.change = (vacant_time/lag(vacant_time,t)-1)*100) %>%
#   mutate(vacant_time.change = ifelse(is.infinite(vacant_time.change),0,vacant_time.change)) %>%
#   mutate(vacant_time.change.demeaned = vacant_time.change - mean(vacant_time.change,na.rm = T)) %>%
#   mutate(mrate_lagged = mrate + lag(mrate,1) + lag(mrate,2) + lag(mrate,3)) %>%
#   mutate(mrate.change = (mrate/lag(mrate,t)-1)*100) %>%
#   #mutate(mrate.change = mrate-lag(mrate,t)) %>%
#   
#   
#   mutate(
#     covid_dummy = ifelse(year(date) %in% c(2020), 1, 0),
#     post2016 = ifelse(year(date) >=2016, 1, 0),
#     post2020 = ifelse(year(date) >=2020, 1, 0),
#     gfc_dummy = ifelse(year(date) %in% c(2007,2008,2009),1,0),
#     recession_dummy =ifelse(year(date) %in% c(2007,2008, 2020,2021), 1, 0),
#     tax_dummy = ifelse(year(date) >=2021, 1, 0),
#     time = ifelse(year(date)<2007, 1,
#                   ifelse(year(date)>=2007 & year(date)<=2014,2,3))) %>%
#   mutate(unemp.demeaned = unemp - mean(unemp, na.rm = T)) %>%
#   # mutate(unemp.change = unemp - lag(unemp,t)) %>%
#   mutate(unemp.change = (unemp/lag(unemp,t)-1)*100) %>%
#   
#   mutate(rent_long.change = (rent_geomean.change + cpi_rent.change)/2) %>%
#   mutate(rent_wage_ratio.change = rpi.change - earnings.change)
# 
# combined_nz_quarterly <- combined_nz %>%
#   filter(month(date) %in% c(3,6,9,12)) %>%
#   mutate(earnings.change.rolling = (earnings.change + lag(earnings.change,1))/2) %>%
#   mutate(earnings.change.rolling.demeaned = earnings.change.rolling - mean(earnings.change.rolling,na.rm = T)) %>%
#   
#   #filter(month(date) %in% c(9)) %>%
#   filter(date >= "2003-12-31") 
# 
# 
# test <- combined_nz_quarterly %>%
#   dplyr::select(rpi.change,earnings.change,ppd.change,mrate.change,cpi_exRent.change,hpi.change, unemp)
# 
# saveRDS(combined_nz, 'data/scenario_tool/combined_df.RDS')
# saveRDS(combined_nz_quarterly, 'data/scenario_tool/combined_df_quarterly.RDS')

#### 2. Models ----

#### 2.1 Baseline ----

# # Pick lag order to alternate between quarterly and annually change
# i = 1 # for quarterly 
# i = 4 # for annual

# Stationary test
rpi_ts <- ts(combined_nz_quarterly$earnings.change, freq = 4 , as.yearqtr(combined_nz_quarterly$date[1])) 
plot(rpi_ts)

rpi_ts_real <- ts(combined_nz_quarterly$rpi_real.change, freq = 4 , as.yearqtr(combined_nz_quarterly$date[1])) 
plot(rpi_ts_real)
tseries::adf.test(na.omit(rpi_ts))
Box.test(na.omit(rpi_ts), lag=4, type="Ljung-Box")
summary(ur.kpss(rpi_ts))

# The ADF test suggests that the earnings.change time series is non-stationary, while the KPSS test indicates that it is stationary. The Ljung-Box test shows evidence of autocorrelation in the data. These mixed results can make it difficult to determine the true nature of the time series, and further analysis or data transformations might be needed to address the issue of stationarity and autocorrelation.

# Lag selection for independent variable
Acf(rpi_ts, lag.max = 12)
Pacf(rpi_ts, lag.max = 12, plot = TRUE, na.action = na.contiguous)
VARselect(na.omit(rpi_ts), lag.max = 3, type = "const")
VARselect(na.omit(test), lag.max = 5, type = "const")

# The ACF plot shows a drop after lag 3 and the PACF plot shows a drop after lag 4, you can infer the following:
#   ACF plot: Since the ACF plot shows a drop after lag 3, it suggests that there is a Moving Average (MA) component of order 3 in the time series model. This is because the autocorrelation coefficients drop sharply after the third lag and remain close to zero afterwards.
# PACF plot: As the PACF plot shows a drop after lag 4, it indicates that there is an Autoregressive (AR) component of order 4 in the time series model. This is because the partial autocorrelation coefficients drop sharply after the fourth lag and remain close to zero afterwards.


# There are a total of 14 linear regression models. These models are variations of the baseline model with different combinations of independent variables and alternative dependent variables. They are used to study the relationships between rent inflation (the dependent variable) and various economic factors (the independent variables).
# 
# Here's a summary of the models and their characteristics:
# model1a (Baseline all variables): Includes all independent variables with their respective lags. Includes contemporaneous variables (lag 0).
# model1b (Remove some variables - contemporaneous): Removes contemporaneous variables and keeps only lagged variables.
# model1b2 (Remove house price related): Removes the house price inflation variable and keeps all other lagged variables.
# model1b3 (Remove both mortgage rate and house price): Removes house price inflation and mortgage rate variables and keeps all other lagged variables.
# model1c (Alternative dependent - RPI stock): Replaces the flow change of rent price inflation with the stock change as the dependent variable. Keeps the same set of independent variables as model1b2.
# model1d (Alternative independent - Real rent flow): Replaces the nominal change in rent price inflation with the real change as the dependent variable. Replaces the nominal wage change with the real wage change. Keeps the same set of independent variables as model1b2.
# model1e (Adult people per dwelling): Replaces the people per dwelling variable with the adults per dwelling variable. Keeps the same set of independent variables as model1b2.
# model1f (Separate population and dwellings change): Includes both population growth and dwellings growth variables instead of people per dwelling. Keeps the same set of independent variables as model1b2.
# model1g (Vacant time as an alternative measure): Replaces the people per dwelling variable with the change in vacant time. Keeps the same set of independent variables as model1b2.
# model1h (Annual change and t-4 lags): Changes the lag structure to annual lags (t-4) instead of quarterly lags. Keeps the same set of independent variables as model1b2.
# model2a (Interaction - Rent): Includes an interaction term between wage growth (rolling two-period average) and people per dwelling (lagged).
# 
# model2b (Interaction with Time Factor): Includes an interaction term between wage growth (rolling two-period average), people per dwelling (lagged), and a time factor.
# model2c (Interaction with Vacant Time): Includes an interaction term between wage growth (rolling two-period average) and change in vacant time (lagged).
# tab_model (Model summary): This is not a separate model but a summary of the selected models (model1b2, model1c, model1d) to be displayed in a table format.


#### 1. Baseline all variables ----
model1a <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              #+lag(rpi.change,i+1) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i)
              #+lag(earnings.change,i+1)
              
              +lag(ppd.change,0)
              +lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              
              +lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              +lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              +lag(unemp.change,0)
              +lag(unemp.change,i)
              #+lag(unemp.change,i+1)
              
              +lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              #+factor(quarter(date))
              
              ,
              data = combined_nz_quarterly) 

tab_model(model1a)

#### 2. Remove some variables ----
# Remove contemporaneous
model1b <- lm(rpi.change ~ 
                +lag(rpi.change,i) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

# Run some tests
tab_model(model1b)

bptest(model1b)
coeftest(model1b, vcov = vcovHC(model1b, type = "HC0"))
BIC(model1b)
AIC(model1b, k=2)

# Remove house price related
model1b2 <- lm(rpi.change ~ 
                 lag(rpi.change,i) 
               +lag(earnings.change,0) 
               +lag(earnings.change,i) 
               +lag(ppd.change,i)
               +lag(mrate.change,i)
               +lag(cpi_exRent.change,i)
               +lag(unemp.change,i)
               #+lag(hpi.change,i)
               ,
               data = combined_nz_quarterly) 

tab_model(model1b2)

# Remove both mortgage rate and house price

model1b3 <- lm(rpi.change ~ 
                 lag(rpi.change,i) 
               +lag(earnings.change,0) 
               +lag(earnings.change,i) 
               +lag(ppd.change,i)
               #+lag(mrate.change,i)
               +lag(cpi_exRent.change,i)
               +lag(unemp.change,i)
               #+lag(hpi.change,i)
               ,
               data = combined_nz_quarterly) 

# Run some tests
tab_model(model1b3)


#### 4. Put everything in a table ----
labels <- c("Rent inflation (lagged)", 
            "Wage growth",
            "Wage growth (lagged)",
            "People per dwelling", 
            "People per dwelling (lagged)", 
            "Mortgage rate",
            "Mortgage rate (lagged)",
            "Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            "Unemployment rate",
            "Unemployment rate (lagged)",
            "House price inflation",
            "House price inflation (lagged)"
)

tab_model(model1a, model1b, model1b2, model1b3,
          dv.labels = c("Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)"),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          file = "results_updated/baseline.doc") 




#### 5. Altenative dependent: RPI stock ----
# Replace flow change with stock change
combined_nz_quarterly_stock <- combined_nz_quarterly %>%
  mutate(rpi.change = rpi_stock.change)

model1c <- lm(rpi.change ~ 
                lag(rpi.change,i)
              +lag(earnings.change,0) 
              +lag(earnings.change,i)
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly_stock) 

tab_model(model1c, digits = 3)

# Run some tests
bptest(model1c)
coeftest(model1c, vcov = vcovHC(model1c, type = "HC0"))
BIC(model1c)
AIC(model1c, k=2)

#### 6. Alternative independent: Real rent flow ----
# Replace nominal change with real change
combined_nz_quarterly_real <- combined_nz_quarterly %>%
  mutate(rpi.change = rpi_real.change) %>%
  #mutate(hpi.change = hpi_real.change) %>%
  mutate(earnings.change = earnings_real.change)
# mutate(earnings_real.change2 = earnings.change - cpi_exRent.change) %>%
# dplyr::select(date, earnings.change, earnings_real.change, earnings_real.change2, cpi_exRent.change)

model1d <- lm(rpi.change ~ 
                lag(rpi.change,i)
              +lag(earnings.change,0) 
              +lag(earnings.change,i)
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(unemp.change,i)
              # +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly_real) 

tab_model(model1d)

# Run some tests
bptest(model1d)
coeftest(model1d, vcov = vcovHC(model1d, type = "HC0"))
BIC(model1d)
AIC(model1d, k=2)


# Put everything in a table 
labels <- c("Rent inflation (lagged)", 
            "Wage growth",
            "Wage growth (lagged)",
            #"People per dwelling", 
            "People per dwelling (lagged)", 
            #"Mortgage rate",
            "Mortgage rate (lagged)",
            #"Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            #"Unemployment rate",
            "Unemployment rate (lagged)"
            #"House price inflation",
            #"House price inflation (lagged)"
)

tab_model(model1b2, model1c, model1d,
          dv.labels = c("Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, stock)",
                        "Rent inflation (real, flow)"),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          file = "results_updated/baseline_alternative_depvar.doc") 

#### 6. Alternative specs - Independent variables

# Adult people per dwelling
model1e <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(adult_ppd.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

tab_model(model1e)

# Separate population and dwellings change
model1f <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(pop.change,i)
              +lag(dwells_nz_stats.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

tab_model(model1f)


# Vacant time as alternative measure
model1g <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(vacant_time.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

tab_model(model1g)


labels <- c("Wage",
            "Wage (lagged)",
            "People per dwelling (lagged)", 
            "Adults per dwelling (lagged)",
            "Population growth (lagged)",
            "Dwellings growth (lagged)",
            "Change in vacant time (lagged)"
)

tab_model(model1b, model1e, model1f, model1g,
          dv.labels = rep("Rent inflation (nominal, flow)", 4),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter|rpi|mrate|cpi|unemp|hpi",
          file = "results_updated/baseline-alternative-indepvar.doc") 


# Annual change and t-4 lags
# Change t and i first
model1h <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

tab_model(model1h)


labels <- c("Rent inflation (t-4)", 
            #"Rent inflation (2nd lag)", 
            #"Rent inflation (3rd lag)",
            "Wage",
            "Wage (t-4)",
            #"People per dwelling", 
            "People per dwelling (t-4)", 
            #"Floating mortgage rate",
            "Floating mortgage rate (t-4)",
            #"Inflation excluding rents",
            "Inflation excluding rents (t-4)",
            #"Unemployment rate",
            "Unemployment rate (t-4)"
            #"House price",
            #"House price (lagged)"
            #"Covid dummy"
)

tab_model(model1h,
          dv.labels = c("Annual rent inflation (nominal, flow)"),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter",
          file = "results_updated/baseline_annual.doc") 


# Interaction
# Interaction rent
model2a <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change.rolling.demeaned,0) * lag(ppd.change.demeaned,i)
              +lag(ppd.change.demeaned,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = filter(combined_nz_quarterly))

tab_model(model2a)

model2b <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change.rolling.demeaned,0) * lag(pop.change.demeaned,i)
              +lag(earnings.change.rolling.demeaned,0) * lag(dwells_nz_stats.change.demeaned,i)
              #+lag(ppd.change.demeaned,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = filter(combined_nz_quarterly))

tab_model(model2b)


labels <- c("Wage (rolling two-period average)",
            "People per dwelling (lagged)", 
            "Wage x People per dwelling"
            #"Population growth (lagged)",
            #"Dwellings growth (lagged)",
            #"Wage x Population growth",
            #"Wage x Dwellings growth"
)

tab_model(model2a, 
          dv.labels = rep("Quarterly rent inflation (nominal, flow)",1),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter|rpi|mrate|cpi|unemp",
          file = "results_updated/baseline_interaction.doc")


model2b <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change.rolling.demeaned,0) * lag(ppd.change.demeaned,i)   * factor(time)
              #+lag(earnings.change.demeaned,0) 
              #+lag(earnings.change.demeaned,i) 
              
              +lag(ppd.change.demeaned,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly)

tab_model(model2b)

model2c <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change.rolling.demeaned,0) * lag(vacant_time.change.demeaned,i)
              #+lag(ppd.change.demeaned,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp.change,i)
              #+lag(hpi.change,i)
              ,
              data = combined_nz_quarterly)

tab_model(model2c, digits = 2)



#### 4. Put everything in a table ----
labels <- c("Rent inflation (lagged)", 
            "Wage growth",
            "Wage growth (lagged)",
            "People per dwelling", 
            "People per dwelling (lagged)", 
            "Mortgage rate",
            "Mortgage rate (lagged)",
            "Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            "Unemployment rate",
            "Unemployment rate (lagged)",
            "House price inflation",
            "House price inflation (lagged)"
)

tab_model(model1a, model1b, model1b2, model1b3,
          dv.labels = c("Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)"),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          file = "results_updated/baseline.doc") 





# Use the model 1f to make future predictions
# Separate population and dwellings change
# model1f <- lm(rpi.change ~ 
#                 lag(rpi.change,i) 
#               
#               +lag(earnings.change,0) 
#               +lag(earnings.change,i) 
#               +lag(pop.change,i)
#               +lag(dwells_nz_stats.change,i)
#               +lag(mrate.change,i)
#               +lag(cpi_exRent.change,i)
#               +lag(unemp.change,i)
#               #+lag(hpi.change,i)
#               ,
#               data = combined_nz_quarterly) 
# 
# tab_model(model1f)

# Save the models to a file  
save(model1a, model1b, model1b2, model1b3, model1c, model1d, model1e, model1f, model1g, model1h, model2a, model2b, model2c, file = "data/scenario_tool/models.RData")  


