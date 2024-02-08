# Drivers of rents regression model
# Nam Ngo, October 2022

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

#### 1. Load data ----

combined_nz <- readRDS("data/combined_nz_20230308.RDS") %>%
  mutate(date = as.Date(date)) %>%
  mutate(vacant_time = ifelse(year(date)<=2000, NA, vacant_time))

# Seasonally adjusted RPI
# Put into timeseries
rpi_ts <- ts(filter(combined_nz, month(date) %in% c(3,6,9,12))$rpi, freq = 4 , start = as.yearqtr(combined_nz$date[1]))
# Seasonally adjusted
rpi_ts_sa <- seas(rpi_ts, x11 = "")

rpi_sa <- data.frame(rpi_ts_sa$data,  date = as.Date(time(trend(rpi_ts_sa)))) %>%
  dplyr::select(date, seasonaladj) %>%
  rename(rpi_sa = seasonaladj) %>%
  mutate(date = date_to_quarter(date))

combined_nz <- combined_nz %>%
  left_join(rpi_sa) 




# Add real rents and real earnings measure
combined_nz <- combined_nz %>%
  mutate(rpi = as.numeric(rpi)) %>%
  arrange(desc(date)) %>%
  fill(cpi) %>%
  fill(cpi_exRent) %>%
  arrange(date) %>%
  mutate(rpi_real = rpi/cpi_exRent*1000,
         earnings_real = earnings/cpi_exRent*1000)

# Seasonally adjusted Vacant time
# Put into timeseries
vacant_time_ts <- ts(combined_nz$vacant_time, freq = 12 , start = as.yearmon("1993-01-31"))
# Seasonally adjusted
vacant_time_ts_sa <- seas(vacant_time_ts, x11 = "")

vacant_time_sa <- data.frame(vacant_time_ts_sa$data,  date = as.Date(time(trend(vacant_time_ts_sa)))) %>%
  dplyr::select(date, seasonaladj) %>%
  rename(vacant_time_sa = seasonaladj) %>%
  mutate(date = date_to_month(date))

combined_nz <- combined_nz %>%
  left_join(vacant_time_sa)


# Calculate changes (annual/quarterly)
t = 3

combined_nz <- combined_nz %>%
  arrange(date) %>%
  # using seasonally adjusted RPI
  #mutate(rpi = rpi_sa) %>%
  # Use stats estimates for longer time-series
  mutate(ppd = pop/dwells_nz_stats) %>%
  mutate(adult_ppd = adult_pop/dwells_nz_stats) %>%
  mutate(rpi.log = log(rpi)) %>%
  mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
  mutate(rpi_flow.change = (rpi_stats_flow/lag(rpi_stats_flow,t)-1)*100) %>%
  mutate(rpi_stock.change = (rpi_stats_stock/lag(rpi_stats_stock,t)-1)*100) %>%
  mutate(rent_geomean.change = (rent_geomean/lag(rent_geomean,t)-1)*100) %>%
  mutate(cpi_rent.change = (cpi_rent/lag(cpi_rent,t)-1)*100) %>%
  mutate(rpi_real.change = (rpi_real/lag(rpi_real,t)-1)*100) %>%
  mutate(income.change = (income/lag(income,t)-1)*100) %>%
  mutate(earnings.change = (earnings/lag(earnings,t)-1)*100) %>%
  mutate(earnings.change.demeaned = earnings.change - mean(earnings.change,na.rm = T)) %>%
  
  mutate(earnings_leed.change = (earnings_leed/lag(earnings_leed,t)-1)*100) %>%
  mutate(earnings_real.change = (earnings_real/lag(earnings_real,t)-1)*100) %>%
  mutate(earnings_sa.change = (earnings_sa/lag(earnings_sa,t)-1)*100) %>%
  mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
  mutate(dwells_nz_stats.change = (dwells_nz_stats/lag(dwells_nz_stats,t)-1)*100) %>%
  mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
  mutate(adult_pop.change = (adult_pop/lag(adult_pop,t)-1)*100) %>%
  mutate(hpi.change = (hpi/lag(hpi,t)-1)*100) %>%
  mutate(cpi_exRent.change = (cpi_exRent/lag(cpi_exRent,t)-1)*100) %>%
  mutate(cpi.change = (cpi/lag(cpi,t)-1)*100) %>%
  mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
  
  mutate(ppd.change = (ppd/lag(ppd,t)-1)*100) %>%
  mutate(ppd.change.demeaned = ppd.change - mean(ppd.change,na.rm = T)) %>%
  
  
  mutate(adult_ppd.change = (adult_ppd/lag(adult_ppd,t)-1)*100) %>%
  
  # mutate(ppd.change = ppd - lag(ppd,t)) %>%
  
  mutate(earnings_leed.change.demeaned = earnings_leed.change - mean(earnings_leed.change,na.rm = T)) %>%
  mutate(earnings_real.change.demeaned = earnings_real.change - mean(earnings_real.change,na.rm = T)) %>%
  mutate(income.change.demeaned = income.change - mean(income.change,na.rm = T)) %>%

  #mutate(vacant_time = vacant_time_sa) %>%
  
  mutate(vacancy.demeaned = vacancy - mean(vacancy,na.rm = T)) %>%
  mutate(vacancy.change = vacancy - lag(vacancy,t)) %>%
  mutate(vacant_time.demeaned = vacant_time - mean(vacant_time,na.rm = T)) %>%
  # mutate(vacant_time.change = (vacant_time/lag(vacant_time,t)-1)*100) %>%
  
  mutate(vacant_time.change = vacant_time-lag(vacant_time,t)) %>%
  
  # mutate(vacant_time.change = (vacant_time/lag(vacant_time,t)-1)*100) %>%
  mutate(mrate_lagged = mrate + lag(mrate,1) + lag(mrate,2) + lag(mrate,3)) %>%
  mutate(mrate.change = (mrate/lag(mrate,t)-1)*100) %>%
  mutate(
    #covid_dummy = ifelse(year(date) %in% c(2020) & month(date) %in% c(3,4,5,6,7,8,9), 1, 0),
        covid_dummy = ifelse(year(date) %in% c(2020), 1, 0),
        post2016 = ifelse(year(date) >=2016, 1, 0),
        post2020 = ifelse(year(date) >=2020, 1, 0),
        
         gfc_dummy = ifelse(year(date) %in% c(2007,2008,2009),1,0),
         recession_dummy =ifelse(year(date) %in% c(2007,2008, 2020,2021), 1, 0),
         tax_dummy = ifelse(year(date) >=2021, 1, 0),) %>%
  mutate(unemp.demeaned = unemp - mean(unemp, na.rm = T)) %>%
  mutate(unemp.change = unemp - lag(unemp,t)) %>%
  
  mutate(rent_long.change = (rent_geomean.change + cpi_rent.change)/2) %>%
  mutate(rent_wage_ratio.change = rpi.change - earnings.change)

combined_nz_quarterly <- combined_nz %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  #filter(month(date) %in% c(9)) %>%
  filter(date >= "2003-12-31") %>%
  mutate(quarter = quarter(date)) %>%
  mutate(ppd.change.average = (ppd.change + lag(ppd.change,1) + lag(ppd.change,2))/3) %>% 
  mutate(earnings.change.average = (earnings.change + lag(earnings.change,1))/2) 


test <- combined_nz_quarterly %>%
  dplyr::select(rpi.change,earnings.change,ppd.change,mrate.change,cpi_exRent.change,hpi.change, unemp)


#### 3. Results ----

#### 3.1 Average weekly earnings ----

# Pick lag order
i = 1

# Test autocorrelation in RPI growth
rpi_ts <- ts(combined_nz_quarterly$rpi.change, freq = 4 , as.yearqtr(combined_nz_quarterly$date[1])) 

plot(rpi_ts)

# Stationary test
tseries::adf.test(na.omit(rpi_ts))
Box.test(na.omit(rpi_ts), lag=4, type="Ljung-Box")
summary(ur.kpss(rpi_ts))


Acf(rpi_ts, lag.max = 8)
Pacf(rpi_ts, lag.max = 8, plot = TRUE, na.action = na.contiguous)


VARselect(na.omit(rpi_ts), lag.max = 3, type = "const")

VARselect(na.omit(test), lag.max = 5, type = "const")

# Baseline
model1a <- lm(rpi.change ~ 
              lag(rpi.change,i) 
             #+lag(rpi.change,i+1) 
            #+lag(rpi.change,i+2)
            # +lag(rpi.change,i+3)
             

             

              
              
              +lag(earnings.change,0) 
             +lag(earnings.change,i)
            #  +lag(earnings.change,i+1) 
             
             
             
             #+lag(income.change,0)
             #+lag(income.change,i)
            # +lag(income.change,i+1)
             
                    
           +lag(ppd.change,0)
            +lag(ppd.change,i)
            #+lag(ppd.change,i+1)
             #+lag(ppd.change,i+2)   

            
            #+lag(adult_ppd.change,0)
            #+lag(adult_ppd.change,i)
             #+lag(adult_ppd.change,i+1)
             #+lag(adult_ppd.change,i+2)   

            #+lag(pop.change,0)
             # +lag(pop.change,i)
            
             # +lag(adult_pop.change,0)
            # +lag(adult_pop.change,i)
            
            #+lag(dwells_nz_stats.change,0)
              # +lag(dwells_nz_stats.change,i)
              
           #+lag(vacant_time.change,0)
          #+lag(vacant_time.change,i)
           
          #+lag(vacancy.change,0)
          #+lag(vacancy.change,i)
              
              +lag(mrate.change,0)
            +lag(mrate.change,i)
            #+lag(mrate.change,i+1)
            
            
             +lag(cpi_exRent.change,0)
            +lag(cpi_exRent.change,i)
            #+lag(cpi_exRent.change,i+1)
            
            
             +lag(unemp,0)
            +lag(unemp,i)
           # +lag(unemp,i+1)
            
              
            +lag(hpi.change,0)
            +lag(hpi.change,i)
            #+lag(hpi.change,i+1)
            
            #+factor(quarter)
            
            #+gfc_dummy
            #+covid_dummy
            #+post2016
            #+post2020
          
              ,
              data = combined_nz_quarterly) 

tab_model(model1a)

BIC(model1a)
AIC(model1a, k=2)

cov2cor(vcov(model1a))

fit <- predict(model1a)

plot_data <- data.frame(predicted = predict(model1a), 
                        observed = combined_nz_quarterly$rpi.change[c(3:75)],
                        date = combined_nz_quarterly$date[c(3:75)]) %>%
  mutate(date = floor_date(date, unit = "months")) %>%
  complete(date = seq.Date(min(date), max(date), by="quarter")) %>%
  mutate(date = date_to_month(date)) %>%
  left_join(dplyr::select(combined_nz_quarterly, date,rpi.change)) %>%
  mutate(observed = rpi.change)


plot_fitted <- ggplot(plot_data, aes(x = date, y = predicted, col = "Fitted")) +
  geom_line(size = 1.5) +
  geom_line(data = plot_data, aes(x = date, y = observed, col = "Actual"), size = 1.5) +
  scale_color_manual(values = col) +
  theme1() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0,0)) +
  labs(col = "",
       y = "", x = "",
       title = "Quarterly rent inflation (%)")

plot_fitted

png("results_updated/baseline_fitted.png",
    width = 1500, height = 900, res=120)
plot_fitted
dev.off()

# Remove some variables
model1b <- lm(rpi.change ~ 
              lag(rpi.change,i) 
             #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              #+lag(rpi.change,i+3) 
              
              
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              #+lag(earnings.change,i+1) 
              
             #+earnings.change.average
             
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              #+lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              #+lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly) 

tab_model(model1b)

# RPI stock

combined_nz_quarterly_stock <- combined_nz_quarterly %>%
  mutate(rpi.change = rpi_stock.change)

model1c <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              #+lag(rpi.change,i+3) 
              
              
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              #+lag(earnings.change,i+1) 
              
              #+earnings.change.average
              
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly_stock) 

tab_model(model1c)

bptest(model1c)
coeftest(model1c, vcov = vcovHC(model1c, type = "HC0"))
BIC(model1c)
AIC(model1c, k=2)

# Real rent flow

combined_nz_quarterly_real <- combined_nz_quarterly %>%
  mutate(rpi.change = rpi_real.change)

model1d <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              #+lag(rpi.change,i+3) 
              
              
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              #+lag(earnings.change,i+1) 
              
              #+earnings.change.average
              
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly_real) 

tab_model(model1d)



labels <- c("Rent inflation (lagged)", 
            #"Rent inflation (2nd lag)", 
            #"Rent inflation (3rd lag)",
            "Wage",
            "Wage (lagged)",
            "People per dwelling", 
            "People per dwelling (lagged)", 
            "Floating mortgage rate",
            "Floating mortgage rate (lagged)",
            "Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            "Unemployment rate",
            "Unemployment rate (lagged)",
            "House price",
            "House price (lagged)"
            #"Covid dummy"
)

tab_model(model1a, model1b, model1c, model1d,
          dv.labels = c("Rent inflation (nominal, flow)",
                        "Rent inflation (nominal, flow)",
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
          drop = "quarter",
          file = "results_updated/baseline.doc") 

# Shapley value to calculate the relative importance of each factors
combined <- combined_nz_quarterly %>%
  #select(-quarter) %>%
  dplyr::select(date, rpi.change, earnings.change.average,earnings.change, ppd.change, mrate.change,unemp,hpi.change, covid_dummy, cpi_exRent.change) %>%
  mutate(rpi.change.lag1 = lag(rpi.change,i),
         rpi.change.lag2 = lag(rpi.change,i+1)
         # ,rpi.change.lag3 = lag(rpi.change,i+2)
  ) %>%
  mutate(earnings.change.lag1 = lag(earnings.change,i)) %>%
  mutate(ppd.change.lag1 = lag(ppd.change,i)) %>%
  mutate(mrate.change.lag1 = lag(mrate.change,i)) %>%
  mutate(unemp.lag1 = lag(unemp,i)) %>%
  mutate(hpi.change.lag1 = lag(hpi.change,i)) %>%
  mutate(cpi_exRent.change.lag1 = lag(cpi_exRent.change,i)) %>%
  
  filter(!is.na(lag(rpi.change,i)) & 
           !is.na(lag(rpi.change,i+1)) &
           #!is.na(lag(rpi.change,i+2)) &
           !is.na(lag(earnings.change,i)) &
           !is.na(lag(ppd.change,i)) &         
           !is.na(lag(mrate.change,i)) &         
           !is.na(lag(hpi.change,i)) &         
           !is.na(lag(unemp,i)) 
  ) 


# Independent variables
y = combined$rpi.change

# Covariates

x = combined %>% 
  dplyr::select(
         rpi.change.lag1,
         #rpi.change.lag2,
         earnings.change, 
         #earnings.change.lag1,
         #earnings.change.average,
         #ppd.change,
         ppd.change.lag1,
         #mrate.change,
         mrate.change.lag1, 
         #cpi_exRent.change,
         cpi_exRent.change.lag1,
         #unemp,
         unemp.lag1, 
        # hpi.change,
         hpi.change.lag1,
         #covid_dummy
         ) %>%
  as.data.frame()

shapleyvalue(y,x) 

colnames(x) = c(
  "Lag rent inflation (t-1)",
  #"Lag rent inflation (t-2)",
  #"Lag rent inflation (t-3)",
  "Wages",
  "Lag wages",
  "People per dwelling",
  "Mortgage rate",
  "Inflation excluding rents",
  "Unemployment",
  "House price"
  #,"Covid dummy"
)

z <- shapleyvalue(y,x) %>% 
  mutate(`Wages` = `Wages`  + `Lag wages`) %>%
  mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` 
         #+ `Lag rent inflation (t-2)`
         #+ `Lag rent inflation (t-3)`
         
         ) %>%
  dplyr::select(-contains("Lag wages"), -contains("t-")) 
  
  # mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` + `Lag rent inflation (t-2)`) %>%

  #mutate(`Quarter dummy` = `Quarter 1` + `Quarter 2` + `Quarter 3`) %>%
  #dplyr::select(-contains("t-"), -`Quarter 1`, -`Quarter 2`, -`Quarter 3`)

sv <- data.frame(var = colnames(z),
                 contribution = t(z[2,])) %>%
  rename(value = Standardized.Shapley.Value) %>%
  #filter(var!="Lag rent inflation" & var!="Quarter dummy") %>%
  mutate(value = value*100) 

plot_sv <- ggplot(sv, aes(x = reorder(var, value, sum), y = value)) +
  geom_col(fill = col[2]) +
  coord_flip() +
  theme1() +
  labs(y = "%", x ="",
       title = "Contribution to the variation of quarterly rent inflation",
       subtitle = "Standardized Shapley value")

plot_sv

# Save as png
png("results_updated/shapleyValue.png",
    width = 1500, height = 1200, res=120)
plot_sv
dev.off()


# Other specification

# Adult people per dwelling
model1e <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 

              #+lag(adult_ppd.change,0)
              +lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly) 

tab_model(model1e)

# Separate population and dwellings change
model1f <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              +lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              +lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly) 

tab_model(model1f)


# Vacant time as alternative measure
model1g <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              #+ lag(ppd.change,i)
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly) 

tab_model(model1g)


labels <- c("Wage",
            "Wage (lagged)",
            "People per dwelling (lagged)", 
            "Adults per dwelling (lagged)",
            "Population growth (lagged)",
            "Dwellings growth (lagged)",
            "Change in vacant time (days, lagged)"
)

tab_model(model1b, model1e, model1f, model1g,
          dv.labels = rep("Rent inflation (nominal, flow)", 5),
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
              #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              #+lag(rpi.change,i+3) 
              
              
              
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              #+lag(earnings.change,i+1) 
              
              #+earnings.change.average
              
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              #+lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              #+lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              #+post2016
              ,
              data = combined_nz_quarterly) 

tab_model(model1h)


labels <- c("Rent inflation (lagged)", 
            #"Rent inflation (2nd lag)", 
            #"Rent inflation (3rd lag)",
            "Wage",
            "Wage (lagged)",
            #"People per dwelling", 
            "People per dwelling (lagged)", 
            #"Floating mortgage rate",
            "Floating mortgage rate (lagged)",
            #"Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            #"Unemployment rate",
            "Unemployment rate (lagged)",
            #"House price",
            "House price (lagged)"
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
          file = "results_updated/baseline_annual_v1.doc") 









# Interaction rent
model2a <- lm(rpi.change ~ 
                lag(rpi.change,i) 
             #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              
              
              +lag(earnings.change.demeaned,0) * lag(ppd.change.demeaned,i) 
              # +lag(earnings.change.demeaned,i) * lag(ppd.change.demeaned,i)
              #+lag(earnings.change,i+1) 
              
              
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change.demeaned,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              # +lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              # +lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              ,
              data = combined_nz_quarterly) 

tab_model(model2a)


model2b <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              #+lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2) 
              
              
              # +lag(earnings.change.demeaned,0) * lag(ppd.change.demeaned,i) 
              # +lag(earnings.change.demeaned,i) * lag(ppd.change.demeaned,i)
              #+lag(earnings.change,i+1) 
              
              +lag(earnings.change.demeaned,0) * 
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              #+lag(ppd.change,0)
              +lag(ppd.change.demeaned,i)
              #+lag(ppd.change,i+1)
              #  +lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              # +lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              # +lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
              #+lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              #+covid_dummy
              ,
              data = combined_nz_quarterly) 

tab_model(model2b)

labels <- c("Wages",
            "People per dwelling (lagged)",
            "Wages x People per dwelling (lagged)")



tab_model(model2a,
          dv.labels = rep("Rent inflation (nominal, flow)", 1),
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
          file = "results_updated/baseline-interaction.doc") 










# Vacant time as an alternative measure of supply and demand
i = 1

# Baseline
model1d <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(rpi.change,i+1) 
              #+lag(rpi.change,i+2)
              
              
              
              
              
              +lag(earnings.change,0) * lag(vacant_time.change,i)
              +lag(earnings.change,i) * lag(vacant_time.change,i)
              #+lag(earnings.change,i+1) 
              
              
              
              #+lag(income.change,0)
              #+lag(income.change,i)
              # +lag(income.change,i+1)
              
              
              # +lag(ppd.change,0)
              #+lag(ppd.change,i)
              #+lag(ppd.change,i+1)
              #+lag(ppd.change,i+2)   
              
              
              #+lag(adult_ppd.change,0)
              #+lag(adult_ppd.change,i)
              #+lag(adult_ppd.change,i+1)
              #+lag(adult_ppd.change,i+2)   
              
              # +lag(pop.change,0)
              #+lag(pop.change,i)
              
              # +lag(adult_pop.change,0)
              # +lag(adult_pop.change,i)
              
              #+lag(dwells_nz_stats.change,0)
              # +lag(dwells_nz_stats.change,i)
              
              #+lag(vacant_time.change,0)
              # +lag(vacant_time.change,i)
              
              
              #+lag(vacant_time.change,0)
              +lag(vacant_time.change,i)
              #+lag(vacant_time.change,i+1)
              #+lag(vacant_time.change,i+2)
              
              
              
              
              #+lag(mrate.change,0)
              +lag(mrate.change,i)
              #+lag(mrate.change,i+1)
              
              
              
              #+lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              #+lag(cpi_exRent.change,i+1)
              
              
              
              
             # +lag(unemp,0)
              +lag(unemp,i)
              #+lag(unemp,i+1)
              
              
              #+lag(hpi.change,0)
              +lag(hpi.change,i)
              #+lag(hpi.change,i+1)
              
              
              
              #+factor(quarter)
              
              #+gfc_dummy
              +covid_dummy
              ,
              data = combined_nz_quarterly) 

tab_model(model1d)




labels <- c("Rent inflation (1st lag)", "Rent inflation (2nd lag)", "Rent inflation (3rd lag)",
            paste0("Wage growth (deviation from sample mean ", round(mean(combined_nz_quarterly$earnings.change, na.rm = T),2), ", lagged)"),
            paste0("People per dwelling (deviation from sample mean ", round(mean(combined_nz_quarterly$ppd, na.rm = T),2), ", lagged)"),
            "Change in floating mortgage rate (lagged)",
            "General inflation",
            "Unemployment rate (lagged)")

tab_model(model1a, model1b, model1c, model1d, model1e,
          #dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          dv.labels = rep("Quarterly rent inflation", 5),
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
          file = "results/baseline-quarterly.doc") 


labels <- c(paste0("Wage growth (deviation from sample mean ", round(mean(combined_nz_quarterly$earnings.change, na.rm = T),2), ", lagged)"),
            paste0("People per dwelling (deviation from sample mean ", round(mean(combined_nz_quarterly$ppd, na.rm = T),2), ", lagged)"),
            "Vacant time")

tab_model(model1e, model1f,
          dv.labels = rep("Quarterly rent inflation", 2),
          pred.labels = labels,
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter|rpi.change|mrate|cpi|unemp",
          file = "results/supply-demand.doc") 

# Shapley value to calculate the relative importance of each factors
combined <- combined_nz_quarterly %>%
  filter(year(date)>=2006) %>%
  mutate(quarter1 = ifelse(quarter == 1, 1, 0), 
         quarter2 = ifelse(quarter == 2, 1, 0),
         quarter3 = ifelse(quarter == 3, 1, 0)
  ) %>%
  select(-quarter) %>%
  dplyr::select(date, rpi.change, earnings.change.demeaned, ppd.demeaned,mrate.change,cpi.change, unemp, 
                contains("quarter"), 
                covid_dummy) %>%
  mutate(rpi.change.lag = lag(rpi.change,i),
         rpi.change.lag2 = lag(rpi.change,i+1),
         rpi.change.lag3 = lag(rpi.change,i+2)
  ) %>%
  filter(!is.na(lag(rpi.change,i)) & 
           !is.na(lag(rpi.change,i+1)) &
           !is.na(lag(rpi.change,i+2)) &
           !is.na(lag(earnings.change.demeaned,i)) &
           !is.na(lag(ppd.demeaned,i)) &         
           !is.na(lag(mrate.change,i)) &         
           !is.na(lag(cpi.change,i)) &         
           !is.na(lag(unemp,i)) &
           covid_dummy == 0
  ) %>%
  select(-covid_dummy)




y = combined$rpi.change

x = as.data.frame(combined[3:13])

colnames(x) = c(
  "Wages",
  "People per dwelling",
  "Mortgage rate",
  "General inflation",
  "Unemployment",
  "Quarter 1",
  "Quarter 2",
  "Quarter 3",
  "Lag rent inflation (t-1)",
  "Lag rent inflation (t-2)",
  "Lag rent inflation (t-3)"
)

z <- shapleyvalue(y,x) %>% 
  # mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` + `Lag rent inflation (t-2)`) %>%
  mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` + `Lag rent inflation (t-2)` + `Lag rent inflation (t-3)`) %>%
  mutate(`Quarter dummy` = `Quarter 1` + `Quarter 2` + `Quarter 3`) %>%
  dplyr::select(-contains("t-"), -`Quarter 1`, -`Quarter 2`, -`Quarter 3`)
# dplyr::select(-contains("t-"))


z

sv <- data.frame(var = colnames(z),
                 contribution = t(z[2,])) %>%
  rename(value = Standardized.Shapley.Value) %>%
  #filter(var!="Lag rent inflation" & var!="Quarter dummy") %>%
  mutate(value = value*100) 

plot_sv <- ggplot(sv, aes(x = reorder(var, value, sum), y = value)) +
  geom_col(fill = col[2]) +
  coord_flip() +
  theme1() +
  labs(y = "%", x ="",
       title = "Contribution to the variation of quarterly rent inflation",
       subtitle = "Standardized Shapley value")

plot_sv

# Save as png
png("results/shapleyValue.png",
    width = 1500, height = 1200, res=120)
plot_sv
dev.off()


# Alternative hypothesis
# 2.1 Does the relationship between rent and general inflation differ in time of recession?
# model1f <- lm(rpi.change ~ 
#                 lag(rpi.change,i)
#               +lag(earnings.change.demeaned,1)
#               +lag(ppd.demeaned,1) 
#               +lag(mrate.change,1)
#               +lag(unemp,1)
#               +lag(cpi_exRent.change,0) * covid_dummy
#               +lag(cpi_exRent.change,0) * gfc_dummy
#               +factor(quarter)
#               ,
#               data = combined_nz_quarterly) 
# 
# labels <- c("Inflation excluding rents",
#             "Covid period",
#             "GFC period",
#             "Covid x inflation",
#             "GFC x inflation")
# 
# tab_model(model1f)
# 
# tab_model(model1f,
#           dv.labels = rep("Annual rent inflation", 1),
#           pred.labels = labels,
#           show.ci = FALSE, 
#           show.intercept = F,
#           show.p = F,
#           collapse.se = T,
#           digits = 3,
#           p.style = "numeric_stars",
#           emph.p = F,
#           p.threshold = c(0.1, 0.05, 0.01),
#           drop = "quarter|rpi|earnings|ppd|mrate|unemp",
#           file = "results/cpi-rpi.doc") 
# 
# ggplot(combined_nz_quarterly, aes(x = date, y = rpi.change, col = "Rent")) +
#   geom_line(size = 2) +
#   geom_line(aes(x = date, y = cpi_exRent.change, col = "CPI excluding rent"), size = 2) +
#   scale_color_manual(values = col) + 
#   theme1()+
#   labs(title = "Annual inflation (%)",
#        y ="",
#        x = "",
#        col = "",
#        alpha ="") +
#   scale_x_date(date_break = "1 year", date_labels = "%Y", expand = c(0, 0)) 
# 
# 
# 
# # 2.2 Should rent inflation and unemployment rate have a negative correlation
# # i.e Landlord more likely to raise rents when the economy is good
# # Better job security induces household formation 
# model1g <- lm(rpi.change ~ 
#                 lag(rpi.change,i)
#               +lag(earnings.change.demeaned,1)
#               +lag(ppd.demeaned,1) 
#               +lag(mrate.change,1)
#               +cpi_exRent.change
#               +lag(unemp,1)
#               +unemp
#               +lead(unemp,1)
#               +factor(quarter)
#               ,
#               data = combined_nz_quarterly) 
# 
# labels <- c("Unemployment rate (lagged)",
#             "Unemployment rate (contemporaneous)",
#             "Unemployment rate (lead)")
# 
# # The negative relationship between rent inflation and unemployment rate only appears at stock level
# # RPI flow is a leading indicator of RPI stock
# # RPI flow would correlate with lead values of unemployment rate
# # However, this is useful to understand the relationship but not for forecasting purposes
# 
# tab_model(model1g,
#           dv.labels = rep("Annual rent inflation", 1),
#           pred.labels = labels,
#           show.ci = FALSE, 
#           show.intercept = F,
#           show.p = F,
#           collapse.se = T,
#           digits = 3,
#           p.style = "numeric_stars",
#           emph.p = F,
#           p.threshold = c(0.1, 0.05, 0.01),
#           drop = "quarter|rpi|earnings|ppd|mrate|cpi",
#           file = "results/unemp.doc") 


# 2.3 Main focus: Does the correlation between wage and rent depends on relative supply and demand
model2a <- lm(rpi.change ~ 
                +lag(rpi.change,i)
              +lag(rpi.change,i+1)
              +lag(rpi.change,i+2)
              
              +lag(earnings.change.demeaned,i)
              +lag(ppd.demeaned,i) 
              +lag(mrate.change,i)
              +lag(cpi.change,i)
              +lag(unemp,i) 
              +lag(earnings.change.demeaned,i) * lag(ppd.demeaned,i)
              +factor(quarter)
              ,
              data = filter(combined_nz_quarterly, covid_dummy==0)) 

tab_model(model2a)

# Alternative measure of relative supply and demand: Vacancy rate (New bonds/Active bonds) 
model2b <- lm(rpi.change ~ 
                +lag(rpi.change,i)
              +lag(rpi.change,i+1)
              +lag(rpi.change,i+2)
              +lag(vacancy,i)
              +lag(mrate.change,i)
              +lag(cpi.change,i)
              +lag(unemp,i) 
              +lag(earnings.change.demeaned,i) * lag(vacancy,i)
              +factor(quarter)
              ,
              data = filter(combined_nz_quarterly, covid_dummy==0)) 

tab_model(model2b)


# Alternative measure of relative supply and demand: Change in median vacant time (Time between tenancies) 
model2c <- lm(rpi.change ~ 
                +lag(rpi.change,i)
              +lag(rpi.change,i+1)
              +lag(rpi.change,i+2)
              
              +lag(vacant_time,i)
              +lag(mrate.change,i)
              +lag(cpi.change,i)
              +lag(unemp,i) 
              +lag(earnings.change.demeaned,i) * lag(vacant_time,i) 
              +factor(quarter)
              ,
              data = filter(combined_nz_quarterly, covid_dummy==0)) 

tab_model(model2c)



labels <- c(paste0("Wage growth (deviation from sample mean ", round(mean(combined_nz_quarterly$earnings.change,na.rm = T),2), ", lagged)"),
            paste0("People per dwelling (deviation from sample mean ", round(mean(combined_nz_quarterly$ppd, na.rm = T),2), ", lagged)"),
            "Wage growth x People per dwelling",
            #"Vacancy rate (lagged)",
            #"Wage growth x Vacancy rate",
            "Change in vacant time (lagged)",
            "Wage growth x Change in vacant time")

tab_model(model2a, model2c,
          dv.labels = rep("Quarterly rent inflation", 2),
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
          file = "results/supply-demand-interaction.doc")


# Data viz
mean(combined_nz_quarterly$ppd, na.rm = T)


ppd = seq(2.48,2.62,0.03)

results <- data.frame(ppd = ppd,
                      marginal_wage = 0.46 + 7.48 * (ppd-2.573),
                      pop_change = round(ppd*max(combined_nz_quarterly$dwellings) - max(combined_nz_quarterly$pop,na.rm = T),-4),
                      dwells_change = round(max(combined_nz_quarterly$pop, na.rm = T)/ppd - max(combined_nz_quarterly$dwellings),-3))


wage = seq(-5,5,0.01)

wage = seq(0,10,0.01)


wage_rent <- data.frame(wage = wage,
                        marginal_wage = results$marginal_wage[1]*wage,
                        pop_change = results$pop_change[1],
                        dwells_change = results$dwells_change[1],
                        ppd = ppd[1],
                        label = ifelse(results$dwells_change[1]<=0, 
                                       paste0(-results$dwells_change[1]/1000, "k less dwellings"),
                                       paste0(results$dwells_change[1]/1000, "k more dwellings")),
                        label_pop = ifelse(results$pop_change[1]<=0, 
                                           paste0(-results$pop_change[1]/1000, "k less people"),
                                           paste0(results$pop_change[1]/1000, "k more people")))

for (i in (2:length(ppd))) {
  
  temp <-  data.frame(wage = wage,
                      marginal_wage = results$marginal_wage[i]*wage,
                      pop_change = results$pop_change[i],
                      dwells_change = results$dwells_change[i],
                      ppd = ppd[i],
                      label = ifelse(results$dwells_change[i]<=0, 
                                     paste0(-results$dwells_change[i]/1000, "k less dwellings"),
                                     paste0(results$dwells_change[i]/1000, "k more dwellings")),
                      label_pop = ifelse(results$pop_change[i]<=0, 
                                         paste0(-results$pop_change[i]/1000, "k less people"),
                                         paste0(results$pop_change[i]/1000, "k more people")))
  
  wage_rent <- wage_rent %>%
    bind_rows(temp)
}

wage_rent <- wage_rent %>%
  mutate(ppd = ifelse(ppd == 2.57, "2.57 (sample mean)", as.character(ppd))) %>%
  mutate(label = if_else(wage == max(wage), paste0(as.character(label), " or ",as.character(label_pop)) , NA_character_))



plot_wage_rent <- ggplot(wage_rent, aes(wage, marginal_wage, col = factor(ppd))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = col)  +
  labs(y = "Percentage points increase in rent inflation",
       x = "Percentage points increase in wage growth",
       title = "Marginal effects of wage on rent",
       col = "People per dwellings") +
  theme1() +
  theme(legend.position = "top") +
  geom_label_repel(aes(label = label),
                   col = "black",
                   nudge_x = 5,
                   size = 5,
                   na.rm = TRUE)

plot_wage_rent

# Save as png
png("results/wage-rent_1.png",
    width = 1500, height = 1200, res=120)
plot_wage_rent
dev.off()  


##### 3.2 Household disposable income ----

model5a <- lm(rpi.change ~ 
                +lag(rpi.change,i)
              +lag(rpi.change,i+1)
              +lag(rpi.change,i+2)
              
              +lag(income.change.demeaned,i)
              #+lag(income_pers.change.demeaned,1)
              
              +lag(ppd.demeaned,i) 
              +lag(mrate.change,i)
              +lag(cpi.change,i)
              +lag(unemp,i)
              +lag(income.change.demeaned,i) * lag(ppd.demeaned,i) 
              
              #+lag(income_pers.change.demeaned,1) * lag(ppd.demeaned,1) 
              +factor(quarter)
              ,
              data = combined_nz_quarterly) 

tab_model(model5a,
          dv.labels = c("Model 1"),
          #show.ci = FALSE, 
          show.intercept = F,
          #show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter") 





#### II. Regional level ---- 
combined_rc <- rpi_rc %>%
  left_join(income_hud_rc) %>%
  left_join(income_hud_personal_rc) %>%
  left_join(dwells_rc) %>%
  left_join(pop_rc) %>%
  left_join(pop_per_dwell_rc) %>%
  left_join(earnings_leed_rc) %>%
  mutate(pop = ppd*dwellings) %>%
  left_join(irate) %>%
  left_join(CPIexRent)

# saveRDS(combined_rc, "combined_rc.RDS")

# combined_rc <- readRDS("combined_rc.RDS")

t=12

combined_rc <- combined_rc %>%
  group_by(area) %>%
  arrange(date) %>%
  mutate(rpi.log = log(rpi)) %>%
  mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
  mutate(income.change = (income/lag(income,t)-1)*100) %>%
  mutate(earnings_leed.change = (earnings_leed/lag(earnings_leed,t)-1)*100) %>%
  mutate(income_pers.change = (income_pers/lag(income,t)-1)*100) %>%
  mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
  mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
  mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
  mutate(ppd.change = ppd - lag(ppd,t)) %>%
  mutate(income.change.demeaned = income.change - mean(income.change,na.rm = T)) %>%
  mutate(earnings_leed.change.demeaned = earnings_leed.change - mean(earnings_leed.change,na.rm = T)) %>%
  mutate(income_pers.change.demeaned = income_pers.change - mean(income_pers.change,na.rm = T)) %>%
  mutate(cpi_exRent.change = (cpi_exRent/lag(cpi_exRent,t)-1)*100) %>%
  mutate(mrate.change = (mrate/lag(mrate,t)-1)*100) 

combined_rc_quarterly <- combined_rc %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  filter(year(date)>=2006) %>%
  mutate(quarter = quarter(date)) 

region_name =  sort(unique(combined_rc$area))

# saveRDS(combined_rc_quarterly, "combined_rc_quarterly.RDS")

# Baseline
# model <- lm(rpi.change ~ 
#               lag(rpi.change,1) + 
#               +lag(income.change.demeaned,1) + 
#               +lag(ppd.demeaned,1) 
#             + factor(are)
#             +factor(quarter)
#             
#             ,
#             data = combined_rc_quarterly) 

# Does LEED earnings work at national level

i = 4

model <- lm(rpi.change ~ 
              lag(rpi.change,i) + 
              lag(rpi.change,i+1) +
              lag(rpi.change,i+2)
            
            +lag(earnings_leed.change.demeaned,1) + 
              +lag(ppd.demeaned,1) +
              +lag(mrate.change,1)  
            +lag(cpi.change,0)
            +lag(unemp,0)
            +factor(quarter)
            
            ,
            data = combined_nz_quarterly) 

summary(model)
tab_model(model)


# Marginal impact of wage on rent by regions
model <- lm(rpi.change ~ 
              lag(rpi.change,1) + 
              lag(rpi.change,i+1) +
              lag(rpi.change,i+2)
            +lag(earnings_leed.change.demeaned,1) + 
              +lag(ppd.demeaned,1) +
              +lag(earnings_leed.change.demeaned,1)*factor(area) 
            +lag(mrate.change,1)  
            +lag(cpi_exRent.change,0)+
              +factor(quarter)
            
            ,
            data = combined_rc_quarterly) 

summary(model)
tab_model(model)


results <- data.frame(area = region_name, 
                      area_coeff = c(model$coefficients[3], model$coefficients[25:39]),
                      p_val = c(tidy(model)$p.value[3], tidy(model)$p.value[25:39])) %>%
  mutate(income_coeff = ifelse(area == "Auckland Region", model$coefficients[3], model$coefficients[3] + area_coeff)) %>%
  mutate(sig = ifelse(p_val < 0.05, 1, 0)) %>%
  mutate(income_coeff = round(income_coeff,2))


ggplot(results, aes(x = reorder(area, income_coeff), y = income_coeff, alpha = factor(sig, level = c(0,1)))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "Change in annual rent inflation from 1 percentage point increase in wage growth",
       y ="percentage points",
       x = "",
       col = "",
       alpha ="") +
  ylim(-0.5,0.7)


# Compare with ppd
# ppd_region <- combined_rc_quarterly %>%
#   group_by(area) %>%
#   summarise(mean_ppd = mean(ppd, na.rm = T))
# 
# results_ppd <- results %>%
#   select(area,income_coeff) %>%
#   left_join(ppd_region) %>%
#   pivot_longer(cols = -area)
# 
# ggplot(results_ppd, aes(x = reorder(area, value), y = value)) +
#   geom_point(size = 5, col = col[2]) +
#   coord_flip() +
#   theme1()+
#   labs(title = "",
#        y ="",
#        x = "",
#        col = "") +
#   facet_wrap(~name)


# Marginal impact of relative supply/demand on rent by regions
model <- lm(rpi.change ~ 
              lag(rpi.change,1) + 
              +lag(earnings_leed.change.demeaned,1) + 
              +lag(ppd.demeaned,1) +
              +lag(ppd.demeaned,1)*factor(area) 
            +lag(mrate.change,1)  
            +lag(cpi_exRent.change,0)
            +factor(quarter)
            ,
            data = combined_rc_quarterly)

summary(model)

mean_ppd <- combined_rc %>%
  group_by(area) %>%
  summarise(mean_ppd = mean(ppd, na.rm = T),
            max_ppd.demeaned = max(ppd.demeaned, na.rm = T),
            max_pop = pop[date == "2021-06-30"],
            max_dwellings = dwellings[date == "2021-06-30"]) 

results <- data.frame(area = region_name, 
                      ppd_coeff = c(model$coefficients[4], model$coefficients[25:39]),
                      p_val = c(tidy(model)$p.value[4], tidy(model)$p.value[25:39])) %>%
  mutate(ppd_coeff = ifelse(area == "Auckland Region", model$coefficients[3], model$coefficients[3] + ppd_coeff)) %>%
  mutate(sig = ifelse(p_val <= 0.05, 1, 0)) %>%
  left_join(mean_ppd) %>%
  mutate(rent_increase = 0.01*max_pop/max_dwellings*ppd_coeff)


ggplot(results, aes(x = reorder(area, ppd_coeff), y = ppd_coeff, alpha = factor(sig, level = c(0,1)))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "Marginal effects of relative supply and demand (people per dwelling) on rent",
       y ="",
       x = "",
       col = "",
       alpha ="")


ggplot(results, aes(x = reorder(area, rent_increase), y = rent_increase, alpha = factor(sig, level = c(0,1)))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "Change in annual rent inflation from 1% increase in population from Q4-2021",
       y ="percentage points",
       x = "",
       col = "",
       alpha ="")




# Relationship between ppd and wage-rent

model <- lm(rpi.change ~ 
              lag(rpi.change,1) + 
              +lag(income.change.demeaned,1)*lag(ppd.demeaned,1)*factor(area) 
            +lag(mrate.change,1)  
            +lag(cpi_exRent.change,0)
            +factor(quarter)
            ,
            data = combined_rc_quarterly)


tab_model(model)

mean_ppd <- combined_rc %>%
  group_by(area) %>%
  summarise(mean_ppd = mean(ppd, na.rm = T),
            max_ppd.demeaned = max(ppd.demeaned, na.rm = T),
            max_pop = pop[date == "2021-06-30"],
            max_dwellings = dwellings[date == "2021-06-30"]) 

results <- data.frame(area = region_name, 
                      ppd_coeff = c(model$coefficients[4], model$coefficients[25:39]),
                      p_val = c(tidy(model)$p.value[4], tidy(model)$p.value[25:39])) %>%
  mutate(ppd_coeff = ifelse(area == "Auckland Region", model$coefficients[3], model$coefficients[3] + ppd_coeff)) %>%
  mutate(sig = ifelse(p_val <= 0.1, 1, 0)) %>%
  left_join(mean_ppd) %>%
  mutate(new_ppd = (max_pop/(max_dwellings*1.01))) %>%
  mutate(rent_increase = (max_pop/(max_dwellings*1.01)-mean_ppd)*ppd_coeff)


ggplot(results, aes(x = reorder(area, ppd_coeff), y = ppd_coeff, alpha = factor(sig, level = c(0,1)))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "Marginal effects of relative supply and demand (people per dwelling) on rent",
       y ="",
       x = "",
       col = "",
       alpha ="")


ggplot(results, aes(x = reorder(area, rent_increase), y = rent_increase, alpha = factor(sig, level = c(0,1)))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "The effect of 1% increase in population on annual rent inflation",
       y ="%",
       x = "",
       col = "",
       alpha ="")


# Emergency housing

eh <- read_excel("data/EHSNG.xlsx") %>%
  rename(region = `Primary recipient's MSD region`) %>%
  pivot_longer(cols = -region) %>%
  rename(date = name) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1900-01-01")) %>%
  mutate(date = date_to_month(date)) %>%
  filter(region %in% c("Auckland Metro", "Wellington","Waikato","Canterbury"))

eh_Auckland <- eh %>%
  filter(region %in% c("Auckland Metro"))

plot_eh <- ggplot(eh, aes(x = date, y = value)) + 
  geom_line(col = col[1], size = 1.5) +
  facet_wrap(~region, scales = "free_y",nrow = 4) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y", expand = c(0,0)) +
  theme1() + 
  labs(y = "", x = "", title = "Emergency housing - Special Needs Grant")+
  theme(axis.text = element_text(size = 8),
        strip.text = element_text(size=12),
        strip.placement = "outside")

plot_eh

# Save as png
png("results/eh_RC.png",
    width = 1200, height = 1500, res=120)
plot_eh
dev.off()

plot_eh_Auckland <- ggplot(eh_Auckland, aes(x = date, y = value)) + 
  geom_line(col = col[1], size = 1.5) +
  facet_wrap(~region, scales = "free_y",nrow = 4) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y", expand = c(0,0)) +
  theme1() + 
  labs(y = "", x = "", title = "Emergency housing - Special Needs Grant")+
  theme(axis.text = element_text(size =12),
        strip.text = element_text(size=12),
        strip.placement = "outside")

plot_eh_Auckland

# Save as png
png("results/eh_Auckland.png",
    width = 1500, height = 600, res=150)
plot_eh_Auckland
dev.off()

vacant_time_RC <- readRDS("data/vacant_time_region_Bedrooms.RDS") %>%
  filter(Region_Name %in% c("Auckland Region",
                            "Wellington Region",
                            "Waikato Region",
                            "Canterbury Region")) 

vacant_time_RC <- readRDS("data/vacant_time_region_Bedrooms.RDS") %>%
  filter(Region_Name %in% c("Auckland Region")) 

plot_vacant_time <- ggplot(filter(vacant_time_RC, YM> "2012-03-31", Beds!="All"), aes(x = YM, y = vacant_time, col = Beds)) +
  geom_line(size = .1, alpha = 0.1) +
  geom_smooth(se = F, span = 0.2, alpha = 1, size = 1) +
  theme1()+
  labs(title = "Vacant time (days)",
       y ="",
       x = "",
       col = "Bedrooms") +
  # annotate(geom = "rect",
  #          xmin = as.Date("2021-01-01"), xmax=as.Date("2022-12-31"),
  #          ymin=0, ymax= 40,
  #          fill=col[4], alpha=0.1)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
  scale_color_manual(values = col) +
  facet_wrap(~Region_Name, scales = "free_y", nrow = 4) +
  theme(legend.position = "right") +
  theme(axis.text = element_text(size = 8),
        strip.text = element_text(size=12),
        strip.placement = "outside")


plot_vacant_time

# Save as png
png("results/vacant_time_Auckland.png",
    width = 1500, height = 900, res=120)
plot_vacant_time
dev.off()

cowplot::plot_grid(plot_eh, plot_vacant_time)

# Save as png
png("results/vacant_time_eh.png",
    width = 2000, height = 1200, res=120)
cowplot::plot_grid(plot_eh, plot_vacant_time)
dev.off()

rpi_RC <- combined_rc_quarterly %>%
  filter(area %in% c("Auckland Region",
                     "Wellington Region",
                     "Waikato Region",
                     "Canterbury Region")) 

plot_rpi <- ggplot(filter(rpi_RC, year(date)>=2013), aes(x = date, y = rpi.change)) +
  geom_line(size = .5, alpha = 0.1) +
  geom_smooth(se = F, span = 0.2, alpha = 1, size = 1.5) +
  theme1()+
  labs(title = "Quarterly rent inflation",
       y ="",
       x = "") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y", expand = c(0,0)) +
  scale_color_manual(values = col) +
  facet_wrap(~area, scales = "free")

plot_rpi









# Geometric mean of rent (not adjusted for quality)

combined_nz_quarterly <- combined_nz %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  #filter(year(date)>=2006) %>%
  mutate(quarter = quarter(date)) %>%
  mutate(rent.change = (rent_geomean.change + cpi_rent.change)/2)


rpi_alternative <- ggplot(filter(combined_nz_quarterly, year(date)>=1990), aes(x = date, y = rpi.change, col = "RPI (Quality adjusted)")) +
  geom_line(size = 1.5) +
  geom_line(data = filter(combined_nz_quarterly, year(date)>=1990), 
            aes(x = date, y = rent_geomean.change, col = "Geomean rent (Non-quality adjusted)"), size = 1.5) +
  theme1()+
  labs(title = "Quarterly rent inflation (%)",
       y ="%",
       x = "",
       col = "",
       alpha ="") +
  scale_color_manual(values = col) +
  scale_x_date(date_break = "5 year", date_labels = "%Y", expand = c(0, 0)) +
  theme(legend.position="top")

rpi_alternative

# Save as png
png("results/rpi_alternative.png",
    width = 1500, height = 1200, res=120)
rpi_alternative
dev.off()

model1g <-  lm(rent_geomean.change ~ 
                 lag(rent_geomean.change,i) 
               +lag(rent_geomean.change,i+1) 
               +lag(rent_geomean.change,i+2)
               # +lag(rpi.change,i+3)
               
               
               
               +lag(earnings.change.demeaned,i)
               
               +lag(ppd.demeaned,i)      
               
               
               
               +lag(mrate.change,i)
               +lag(cpi.change,i)
               +lag(unemp,i)
               +factor(quarter)
               +tax_dummy
               #+lag(hpi.change,i)
               ,
               data = filter(combined_nz_quarterly, covid_dummy ==0))


tab_model(model1g)

labels <- c("Rent inflation (1st lag)", "Rent inflation (2nd lag)", "Rent inflation (3rd lag)",
            paste0("Wage growth (deviation from sample mean ", round(mean(combined_nz_quarterly$earnings.change, na.rm = T),2), ", lagged)"),
            paste0("People per dwelling (deviation from sample mean ", round(mean(combined_nz_quarterly$ppd, na.rm = T),2), ", lagged)"),
            "Change in floating mortgage rate (lagged)",
            "General inflation",
            "Unemployment rate (lagged)",
            "Rent inflation (1st lag)", "Rent inflation (2nd lag)", "Rent inflation (3rd lag)")

tab_model(model1e, model1g,
          dv.labels = rep("Quarterly rent inflation", 2),
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
          file = "results/baseline-quarterly_geomean_rpi.doc")



# Shapley value to calculate the relative importance of each factors
combined <- combined_nz_quarterly %>%
  mutate(quarter1 = ifelse(quarter == 1, 1, 0), 
         quarter2 = ifelse(quarter == 2, 1, 0),
         quarter3 = ifelse(quarter == 3, 1, 0)
  ) %>%
  select(-quarter) %>%
  dplyr::select(date, rent_geomean.change, earnings.change.demeaned, ppd.demeaned,mrate.change,cpi.change, unemp, contains("quarter"), covid_dummy) %>%
  mutate(rent_geomean.change.lag = lag(rent_geomean.change,i),
         rent_geomean.change.lag2 = lag(rent_geomean.change,i+1),
         rent_geomean.change.lag3 = lag(rent_geomean.change,i+2)
  ) %>%
  filter(!is.na(lag(rent_geomean.change,i)) & 
           !is.na(lag(rent_geomean.change,i+1)) &
           !is.na(lag(rent_geomean.change,i+2)) &
           !is.na(lag(earnings.change.demeaned,i)) &
           !is.na(lag(ppd.demeaned,i)) &         
           !is.na(lag(mrate.change,i)) &         
           !is.na(lag(cpi.change,i)) &         
           !is.na(lag(unemp,i)) &
           covid_dummy == 0
  ) %>%
  select(-covid_dummy)




y = combined$rent_geomean.change

x = as.data.frame(combined[3:13])

colnames(x) = c(
  "Wages",
  "People per dwelling",
  "Mortgage rate",
  "General inflation",
  "Unemployment",
  "Quarter 1",
  "Quarter 2",
  "Quarter 3",
  "Quarter 4",
  "Lag rent inflation (t-1)",
  "Lag rent inflation (t-2)",
  "Lag rent inflation (t-3)"
)

z <- shapleyvalue(y,x) %>% 
  # mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` + `Lag rent inflation (t-2)`) %>%
  mutate(`Lag rent inflation` = `Lag rent inflation (t-1)` + `Lag rent inflation (t-2)` + `Lag rent inflation (t-3)`) %>%
  mutate(`Quarter dummy` = `Quarter 1` + `Quarter 2` + `Quarter 3`) %>%
  dplyr::select(-contains("t-"), -`Quarter 1`, -`Quarter 2`, -`Quarter 3`)

z

sv <- data.frame(var = colnames(z),
                 contribution = t(z[2,])) %>%
  rename(value = Standardized.Shapley.Value)

plot_sv <- ggplot(sv, aes(x = reorder(var, value, sum), y = value)) +
  geom_col(fill = col[2]) +
  coord_flip() +
  theme1() +
  labs(y = "%", x ="",
       title = "Contribution to the variation of quarterly rent inflation",
       subtitle = "Standardized Shapley value")

plot_sv

# Save as png
png("results/shapleyValue_geomean.png",
    width = 1500, height = 1200, res=120)
plot_sv
dev.off()
