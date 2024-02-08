# Drivers of rents regression model
# Nam Ngo, May 2022

rm(list = ls())

library(DBI)
library(tidyverse)
library(lubridate)
library(devtools)
library(FEWS)
library(FEWS) 
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
library(urca)


con <- dbConnect(odbc::odbc(), "HUD", database = "property",
                 uid = "Property_ReadOnly", pwd = "88fs97TaQbvU", timeout = 1000)

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


#### 2. Regression results ----

combined_nz <- readRDS("data/combined_nz.RDS") 


# Add real rents measure
combined_nz <- combined_nz %>%
  mutate(rpi = as.numeric(rpi)) %>%
  arrange(desc(date)) %>%
  fill(cpi) %>%
  fill(cpi_exRent) %>%
  arrange(date) %>%
  mutate(rpi_real = rpi/cpi*1000,
         earnings_real = earnings/cpi*1000)

# Calculate changes (annual/quarterly)
t = 3

combined_nz <- combined_nz %>%
  arrange(date) %>%
  mutate(rpi.log = log(rpi)) %>%
  mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
  mutate(rpi_real.change = (rpi_real/lag(rpi_real,t)-t)*100) %>%
  mutate(income.change = (income/lag(income,t)-1)*100) %>%
  mutate(income_pers.change = (income_pers/lag(income,t)-1)*100) %>%
  mutate(earnings.change = (earnings/lag(earnings,t)-1)*100) %>%
  mutate(earnings_real.change = (earnings_real/lag(earnings_real,t)-1)*100) %>%
  mutate(earnings_sa.change = (earnings_sa/lag(earnings_sa,t)-1)*100) %>%
  mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
  mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
  mutate(hpi.change = (hpi/lag(hpi,t)-1)*100) %>%
  mutate(cpi_exRent.change = (cpi_exRent/lag(cpi_exRent,t)-1)*100) %>%
  mutate(cpi.change = (cpi/lag(cpi,t)-1)*100) %>%
  mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
  mutate(ppd.change = ppd - lag(ppd,t)) %>%
  mutate(earnings.change.demeaned = earnings.change - mean(earnings.change,na.rm = T)) %>%
  mutate(income.change.demeaned = income.change - mean(income.change,na.rm = T)) %>%
  mutate(income_pers.change.demeaned = income_pers.change - mean(income_pers.change,na.rm = T)) %>%
  mutate(vacancy.demeaned = vacancy - mean(vacancy,na.rm = T)) %>%
  mutate(vacant_time.demeaned = vacant_time - mean(vacant_time,na.rm = T)) %>%
  mutate(mrate_lagged = mrate + lag(mrate,1) + lag(mrate,2) + lag(mrate,3)) %>%
  mutate(mrate.change = (mrate/lag(mrate,t)-1)*100) %>%
  
  mutate(covid_dummy = ifelse(year(date) %in% c(2020,2021), 1, 0),
         gfc_dummy = ifelse(year(date) %in% c(2007,2008),1,0))

 

combined_nz_quarterly <- combined_nz %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  #filter(year(date)>=2007) %>%
  mutate(quarter = quarter(date)) 

# Stationary test

summary(ur.df(combined_nz_quarterly$rpi.log, lags = 4, 
      type = "trend"))

summary(ur.df(combined_nz_quarterly$rpi.change[!is.na(combined_nz_quarterly$rpi.change)], lags = 4))

adf.test(combined_nz_quarterly$rpi.change[!is.na(combined_nz_quarterly$rpi.change)])
adf.test(combined_nz_quarterly$rpi.log)

# Timeseries plot

plot(combined_nz_quarterly$rpi)

plot(combined_nz_quarterly$rpi.change)

plot(combined_nz_quarterly$income.change)

plot(combined_nz_quarterly$earnings.change)

plot(combined_nz_quarterly$vacancy.demeaned)

plot(combined_nz_quarterly$vacant_time.demeaned)

# Correlation matrix
cor_matrix <- cor(combined_nz_quarterly %>%
      filter(year(date)>=2007) %>%
      dplyr::select(rpi.change,
             income.change,
             earnings.change,
             pop.change,
             dwellings.change,
             ppd.demeaned,
             unemp,
             cpi.change,
             vacancy,
             hpi.change,
             mrate))

round(cor_matrix, 2)

library(corrplot)
corrplot(cor_matrix, method="number")

model <- lm(rpi.change ~ 
              lag(rpi.change,1) +
              #lag(rpi.change,2) +
              #lag(rpi.change,3) +
              #lag(rpi.change,4) +
              lag(earnings.change.demeaned,1)
              + lag(ppd.demeaned,1) 
              #lag(ppd.demeaned,1)*lag(earnings.change.demeaned,1) +
              
              #lag(mrate.change,1) 
            
            
            #lag(pop.change,1) + 
            #lag(dwellings.change,1) + 
            #+ lag(cpi_exRent.change,0) 
            
            #+lag(unemp,1) 
            
            #+ covid_dummy 
            #+ gfc_dummy 
            #+ lag(earnings.change.demeaned,1) * covid_dummy 
            #+ lag(earnings.change.demeaned,1) * gfc_dummy 
            
            
            #lag(cpi.change,1) +
            #lag(vacant_time.demeaned,1)  + 
            # + factor(quarter) 
            #+ factor(year(date)) 
            ,
            data = combined_nz_quarterly) 

tab_model(summary(model),   
          show.intercept = F,
          digits = 3,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01))  

model <- lm(rpi.change ~ 
              lag(rpi.change,1) +
              #lag(rpi.change,2) +
              #lag(rpi.change,3) +
              #lag(rpi.change,4) +
              lag(earnings.change.demeaned,1) + 
              
              lag(ppd.demeaned,1) +
              lag(ppd.demeaned,1)*lag(earnings.change.demeaned,1) +
              
              lag(mrate.change,1) 
              
              
              #lag(pop.change,1) + 
              #lag(dwellings.change,1) + 
              + lag(cpi_exRent.change,0) 
              
              +lag(unemp,1) 
            
            #+ covid_dummy 
            #+ gfc_dummy 
            #+ lag(earnings.change.demeaned,1) * covid_dummy 
            #+ lag(earnings.change.demeaned,1) * gfc_dummy 

            
            #lag(cpi.change,1) +
            #lag(vacant_time.demeaned,1)  + 
             # + factor(quarter) 
            #+ factor(year(date)) 
            ,
            data = combined_nz_quarterly) 


# labels <- c("Lag inflation", 
#            "Lag wage growth", 
#            "Lag people per dwelling", 
#            "Lag unemployment rate",
#            "Lag vacancy rate")

tab_model(summary(model),   
          show.intercept = F,
          digits = 3,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01))  
  
# Coefficients
# wage inflation 
wage <- model$coefficients[3]

# PPD demeaned
ppd <- model$coefficients[4]

# Interaction
wageXppd <- model$coefficients[7]

mean(combined_nz_quarterly$pop)
mean(combined_nz_quarterly$dwellings, na.rm = T)

mean(combined_nz_quarterly$pop)/mean(combined_nz_quarterly$dwellings, na.rm = T)



# 
wage_ppd <- data.frame(ppd_seq = seq(-0.05,0.05,0.001),
                      wage_ppd = wage + wageXppd * ppd_seq)

ggplot(wage_ppd, aes(x = ppd_seq, y = wage_ppd)) + 
  geom_line() +
  geom_hline(yintercept = -wage/wageXppd, col = col[2], linetype = "dashed") +
  geom_text
  theme1()


# Median personal income    
model <- lm(rpi.change ~ 
               #lag(rpi.change,1) + 
              #lag(rpi.change,2) + 
              #lag(rpi.change,3) + 
              #lag(rpi.change,4) + 
              
              
              
                lag(income_pers.change.demeaned,1) + 
                
                lag(ppd.demeaned,1) +
                lag(ppd.demeaned,1)*lag(income_pers.change.demeaned,1) +
                
                lag(mrate.change,1) 
                
                
                #lag(pop.change,1) + 
                #lag(dwellings.change,1) + 
              + lag(cpi_exRent.change,1) 
            
                
               + lag(unemp,1) 
              
              #  + covid_dummy +
              # gfc_dummy 
              # + lag(earnings.change,1) * covid_dummy * lag(ppd.demeaned,1) +
              # lag(earnings.change,1) * gfc_dummy *lag(ppd.demeaned,1)
              
              
              #lag(cpi.change,1) +
              #lag(vacant_time.demeaned,1)  + 
              #+ factor(quarter) 
              #+ factor(year(date)) 
              ,
              data = filter(combined_nz_quarterly, year(date)>=1990)) 
  
  
  # labels <- c("Lag inflation", 
  #            "Lag wage growth", 
  #            "Lag people per dwelling", 
  #            "Lag unemployment rate",
  #            "Lag vacancy rate")
  
  tab_model(summary(model),   
            #show.intercept = F,
            digits = 3,
            p.style = "numeric_stars",
            emph.p = F,
            p.threshold = c(0.1, 0.05, 0.01))  
  
  
  
  

#### 3. Regional level ----

combined_rc <- rpi_rc %>%
  left_join(income_hud_rc) %>%
  left_join(income_hud_personal_rc) %>%
  left_join(dwells_rc) %>%
  mutate(year = year(date)) %>%
  left_join(pop_rc) %>%
  left_join(pop_per_dwell_rc) 

t=12

combined_rc <- combined_rc %>%
  group_by(area) %>%
  arrange(date) %>%
  mutate(rpi.log = log(rpi)) %>%
  mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
  mutate(rpi_real.change = (rpi_real/lag(rpi_real,t)-t)*100) %>%
  mutate(income.change = (income/lag(income,t)-1)*100) %>%
  mutate(income_pers.change = (income_pers/lag(income,t)-1)*100) %>%
  mutate(earnings.change = (earnings/lag(earnings,t)-1)*100) %>%
  mutate(earnings_real.change = (earnings_real/lag(earnings_real,t)-1)*100) %>%
  mutate(earnings_sa.change = (earnings_sa/lag(earnings_sa,t)-1)*100) %>%
  mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
  mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
  mutate(hpi.change = (hpi/lag(hpi,t)-1)*100) %>%
  mutate(cpi_exRent.change = (cpi_exRent/lag(cpi_exRent,t)-1)*100) %>%
  mutate(cpi.change = (cpi/lag(cpi,t)-1)*100) %>%
  mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
  mutate(ppd.change = ppd - lag(ppd,t)) %>%
  mutate(earnings.change.demeaned = earnings.change - mean(earnings.change,na.rm = T)) %>%
  mutate(income.change.demeaned = income.change - mean(income.change,na.rm = T)) %>%
  mutate(income_pers.change.demeaned = income_pers.change - mean(income_pers.change,na.rm = T)) %>%
  mutate(vacancy.demeaned = vacancy - mean(vacancy,na.rm = T)) %>%
  mutate(vacant_time.demeaned = vacant_time - mean(vacant_time,na.rm = T)) %>%
  mutate(mrate_lagged = mrate + lag(mrate,1) + lag(mrate,2) + lag(mrate,3)) %>%
  mutate(mrate.change = (mrate/lag(mrate,t)-1)*100) %>%
  
  mutate(covid_dummy = ifelse(year(date) %in% c(2020,2021), 1, 0),
         gfc_dummy = ifelse(year(date) %in% c(2007,2008),1,0))
  mutate(lag.rpi = lag(rpi.change,1),
         lag.income = lag(income.change,1),
         area = to_factor(area))


model <- lm(rpi.change ~ 
              lag(rpi.change,1) + 
              lag(income.change,1) + 
              
              lag(ppd.demeaned,1) +
              lag(ppd.demeaned,1)*lag(income.change,1) *factor(area) +
            + factor(area)
            ,
            data = combined_rc) 

model <- lm(rpi.change ~ 
              lag(rpi.change,1) + 
              lag(income.change,1) + 
              
              lag(ppd.demeaned,1) +
              lag(ppd.demeaned,1)*lag(income.change,1) 
              + factor(area)
            ,
            data = combined_rc) 

tab_model(summary(model),
          show.intercept = F,
          digits = 3, 
          drop = "date")

model <- lm(rpi.change ~ 
              lag.rpi + 
              lag.income + 
              
              #lag(ppd.demeaned,1) +
              #lag(ppd.demeaned,1)*lag(income.change,1) +
              
              lag.income*area +
              
              
              # factor(quarter) 
              + factor(year(date))
            + area
            ,
            data = combined_rc) 

tab_model(summary(model),
          show.intercept = F,
          digits = 3, 
          drop = "date")

emmip(model,lag.income~area, CIs=TRUE) +
  coord_flip()


model <- lm(log(rpi) ~ 
              lag(log(rpi),1) + 
              lag(income.change,0) + 
              lag(ppd,1) +
              lag(log(rpi),1) * lag(ppd,1) +
              factor(area) +
              factor(year(date)), data = combined_rc)

tab_model(summary(model),
          show.intercept = F,
          digits = 3)
