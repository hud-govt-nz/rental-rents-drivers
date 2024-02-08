
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

# con <- dbConnect(odbc::odbc(), "HUD", database = "property",
#                  uid = "Property_ReadOnly", pwd = "88fs97TaQbvU", timeout = 1000)


############
rpi_rc <- read.csv("data/region/integrated_RPI_monthly.csv") %>%
  select(date,area,index) %>%
  mutate(date = as.Date(date)) %>%
  filter(grepl("Region", area)) %>%
  rename(rpi = index)

earnings_rc <- read.csv("data/region/LEED_earnings.csv") %>%
  select(Quarter, Region, Mean.earnings.of.full.quarter.jobs) %>%
  mutate(Quarter = ifelse(Quarter == "", NA, Quarter)) %>%
  fill(Quarter, Region) %>%
  rename(date = Quarter, 
         area = Region,
         earnings_leed =Mean.earnings.of.full.quarter.jobs) %>%
  mutate(date = as.Date (date, format = "%d/%m/%Y")) %>%
  mutate(date = date_to_quarter(date)) %>% 
  mutate(area = paste0(area, " Region")) %>%
  mutate(earnings_leed = as.numeric(earnings_leed))

mrate <- read.csv("data/region/mortgage.csv") %>%
  rename(date = Date,
         mrate_float = Floating.rate,
         mrate_fixed =X2yr.rate ) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

ppd <- read.csv("data/region/PPdwell.csv") %>%
  filter(type == "people per dwelling") %>%
  select(date, index, area)  %>%
  mutate(area = paste0(area, " Region"))


pop_rc <- read.csv("data/pop_RC.csv") %>% 
  filter(!is.na(pop)) %>%
  mutate(area=str_trim(area),
         area=ifelse(area == "", NA, area),
         area=recode(area,
                     "Total New Zealand by regional council/statistical area 2"="New Zealand"),
         date=as.Date(paste0(year,"-06-30"))) %>%
  fill(area) %>%
  filter(!(area %in% c("Area outside region", "Total, North Island regions",
                       "Total, South Island regions", "New Zealand"))) %>%
  mutate(area=gsub("region","Region",area)) %>%
  group_by(area) %>% 
  arrange(date) %>%
  #dplyr::select(-date) %>%
  mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) 

# Consents interpolate
dwells_rc <- read.csv("data/dwellings_RC.csv") %>%
  mutate(area = paste0(area, " Region")) %>%
  mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(-ann.change)


pop_per_dwell_rc <- dwells_rc %>%
  #mutate(year = year(date)) %>%
  left_join(pop_rc) %>%
  filter(date <= "2021-06-30" & date>="2006-06-30") %>%
  # Linear interpolation
  group_by(area) %>%
  arrange(area, date) %>%
  filter(date >= "2007-06-30") %>%
  mutate(pop = na.approx(pop)) %>%
  mutate(ppd = pop/dwellings)  %>%
  dplyr::select(area, date, ppd)

unemp_rc  <-  read.csv("data/region/unemp_rc.csv", skip = 2) %>%
  rename(area = X.,
         date = X..1,
         unemp = Unemployment.Rate) %>%
  mutate(area = ifelse(area == " ", NA, area)) %>%
  fill(area) %>%
  mutate(area = paste0(area, " Region")) %>%
  mutate(unemp = as.numeric(unemp))

unemp_rc$date <- str_replace(unemp_rc$date, "Q1", ".03")
unemp_rc$date <- str_replace(unemp_rc$date, "Q2", ".06")
unemp_rc$date <- str_replace(unemp_rc$date, "Q3", ".09")
unemp_rc$date <- str_replace(unemp_rc$date, "Q4", ".12")
unemp_rc$date <- paste(unemp_rc$date, "01", sep=".")
unemp_rc$date <- date_to_quarter(as.Date(unemp_rc$date, format="%Y.%m.%d"))


# Combined 
combined_rc <- rpi_rc %>%
  left_join(earnings_rc) %>%
  left_join(mrate) %>%
  left_join(dwells_rc) %>%
  left_join(pop_rc) %>%
  left_join(pop_per_dwell_rc) %>%
  left_join(unemp_rc) 

t=3

combined_rc <- combined_rc %>%
  group_by(area) %>%
  arrange(area,date) %>%
  mutate(rpi.log = log(rpi)) %>%
  dplyr::mutate(rpi.change = (rpi/lag(rpi,t)-1)*100) %>%
  mutate(earnings_leed.change = (earnings_leed/lag(earnings_leed,t)-1)*100) %>%
  mutate(dwellings.change = (dwellings/lag(dwellings,t)-1)*100) %>%
  mutate(pop.change = (pop/lag(pop,t)-1)*100) %>%
  mutate(ppd.demeaned = ppd - mean(ppd,na.rm = T)) %>%
  mutate(ppd.change = ppd - lag(ppd,t)) %>%
  mutate(mrate.change = (mrate_float/lag(mrate_float,t)-1)*100) %>%
  mutate(covid_dummy = ifelse(year(date) %in% c(2020) & month(date) %in% c(3,4,5,6,7,8,9), 1, 0))

combined_rc_quarterly <- combined_rc %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  filter(year(date)>=2006) %>%
  mutate(quarter = quarter(date)) 

region_name =  sort(unique(combined_rc$area))

i=1

# Pooled OLS

model1 <- lm(rpi.change ~ 
              lag(rpi.change,i) + 
              lag(rpi.change,i+1) +
              lag(rpi.change,i+2)
            
            +lag(earnings_leed.change,1)  
              +lag(ppd.demeaned,1) 
             +lag(mrate.change,1)  
            +factor(quarter)
            #+factor(year)
            ,
            data = combined_rc_quarterly)

summary(model1)
tab_model(model1)

# Regional FE
model2 <- lm(rpi.change ~ 
               lag(rpi.change,i) + 
               lag(rpi.change,i+1) +
               lag(rpi.change,i+2)
             
             +lag(earnings_leed.change,1)

               +lag(ppd.demeaned,1) +
               +lag(mrate.change,1)  
             #+lag(unemp,1)
             # +factor(quarter)
             +factor(year)
             +factor(area)
              
             ,
            data = combined_rc_quarterly)

summary(model2)
tab_model(model2)

tab_model(model1, model2,
          #dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          dv.labels = rep("Quarterly rent inflation", 2),
          show.ci = FALSE, 
          show.intercept = F,
          show.p = F,
          collapse.se = T,
          digits = 2,
          p.style = "numeric_stars",
          emph.p = F,
          p.threshold = c(0.1, 0.05, 0.01),
          drop = "quarter",
          file = "results/quarterly-RC.doc") 

model <- lm(rpi.change ~ 
              lag(rpi.change,i) + 
              lag(rpi.change,i+1) +
              lag(rpi.change,i+2)
            
            +lag(earnings_leed.change,1) *  factor(area)

            
            +lag(ppd.demeaned,1) +
              +lag(mrate.change,1)  
            #+lag(unemp,1)
            +factor(quarter)
            
            ,
            data = combined_rc_quarterly)

summary(model)
tab_model(model)




# model <- lm(rpi.change ~ 
#               lag(rpi.change,i) + 
#               lag(rpi.change,i+1) +
#               lag(rpi.change,i+2)
#             
#             +lag(earnings_leed.change,1) 
#             
#             
#             +lag(ppd.demeaned,1) *  factor(area)
#               +lag(mrate.change,1)  
#             #+lag(unemp,1)
#             +factor(quarter)
#             
#             ,
#             data = combined_rc_quarterly)
# 
# summary(model)
# tab_model(model)


# Plot
results <- data.frame(area = region_name, 
                      area_coeff = c(model$coefficients[5], model$coefficients[26:40]),
                      p_val = c(tidy(model)$p.value[5], tidy(model)$p.value[26:40])) %>%
  mutate(income_coeff = ifelse(area == "Auckland Region", model$coefficients[5], model$coefficients[5] + area_coeff)) %>%
  mutate(sig = ifelse(p_val <= 0.1, 1, 0)) %>%
  mutate(income_coeff = round(income_coeff,2))


p1 <- ggplot(results, aes(x = reorder(area, income_coeff), y = income_coeff, alpha = factor(sig))) +
  geom_point(size = 5, col = col[2]) +
  coord_flip() +
  scale_alpha_manual(values = c(0.2,1), labels = c("Non-significant", "Significant")) +
  theme1()+
  geom_hline(yintercept = 0, col = col[4], alpha = 0.2) +
  labs(title = "Change in annual rent inflation from 1 percentage point increase in wage growth",
       y ="percentage points",
       x = "",
       col = "",
       alpha ="") 

png("results/wage_rpi_RC.png", width = 1500, height = 1200, res=120)
p1
dev.off()
