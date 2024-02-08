# Data cleaning
# Update March 2022

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
library(seasonal)
library(forecast)
library(corrplot)
library(margins)
library(texreg)
library(broom)

source("utility_functions.R")

con <- dbConnect(odbc::odbc(), "HUD", database = "property",
                 uid = "Property_ReadOnly", pwd = "88fs97TaQbvU", timeout = 1000)

#### 1. Load data ----
#### 1.1 RPI - NZ - latest ----
rpi_stats_flow_nz <- read.csv(paste0("https://www.stats.govt.nz/assets/Uploads/Rental-price-indexes/Rental-price-indexes-August-2022/Download-data/rental-price-indexes-august-2022-csv.csv"), stringsAsFactors = F) %>%
  rename(timing=Series_title_3,
         region=Series_title_1) %>%
  mutate(date=as.Date(ifelse(as.character(substr(TIME_REF, 6, 7))!="1",paste0(TIME_REF,".01"),paste0(as.character(TIME_REF),"0.01")),"%Y.%m.%d")) %>%
  filter(timing=="Flow") %>%
  dplyr::select(date,region,DATA_VAL) %>%
  rename(index=DATA_VAL) %>%
  mutate(region=recode(region, 'AKL'="Auckland",
                       'WLG'="Wellington",
                       'RNI'="Rest of North Island",
                       'CAN'="Canterbury",
                       'RSI'="Rest of South Island",
                       'National'="New Zealand")) %>%
  arrange(date) %>%
  group_by(region) %>%
  mutate(ann.change = index/lag(index,12)-1,
         ann.change=round(ann.change*100,1)) %>%
  mutate(date = date_to_month(date)) %>%
  filter(region == 'New Zealand') %>%
  ungroup() %>%
  rename(area = region) %>%
  select(date, index, area) %>%
  rename(rpi_stats_flow = index)

rpi_stats_stock_nz <- read.csv(paste0("https://www.stats.govt.nz/assets/Uploads/Rental-price-indexes/Rental-price-indexes-August-2022/Download-data/rental-price-indexes-august-2022-csv.csv"), stringsAsFactors = F) %>%
  rename(timing=Series_title_3,
         region=Series_title_1) %>%
  mutate(date=as.Date(ifelse(as.character(substr(TIME_REF, 6, 7))!="1",paste0(TIME_REF,".01"),paste0(as.character(TIME_REF),"0.01")),"%Y.%m.%d")) %>%
  filter(timing=="Stock") %>%
  dplyr::select(date,region,DATA_VAL) %>%
  rename(index=DATA_VAL) %>%
  mutate(region=recode(region, 'AKL'="Auckland",
                       'WLG'="Wellington",
                       'RNI'="Rest of North Island",
                       'CAN'="Canterbury",
                       'RSI'="Rest of South Island",
                       'National'="New Zealand")) %>%
  arrange(date) %>%
  group_by(region) %>%
  mutate(ann.change = index/lag(index,12)-1,
         ann.change=round(ann.change*100,1)) %>%
  mutate(date = date_to_month(date)) %>%
  filter(region == 'New Zealand') %>%
  ungroup() %>%
  rename(area = region) %>%
  select(date, index, area) %>%
  rename(rpi_stats_stock = index)

#### 1.2 RPI - TA/region/nz ----
# rpi_combine <- read.csv("data/integrated_RPI_monthly.csv") %>%
#   mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) %>%
#   mutate(date = as.Date(date))

rpi_combine <- read.csv("data/RPI_tidy.csv") %>%
  mutate(level = ifelse(area == "New Zealand", area, "Regional Council")) %>%
  mutate(area = ifelse(level == "Regional Council", paste0(area, " Region"), area)) %>%
  mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) %>%
  mutate(date = as.Date(date))

rpi_nz <- rpi_combine %>%
  filter(level == "New Zealand") %>%
  dplyr::select(date, area, index) %>%
  rename(rpi = index)

rpi_rc <- rpi_combine %>%
  filter(level == "Regional Council") %>%
  dplyr::select(date, area, index) %>%
  rename(rpi = index)



# rpi_combine <- read_excel("data/BigCombo.xlsx") %>%
#   filter(type == "Rent price index") %>%
#   mutate(date = as.Date(date)) %>%
#   dplyr::select(date, index,area) %>%
#   mutate(index = as.numeric(index))

# rpi_nz <- rpi_combine %>%
#   filter(area == "New Zealand") %>%
#   dplyr::select(date, area, index) %>%
#   rename(rpi = index)

# rpi_rc <- rpi_combine %>%
#   filter(grepl("Region", area)) %>%
#   dplyr::select(date, area, index) %>%
#   rename(rpi = index)


#### 1.3 Income - HUD modelled ----
# Median household disposable income

income_hud <- read.csv("data/Income_tidy.csv") %>%
  mutate(level = ifelse(area == "New Zealand", area, "Regional Council")) %>%
  mutate(area = ifelse(level == "Regional Council", paste0(area, " Region"), area)) %>%
  mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) %>%
  mutate(date = as.Date(date))
  

income_hud_nz <- income_hud %>%
  filter(level == "New Zealand") %>%
  mutate(date = date_to_month(as.Date(date))) %>%
  arrange(area, date) %>%
  rename(income = index)

income_hud_rc <- income_hud %>%
  filter(level == "Regional Council") %>%
  mutate(date = date_to_month(as.Date(date))) %>%
  arrange(area, date) %>%
  rename(income = index)

# income_hud <- read.csv("data/HUD_modelled_income_monthly_2022-06-29.csv")
# 
# income_hud_nz <- income_hud %>%
#   filter(level == "New Zealand") %>%
#   filter(inc_type == "Disposable") %>%
#   filter(tenure == "All") %>%
#   filter(series == "Median quarterly household disposable income for all") %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   arrange(area, date) %>%
#   dplyr::select(date, value, area) %>%
#   rename(income = value)
# 
# income_hud_rc <- income_hud %>%
#   filter(level == "Regional Council") %>%
#   filter(inc_type == "Disposable") %>%
#   filter(tenure == "All") %>%
#   filter(series == "Median quarterly household disposable income for all") %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   arrange(area, date) %>%
#   dplyr::select(date, value, area) %>%
#   rename(income = value) %>%
#   mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) 
#   
# 
# income_hud_personal_nz <- income_hud %>%
#   filter(level == "New Zealand") %>%
#   filter(inc_type == "Gross") %>%
#   filter(tenure == "All") %>%
#   filter(series == "Median quarterly personal gross income for all") %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   arrange(area, date) %>%
#   dplyr::select(date, value, area) %>%
#   rename(income_pers = value)
# 
# income_hud_personal_rc <- income_hud %>%
#   filter(level == "Regional Council") %>%
#   filter(inc_type == "Gross") %>%
#   filter(tenure == "All") %>%
#   filter(series == "Median quarterly personal gross income for all") %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   arrange(area, date) %>%
#   dplyr::select(date, value, area) %>%
#   rename(income_pers = value) %>%
#   mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) 

# LEED earnings
earnings_leed <- read.csv("data/leed.csv", skip = 1) %>%
  rename(Period = X)

#Remove rows we don't need
earnings_leed <- earnings_leed[-c(1:2, 90:110), ]

#replace Q1-Q4 with 03,06,09,12
earnings_leed$Period <- str_replace(earnings_leed$Period, "Q1", ".03")
earnings_leed$Period <- str_replace(earnings_leed$Period, "Q2", ".06")
earnings_leed$Period <- str_replace(earnings_leed$Period, "Q3", ".09")
earnings_leed$Period <- str_replace(earnings_leed$Period, "Q4", ".12")

earnings_leed$date <- paste(earnings_leed$Period, "01", sep=".")
earnings_leed$date <- date_to_month(as.Date(earnings_leed$date, format="%Y.%m.%d"))

earnings_leed_nz <- earnings_leed %>%
  dplyr::select(date, Total.all.regions) %>%
  rename(earnings_leed = Total.all.regions) %>%
  mutate(area = "New Zealand") %>%
  mutate(earnings_leed = as.numeric(earnings_leed))

earnings_leed_rc <- earnings_leed %>%
  dplyr::select(-Period, -Total.all.regions) %>%
  rename(`Bay of Plenty` = Bay.of.Plenty,
         `Hawke's Bay` = Hawke.s.Bay,
         `Manawatu-Wanganui` = Manawatu.Wanganui,
         `West Coast` = West.Coast) %>%
  pivot_longer(cols = -date, names_to = "area") %>%
  mutate(area = paste0(area, " Region")) %>%
  rename(earnings_leed = value) %>%
  mutate(earnings_leed = as.numeric(earnings_leed))
  


#### 1.4 Income - average weekly earnings - seasonally adjusted - Stats NZ ----
earnings_sa <- read.csv("data/earnings_sa.csv") %>%
  rename("Period" = "Average.Weekly.Earnings..FTES..Total.All.Ind....Both.Sexes...Seasonally.Adj..Qrtly.Mar.Jun.Sep.Dec.",
         "earnings_sa" = `X`) 
#Remove rows we don't need
earnings_sa <- earnings_sa[-c(1:3, 137:157), ]
#replace Q1-Q4 with 03,06,09,12
earnings_sa$Period <- str_replace(earnings_sa$Period, "Q1", ".03")
earnings_sa$Period <- str_replace(earnings_sa$Period, "Q2", ".06")
earnings_sa$Period <- str_replace(earnings_sa$Period, "Q3", ".09")
earnings_sa$Period <- str_replace(earnings_sa$Period, "Q4", ".12")
earnings_sa$date <- paste(earnings_sa$Period, "01", sep=".")
earnings_sa$date <- as.Date(earnings_sa$date, format="%Y.%m.%d")

earnings_sa <- earnings_sa %>%
  dplyr::select(date, earnings_sa) %>%
  mutate(date = date_to_month(date)) %>%
  mutate(area = "New Zealand") %>%
  mutate(earnings_sa = as.numeric(earnings_sa))


#### 1.5 Income - average weekly earnings - raw - Stats NZ ----
earnings <- read.csv("data/earnings.csv") %>%
  rename("Period" = `Average.Weekly.Earnings..Employees..by.Industry..ANZSIC06..and.Sex..Qrtly.Mar.Jun.Sep.Dec.`,
         "earnings" = `X`) 

#Remove rows we don't need
earnings <- earnings[-c(1:3, 140:160), ]

#replace Q1-Q4 with 03,06,09,12
earnings$Period <- str_replace(earnings$Period, "Q1", ".03")
earnings$Period <- str_replace(earnings$Period, "Q2", ".06")
earnings$Period <- str_replace(earnings$Period, "Q3", ".09")
earnings$Period <- str_replace(earnings$Period, "Q4", ".12")
earnings$date <- paste(earnings$Period, "01", sep=".")
earnings$date <- as.Date(earnings$date, format="%Y.%m.%d")

earnings <- earnings %>%
  dplyr::select(date, earnings) %>%
  mutate(date = date_to_month(date)) %>%
  mutate(area = "New Zealand") %>%
  mutate(earnings = as.numeric(earnings))

#### 1.6 Number of dwellings ----
# Consents interpolate

dwells_rc <- read.csv("data/dwellings_RC.csv") %>%
  mutate(area = paste0(area, " Region")) %>%
  mutate(area=ifelse(area == "Manawatu-Whanganui Region", "Manawatu-Wanganui Region", area)) %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(-ann.change)
 
dwells_nz <- dwells_rc %>%
  group_by(date) %>%
  summarise(dwellings = sum(dwellings, na.rm = T)) %>%
  mutate(area = "New Zealand")

# Stats estimate
dwells_nz_stats <-  read.csv("data/dwells_nz_long.csv") %>%
  rename("Period" = `Estimated.Private.Dwellings..As.At.Quarter.Ended..Qrtly.Mar.Jun.Sep.Dec.`,
         "dwells_nz_stats" = `X`)

#Remove rows we don't need
dwells_nz_stats <- dwells_nz_stats[-c(1, 129:157), ]

#replace Q1-Q4 with 03,06,09,12
dwells_nz_stats$Period <- str_replace(dwells_nz_stats$Period, "Q1", ".03")
dwells_nz_stats$Period <- str_replace(dwells_nz_stats$Period, "Q2", ".06")
dwells_nz_stats$Period <- str_replace(dwells_nz_stats$Period, "Q3", ".09")
dwells_nz_stats$Period <- str_replace(dwells_nz_stats$Period, "Q4", ".12")
dwells_nz_stats$date <- paste(dwells_nz_stats$Period, "01", sep=".")
dwells_nz_stats$date <- as.Date(dwells_nz_stats$date, format="%Y.%m.%d")

dwells_nz_stats <- dwells_nz_stats %>%
  dplyr::select(date, dwells_nz_stats) %>%
  mutate(date = date_to_month(date)) %>%
  mutate(area = "New Zealand") %>%
  mutate(dwells_nz_stats = as.numeric(dwells_nz_stats))

# HUD numbers
# NZ
# dwells_hud_nz <- tbl(con, in_schema("HUD","Stats_Stock")) %>%
#   filter(Group_Type == "NZ") %>%
#   collect() %>%
#   dplyr::select(Year_Month, Dwellings) %>%
#   arrange(Year_Month) %>%
#   rename(date = Year_Month) %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   rename(dwells_hud = Dwellings) %>%
#   mutate(area = "New Zealand")
# 
# region_lookup <- readRDS('TA_lookup.RDS') %>%
#   dplyr::select(Region,Region_Name) %>%
#   distinct()
# 
# dwells_hud_rc <- tbl(con, in_schema("HUD","Stats_Stock")) %>%
#   filter(Group_Type == "Reg") %>%
#   collect() %>%
#   left_join(region_lookup, by = c("Group_ID" = "Region")) %>%
#   dplyr::select(Year_Month, Region_Name, Dwellings) %>%
#   arrange(Year_Month) %>%
#   rename(date = Year_Month) %>%
#   mutate(date = date_to_month(as.Date(date))) %>%
#   filter(!grepl("Outside", Region_Name)) %>%
#   rename(area = Region_Name) %>%
#   rename(dwells_hud = Dwellings)


#### 1.7 Population estimates ----
# Nationally

pop_nz <- read.csv("data/pop_nz.csv") %>%
  rename(Period = `Estimated.Resident.Population.by.Age.and.Sex..1991....Qrtly.Mar.Jun.Sep.Dec.`,
         pop = `X`)

#Remove rows we don't need
pop_nz <- pop_nz[-c(1:3, 132:154), ]
#replace Q1-Q4 with 03,06,09,12
pop_nz$Period <- str_replace(pop_nz$Period, "Q1", ".03")
pop_nz$Period <- str_replace(pop_nz$Period, "Q2", ".06")
pop_nz$Period <- str_replace(pop_nz$Period, "Q3", ".09")
pop_nz$Period <- str_replace(pop_nz$Period, "Q4", ".12")
pop_nz$date <- paste(pop_nz$Period, "01", sep=".")
pop_nz$date <- as.Date(pop_nz$date, format="%Y.%m.%d")

pop_nz <- pop_nz %>%
  dplyr::select(date, pop) %>%
  mutate(date = date_to_month(date),
         pop = as.numeric(pop)) %>%
  mutate(area = "New Zealand")

# Region council
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

#### 1.7a Adult population
adult_pop_nz <- read.csv("data/adult_pop_nz.csv") %>%
  rename(Period = `Estimated.Resident.Population.by.Age.and.Sex..1991....Qrtly.Mar.Jun.Sep.Dec.`,
         adult_pop = `X`)

#Remove rows we don't need
adult_pop_nz <- adult_pop_nz[-c(1:3, 132:154), ]
#replace Q1-Q4 with 03,06,09,12
adult_pop_nz$Period <- str_replace(adult_pop_nz$Period, "Q1", ".03")
adult_pop_nz$Period <- str_replace(adult_pop_nz$Period, "Q2", ".06")
adult_pop_nz$Period <- str_replace(adult_pop_nz$Period, "Q3", ".09")
adult_pop_nz$Period <- str_replace(adult_pop_nz$Period, "Q4", ".12")
adult_pop_nz$date <- paste(adult_pop_nz$Period, "01", sep=".")
adult_pop_nz$date <- as.Date(adult_pop_nz$date, format="%Y.%m.%d")

adult_pop_nz <- adult_pop_nz %>%
  dplyr::select(date, adult_pop) %>%
  mutate(date = date_to_month(date),
         adult_pop = as.numeric(adult_pop)) %>%
  mutate(area = "New Zealand")

# Region council
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




#### 1.8 People per dwelling ----
pop_per_dwell_nz <- pop_nz %>%
  left_join(dwells_nz) %>%
  mutate(ppd = pop/dwellings) %>%
  dplyr::select(area, date, ppd)

pop_per_dwell_rc <- dwells_rc %>%
  #mutate(year = year(date)) %>%
  left_join(pop_rc) %>%
  filter(date <= "2021-06-30" & date>="2006-06-30") %>%
  # Linear interpolation
  group_by(area) %>%
  arrange(area, date) %>%
  mutate(pop = na.approx(pop)) %>%
  mutate(ppd = pop/dwellings)  %>%
  dplyr::select(area, date, ppd)

#### 1.9 Unemployment rate ----

# unemp <- read_csv("data/unemployment.csv")
# colnames(unemp) = c("Quarter", "Men", "Women", "Total")
# 
# unemp <- unemp %>%
#   dplyr::select(Quarter, Total) %>%
#   rename(unemp = Total) %>%
#   mutate(month = match(substr(Quarter,1,3), month.abb),
#          year = as.numeric(paste0("20",substr(Quarter,5,6)))) %>%
#   mutate(date = date_to_month(as.Date(paste0(year,"-",as.character(month), "-01")))) %>%
#   dplyr::select(date, unemp) 


unemp <- read_csv("data/unemployment_long.csv")

unemp <- unemp %>%
  rename("Period" = `Labour Force Status by Sex by Age Group (Qrtly-Mar/Jun/Sep/Dec)`,
         "unemp" = `...2`) 

#Remove rows we don't need
unemp <- unemp[-c(1:3, 150:178), ]

#replace Q1-Q4 with 03,06,09,12
unemp$Period <- str_replace(unemp$Period, "Q1", ".03")
unemp$Period <- str_replace(unemp$Period, "Q2", ".06")
unemp$Period <- str_replace(unemp$Period, "Q3", ".09")
unemp$Period <- str_replace(unemp$Period, "Q4", ".12")

unemp$date <- paste(unemp$Period, "01", sep=".")
unemp$date <- as.Date(unemp$date, format="%Y.%m.%d")

unemp <- unemp %>%
  filter(unemp!="..") %>%
  mutate(date = date_to_month(date),
         unemp = as.numeric(unemp)) %>%
  dplyr::select(date, unemp) 

#### 1.11 General inflation - CPI excludes rents ---- 
CPI <- read.csv("data/CPI.csv")

CPI <- CPI %>%
  rename("Period" = `CPI.All.Groups.for.New.Zealand..Qrtly.Mar.Jun.Sep.Dec.`,
         "cpi" = `X`) 


#### 1.10 General inflation - CPI excludes rents ---- 
CPIexRent_one <- read.csv("data/CPIexclRents.csv")

CPIexRent_two <- CPIexRent_one %>%
  rename("Period" = `CPI.Non.standard.All.Groups.Less.Plus.Selected.Groupings.for.New.Zealand..Qrtly.Mar.Jun.Sep.Dec.`,
         "cpi_exRent" = `X`) 

#Remove rows we don't need
CPIexRent_two <- CPIexRent_two[-c(1, 139:169), ]

#replace Q1-Q4 with 03,06,09,12
CPIexRent_two$Period <- str_replace(CPIexRent_two$Period, "Q1", ".03")
CPIexRent_two$Period <- str_replace(CPIexRent_two$Period, "Q2", ".06")
CPIexRent_two$Period <- str_replace(CPIexRent_two$Period, "Q3", ".09")
CPIexRent_two$Period <- str_replace(CPIexRent_two$Period, "Q4", ".12")

CPIexRent_two$date <- paste(CPIexRent_two$Period, "01", sep=".")
CPIexRent_two$date <- as.Date(CPIexRent_two$date, format="%Y.%m.%d")

CPIexRent <- CPIexRent_two %>%
  filter(cpi_exRent!="..") %>%
  mutate(date = date_to_month(date),
         cpi_exRent = as.numeric(cpi_exRent)) %>%
  dplyr::select(date, cpi_exRent) 

#### 1.11 General inflation - CPI excludes rents ---- 
CPI <- read.csv("data/CPI.csv")

CPI <- CPI %>%
  rename("Period" = `CPI.All.Groups.for.New.Zealand..Qrtly.Mar.Jun.Sep.Dec.`,
         "cpi" = `X`) 

#Remove rows we don't need
CPI_two <- CPI[-c(1, 434:458), ]

#replace Q1-Q4 with 03,06,09,12
CPI_two$Period <- str_replace(CPI_two$Period, "Q1", ".03")
CPI_two$Period <- str_replace(CPI_two$Period, "Q2", ".06")
CPI_two$Period <- str_replace(CPI_two$Period, "Q3", ".09")
CPI_two$Period <- str_replace(CPI_two$Period, "Q4", ".12")

CPI_two$date <- paste(CPI_two$Period, "01", sep=".")
CPI_two$date <- as.Date(CPI_two$date, format="%Y.%m.%d")

CPI <- CPI_two %>%
  filter(cpi!="..") %>%
  mutate(date = date_to_month(date),
         cpi = as.numeric(cpi)) %>%
  dplyr::select(date, cpi) %>%
  filter(year(date)>=2000)


#### 1.12 House price index ----
hpi_combine <- dbGetQuery(con, "select * FROM [HUD].[Stats_HPI]")

hpi_nz <- hpi_combine %>%
  filter(Stat_Type_ID == 100,
         Group_Type == "NZ",
         Grouping2 == 0) %>%
  mutate(date = date_to_month(as.Date(Year_Month))) %>%
  dplyr::select(date, HPI_Index) %>%
  arrange(date) %>%
  mutate(area = "New Zealand") %>%
  rename(hpi = HPI_Index)

#### 1.13 Vacancy rate ----
#------Tenancies starting by month
stats_bonds_new <- dbGetQuery(con, "select * from property.hud.stats_bonds_new as N
                                      where N.group_type in ('NZ') and N.Group_ID in ('0') and
                                      N.stat_type_id = '100' and
                                      N.bedrooms = '0'")

stats_bonds_new <- stats_bonds_new %>% 
  arrange(Group_ID,Bond_Year_Month) %>% 
  dplyr::select(Bond_Year_Month, How_Many_Private, Group_ID) %>% 
  group_by(Group_ID) 

#Create month/year variable
stats_bonds_new$START <- as.Date(stats_bonds_new$Bond_Year_Month, format = "%Y-%m-%d")
stats_bonds_new$Start_Mth <- month(stats_bonds_new$Bond_Year_Month)
stats_bonds_new$Start_Yr <- as.numeric(year(stats_bonds_new$Bond_Year_Month))
stats_bonds_new$Start_monthYr <- format(as.Date(stats_bonds_new$Bond_Year_Month), "%Y-%m")
stats_bonds_new$Date <- as.Date(stats_bonds_new$Bond_Year_Month)

##### Vacancy rate
#new tenancies/active bonds (for private rentals only)
Bondsnew_vacancy <- stats_bonds_new %>% 
  dplyr::select(Bond_Year_Month, How_Many_Private, Group_ID) %>% 
  #filter(Bond_Year_Month >= "2006-01-01") %>% 
  rename("Num_new"=How_Many_Private) %>% 
  arrange(Bond_Year_Month) %>%
  mutate(Bond_Year_Month = as.Date(Bond_Year_Month)) 

# access active bonds per month
bonds_active <- dbGetQuery(con, "select * from property.hud.stats_bonds_active as rentals
                                      where rentals.group_type in ('NZ') and Group_ID in ('0') and
                                      rentals.stat_type_id = '100' and
                                      rentals.bedrooms = '0'") 


bonds_active <- bonds_active %>% 
  filter(Bond_Year_Month >= "2000-01-01") %>% 
  rename("Num_active" = How_Many_Private) %>% 
  dplyr::select(Bond_Year_Month, Num_active, Group_ID) %>% 
  mutate(Bond_Year_Month = as.Date(Bond_Year_Month)) %>%
  arrange(Bond_Year_Month)

#Create 'vacancy rate' (number of new tenancies to total active bonds)
vacancy_rate <- bonds_active %>%
  left_join(Bondsnew_vacancy, by = c("Bond_Year_Month" = "Bond_Year_Month", "Group_ID" = "Group_ID")) %>% 
  mutate(vacancy = ((Num_new/Num_active)*100)) %>%
  mutate(date = date_to_month(as.Date(Bond_Year_Month))) %>%
  dplyr::select(date, vacancy) %>%
  mutate(area = "New Zealand")

#### 1.14 Floating mortgage rates ----
irate <- read.csv("data/mortgage_rate.csv") %>%
  mutate(date = date_to_month(ymd("1987-03-01")+ months(0:431))) %>%
  #rename(Date = `ï..Date`) %>%
  #mutate(date = zoo::as.Date(Date, format = "%d-%m-%Y")) %>%
  rename(mrate = Floating.mortgage.rate) %>%
  dplyr::select(mrate,date)

irate_2y <- dbGetQuery(con, "select * from [Source].[RBNZ_2Y_Special_Interest_Rate]") %>%
  mutate(date = date_to_month(as.Date(Date))) %>%
  rename(mrate_2y = Interest_rate) %>%
  dplyr::select(date, mrate_2y)
  


#### 1.15 Median vacant time ----
vacant_time <- readRDS("data/vacant_time_quarter.RDS") %>%
  rename(date = YM) %>%
  select(-bond_count)


#### 1.16 Geometric mean of rents ----
mbie_bonds <- read_excel("data/mbie_bonds.xlsx") %>%
  rename(rent_geomean = `Geometric Mean Rent`,
         date = `Time Frame`) %>%
  dplyr::select(date, rent_geomean) %>%
  mutate(date = as.Date(date_to_month(date))) %>%
  mutate(area = "New Zealand")

#### 1.17 CPI rents ----
CPI_rent <- read.csv("data/cpi_rents.csv")

CPI_rent <- CPI_rent %>%
  rename("Period" = `CPI.Level.2.Subgroups.for.New.Zealand..Qrtly.Mar.Jun.Sep.Dec.`,
         "cpi_rent" = `X`) 

#Remove rows we don't need
CPI_rent <- CPI_rent[-c(1:98, 193:215), ]

#replace Q1-Q4 with 03,06,09,12
CPI_rent$Period <- str_replace(CPI_rent$Period, "Q1", ".03")
CPI_rent$Period <- str_replace(CPI_rent$Period, "Q2", ".06")
CPI_rent$Period <- str_replace(CPI_rent$Period, "Q3", ".09")
CPI_rent$Period <- str_replace(CPI_rent$Period, "Q4", ".12")

CPI_rent$date <- paste(CPI_rent$Period, "01", sep=".")
CPI_rent$date <- as.Date(CPI_rent$date, format="%Y.%m.%d")

CPI_rent <- CPI_rent %>%
  filter(cpi_rent!="..") %>%
  mutate(date = date_to_month(date),
         cpi_rent = as.numeric(cpi_rent)) %>%
  dplyr::select(date, cpi_rent) 




combined_nz <- mbie_bonds %>%
  left_join(rpi_nz) %>%
  left_join(rpi_stats_flow_nz) %>%
  left_join(rpi_stats_stock_nz) %>%
  left_join(income_hud_nz) %>%
  #left_join(income_hud_personal_nz) %>%
  left_join(earnings) %>%
  left_join(earnings_sa) %>%
  left_join(earnings_leed_nz) %>%
  left_join(unemp) %>%
  left_join(dwells_nz) %>%
  #left_join(dwells_hud_nz) %>%
  left_join(pop_nz) %>%
  left_join(adult_pop_nz) %>%
  left_join(pop_per_dwell_nz) %>%
  left_join(vacancy_rate) %>%
  left_join(hpi_nz) %>%
  left_join(CPIexRent) %>%
  left_join(CPI) %>%
  left_join(irate) %>%
  left_join(irate_2y) %>%
  
  left_join(vacant_time) %>%
  left_join(CPI_rent) %>%
  left_join(dwells_nz_stats) 

saveRDS(combined_nz, "data/combined_nz_20230308.RDS")

