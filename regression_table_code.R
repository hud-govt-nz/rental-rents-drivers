#### 1. Baseline all variables ----
model1a <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i)
              +lag(ppd.change,0)
              +lag(ppd.change,i)
              +lag(mrate.change,0)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,0)
              +lag(cpi_exRent.change,i)
              +lag(unemp,0)
              +lag(unemp,i)
              +lag(hpi.change,0)
              +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

# Run some tests
tab_model(model1a)

bptest(model1a)
coeftest(model1a, vcov = vcovHC(model1b, type = "HC0"))
BIC(model1a)
AIC(model1a, k=2)

#### 2. Remove some variables ----
model1b <- lm(rpi.change ~ 
                lag(rpi.change,i) 
              +lag(earnings.change,0) 
              +lag(earnings.change,i) 
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(cpi_exRent.change,i)
              +lag(unemp,i)
              +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly) 

# Run some tests
tab_model(model1b)

bptest(model1b)
coeftest(model1b, vcov = vcovHC(model1b, type = "HC0"))
BIC(model1b)
AIC(model1b, k=2)

#### 3. RPI stock ----
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
              +lag(unemp,i)
              +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly_stock) 

tab_model(model1c)

# Run some tests
bptest(model1c)
coeftest(model1c, vcov = vcovHC(model1c, type = "HC0"))
BIC(model1c)
AIC(model1c, k=2)

#### 4. Real rent flow ----
# Replace nominal change with real change
combined_nz_quarterly_real <- combined_nz_quarterly %>%
  mutate(rpi.change = rpi_real.change) %>%
  mutate(hpi.change = hpi_real.change) %>%
  mutate(earnings.change = earnings_real.change)

model1d <- lm(rpi.change ~ 
                lag(rpi.change,i)
              +lag(earnings.change,0) 
              +lag(earnings.change,i)
              +lag(ppd.change,i)
              +lag(mrate.change,i)
              +lag(unemp,i)
              +lag(hpi.change,i)
              ,
              data = combined_nz_quarterly_real) 

tab_model(model1d)

# Run some tests
bptest(model1d)
coeftest(model1d, vcov = vcovHC(model1d, type = "HC0"))
BIC(model1d)
AIC(model1d, k=2)


#### 5. Put everything in a table ----
labels <- c("Rent inflation (lagged)", 
            "Wage growth",
            "Wage growth (lagged)",
            "People per dwelling", 
            "People per dwelling (lagged)", 
            "Floating mortgage rate",
            "Floating mortgage rate (lagged)",
            "Inflation excluding rents",
            "Inflation excluding rents (lagged)",
            "Unemployment rate",
            "Unemployment rate (lagged)",
            "House price inflation",
            "House price inflation (lagged)"
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
          file = "results_updated/baseline_test.doc") 