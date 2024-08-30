library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
setwd("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/code")
pce <- read.csv("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/data/pce.csv") %>% 
  select(-X)

tail(pce)

updates <- tibble(date=c("2024-04-01", "2024-05-01", "2024-06-01", "2024-07-01"), 
                       pce=c(2.7, 2.6, 2.5, 2.5))
  
pce <- rbind(pce, updates)

#write.csv(pce, "pce.csv")

pce$date <- as.Date(pce$date)

sapply(pce, class)

pce %>% 
  ggplot(aes(x = date, y = pce)) +
  geom_line()

pce %>% 
  ggplot(aes(x = date, y = pce)) +
  geom_line() 

core_pce <- read.csv("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/data/core_pce.csv") %>% 
  select(-X)
tail(core_pce)
updates <- tibble(date=c("2024-04-01", "2024-05-01", "2024-06-01", "2024-07-01"), 
                            core_pce=c(2.8, 2.6, 2.6, 2.7))

core_pce <- rbind(core_pce, updates)

tail(core_pce)

#write.csv(core_pce, "core_pce.csv")

core_pce$date <- as.Date(core_pce$date)

head(core_pce)

tail(core_pce)

sapply(core_pce, class)

## understand the core inflation each period 
dated <- core_pce %>% 
  filter(date >= "2020-01-01" & date <= "2024-12-31")

dated %>% 
  ggplot(aes(x = date, y = core_pce)) +
  geom_line()

dated %>% 
  mutate(average_core_pce = mean(core_pce))


pce_cpce <- merge(pce, core_pce, by = "date")

#write.csv(pce_cpce, "pce_cpce.csv")

pce_cpce %>% 
  filter(date > "2020-01-01") %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2) +
  labs(title = "The PCE inflation index eased to 2.6% and Core index to 2.6% in May, from April 2.7& and 2.8%, in-line with market expected",
       #subtitle = "Housing & related ccount for 34% in CPI, but only 15% in PCE",
       caption = "Source:Department of Commerce",
       x = NULL, y = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(vjust = 1.5),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white")
  )

# addting monthly pce_change and pce_change_status

pce_cpce <- pce_cpce %>% 
  mutate(pce_monthly_change = c(NA, diff(pce)),
         pce_montly_change_status = if_else(is.na(pce_monthly_change), NA_character_,
                                            if_else(pce_monthly_change> 0, "increased",
                                                    if_else(pce_monthly_change <0, "eased", "flat")))) %>% 
  mutate(cpce_monthly_change = c(NA, diff(core_pce)),
         cpce_montly_change_status = if_else(is.na(cpce_monthly_change), NA_character_,
                                            if_else(cpce_monthly_change> 0, "increased",
                                                    if_else(cpce_monthly_change <0, "eased", "flat"))))

# latest month

max(pce_cpce$date)

latest_data <- pce_cpce %>% 
  filter(date == max(date))

sapply(latest_data, class)

latest_data$date <- as.Date(latest_data$date)

latest_month <- latest_data[,1]
class(latest_month)
month_abbr <- format(latest_month, "%b")
latest_pce <- latest_data[,2]
latest_cpce <- latest_data[,3]
latest_pce_status <- latest_data[,5]
latest_cpce_status <- latest_data[,7]

pce_cpce %>% 
  filter(date >="2015-01-01") %>% 
  select(date, pce, core_pce) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_hline(aes(yintercept = 2)) +
  labs(title = glue("US inflation {latest_pce_status } in {month_abbr}
                    (guaged by Personal Consumption Expeditures, which is used as 
                    primary inflation indicator by Fed for their rate decision) 
                    to {latest_pce}, 
                    approaching to their target at 2.0%"),
       x = NULL, y = "percent",
       caption = "Labor Department, FRED") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/pce_inflation_2024_05_01.png", width = 6, height = 5)

# cip & ccpi

cpi_ccpi <- read.csv("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/data/cpi_ccpi_03_2024.csv") %>% 
  select(-X)

updates <- tibble(date = c("2024-04-01", "2024-05-01", "2024-06-01", "2024-07-01"),
                              cpi = c(3.4, 3.3, 3.0, 2.9),
                              ccpi = c(3.6, 3.4, 3.3, 3.2))
updates

cpi_ccpi <- rbind(cpi_ccpi, updates)


head(cpi_ccpi)
tail(cpi_ccpi)

cpi_ccpi <- cpi_ccpi %>% 
  mutate(cpi_monthly_change = c(NA, diff(cpi)),
         cpi_montly_change_status = if_else(is.na(cpi_monthly_change), NA_character_,
                                         if_else(cpi_monthly_change> 0, "increased",
                                                 if_else(cpi_monthly_change <0, "eased", "flat")))) %>% 
  mutate(ccpi_monthly_change = c(NA, diff(ccpi)),
         ccpi_montly_change_status = if_else(is.na(ccpi_monthly_change), NA_character_,
                                            if_else(ccpi_monthly_change> 0, "increased",
                                                    if_else(ccpi_monthly_change <0, "eased", "flat"))))

max(cpi_ccpi$date)
latest_data <- cpi_ccpi %>% 
  filter(date == max(date))

latest_data$date <- as.Date(latest_data$date)

latest_month <- latest_data[,1]
class(latest_month)
month_abbr <- format(latest_month, "%b")
latest_cpi <- latest_data[,2]
latest_ccpi <- latest_data[,3]
latest_cpi_status <- latest_data[,5]
latest_ccpi_status <- latest_data[,7]


####
# cpi <- cpi %>% 
#   mutate(m_change = c(NA, diff(cpi)),
#          change_status = if_else(is.na(m_change), NA_character_,
#                                  if_else(m_change> 0, "increased",
#                                          if_else(m_change <0, "eased", "flat")))
#   )

####

sapply(cpi_ccpi, class)

cpi_ccpi$date <- as.Date(cpi_ccpi$date)


cpi_ccpi %>% 
  filter(date >="2015-01-01") %>% 
  select(date, cpi, ccpi) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_hline(aes(yintercept = 2)) +
  labs(title = glue("US inflation {latest_cpi_status} in {month_abbr} to {latest_cpi}%(cpi), {latest_ccpi}%(core cpi) vs. Fed's target at 2.0%, lower than the market expected"),
       x = NULL, y = "percent",
       caption = "Labor Department, FRED") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple()
  )

  
ggsave("us_cpi.png", height = 4, width = 5)  
ggsave("/Users/takayukitamura/Documents/R_Computing/figures/us_cpi.png", height = 4, width = 5)

cpi_pce_full <- merge(cpi_ccpi, pce_cpce, by = "date") 

head(cpi_pce_full)
tail(cpi_pce_full)
  
cpi_pce_full %>% 
  select(date, cpi, pce) %>% 
  # filter(date >= "2020-01-01") %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2) +
  labs(title = "The gap between CPI and PCE is wide in this cycle",
       subtitle = "Housing & related ccount for 34% in CPI, but only 15% in PCE",
       caption = "Source: Department of Labor, Department of Commerce",
       x = NULL, y = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white")
  )



cpi_pce_full %>% 
  select(date, cpi, pce) %>% 
  filter(date >= "2015-01-01") %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2) +
  labs(title = glue("US inflation {latest_pce_status } in {month_abbr}(guaged by PCE, which is used primarily for macroeconomic analysis and forecast) to {latest_pce}, approaching to Fed's target at 2.0%"),
       #subtitle = "Housing & related ccount for 34% in CPI, but only 15% in PCE",
       caption = "Source: Department of Labor, Department of Commerce",
       x = NULL, y = NULL) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white")
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/cpi_pce.png", height = 4.5, width = 6)



