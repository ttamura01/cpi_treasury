setwd("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/code")
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(fredr) 

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

#---Download Data---
# CPI
cpi <- fredr(series_id = "CPIAUCSL") %>% 
  mutate("cpi_yoy" = ((value/lag(value, 12)-1)*100)) %>% 
  select(date, cpi = cpi_yoy)

#core-cpi
ccpi <- fredr(series_id = "CPILFESL") %>% 
  mutate("ccpi_yoy" = ((value/lag(value, 12)-1)*100)) %>% 
  select(date, ccpi = ccpi_yoy)

cpi_ccpi <-  cpi %>% 
  left_join(ccpi, by = "date")

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
year <- year(latest_data$date)
latest_month <- month(latest_data$date)
latest_month_label <- month(latest_month, label = TRUE, abbr = FALSE)
latest_cpi <- round(latest_data$cpi, 1)
latest_ccpi <- round(latest_data$ccpi, 1)
latest_cpi_status <- latest_data$cpi_montly_change_status
latest_ccpi_status <- latest_data$ccpi_montly_change_status

#Visualazing with GGPLOT
sapply(cpi_ccpi, class)
#cpi_ccpi$date <- as.Date(cpi_ccpi$date)

cpi_ccpi %>% 
  filter(date >="2011-01-01") %>% 
  select(date, cpi, ccpi) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  scale_color_manual(breaks = c("ccpi", "cpi"),
                     values = c("#0079ae","#000000" )) +
  geom_hline(aes(yintercept = 2)) +
  labs(title = glue("US inflation in {latest_month_label}-{year} {latest_cpi_status} to {latest_cpi}%, core-CPI {latest_ccpi_status} at {latest_ccpi}% in-line with market expected"),
       x = NULL, y = "Inflation(%)",
       caption = "Labor Department, FRED, by Takayuki Tamura") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )

cpi_ccpi %>% 
  filter(date >="2015-01-01") %>% 
  select(date, cpi, ccpi) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line(linewidth = 2,lineend = "round", show.legend = TRUE) +
  geom_hline(aes(yintercept = 2)) +
  labs(title = glue("US inflation in {latest_month_label} at {latest_cpi}%(cpi), {latest_ccpi}%(core) 
                    keeping a Federal Reserve rate cut in play for September"),
       subtitle = "in the finaincal market 89% expect 25bps of rate cut, and 11% expect 50pbs rate cut in the meeting on Sep-17", 
       x = NULL, y = "inflation (%)",
       caption = "Labor Department, FRED, by Takayuki Tamura") +
  scale_color_manual(breaks = c("ccpi", "cpi"),
                     values = c("darkred", "darkgreen")) +
  # scale_y_continuous(label = scales::label_percent()) +
  # annotate("text", x =c(2024-11-01, 2024-11-01), y = c(3.5, 2.8),
  #          label = c("core-CPI", "CPI")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 18, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 14, face = "italic"),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "top",
    panel.grid.major.y = element_line(),
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15, face = "bold")
  )

ggsave("us_cpi.png", height = 4, width = 6)    
