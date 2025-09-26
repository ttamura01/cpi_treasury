library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(fredr)

setwd("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/code")

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

pce <- fredr(series_id = "PCEPI") %>% 
  select(date, pce = value) %>% 
  mutate(pce_yoy = round((pce/lag(pce, 12)-1)*100, 1)) %>% 
  na.omit() %>% 
  select(date, pce_yoy)

pce %>% 
  ggplot(aes(x = date, y = pce_yoy)) +
  geom_line()

core_pce <- fredr(series_id = "PCEPILFE") %>% 
  select(date, core_pce = value) %>% 
  mutate(core_pce_yoy = round((core_pce/lag(core_pce, 12)-1)*100, 1)) %>% 
  na.omit() %>% 
  select(date, core_pce_yoy)

core_pce %>% 
  ggplot(aes(x = date, y = core_pce_yoy)) +
  geom_line()

## understand the core inflation each period 
dated <- core_pce %>% 
  filter(date >= "1960-01-01" & date <= "2025-12-31")

dated %>% 
  ggplot(aes(x = date, y = core_pce_yoy)) +
  geom_line()

dated %>% 
  mutate(average_core_pce = mean(core_pce_yoy))


pce_core_pce <- merge(pce, core_pce, by = "date") #%>% select(-X)

# write.csv(pce_cpce, "pce_cpce.csv")
# 
# read_csv("pce_cpce.csv") %>% 
#   select(-...1)

tail(pce_core_pce)

pce_core_pce %>% 
  filter(date > "2015-01-01") %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2) +
  labs(title = "The PCE inflation remain stubbornly high at 2.6%, Core index 2.8% in June",
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

pce_cpce <- pce_core_pce %>% 
  mutate(pce_monthly_change = c(NA, diff(pce_yoy)),
         pce_montly_change_status = if_else(is.na(pce_monthly_change), NA_character_,
                                            if_else(pce_monthly_change> 0, "increased",
                                                    if_else(pce_monthly_change <0, "eased", "flat")))) %>% 
  mutate(core_pce_monthly_change = c(NA, diff(core_pce_yoy)),
         core_pce_montly_change_status = if_else(is.na(core_pce_monthly_change), NA_character_,
                                            if_else(core_pce_monthly_change> 0, "increased",
                                                    if_else(core_pce_monthly_change <0, "eased", "flat"))))

# latest month

latest_data <- pce_cpce %>% 
  filter(date == max(date))

latest_month <- format(latest_data$date, "%B")

latest_pce_yoy <- latest_data$pce_yoy
latest_core_pce <- latest_data$core_pce_yoy
latest_pce_status <- latest_data$pce_montly_change_status
latest_core_pce_status <- latest_data$core_pce_montly_change_status

order_levels <- pce_core_pce %>% 
  filter(date == max(pce_core_pce$date)) %>% 
  # select(-date) %>% 
  pivot_longer(-date, names_to = "name", values_to = "value" ) %>% 
  arrange(desc(value)) %>% 
  pull(name)

pce_core_pce %>% 
  filter(date >= "2015-01-01") %>% 
  select(date, pce_yoy, core_pce_yoy) %>% 
  pivot_longer(cols = -date, names_to = "name", values_to = "inflation") %>% 
  mutate(name = factor(name, levels = order_levels)) %>% 
  ggplot(aes(x = date, y = inflation, color = name)) +
  geom_line() +
  geom_hline(yintercept = 2) +
  scale_color_manual(
    name = NULL,
    # breaks = c(pce_yoy, core_pce_yoy), 
    values = c("#000000", "#0079ae" ),
    labels = c("core-PCE (%)", "PCE(%)")) +
  labs(
    title = glue("US PCE inflation {latest_pce_status} in {latest_month} to {latest_pce_yoy}%, core_PCE {latest_core_pce_status}, to {latest_core_pce}%, modest inflation vs. 2.0% target"),
    x = NULL, y = "percent",
    caption = "Labor Department, FRED, by Takayuki Tamura"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    panel.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.2)
  )

ggsave("pce_inflation.png", width = 6, height = 5)

# cip & ccpi
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

cpi <- fredr(series_id = "CPIAUCSL") %>% 
  select(date, cpi = value) %>% 
  mutate(cpi_yoy = round((cpi/lag(cpi, 12)-1)*100, 1)) %>% 
  na.omit() %>% 
  select(date, cpi_yoy)

core_cpi <- fredr(series_id = "CPILFESL") %>% 
  select(date, core_cpi = value) %>% 
  mutate(core_cpi_yoy = round((core_cpi/lag(core_cpi, 12)-1)*100, 1)) %>% 
  na.omit() %>% 
  select(date, core_cpi_yoy)

cpi_core_cpi <- cpi %>% 
  left_join(., core_cpi, by = "date")

cpi_ccpi <- cpi_core_cpi %>% 
  mutate(cpi_monthly_change = c(NA, diff(cpi_yoy)),
         cpi_montly_change_status = if_else(is.na(cpi_monthly_change), NA_character_,
                                         if_else(cpi_monthly_change> 0, "increased",
                                                 if_else(cpi_monthly_change <0, "eased", "flat")))) %>% 
  mutate(core_cpi_monthly_change = c(NA, diff(core_cpi_yoy)),
         core_cpi_montly_change_status = if_else(is.na(core_cpi_monthly_change), NA_character_,
                                            if_else(core_cpi_monthly_change> 0, "increased",
                                                    if_else(core_cpi_monthly_change <0, "eased", "flat"))))

# latest data for labeling
latest_data <- cpi_ccpi %>% 
  filter(date == max(date))

latest_month <- format(latest_data$date, "%B")

latest_cpi_yoy <- latest_data$cpi_yoy
latest_core_cpi_yoy <- latest_data$core_cpi_yoy
latest_cpi_status <- latest_data$cpi_montly_change_status
latest_core_cpi_status <- latest_data$core_cpi_montly_change_status

order_levels <- cpi_core_cpi %>% 
  filter(date == max(cpi_core_cpi$date)) %>% 
  # select(-date) %>% 
  pivot_longer(-date, names_to = "index", values_to = "inflation" ) %>% 
  arrange(desc(inflation)) %>% 
  pull(index)

cpi_core_cpi %>% 
  filter(date >="2011-01-01") %>% 
  select(date, cpi_yoy, core_cpi_yoy) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  mutate(name = factor(index, levels = order_levels)) %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  scale_color_manual(breaks = c("core_cpi_yoy", "cpi_yoy"),
                     values = c("#0079ae", "#000000"),
                     label = c("core-CPI (%)", "CPI (%)")) +
  geom_hline(yintercept = 2) +
  labs(title = glue("US inflation in {latest_month} {latest_cpi_status} to {latest_cpi_yoy}%, core-CPI {latest_core_cpi_status} at {latest_core_cpi_yoy}% in-line with market expected"),
       x = NULL, y = "Inflation(%)",
       caption = "Labor Department, FRED, by Takayuki Tamura") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",
    panel.background = element_blank()
  )

cpi_core_cpi %>% 
  filter(date >="2015-01-01") %>% 
  select(date, cpi_yoy, core_cpi_yoy) %>% 
  pivot_longer(cols = -date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line(linewidth = 2,lineend = "round", show.legend = TRUE) +
  geom_hline(yintercept = 2) +
  labs(title = glue("US inflation in {latest_month} at {latest_cpi_yoy}%(cpi), {latest_core_cpi_yoy}%(core)"),
       x = NULL, y = "inflation (%)",
       caption = "Labor Department, FRED, by Takayuki Tamura") +
  scale_color_manual(breaks = c("core_cpi_yoy", "cpi_yoy"),
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

ggsave("/Users/takayukitamura/Documents/R_Computing/figures/cpi_pce.png", height = , width = 6)



