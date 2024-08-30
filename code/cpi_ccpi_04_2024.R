rm(list = ls())
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
setwd("/Users/takayukitamura/Documents/R_Computing/cpi_treasury")

## CPI
# cpi <- read.table("/Users/takayukitamura/Documents/US Economics/consumer_price.csv",
#            sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
#   rename_all(tolower)

cpi_1 <- cpi %>% 
  gather(key = "month", value = "cpi", - year) 

head(cpi_1)
sapply(cpi_1, class)

cpi_1$date <- as.Date(paste(cpi_1$year, cpi_1$month, "01", sep = "-"), format = "%Y-%b-%d")

cpi_1 <- cpi_1 %>% 
  select(date, cpi)

ggplot(cpi_1, aes(x = date, y = cpi)) +
  geom_line()

## Core_CPI
ccpi <- read.table("/Users/takayukitamura/Documents/US Economics/core_cpi.csv",
           sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  select(-HALF1, -HALF2) %>% 
  rename_all(tolower) 

ccpi_1 <- ccpi %>% 
  gather(key = "month", value = "ccpi", - year) 

head(ccpi_1)
sapply(ccpi_1, class)

ccpi_1$date <- as.Date(paste(ccpi_1$year, ccpi_1$month, "01", sep = "-"), format = "%Y-%b-%d")

ccpi_1 <- ccpi_1 %>% 
  select(date, ccpi)

ggplot(ccpi_1, aes(x = date, y = ccpi)) +
  geom_line()

## merge cpi_1 and ccpi_1
cpi_ccpi <- merge(cpi_1, ccpi_1, by = "date") %>% filter(date < "2023-11-01")

tail(cpi_ccpi)

cpi_ccpi_2023_03 <- tibble(
  date = c("2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01", "2024-03-01"),
  cpi = c(3.1, 3.4, 3.1, 3.2, 3.5),
  ccpi = c(3.7, 3.9, 3.9, 3.8, 3.8)) %>% 
  mutate(date = as.Date(date))

cpi_ccpi <- rbind(cpi_ccpi,cpi_ccpi_2023_03)

write.csv("cpi_ccpi.csv")

tail(cpi_ccpi)
head(cpi_ccpi)

write.csv(cpi_ccpi, "cpi_ccpi_03_2024.csv")

cpi_ccpi <- read.csv("cpi_ccpi_03_2024.csv") %>% 
  select(-X)

sapply(cpi_ccpi, class)

cpi_ccpi$date <- as.Date(cpi_ccpi$date)

cpi_ccpi %>% 
  filter(date > "1999-12-01") %>% 
  pivot_longer(cols = - date, names_to = "index", values_to = "inflation") %>% 
  ggplot(aes(x = date, y = inflation, color = index)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2) +
  labs(title = "CPI is stubbernly high vs Fed's targeted inflation rate at 2%",
       x = NULL,
       y = "Inflation rate(%)",
       caption = "Source: Labor Department") +
  theme(
    plot.title.position = "plot",
    legend.text = element_markdown()) 


  