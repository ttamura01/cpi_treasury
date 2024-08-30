# "Sat Nov 25 06:22:59 2023"
rm(list = ls())
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
setwd("/Users/takayukitamura/Documents/R_Computing/cpi_treasury")
getwd()
# read_table("https://data.bls.gov/timeseries/CUSR0000SA0&output_view=pct_1mth", header = TRUE, sep=",", stringsAsFactor = FALSE)
# cpi <- read.csv('/Users/takayukitamura/Documents/R_Computing/cpi_treasury/cpi.csv', sep = ",",
#                   header = TRUE, stringsAsFactors = FALSE)
# 
# head(cpi)
# tail(cpi)
# sapply(cpi, class)
# sum(is.na(cpi$Aug))
# cpi$Aug <- as.numeric(cpi$Aug)
# sapply(cpi, class)
# 
# cpi_longer <- cpi %>% 
#    pivot_longer(-Year)
# cpi_longer <- cpi %>% 
#   pivot_longer(cols = c("Jan", "Feb",  "Mar",  "Apr",  "May",  "Jun",  "Jul",
#                         "Aug",  "Sep", "Oct",  "Nov", "Dec"),
#                names_to = "month",
#                values_to = "CPI"
#   ) %>% 
#   na.omit() %>% 
#   mutate(month = recode(month, Jan = "01-01", Feb = "02-01", Mar = "03-01", Apr = "04-01", May = "05-01", 
#                         Jun = "06-01", Jul = "07-01", Aug = "08-01", Sep = "09-01", Oct = "10-01", 
#                         Nov = "11-01", Dec = "12-01")) %>% 
#   unite(col = "Year", c(Year, month), sep = "-") %>% 
#   mutate(Year = as.Date(Year)) %>% 
#   arrange(Year)
# 
# head(cpi_longer)
# tail(cpi_longer)
# sapply(cpi_longer, class)
# 
# 
# ggplot(cpi_longer, aes(x = Year, y = CPI)) +
#   geom_line()

# index ID: CUUR0000SA0L1E
# core_cpi <- read.table("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/core_cpi.csv",
#                        sep = ",", header = TRUE, stringsAsFactors = FALSE) 
# head(core_cpi)
# tail(core_cpi)
# sapply(core_cpi,class)
# 
# core_cpi_longer <- core_cpi %>% 
#   pivot_longer(cols = c("Jan", "Feb",  "Mar",  "Apr",  "May",  "Jun",  "Jul",
#                         "Aug",  "Sep", "Oct",  "Nov", "Dec"),
#                names_to = "month",
#                values_to = "Core_CPI"
#   ) %>% 
#   na.omit() %>% 
#   mutate(month = recode(month, Jan = "01-01", Feb = "02-01", Mar = "03-01", Apr = "04-01", May = "05-01", 
#                         Jun = "06-01", Jul = "07-01", Aug = "08-01", Sep = "09-01", Oct = "10-01", 
#                         Nov = "11-01", Dec = "12-01")) %>% 
#   unite(col = "Year", c(Year, month), sep = "-") %>% 
#   mutate(Year = as.Date(Year)) %>% 
#   arrange(Year)
# 
# head(core_cpi_longer)
# tail(core_cpi_longer)
# sapply(core_cpi_longer, class)
# 
# ggplot(core_cpi_longer, aes(x = Year, y = Core_CPI)) +
#   geom_line()
# 
# cpi_ccpi <- merge(cpi_longer, core_cpi_longer, by = "Year")
# tail(cpi_ccpi) %>% sapply(., class)
# cpi_ccpi_01_2024$Year <- as.Date(cpi_ccpi_01_2024$Year)
# 
# sapply(cpi_ccpi_01_2024, class)
# cpi_ccpi <- rbind(cpi_ccpi, cpi_ccpi_01_2024) 
# tail(cpi_ccpi)

getwd()
cpi_ccpi <- read.csv("cpi_ccpi.csv") %>% select(-X)
tail(cpi_ccpi)

# cpi_ccpi_02_2024 <- tribble(
#   ~Year, ~CPI, ~Core_CPI,
#   "2024-02-01", 3.2, 3.8)

# cpi_ccpi <- rbind(cpi_ccpi, cpi_ccpi_02_2024) 


sapply(cpi_ccpi, class)
cip_ccpi <- cpi_ccpi$Year <- as.Date(cpi_ccpi$Year)
  
write.csv(cpi_ccpi, file = "cpi_ccpi.csv")

cpi_ccpi_longer <- cpi_ccpi %>% 
  pivot_longer(-Year, names_to = "Index", values_to = "CPI" ) 

a <- cpi_ccpi_longer %>% 
  filter(Year >= "1963-01-01") %>%  
  ggplot(aes(x = Year, y = CPI, color = Index)) +
  geom_line()+
  geom_abline(slope = 0, intercept = 2, color = "gray") +
  labs(title = "US inflation rate, CPI & Core CPI(1964~)") +
  theme_classic() +
  theme(
    legend.position = "none"
  )

b <- cpi_ccpi_longer %>% 
  filter(Year >= "2000-01-01") %>%  
  ggplot(aes(x = Year, y = CPI, color = Index)) +
  geom_line()+
  geom_abline(slope = 0, intercept = 2, color = "gray") +
  labs(title = "US inflation rate, CPI & Core CPI(2000~)",
       caption = "Source: US Bureau of Labor Statistics, St.Louis Fed"
       ) +
  theme_classic() +
  theme(
    plot.caption = element_text(face = "italic"),
    plot.caption.position = "plot"
  )
b

a + b

pce <- read_csv("PCE.csv") 
sapply(pce, class)
pce$Year <- as.Date(pce$Year, format = "%m/%d/%Y")
tail(pce)

pce %>% 
  inner_join(cpi_ccpi, by = "Year") %>% 
  select(Year, PCE, CPI) %>% 
  pivot_longer(cols = -Year, names_to = "Index", values_to = "Inflation") %>% 
  filter(Year > "1999-12-01") %>% 
  ggplot(aes(x=Year, y = Inflation, color = Index )) +
  geom_line() +
  geom_abline(slope = 0, intercept = 2, color = "gray") +
  labs(title = "PCE, Fed's favorite inflation gauge at 2.5% in Feb vs. target at 2%",
       subtitle = "The other inflation gauge CPI stubbornly high, keeps creating noise in the markets?",
       caption = "Source: St.Louis Fed",
       x = NULL, y= "inflation (%)") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.caption = element_text(face = "italic"),
    plot.caption.position = "plot",
    plot.background = element_rect(),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
  

ggsave("/Users/takayukitamura/Documents/R_Computing/cpi_treasury/cpi_core.png", width = 6, height = 4)

## 10 year treasury

G10 <- read.table("/Users/takayukitamura/Documents/R_Computing/GS10.csv",
                  sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(Year = as.Date(DATE)) %>% 
  select(Year, GS10) %>% 
  rename("Yield" = GS10) %>% 
  arrange(Year)

sapply(G10, class)

ggplot(G10, aes(x = Year, y = Yield)) +
  geom_line()
