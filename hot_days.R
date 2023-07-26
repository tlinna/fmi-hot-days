library(tidyverse)
library(tidync)
library(ggdark)

# if computed and saved already:
# read_rds("annual_hot_days.rds")

# Read and summarise data ----------------------------------------

# Uses around 18 gigabytes of memory
# Download Tmax data into Tmax folder from https://en.ilmatieteenlaitos.fi/gridded-observations-on-aws-s3

years <- 1961:2022
number_of_hot_days <- rep(0, length(years))
annual_hot_days <- tibble(year = years,
                          hot_days = number_of_hot_days)

for (year in years) {
  filename <- paste0("Tmax/tmax_", year, ".nc")
  annual_data <- tidync(filename) %>% 
    hyper_tibble(force = T) %>% 
    group_by(Time) %>%
    summarise(Temp = max(Tmax)) %>% 
    filter(Temp >= 25) %>% 
    count()
  annual_hot_days$hot_days[annual_hot_days$year == year] <- annual_data[[1]]
}

# download official FMI stats CSV from https://www.ilmatieteenlaitos.fi/helletilastot
official_hot_days <- read_delim("koko-maan-hellepivt-1961.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  rename(year = Category)

official_hot_days <- official_hot_days %>% mutate(sum = Syyskuu + Elokuu + Heinäkuu + Kesäkuu + Toukokuu)
annual_hot_days$official_number <- official_hot_days$sum
annual_hot_days <- annual_hot_days %>% mutate(difference = hot_days - official_number)

write_rds(annual_hot_days, file = "annual_hot_days.rds")

# Linear fit ---------------------------------------

fit <- lm(difference ~ year, data = annual_hot_days)
summary(fit)

# Plotting ----------------------------------------

ggplot(annual_hot_days, aes(x = year, y = hot_days)) +
  geom_col(fill = "pink") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5, sp = 20),
              color = "skyblue", fill = "darkgray", se = T) +
  labs(title = "Hellepäivien määrä Suomessa vuosittain",
       subtitle = "Data: Ilmatieteen laitos, ClimGrid 1km x 1km",
       x = "Vuosi",
       y = "Hellepäivien lukumäärä",
       caption = "@TLinnaluoto") +
  scale_y_continuous(breaks = seq(0,100,5),
                     minor_breaks = seq(0,100,1),
                     limits = c(0, 75),
                     expand = c(0,0)
                     ) +
  scale_x_continuous(breaks = seq(1960,2030,10)) +
  dark_theme_light()

ggsave("Hellepäivät.png", width = 5, height = 5, dpi = 600)

ggplot(annual_hot_days, aes(x = year, y = difference)) +
  geom_col(fill = "pink") +
  geom_smooth(method = "lm", color = "skyblue", fill = "darkgray", se = T) +
  labs(title = "Hellepäivien määrän ero",
       subtitle = "ClimGrid vs. virallinen tilasto",
       x = "Vuosi",
       y = "Erotus (ClimGrid - virallinen tilasto)") +
  scale_x_continuous(breaks = seq(1960,2030,10)) +
  scale_y_continuous(breaks = seq(-10, 10, 2),
                     minor_breaks = seq(-10, 10, 1)) +
  dark_theme_light()

ggsave("Hellepäivät erotus.png", width = 5, height = 5, dpi = 600)
