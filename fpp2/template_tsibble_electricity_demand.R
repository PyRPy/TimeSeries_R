
# Electricity demand data in tsibble format -------------------------------

# Australian state-level demand
library(tidyverse)
library(lubridate)
library(tsibble)

repo <- "https://raw.githubusercontent.com/tidyverts/tsibbledata/master/data-raw/vic_elec/"
states <- c("NSW", "QLD", "SA", "TAS", "VIC")
dirs <- paste0(repo, states, "2015")
dirs

# read holidays data
holidays <- paste0(dirs, "/holidays.txt") %>% 
  as.list() %>% 
  map_dfr(read_csv, col_names = FALSE, .id = "State") %>% 
  transmute(
    State = states[as.numeric(State)],
    Date = dmy(X1), 
    Holiday = TRUE
  )

# Read temperature data
temperatures <- paste0(dirs,"/temperature.csv") %>%
  as.list() %>%
  map_dfr(read_csv, .id = "State") %>%
  mutate(
    State = states[as.numeric(State)],
    Date = as_date(Date, origin = ymd("1899-12-30"))
  )

# Read demand data
demands <- paste0(dirs,"/demand.csv") %>%
  as.list() %>%
  map_dfr(read_csv, .id = "State") %>%
  mutate(
    State = states[as.numeric(State)],
    Date = as_date(Date, origin = ymd("1899-12-30"))
  )

# Join demand, temperatures and holidays
aus_elec <- demands %>%
  left_join(temperatures, by = c("State", "Date", "Period")) %>%
  transmute(
    State,
    Time = as.POSIXct(Date + minutes((Period-1) * 30)),
    Period,
    Date = as_date(Time),
    DOW = wday(Date, label=TRUE),
    Demand = OperationalLessIndustrial, 
    Temperature = Temp,
  ) %>%
  left_join(holidays, by = c("State", "Date")) %>%
  replace_na(list(Holiday = FALSE))
head(aus_elec)

# Remove duplicates and create a tsibble
aus_elec <- aus_elec %>%
  filter(!are_duplicated(aus_elec, index=Time, key=State)) %>%
  as_tsibble(index = Time, key=State)

aus_elec
