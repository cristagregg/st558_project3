library(DBI)
library(RSQLite)
library(tidyverse) 
setwd('C:/Users/crist/OneDrive/Documents/A- Classes/ST 558/PreWork for project 3')
con <- dbConnect(SQLite(), 'wildfires.sqlite')
fires <- dbGetQuery(con, paste0("select count(FIRE_NAME) as total_fires, FIRE_YEAR as Year, substring(CONT_DATE, 0, charindex('/', CONT_DATE)) as Month, sum(FIRE_SIZE) as total_acres_burned, STATE as State 
               from Fires
               group by State, Year, Month
               ")) %>% collect
dbDisconnect(con)

avg <- read_csv('110-tavg.csv', skip=3) %>%
  rename(avg_temp = Value)
max <- read_csv('110-tmax.csv', skip=3) %>%
  rename(max_temp = Value) 
drought <- read_csv('110-pdsi.csv', skip=2) %>%
  rename(drought_severity = Value)
precip <- read_csv('110-pcp.csv', skip=3) %>%
  rename(avg_precip = Value)

climate <- left_join(avg, max, by = c('Location', 'Date')) %>%
  left_join(drought, by = c('Location', 'Date')) %>%
  left_join(precip, by = c('Location', 'Date')) %>%
  select(-starts_with('Rank'), - starts_with('Location ID'), - starts_with('Anomaly'), -starts_with('1901-2000 Mean')) %>%
  separate(Date, c('Year', 'Month'), 4, convert = T) %>%
  filter(Year >= 1992, Year <= 2018) %>%
  rename(State = Location)

climate$State <- state.abb[match(climate$State, state.name)]

str(climate)
str(fires)

fires$Month <- as.numeric(fires$Month)

fire_climate <- inner_join(climate, fires, by = c('State', 'Year', 'Month'))
str(fire_climate)

write.csv(fire_climate, 'C:/Users/crist/OneDrive/Documents/A- Classes/ST 558/st558_project3/combined.csv')
