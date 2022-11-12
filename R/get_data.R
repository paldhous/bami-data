# load required packages
library(dplyr)
library(readr)

# load raw data
populations <- read_csv("state_pops.csv") %>% 
  select(state_postal, state_fips, population)

states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# process data
states <- inner_join(states, populations, by = c("fips" = "state_fips")) %>%
  group_by(fips) %>%
  mutate(cases_new = cases - lag(cases),
         deaths_new = deaths - lag(deaths)) %>%
  ungroup() %>%
  arrange(state, date) %>%
  select(state,date,cases,deaths,cases_new,deaths_new,state_postal,state_fips = fips,population) %>%
  mutate(cases_new = case_when(cases_new < 0 ~ 0,
                               TRUE ~ cases_new),
         deaths_new = case_when(deaths_new < 0 ~ 0,
                               TRUE ~ deaths_new))

states_latest <- states %>%
  filter(date == max(date))

states_partial <- states %>%
  filter(date <= Sys.Date() - 547.5)

# write to csv
write_csv(states, "states_timeline_full.csv", na = "")
write_csv(states_latest, "states_latest.csv", na = "")
write_csv(states_partial, "states_timeline_partial.csv", na = "")

  

  