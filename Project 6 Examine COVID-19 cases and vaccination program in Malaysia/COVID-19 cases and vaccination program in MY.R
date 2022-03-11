## Load library
library(tidyverse)

## Import datasets
covid_19_my <- read.csv("covid-19_Malaysia.csv")
vax_my <- read.csv("vax_malaysia.csv")

## Data exploratory
summary(covid_19_my)
summary(vax_my)

### Past 7 days' COVID-19 cases (new, recovered, import, active)
covid_19_my %>% 
  select(date, cases_new, cases_import, cases_recovered, cases_active, cases_cluster) %>%
  mutate(date = as.Date(date, "%d/%m/%Y")) %>% 
  tail(7)

### Total cases (by vaccination status)
(total_cases <- covid_19_my %>% 
  select(cases_unvax, cases_pvax, cases_fvax, cases_boost) %>% 
  summarise_all(sum))

### Number of vaccinated individuals
(no_vax <- vax_my %>% 
  select(daily_partial, daily_full, daily_booster) %>% 
  summarise_all(sum) %>% 
  mutate(unvax = 33181072 - sum(daily_partial)) %>% 
  select(unvax, daily_partial, daily_full, daily_booster))

### Percentage of Malaysian population who are vaccinated
vax_my %>% 
  select(daily_partial, daily_full, daily_booster) %>% 
  summarise_all(sum) %>% 
  mutate(unvax = round((33181072 - daily_partial)/33181072*100,2),
         partial = round(daily_partial/33181072*100,2),
         full = round(daily_full/33181072*100,2),
         booster = round(daily_booster/33181072*100,2)) %>% 
  select(unvax, partial, full, booster)

### Percentage of infection
round(total_cases$cases_unvax/no_vax$unvax*100,2)

#### Partial vax
round(total_cases$cases_pvax/no_vax$daily_partial*100,2)

#### Full vax
round(total_cases$cases_fvax/no_vax$daily_full*100,2)

#### Booster vax
round(total_cases$cases_boost/no_vax$daily_booster*100,2)

#### Child vaccination
vax_my %>% 
  select(cumul_partial_child, cumul_full_child) %>% 
  tail(1)

#### Adolescence vaccination
vax_my %>% 
  select(cumul_partial_adol, cumul_full_adol) %>% 
  tail(1)

### Data visualisation
#### New cases
ggplot(data = covid_19_my %>% 
         select(date, cases_new) %>% 
         mutate(date = as.Date(date, "%d/%m/%Y"))) +
  geom_area(aes(x = date, y = cases_new), fill = "orange") +
  labs(title = "New COVID-19 Cases in Malaysia", x = "Date", y = "Cases",
       caption = "Ministry of Health of Malaysia")
  
#### Recovered cases
ggplot(data = covid_19_my %>% 
         select(date, cases_recovered) %>% 
         mutate(date = as.Date(date, "%d/%m/%Y"))) +
  geom_area(aes(x = date, y = cases_recovered), fill = "blue") +
  labs(title = "Recovered COVID-19 Cases in Malaysia", x = "Date", y = "Cases",
       caption = "Ministry of Health of Malaysia")

#### Total cases
covid_19_my %>% 
  mutate(date = as.Date(date, "%d/%m/%Y")) %>% 
  separate(date, c("year","month","day"),sep = "-") %>% 
  select(year, cases_child, cases_adolescent, cases_adult, cases_elderly) %>% 
  group_by(year) %>% 
  summarise_all(sum) %>%
  pivot_longer(cols = -year,
               names_to = "age_group",
               values_to = "cases") %>% 
  ggplot(.,) +
  geom_col(aes(x = age_group, y = cases)) +
  facet_wrap(~year, scales = "free")






