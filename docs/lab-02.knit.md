---
title: "Geography 176A"
author: "[Leslie Alonzo](https://LeslieAlonzo98.github.io)"
subtitle: 'Lab 02: COVID-19 Pandemic'
output:
  html_document:
    theme: journal
---

1. The counties in California with the most COVID cases are Los Angeles, Riverside, Orange, San Bernardino, and San Diego.
2. The counties in California with the most new COVID cases are Los Angeles, San Bernardino, Riverside, Fresno, and Orange.
3. Our data shows that these were the top counties for COVID cases in the state. The county of Alpine showed zero cases, making it the safest county in the state.



```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------ tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.3     v dplyr   1.0.1
## v tidyr   1.1.1     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts --------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(knitr)

library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(readxl)

PopulationEstimates <- read_excel("C:/Users/princ/Documents/github/geog-176A-labs/data/PopulationEstimates.xls",
  skip = 2)

url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

covid = read_csv(url)
```

```
## Parsed with column specification:
## cols(
##   date = col_date(format = ""),
##   county = col_character(),
##   state = col_character(),
##   fips = col_character(),
##   cases = col_double(),
##   deaths = col_double()
## )
```

```r
state.of.interest = "California"

covid2 = covid %>%
  filter(state == state.of.interest) %>% 
  group_by(county) %>% 
  mutate(new_cases = cases - lag(cases)) %>% 
  ungroup()

mostcases = covid2 %>% 
  filter(date == max(date)) %>% 
  slice_max(cases, n = 5) %>% 
  select(county, cases)

mostnewcases = covid2 %>% 
  filter(date == max(date)) %>% 
  slice_max(new_cases, n = 5) %>% 
  select(county, new_cases)
  
knitr::kable(mostcases, caption = "California Counties with the Most COVID Cases",
  col.names = c("County","Cases"),
  format.args = list(big.mark = ","))
```



Table: California Counties with the Most COVID Cases

|County         |   Cases|
|:--------------|-------:|
|Los Angeles    | 221,950|
|Riverside      |  46,720|
|Orange         |  43,709|
|San Bernardino |  41,231|
|San Diego      |  34,741|

```r
knitr::kable(mostnewcases, caption = "California Counties with the Most New COVID Cases",
  col.names = c("County", "New Cases"),
  format.args = list(big.mark = ","))
```



Table: California Counties with the Most New COVID Cases

|County         | New Cases|
|:--------------|---------:|
|Los Angeles    |     1,188|
|San Bernardino |       789|
|Fresno         |       703|
|Sacramento     |       373|
|Orange         |       342|

```r
PopEstCA = PopulationEstimates %>% 
  filter(State == "CA") %>% 
  select(POP_ESTIMATE_2019, FIPStxt)

left_join(covid2, PopEstCA, by = c('fips' = 'FIPStxt')) ->
CAcovidpop

mostcasescap = CAcovidpop %>% 
  filter(date == max(date)) %>% 
  mutate(Most_cases_capita = (cases / POP_ESTIMATE_2019) * 100) %>% 
  slice_max(Most_cases_capita, n = 5) %>% 
  select(county, Most_cases_capita)

knitr::kable(mostcasescap, caption = "California Counties with the Most COVID Cases per capita",
  col.names = c("County", "Cases per capita"))  
```



Table: California Counties with the Most COVID Cases per capita

|County   | Cases per capita|
|:--------|----------------:|
|Imperial |         5.550313|
|Kings    |         3.605335|
|Kern     |         2.951560|
|Tulare   |         2.630230|
|Merced   |         2.440579|

```r
mostnewcasescap = CAcovidpop %>% 
  filter(date == max(date)) %>% 
  mutate(Most_new_cases_capita = (new_cases / POP_ESTIMATE_2019) * 100) %>% 
  slice_max(Most_new_cases_capita, n = 5) %>% 
  select(county, Most_new_cases_capita)

knitr::kable(mostnewcasescap, caption = "California Counties with the Most New COVID Cases per capita",
  col.names = c("County", "New Cases per Capita"))
```



Table: California Counties with the Most New COVID Cases per capita

|County   | New Cases per Capita|
|:--------|--------------------:|
|Monterey |            0.0783300|
|Fresno   |            0.0703633|
|Kings    |            0.0686544|
|Inyo     |            0.0443484|
|Sutter   |            0.0443432|

```r
CAcovidpop3 = CAcovidpop %>% 
  group_by(county) %>% 
  mutate(total_new_cases = rollsum(new_cases, 14, fill = NA, align = "right")) %>% 
  ungroup() %>% 
  filter(date > max(date) - 14) %>% 
  group_by(county) %>% 
  summarise(total_new_cases)
```

```
## `summarise()` regrouping output by 'county' (override with `.groups` argument)
```


```r
states.of.interest = c("California", "New York", "Louisiana", "Florida")

CACOVIDSTATES2 = covid %>% 
  filter(state %in% states.of.interest) %>%
  group_by(state, date) %>% 
  summarise(cases = sum(cases)) %>% 
   mutate(new_states_cases = cases - lag(cases),
         dailymean = rollmean(new_states_cases, 7, fill = NA, align = "right")) %>%
  filter(new_states_cases >= 0) %>% 
  ungroup()
```

```
## `summarise()` regrouping output by 'state' (override with `.groups` argument)
```

```r
CACOVIDSTATES2 %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = new_states_cases, col = state)) +
  geom_line(aes(y = dailymean), size = 1, col = "blue") +
  facet_wrap(~state, scales = 'free_y') +
  labs(title = "New COVID Cases by State",
       x = "",
       y = "",
       caption = "NY Times COVID Data",
       subtitle = "7-day Average (Blue)") +
  theme_dark() +
  theme(legend.position = "NA") ->
  lab2plot
ggsave(lab2plot, file = "lab2plot.png",
       width = 8,
       unit = "in")
```

```
## Saving 8 x 5 in image
```

```
## Warning: Removed 6 row(s) containing missing values (geom_path).
```
