library(readr)
library(httr)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)
library(tidycensus)
library(zoo)

### FUNCTIONS ###
get_bls_data <- function(url, email) {
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)
  
  bls_content <- content(bls_res, 
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)
  
}

avg_ann_growth <- function(later_val, earlier_val, 
                           periodcity = 12,
                           periods) {
  # Things this function needs to do:
  # 1. Check if `later_val`, `earlier_val`, and `periods` are numeric, if not, throw an error.
  # 2. Check if `periodcity` is one of these values: annual, monthly, or quarterly
  # 3. Calculate the average annual growth between the two values: https://www.bea.gov/help/faq/463
  # 3. Return new average annual growth value.
  
  # TODO: Change this data type checker to something more elegant
  if (!is.numeric(later_val) | !is.numeric(earlier_val) | !is.numeric(periods) | !is.numeric(periodcity)) {
    stop("The supplied object to the `later_val`, `earlier_val`, `periodcity`, or `periods` argument object is not a numeric vector of at least two items",
         call. = T)
  }
  
  if (!(periodcity %in% c(1, 4, 12))) {
    stop("The supplied object `periodcity` is not one of 1 for annual, 4 for quarterly or 12 for monthly data.")
  }
  
  
  
  avg_ann_growth <- (((later_val / earlier_val) ^ (periodcity / periods)) - 1) * 100
  
  return(avg_ann_growth)
}

create_index_col <- function(vec, index_type = 100) {
  # Things this function needs to do:
  # 1. Check if `vec` is a numeric vector, if not, throw an error.
  # 2. Create a new vector of either a 100 base or 0 based index depending on
  # the value provided in the `index_type` argument.
  # 3. Return new index vector.
  
  if (!is.vector(vec) | length(vec) < 2 | !is.numeric(vec)) {
    stop("The supplied object to the `vec` argument object is not a numeric vector of at least two items",
         call. = T)
  }
  
  if (!(index_type %in% c(0, 100))) {
    stop("Please provide integer `0` or `100` as the index_type argument.")
  }
  
  if (index_type == 100L) {
    index_vec <- (vec / vec[1]) * 100
    message("Using 100-base index")
    return(index_vec)
  } else {
    index_vec <- ((vec / vec[1]) - 1) * 100
    message("Using 0-base index")
    return(index_vec)
  }
  
}

###

### REFERENCE DATA ###
# This is a reference join file of all 10 MSAs plus the U.S. overall
# and the various codes for each region that are used for each region
# plus a short-hand name of each region.

# Getting full join file
msa_fips_join <- read_csv("./data/working_data/msa_id_join_table.csv",
                          col_names = T, col_types = "cccccc") %>% 
  mutate(bea_fips = if_else(
    sae_fips == "2571650",
    "14460",
    str_sub(sae_fips, 3, 8))
    )

### ANALYSIS ###

## QCEW data not included in GitHub repo due to size limitations but 2019 annual by industry
## and 2019 to 2023 quarterly by industry can be found and downloaded from this page
# https://www.bls.gov/cew/downloadable-data-files.htm

# First we want to see just by how much the 10 MSAs that have restaurant/accomodation & food services
# data in the BEA, QCEW, CPI, and Opportunity Insights overindex on employment, wages, and establishments
# for the restaurant industry.

# Getting location quotient data from 2019 for restaurant industry
# Restaurant industry NAICS code: 722511
qcew_annual_zip_results <- unzip(
  zipfile = "./data/raw_data/2019_annual_by_industry.zip", list = T)

qcew_restaurant_filename <- filter(
  qcew_annual_zip_results, 
  str_detect(Name, "722511")
  ) %>% 
  pull(Name)

unzip(zipfile = "./data/raw_data/2019_annual_by_industry.zip",
      files = qcew_restaurant_filename,
      junkpaths = T,
      exdir = "./data/working_data/")


qcew_restaurant_19 <- read_csv("./data/working_data/2019.annual 722511 NAICS 722511 Full-service restaurants.csv",
         col_names = T,
         col_types = cols(.default = col_character()))


qcew_restaurant_19_cbsas <- qcew_restaurant_19 %>% 
  filter(area_fips %in% msa_fips_join$qcew_fips,
         own_code == "5") %>%  # Filtering to only desired CBSAs and private sector restaurants
  mutate(across(c(lq_annual_avg_emplvl,
                  lq_annual_avg_estabs_count,
                  lq_annual_avg_wkly_wage), ~as.numeric(.x))) %>% 
  rowwise() %>% 
  mutate(lq_emp_estabs_wage_avg = mean(c_across(c(lq_annual_avg_emplvl, # Calculating average location quotient for employment, establishments, and wages
                         lq_annual_avg_estabs_count,
                         lq_annual_avg_wkly_wage)))) %>% 
  ungroup() %>% 
  arrange(desc(lq_emp_estabs_wage_avg)) # All 10 CBSAs should be very close to, or above 1.

qcew_restaurant_19_cbsas_dw <- qcew_restaurant_19_cbsas %>% 
  filter(area_title != "U.S. TOTAL") %>% 
  select(area_title, lq_annual_avg_emplvl, lq_annual_avg_estabs_count, lq_annual_avg_wkly_wage) %>% 
  mutate(area_title = case_when(
    area_title == "Urban Honolulu, HI MSA" ~ "Honolulu",
    T ~ str_remove(str_remove(area_title, "-.*$"), ",.*$")
  )) %>% 
  mutate(across(-area_title, function(x) (x - 1) * 100))

names(qcew_restaurant_19_cbsas_dw) <- c("City", "Employment", "Establishments", "Wages")

write_csv(qcew_restaurant_19_cbsas_dw,
          "./data/clean_data/qcew_restaurant_19_cbsa_dw_dot_plot.csv")

# Gathering QCEW data
# This section will now gather the quarterly QCEW data for the 
# restaurant industry (NAICS 722511) from Q1 '19 to Q4 '23
qcew_qtrly_zips <- list.files("./data/raw_data/")[str_detect(list.files("./data/raw_data/"), "qtrly")]

walk(qcew_qtrly_zips, function(x) {
  
  qcew_annual_zip_results <- unzip(
    zipfile = paste0("./data/raw_data/", x), list = T)
  
  qcew_restaurant_filename <- filter(
    qcew_annual_zip_results, 
    str_detect(Name, "722511")
  ) %>% 
    pull(Name)
  
  unzip(zipfile = paste0("./data/raw_data/", x),
        files = qcew_restaurant_filename,
        junkpaths = T,
        exdir = "./data/working_data/")
  
  message(paste("Done with file", x))
  Sys.sleep(2)
  
})

qcew_qtrly_csvs <- list.files("./data/working_data")[str_detect(list.files("./data/working_data"), "q1-q4")]

bls_qcew_present <- list_rbind(map(
  qcew_qtrly_csvs, function(x) {
    
    raw <- read_csv(
      paste0("./data/working_data/", x),
      col_names = T, col_types = cols(.default = col_character())) 
    
    
    present <- raw %>% 
      filter(own_code == "5", area_fips %in% msa_fips_join$qcew_fips) %>% 
      select(area_fips, industry_code, year, qtr, qtrly_estabs_count, month1_emplvl,
             month2_emplvl, month3_emplvl, avg_wkly_wage) %>% 
      pivot_longer(cols = ends_with("_emplvl"),
                   names_to = "qtr_month",
                   names_prefix = "month",
                   values_to = "employment") %>% 
      mutate(qtr_month = as.numeric(str_extract(qtr_month, "\\d{1}")),
             month = as.character(case_when(qtr == "1" ~ qtr_month,
                                            qtr == "2" ~ 3 + qtr_month,
                                            qtr == "3" ~ 6 + qtr_month,
                                            qtr == "4" ~ 9 + qtr_month,
                                            
             )),
             date = base::as.Date(paste0(year, "-", str_pad(month, 
                                                            width = 2, 
                                                            side = "left",
                                                            pad = "0"), "-01")),
             qtrly_estabs_count = as.numeric(qtrly_estabs_count),
             avg_wkly_wage = as.numeric(avg_wkly_wage),
             employment = as.numeric(employment)) %>% 
      select(-c(year, qtr, qtr_month, month))
    
    message(paste("Done with year", unique(year(present$date))))
    
    
    return(present)
    
  }
))

## EMPLOYMENT ##
# How have employment levels changed each month post-pandemic
# compared with the same month in 2019?
bls_qcew_employment_19_23 <- bls_qcew_present %>% 
  inner_join(msa_fips_join, by = c("area_fips" = "qcew_fips")) %>% 
  select(date, simple_area_name, employment) %>%
  arrange(simple_area_name, desc(date)) %>%
  mutate(employment = if_else(employment == 0L, NA, employment)) %>% 
  group_by(simple_area_name) %>% 
  mutate(pct_chg_19 = case_when(
    year(date) == 2023 ~ ((employment / lead(employment, n = 48)) - 1) * 100,
    year(date) == 2022 ~ ((employment / lead(employment, n = 36)) - 1) * 100,
    year(date) == 2021 ~ ((employment / lead(employment, n = 24)) - 1) * 100,
    year(date) == 2020 ~ ((employment / lead(employment, n = 12)) - 1) * 100,
    T ~ NA
  )) %>% 
  ungroup() %>%
  select(-employment) %>% 
  pivot_wider(names_from = simple_area_name, values_from = pct_chg_19) %>% 
  filter(date > base::as.Date("2020-01-01"))
  
write_csv(bls_qcew_employment_19_23,
          "./data/clean_data/bls_qcew_emp_19_23_chg_line_dw.csv")

## WAGES ##
# How have average weekly wages changed each quarter post-pandemic compared
# to the same quarter in 2019
bls_qcew_wages_19_23 <- bls_qcew_present %>% 
  inner_join(msa_fips_join, by = c("area_fips" = "qcew_fips")) %>% 
  select(date, simple_area_name, avg_wkly_wage) %>% 
  filter(month(date) %in% c(3, 6, 9, 12)) %>% 
  arrange(simple_area_name, desc(date)) %>%
  mutate(avg_wkly_wage = if_else(avg_wkly_wage == 0L, NA, avg_wkly_wage)) %>% 
  group_by(simple_area_name) %>% 
  mutate(pct_chg_19 = case_when(
    year(date) == 2023 ~ ((avg_wkly_wage / lead(avg_wkly_wage, n = (48 / 3))) - 1) * 100,
    year(date) == 2022 ~ ((avg_wkly_wage / lead(avg_wkly_wage, n = (36 / 3))) - 1) * 100,
    year(date) == 2021 ~ ((avg_wkly_wage / lead(avg_wkly_wage, n = (24 / 3))) - 1) * 100,
    year(date) == 2020 ~ ((avg_wkly_wage / lead(avg_wkly_wage, n = (12 / 3))) - 1) * 100,
    T ~ NA
  )) %>% 
  ungroup() %>%
  select(-avg_wkly_wage) %>% 
  pivot_wider(names_from = simple_area_name, values_from = pct_chg_19) %>% 
  filter(date > base::as.Date("2019-12-01")) 

# Getting all items CPI and averaging by quarter to inflation-adjust wages
cpi_all_items <- get_bls_data(
  "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",
  "justanesta@protonmail.com"
)

cpi_all_items_qtrly_19_23 <- cpi_all_items %>% 
  mutate(
    seas_adj = str_sub(series_id, start = 3, end = 3),
    period_code = str_sub(series_id, start = 4, end = 4),
    region = str_sub(series_id, start = 5, end = 8),
    item_code = str_sub(series_id, start = 9, end = 16),
    date = base::as.Date(paste0(year, "-", 
                                str_remove(period, "M"), "-01")),
    value = as.numeric(value),
    quarter = as.yearqtr(date)
  ) %>% 
  filter(
    date >= base::as.Date("2019-01-01"),
    region == "0000",
    item_code == "SA0",
    seas_adj == "U"
  ) %>% 
  group_by(quarter) %>% 
  summarize(date = max(date), value = mean(value)) %>%
  filter(date < base::as.Date("2024-01-01")) %>% 
  arrange(desc(date)) %>% 
  mutate(inf_adj_19 = case_when(
    year(date) == 2023 ~ ((value / lead(value, n = (48 / 3))) - 1) * 100,
    year(date) == 2022 ~ ((value / lead(value, n = (36 / 3))) - 1) * 100,
    year(date) == 2021 ~ ((value / lead(value, n = (24 / 3))) - 1) * 100,
    year(date) == 2020 ~ ((value / lead(value, n = (12 / 3))) - 1) * 100,
    T ~ NA
  )) %>% 
  filter(date > base::as.Date("2019-12-01")) %>% 
  select(date, inf_adj_19)

# Joining wages data and all items inflation-adjustment
qcew_rest_qtrly_wages <- bls_qcew_wages_19_23 %>% 
  inner_join(cpi_all_items_qtrly_19_23, by = "date") %>% 
  mutate(across(-c(date, inf_adj_19), function(x) x - inf_adj_19)) %>% 
  select(-inf_adj_19)

write_csv(qcew_rest_qtrly_wages, 
          "./data/clean_data/bls_qcew_19_23_wages_line_dw.csv")

## ESTABLISHMENTS ##
# How have average quarterly level of restaurant establishments changed from 
# 2019 to 2023?

bls_qcew_estabs_19_23 <- bls_qcew_present %>% 
  inner_join(msa_fips_join, by = c("area_fips" = "qcew_fips")) %>% 
  select(date, simple_area_name, qtrly_estabs_count) %>% 
  filter(month(date) %in% c(3, 6, 9, 12)) %>% 
  arrange(simple_area_name, desc(date)) %>%
  mutate(qtrly_estabs_count = if_else(qtrly_estabs_count == 0L, NA, qtrly_estabs_count)) %>% 
  group_by(simple_area_name, year = year(date)) %>% 
  summarize(yrly_avg_estabs = mean(qtrly_estabs_count, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year %in% c(2019, 2023)) %>% 
  arrange(simple_area_name, desc(year)) %>% 
  mutate(estabs_pct_chg_19_23 = ((yrly_avg_estabs / lead(yrly_avg_estabs)) - 1) * 100) %>% 
  filter(year == max(year)) %>% 
  select(simple_area_name, estabs_pct_chg_19_23) %>% 
  arrange(desc(estabs_pct_chg_19_23))

bls_qcew_estabs_19_23 <- bls_qcew_estabs_19_23 %>% 
  mutate(us_avg_pct_chg = filter(bls_qcew_estabs_19_23, simple_area_name == "U.S.")$estabs_pct_chg_19_23) %>% 
  filter(simple_area_name != "U.S.")

write_csv(bls_qcew_estabs_19_23, 
          "./data/clean_data/bls_qcew_estabs_19_23_bar_dw.csv")

## SPENDING ##
# How has spending in the 72 NAICS accomodation and food services 
# supersector changed from 2020 to 2024 indexed to Jan/Feb. 2020 (pre-pandemic)
oi_cs_city <- read_csv(
  "https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Affinity%20-%20City%20-%20Daily.csv",
  col_names = T,
  col_types = cols(.default = col_character()),
  na = c(".")
)

write_csv(oi_cs_city, 
          "./data/raw_data/oi_affinity_city_raw_2024-06-11.csv")

# Per documentation daily was a 7 day moving average and data switched from
# daily to monthly on 06/05/2022: https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/docs/oi_tracker_data_documentation.pdf

data_start_date <- oi_cs_city %>%
  mutate(date = base::as.Date(paste0(year, "-", str_pad(month, 2, "left", "0"), "-", day))) %>% 
  filter(!is.na(spend_acf)) %>%
  pull(date) %>% 
  min()

oi_weekday <- weekdays.Date(base::as.Date("2022-06-05"))

oi_cs_acf_city_wkly <- oi_cs_city %>% 
  filter(!is.na(spend_acf), freq == "w", cityid %in% msa_fips_join$oi_id) %>% 
  mutate(date = base::as.Date(paste0(year, "-", str_pad(month, 2, "left", "0"), "-", day)),
         spend_acf = as.numeric(spend_acf)) %>% 
  select(date, cityid, spend_acf) %>% 
  inner_join(msa_fips_join, by = c("cityid" = "oi_id")) %>% 
  arrange(desc(date)) %>% 
  select(date, simple_area_name, spend_acf) %>% 
  pivot_wider(names_from = simple_area_name, values_from = spend_acf) 

oi_cs_acf_city_daily <- oi_cs_city %>% 
  filter(!is.na(spend_acf), freq == "d", cityid %in% msa_fips_join$oi_id) %>% 
  mutate(date = base::as.Date(paste0(year, "-", str_pad(month, 2, "left", "0"), "-", day)),
         spend_acf = as.numeric(spend_acf),
         weekday = weekdays.Date(date)) %>% 
  filter(weekday == oi_weekday) %>% 
  select(date, cityid, spend_acf) %>% 
  inner_join(msa_fips_join, by = c("cityid" = "oi_id")) %>% 
  arrange(desc(date)) %>% 
  select(date, simple_area_name, spend_acf) %>% 
  pivot_wider(names_from = simple_area_name, values_from = spend_acf)  

# Combining weekly and daily data
oi_cs_acf_city <- bind_rows(oi_cs_acf_city_wkly,
          oi_cs_acf_city_daily) %>% 
  mutate(across(-date, ~rollmean(.x, k = 12, fill = NA, align = "left") * 100)) %>% 
  filter(if_all(-date, ~!is.na(.x)))

# Now doing national
oi_cs_natl <- read_csv(
  "https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Affinity%20-%20National%20-%20Daily.csv",
  col_names = T,
  col_types = cols(.default = col_character()),
  na = c(".")
)

write_csv(oi_cs_natl, 
          "./data/raw_data/oi_affinity_national_raw_2024-06-11.csv")

oi_cs_acf_natl_wkly <- oi_cs_natl %>% 
  filter(!is.na(spend_acf), freq == "w") %>% 
  mutate(date = base::as.Date(paste0(year, "-", str_pad(month, 2, "left", "0"), "-", day)),
         spend_acf = as.numeric(spend_acf),
         simple_area_name = "U.S.") %>% 
  select(date, simple_area_name, spend_acf) %>% 
  arrange(desc(date)) %>% 
  pivot_wider(names_from = simple_area_name, values_from = spend_acf) 

oi_cs_acf_natl_daily <- oi_cs_natl %>% 
  filter(!is.na(spend_acf), freq == "d") %>% 
  mutate(date = base::as.Date(paste0(year, "-", str_pad(month, 2, "left", "0"), "-", day)),
         spend_acf = as.numeric(spend_acf),
         weekday = weekdays.Date(date),
         simple_area_name = "U.S.") %>% 
  filter(weekday == oi_weekday) %>% 
  select(date, simple_area_name, spend_acf) %>% 
  arrange(desc(date)) %>% 
  pivot_wider(names_from = simple_area_name, values_from = spend_acf)  

# Combining weekly and daily data
oi_cs_acf_natl <- bind_rows(oi_cs_acf_natl_wkly,
                            oi_cs_acf_natl_daily) %>% 
  mutate(across(-date, ~rollmean(.x, k = 12, fill = NA, align = "left") * 100)) %>% 
  filter(if_all(-date, ~!is.na(.x)))

oi_cs_acf_combined <- inner_join(oi_cs_acf_city, oi_cs_acf_natl, by = "date")

write_csv(
  oi_cs_acf_combined,
  "./data/clean_data/oi_cs_acf_line_dw.csv"
)

## GDP ##
# How far off 2017-2019 NAICS 72 supersector Accommodation and Food Services
# GDP growth trend is each MSA as of 2022?
# What was the CAGR from 2017-2019, 2019-2020, and 2020-2022 for each MSA?
# Metros
"https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*yohei4*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIwNTQ4My4xLjEuMTcxODIwNjI0MC41MC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI1MDMiXSxbIk1ham9yX0FyZWEiLCI1Il0sWyJTdGF0ZSIsWyI1Il1dLFsiQXJlYSIsWyIxMjA2MCIsIjE0NDYwIiwiMTkxMDAiLCIzMTA4MCIsIjMzMTAwIiwiMzU2MjAiLCI0MTc0MCIsIjQxODYwIiwiNDUzMDAiLCI0NjUyMCJdXSxbIlN0YXRpc3RpYyIsWyI3OSJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkxldmVscyJdLFsiWWVhciIsWyItMSJdXSxbIlllYXJCZWdpbiIsIi0xIl0sWyJZZWFyX0VuZCIsIi0xIl1dfQ=="
# Suggested citation: U.S. Bureau of Economic Analysis, "CAGDP9 Real GDP by county and metropolitan area 1" (accessed Wednesday, June 12, 2024). 

"https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*kmdetp*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIwNTQ4My4xLjAuMTcxODIwNTQ4My42MC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI1MDIiXSxbIk1ham9yX0FyZWEiLCI1Il0sWyJTdGF0ZSIsWyI1Il1dLFsiQXJlYSIsWyIxMjA2MCIsIjE0NDYwIiwiMTkxMDAiLCIzMTA4MCIsIjMzMTAwIiwiMzU2MjAiLCI0MTc0MCIsIjQxODYwIiwiNDUzMDAiLCI0NjUyMCJdXSxbIlN0YXRpc3RpYyIsWyI3OSJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkxldmVscyJdLFsiWWVhciIsWyItMSJdXSxbIlllYXJCZWdpbiIsIi0xIl0sWyJZZWFyX0VuZCIsIi0xIl1dfQ=="
# Suggested citation: U.S. Bureau of Economic Analysis, "CAGDP8 Chain-type quantity indexes for real GDP by county and metropolitan area (2017=100.0) 1" (accessed Wednesday, June 12, 2024).

# Reading in chained 2017 real GDP accommodation and food services for MSAs
bea_naics_72_gdp_17_22 <- read_csv(
  "./data/raw_data/CAGDP9 Real GDP by county and metropolitan area accommodation and food services in thousands of 2017 dollars.csv",
  col_names = T,
  col_types = "ccdddddd",
  skip = 3,
  n_max = 10
)

bea_naics_72_gdp_17_22_long <- bea_naics_72_gdp_17_22 %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "year",
               values_to = "gdp_2017_chained",
               names_transform = as.numeric) %>% 
  rename_with(make_clean_names)

bea_naics_72_gdp_17_19 <- bea_naics_72_gdp_17_22_long %>% 
  filter(year < 2020)

bea_naics_72_gdp_17_22_avg_ann_chg <- bea_naics_72_gdp_17_22_long %>% 
  filter(year %in% c(2017, 2019)) %>% 
  group_by(geo_fips) %>% 
  summarize(avg_ann_chg = avg_ann_growth(
    later_val = gdp_2017_chained[year == max(year)],
    earlier_val = gdp_2017_chained[year == min(year)],
    periodcity = 1,
    periods = (max(year) - min(year))
  ))

bea_naics_72_19_22_real <- bea_naics_72_gdp_17_22_long %>% 
  filter(year >= 2019) %>% 
  rename(real_19_22_vals = gdp_2017_chained) %>% 
  select(geo_fips, year, real_19_22_vals)

msa_gdp_naics_72 <- bea_naics_72_19_22_real %>% 
  inner_join(bea_naics_72_gdp_17_22_avg_ann_chg, by = "geo_fips") %>% 
  mutate(avg_ann_chg = (avg_ann_chg / 100) + 1) %>% 
  group_by(geo_fips) %>% 
  mutate(trend_20_22_vals = case_when(
    year == 2020 ~ lag(real_19_22_vals, 1) * avg_ann_chg,
    year == 2021 ~ (lag(real_19_22_vals, 2) * avg_ann_chg) * avg_ann_chg,
    year == 2022 ~ ((lag(real_19_22_vals, 3) * avg_ann_chg) * avg_ann_chg) * avg_ann_chg,
    T ~ real_19_22_vals
  )) %>% 
  ungroup() %>% 
  mutate(trend_diff = ((real_19_22_vals / trend_20_22_vals) - 1) * 100) %>% 
  inner_join(msa_fips_join, by = c("geo_fips" = "bea_fips")) %>% 
  select(simple_area_name, year, trend_diff) %>% 
  pivot_wider(names_from = simple_area_name, values_from = trend_diff)

## National data
# https://apps.bea.gov/iTable/?reqid=150&step=2&isuri=1&categories=ugdpxind&_gl=1*4fntw0*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIyNjIzMS40LjEuMTcxODIyNjY1Ni40MS4wLjA.#eyJhcHBpZCI6MTUwLCJzdGVwcyI6WzEsMiwzLDNdLCJkYXRhIjpbWyJjYXRlZ29yaWVzIiwiR2RweEluZCJdLFsiVGFibGVfTGlzdCIsIjIxNCJdLFsiU2NhbGUiLCItOSJdLFsiRmlyc3RfWWVhciIsIjIwMTciXSxbIkxhc3RfWWVhciIsIjIwMjIiXSxbIlJvd3MiLFsiNzIiXV0sWyJTZXJpZXMiLCJBIl0sWyJDb2x1bW5zIixbIjIwMjAiXV1dfQ==
#  Suggested citation: U.S. Bureau of Economic Analysis, "U.Real Value Added by Industry" (accessed Wednesday, June 12, 2024). 

bea_nais_72_17_22_natl <- read_csv(
  "./data/raw_data/Real Value Added by Industry NAICS 72 2017 to 2022.csv",
  col_names = F,
  col_types = "ccdddddd",
  skip = 5,
  n_max = 1
) %>% 
  select(-c(X1, X2)) %>% 
  rename_with(~as.character(2017:2022)) 
  
cagr_17_19_natl <- ((
  bea_nais_72_17_22_natl$`2019` / bea_nais_72_17_22_natl$`2017`
) ^ (
  1 / (2019-2017)
))

natl_gdp_naics_72 <- tibble(
  year = 2019:2022,
 `U.S.` =  c(
   0,
   ((bea_nais_72_17_22_natl$`2020` / (bea_nais_72_17_22_natl$`2019` * cagr_17_19_natl)) - 1) * 100,
   ((bea_nais_72_17_22_natl$`2021` / ((bea_nais_72_17_22_natl$`2019` * cagr_17_19_natl) * cagr_17_19_natl) - 1) * 100),
   ((bea_nais_72_17_22_natl$`2022` / (((bea_nais_72_17_22_natl$`2019` * cagr_17_19_natl) * cagr_17_19_natl) * cagr_17_19_natl) - 1) * 100)
   )
)

gdp_naics_real_vs_17_19_trend <- inner_join(
  msa_gdp_naics_72,
  natl_gdp_naics_72, 
  by = "year"
)

write_csv(gdp_naics_real_vs_17_19_trend, 
          "./data/clean_data/bea_gdp_naics_72_real_v_17_19_trend_dw_line.csv")

# Make grouped bar chart with these thee percent changes
# https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*1a3j64n*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIxNjgyOC4yLjAuMTcxODIxNjgyOC42MC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI1MDMiXSxbIk1ham9yX0FyZWEiLCI1Il0sWyJTdGF0ZSIsWyI1Il1dLFsiQXJlYSIsWyIxMjA2MCIsIjE0NDYwIiwiMTkxMDAiLCIzMTA4MCIsIjMzMTAwIiwiMzU2MjAiLCI0MTc0MCIsIjQxODYwIiwiNDUzMDAiLCI0NjUyMCJdXSxbIlN0YXRpc3RpYyIsWyI3OSJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkFBR1IiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCIyMDE3Il0sWyJZZWFyX0VuZCIsIjIwMTkiXV19
# https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*cep7t*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIxNjgyOC4yLjEuMTcxODIxNzU5Ni42MC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI1MDMiXSxbIk1ham9yX0FyZWEiLCI1Il0sWyJTdGF0ZSIsWyI1Il1dLFsiQXJlYSIsWyIxMjA2MCIsIjE0NDYwIiwiMTkxMDAiLCIzMTA4MCIsIjMzMTAwIiwiMzU2MjAiLCI0MTc0MCIsIjQxODYwIiwiNDUzMDAiLCI0NjUyMCJdXSxbIlN0YXRpc3RpYyIsWyI3OSJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkFBR1IiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCIyMDIwIl0sWyJZZWFyX0VuZCIsIjIwMjIiXV19
# https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*cep7t*_ga*NzUzMzc0Nzc2LjE3MTgyMDU0ODQ.*_ga_J4698JNNFT*MTcxODIxNjgyOC4yLjEuMTcxODIxNzU5Ni42MC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCI1MDMiXSxbIk1ham9yX0FyZWEiLCI1Il0sWyJTdGF0ZSIsWyI1Il1dLFsiQXJlYSIsWyIxMjA2MCIsIjE0NDYwIiwiMTkxMDAiLCIzMTA4MCIsIjMzMTAwIiwiMzU2MjAiLCI0MTc0MCIsIjQxODYwIiwiNDUzMDAiLCI0NjUyMCJdXSxbIlN0YXRpc3RpYyIsWyI3OSJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkFBR1IiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCIyMDE5Il0sWyJZZWFyX0VuZCIsIjIwMjAiXV19

bea_72_naics_gdp_cagr <- list_rbind(
  map(
    c("./data/raw_data/CAGDP9 Real GDP by county NAICS 72 CAGR 2017-2019.csv",
      "./data/raw_data/CAGDP9 Real GDP by county NAICS 72 CAGR 2019-2020.csv",
      "./data/raw_data/CAGDP9 Real GDP by county NAICS 72 CAGR 2020-2022.csv"),
    function(x) {
      read_csv(
        x,
        col_names = T,
        col_types = "ccd",
        skip = 3,
        n_max = 10
      ) %>% 
        select(-GeoName) %>% 
        pivot_longer(-GeoFips, names_to = "time_frame", values_to = "cagr") %>% 
        rename(geofips = GeoFips)
    }
  )
) %>% 
  inner_join(msa_fips_join, by = c("geofips" = "bea_fips")) %>% 
  select(simple_area_name, time_frame, cagr) %>% 
  pivot_wider(names_from = time_frame, values_from = cagr) %>% 
  mutate(abs_chg_growth_19_20_20_22 = (`2020-2022` * 2) - abs(`2019-2020`)) %>% 
  arrange(desc(abs_chg_growth_19_20_20_22)) %>% 
  select(-abs_chg_growth_19_20_20_22)

write_csv(
  bea_72_naics_gdp_cagr,
  "./data/clean_data/bea_72_naics_gdp_cagr_grouped_bar_dw.csv"
)  

## PRICES ##
# What is average annual inflation from ~March/April 2019 to ~March/April 2024
# in the food away from home category for each MSA?
# Gathering CPI data

cbsa_cpi_urls <- c(
  "https://download.bls.gov/pub/time.series/cu/cu.data.10.OtherWest",
  "https://download.bls.gov/pub/time.series/cu/cu.data.7.OtherNorthEast",
  "https://download.bls.gov/pub/time.series/cu/cu.data.8.OtherNorthCentral",
  "https://download.bls.gov/pub/time.series/cu/cu.data.9.OtherSouth",
  "https://download.bls.gov/pub/time.series/cu/cu.data.11.USFoodBeverage"
)

cpi_cbsas <- list_rbind(
  map(cbsa_cpi_urls, ~get_bls_data(.x, "justanesta@protonmail.com"))
)

cpi_cbsas <- cpi_cbsas %>% 
  mutate(
    seas_adj = str_sub(series_id, start = 3, end = 3),
    period_code = str_sub(series_id, start = 4, end = 4),
    region = str_sub(series_id, start = 5, end = 8),
    item_code = str_sub(series_id, start = 9, end = 16),
    date = base::as.Date(paste0(year, "-", 
                                str_remove(period, "M"), "-01")),
    value = as.numeric(value)
  )

cpi_rest_cbsas <- cpi_cbsas %>% 
  filter(
    date >= base::as.Date("2019-03-01"),
    region %in% msa_fips_join$cpi_fips,
    item_code == "SEFV",
    seas_adj == "U"
  ) 

cpi_fafh_cbsas <- cpi_rest_cbsas %>% 
  group_by(region) %>% 
  filter(date %in% c(max(date), min(date))) %>% 
  summarize(
    latest_date = max(date),
    avg_ann_chg = avg_ann_growth(later_val = value[date == max(date)],
                                 earlier_val = value[date == min(date)],
                                 periodcity = 12,
                                 periods = (interval(min(date), max(date)) %/% months(1))
  )
) %>% 
  arrange(desc(avg_ann_chg)) %>% 
  inner_join(msa_fips_join, by = c("region" = "cpi_fips")) %>% 
  select(latest_date, area_name, avg_ann_chg) %>% 
  mutate(area_name = case_when(
    area_name == "Urban Honolulu, HI MSA" ~ "Honolulu",
    area_name == "U.S. city average" ~ "U.S. overall",
    T ~ str_remove(str_remove(area_name, "-.*$"), ",.*$")
  ))

cpi_fafh_cbsas <- cpi_fafh_cbsas %>% 
  mutate(us_avg_ann_chg = filter(cpi_fafh_cbsas, area_name == "U.S. overall")$avg_ann_chg) %>% 
  filter(area_name != "U.S. overall")

write_csv(cpi_fafh_cbsas, 
          "./data/clean_data/bls_cpi_avg_ann_chg_bar_dw.csv")

## GROUPS OF THE ABOVE ##
us_msa_naics_72_comparison <- tibble(
  Area = c(msa_fips_join$simple_area_name[1:9], msa_fips_join$simple_area_name[11]),
  Jobs = c("Similar", "Slightly worse", "Much better", "Much worse", "Much better", "Similar", "Similar", "Much worse", "Slightly better", "Much worse"),
  Wages = c("Similar", "Similar", "Slightly worse", "Slightly better", "Much better", "Much worse", "Slightly better", "Much worse", "Much better", "Slightly worse"),
  Firms = c("Much better", "Much worse", "Much better", "Much worse", "Much better", "Much worse", "Much worse", "Much worse", "Similar", "Much better"),
  Spending = c("Similar", "Similar", "Slightly worse", "Much worse", "Slightly better", "Much worse", "Much worse", "Much worse", "Similar", "Much better"),
  GDP = c("Similar", "Much worse", "Much better", "Similar", "Much better", "Much worse", "Similar", "Much worse", "Slightly better", "Much worse"),
  Inflation = c("Slightly worse", "Much worse", "Much worse", "Similar", "Slightly better", "Similar", "Much better", "Similar", "Much better", "Similar")
) %>% 
  arrange(Area)

write_csv(
  us_msa_naics_72_comparison,
  "./data/clean_data/msa_naics_72_comparison_table_flourish.csv"
)

## STATE POLICIES ##
# https://github.com/OpportunityInsights/EconomicTracker/blob/main/data/Policy%20Milestones%20-%20State.csv
