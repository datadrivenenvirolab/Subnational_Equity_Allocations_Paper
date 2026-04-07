# Results for Climate fair share of subnational governments 

setwd("~/GitHub/Equity_Allocations_Subnationals/")

{
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(hrbrthemes)
  library(arrow)
  library(Cairo)
}
uesi_gdam <- read_csv('data/uesi_city_gdam.csv')%>% select(uesi_id, GID_1)

# 1. population data ----
# final population data: POP_1970_2100

POP_1970_2000_raw <- read_parquet("data/POP_1970_2000_UESI.parquet") %>%
  filter(population_year != 2000)
  #filter(population_year != 2000)

POP_2000_2020_raw <- read_parquet("data/POP_2000_2020_UESI.parquet")

  
POP_2020_2100_raw<- read_parquet("data/POP_2020_2100_UESI.parquet") %>%
  filter(population_year != 2020)
  
  #filter(population_year != 2020)

POP_1970_2100_raw  <- rbind(POP_1970_2000_raw,POP_2000_2020_raw,POP_2020_2100_raw)

POP_1970_2100_raw_GID_1 <- read_parquet("results/ADM1_POP_1970_2100.parquet")

POP_1970_2100_comb <- POP_1970_2100_raw %>%
  left_join(uesi_gdam, by = 'uesi_id')%>%
  left_join(POP_1970_2100_raw_GID_1, by = c("GID_1", "population_year"), suffix = c("_city", "_region"))%>%
  group_by(GID_1, population_year)%>%
  mutate(population_cities = sum(population_city))%>%
  mutate(population_no_city = population_region-population_cities)%>%
  mutate(pop_diff_ratio = 100*(population_no_city)/population_region)%>%
  group_by(uesi_id)%>%
  mutate(min_pop_diff_ratio = min(pop_diff_ratio))%>%
  filter(min_pop_diff_ratio > -5)%>%
  mutate(population_city = ifelse(pop_diff_ratio < 0, population_region, population_city))%>%
  group_by(GID_1, population_year)%>%
  mutate(population_cities = sum(population_city))%>%
  mutate(population_no_city = population_region-population_cities)%>%
  select(-pop_diff_ratio)  

POP_1970_2100_uesi <- POP_1970_2100_comb %>% select(uesi_id, GID_1, population_city, population_year) %>% rename(population = population_city)
POP_1970_2100_no_uesi <- POP_1970_2100_comb %>% select(uesi_id, GID_1, population_no_city, population_year) %>% rename(population = population_no_city) %>%
  mutate(uesi_id = paste0("NO_cities_", GID_1))%>% distinct()

POP_1970_2100_pre <- rbind(POP_1970_2100_uesi, POP_1970_2100_no_uesi)%>%
  group_by(uesi_id) %>% add_count() %>% filter( n == 131)%>% select(-n)

## GDP

GDP_1970_2100_raw<- read_parquet("data/GDP_1970_2100_UESI.parquet")%>% rename(uesi_id = GDAM_id)
GDP_1970_2100_raw_GID_1 <- read_parquet("data/GDP_1970_2100_GADM_1.parquet") %>%
  dplyr::rename(GID_0 = "iso",
                GID_1 = "GDAM_id")

GDP_1970_2100_comb <- GDP_1970_2100_raw %>%
  left_join(uesi_gdam, by = 'uesi_id')%>%
  left_join(GDP_1970_2100_raw_GID_1, by = c("GID_1", "gdp_year"), suffix = c("_city", "_region"))%>%
  group_by(GID_1, gdp_year)%>%
  mutate(gdp_cities = sum(gdp_city))%>%
  mutate(gdp_no_city = gdp_region-gdp_cities)%>%
  mutate(gdp_diff_ratio = 100*(gdp_no_city)/gdp_region)%>%
  group_by(uesi_id)%>%
  filter(min(gdp_diff_ratio) > -10)%>%
  mutate(gdp_city = ifelse(gdp_diff_ratio < 0, gdp_region, gdp_city))%>%
  group_by(GID_1, gdp_year)%>%
  mutate(gdp_cities = sum(gdp_city))%>%
  mutate(gdp_no_city = gdp_region-gdp_cities)%>%
  select(-gdp_diff_ratio)

GDP_1970_2100_uesi <- GDP_1970_2100_comb %>% select(uesi_id, GID_1, gdp_city, gdp_year) %>% rename(gdp = gdp_city)
GDP_1970_2100_no_uesi <- GDP_1970_2100_comb %>% select(uesi_id, GID_1, gdp_no_city, gdp_year) %>% rename(gdp = gdp_no_city) %>%
  mutate(uesi_id = paste0("NO_cities_", GID_1))%>% distinct()

GDP_1970_2100_pre <- rbind(GDP_1970_2100_uesi, GDP_1970_2100_no_uesi)%>%
  group_by(uesi_id) %>% add_count() %>% filter( n %in% c(131))%>%select(-n)

## Emissions
emissions <- read_parquet("data/edgar_emissions_TOTAL_UESI.parquet")

pop_zero <- POP_1970_2100_raw %>% mutate(population = round(population,0)) %>% filter(population ==0)%>% group_by(uesi_id)%>%add_count()%>% select(uesi_id, n)%>% distinct()

POP_1970_2100 <- POP_1970_2100_pre %>%
  filter(uesi_id %in% GDP_1970_2100_pre$uesi_id)

GDP_1970_2100 <- GDP_1970_2100_pre %>%
  filter(uesi_id %in% POP_1970_2100_pre$uesi_id)

write_parquet(POP_1970_2100, 'results/UESI_POP_1970_2100.parquet')
write_parquet(GDP_1970_2100, 'results/UESI_GDP_1970_2100.parquet')

POP_1970_2100_GLOBAL <- read_parquet('results/ADM1_POP_1970_2100.parquet')
# str(POP_1970_2100)

# plot global pop from 1970 to 2100
POP_1970_2100 %>% group_by(population_year) %>%
  summarise(total_pop = sum(population)/(10^6))%>%
  ggplot(aes(x = population_year, y = total_pop))+
  geom_line() +
  scale_x_continuous(breaks = c(seq(1970, 2100, by = 20)), limits = c(1960, 2100), expand = c(0,0))+
  ylab("Million people")+
  xlab("Year")+
  theme_ipsum()

# 2. subnational historical emission dataset -----
# final historical emission dataset : edgar_emissions_1970_2022
# Input units: Tons
edgar_emissions_1970_2022_raw <- emissions %>% filter(year > 1969)%>%
  pivot_wider(names_from = GHG, values_from = total_emissions)%>%
  mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  mutate(total_GHG = total_GHG/(10^6))%>%
  # mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  dplyr::select(-sector, -CO2bio)

edgar_emissions_1970_2022_raw_GID_1 <- read_parquet("results/ADM1_emissions_1970_2022.parquet")

edgar_emissions_1970_2022_comb <- edgar_emissions_1970_2022_raw %>%
  left_join(uesi_gdam, by = 'uesi_id')%>%
  left_join(edgar_emissions_1970_2022_raw_GID_1, by = c("GID_1", "year"), suffix = c("_city", "_region"))%>%
  group_by(GID_1, year)%>%
  mutate(total_GHG_cities = sum(total_GHG_city))%>%
  mutate(total_GHG_no_city = total_GHG_region-total_GHG_cities)%>%
  mutate(total_GHG_diff_ratio = 100*(total_GHG_no_city)/total_GHG_region)%>%
  filter(uesi_id %in% GDP_1970_2100$uesi_id)%>%
  filter(uesi_id %in% POP_1970_2100$uesi_id)%>%
  mutate(total_GHG_city = ifelse(total_GHG_diff_ratio < 0, total_GHG_region, total_GHG_city))%>%
  group_by(GID_1, year)%>%
  mutate(total_GHG_cities = sum(total_GHG_city))%>%
  mutate(total_GHG_no_city = total_GHG_region-total_GHG_cities)%>%
  select(-total_GHG_diff_ratio)

edgar_emissions_1970_2022_uesi <- edgar_emissions_1970_2022_comb %>% select(uesi_id, GID_1, total_GHG_city, year) %>% rename(total_GHG = total_GHG_city)
edgar_emissions_1970_2022_no_uesi <- edgar_emissions_1970_2022_comb %>% select(uesi_id, GID_1, total_GHG_no_city, year) %>% rename(total_GHG = total_GHG_no_city) %>%
  mutate(uesi_id = paste0("NO_cities_", GID_1))%>% distinct()

edgar_emissions_1970_2022 <- rbind(edgar_emissions_1970_2022_uesi, edgar_emissions_1970_2022_no_uesi)

write_parquet(edgar_emissions_1970_2022, 'results/UESI_emissions_1970_2022.parquet')


# Output units: Mt CO2e

# plot total historical GHG emissions (incl.CO2, CH4, N2O)
edgar_year <- edgar_emissions_1970_2022 %>%
  group_by(year) %>%
  summarise(emiss_total = sum(total_GHG))


# 3. national emission projection dataset ----
# Positive Emissions

emiss_2020_2100 <- read_parquet('results/GBL_emissions_2020_2100_LD15deg.parquet')
#output =  MtCO2/yr

# 4. fair share calculation ----

# 4.1 Reponsibility approach
#E_gvt(t) = (B_gvt / B_global)*E_global(t) # for each time t in the future
#where:
# B_global = sum(E_global(t)) for each t in the future
# Hist_global = sum(E_global(t)) for each t in the past
# Hist_gvt = sum(E_gvt(t)) for each t in the past
# Pop_gvt = sum(Pop_gvt(t)) for all t
# Pop_global = sum(Pop_global(t)) for all t
# B_gvt = ((B_global + Hist_global)*(sum(Pop_gvt(t))/sum(Pop_global(t))))-Hist_gvt

# calculation of budget (B_global)
t <- 2022

## Capacity approach
capacity_region_full_df <- read_parquet('results/ADM1_fairshare_capacity_region_emissions_1970_2100_wide_LD15deg.parquet')%>%
  select(GID_1, year, E_region_full, E_gvt_pos, E_gvt_neg)%>%
  mutate(E_gvt = E_gvt_pos + E_gvt_neg)%>%
  mutate(E_gvt_pos = ifelse(E_gvt >0, E_gvt, 0))%>%
  mutate(E_gvt_neg = ifelse(E_gvt <0, E_gvt, 0))
  
B_positive_region <- capacity_region_full_df  %>% select(GID_1, year, E_gvt_pos)
# in MtCO2e

Pop_uesi_df <-  POP_1970_2100  %>% filter(population_year > t) %>% ungroup()%>%
  group_by(uesi_id, population_year, GID_1)%>% summarise(Pop_gvt = sum(population)) %>%
  rename(year = population_year)%>% ungroup()

GDP_uesi_df <-  GDP_1970_2100%>% filter(gdp_year > t)%>% 
  group_by(uesi_id, gdp_year, GID_1) %>% summarise(GDP_gvt = sum(gdp)) %>%
  rename(year = gdp_year)%>%  ungroup()

capacity_positive_uesi_df <- Pop_uesi_df %>% 
  left_join(GDP_uesi_df, by= c('uesi_id', 'year', 'GID_1'))%>%
  left_join(B_positive_region, by= c('GID_1', 'year'))%>%
  filter(GDP_gvt != 0)%>%
  mutate(POP2_GDP = Pop_gvt^2/GDP_gvt)%>%
  group_by(GID_1, year)%>%
  mutate(Sum_POP2_GDP = sum(Pop_gvt^2/GDP_gvt))%>%
  mutate(E_city = E_gvt_pos*POP2_GDP/Sum_POP2_GDP)

B_negative_region <- capacity_region_full_df  %>% select(GID_1, year, E_gvt_neg)

capacity_negative_uesi_df <- Pop_uesi_df %>% 
  left_join(GDP_uesi_df, by= c('uesi_id', 'year', 'GID_1'))%>%
  left_join(B_negative_region, by= c('GID_1', 'year'))%>%
  filter(GDP_gvt != 0)%>%
  group_by(GID_1, year)%>%
  mutate(Sum_GDP = sum(GDP_gvt))%>%
  mutate(E_city = E_gvt_neg*GDP_gvt/Sum_GDP)

capacity_city_full_df <- capacity_positive_uesi_df %>%
  left_join(capacity_negative_uesi_df, by = c('uesi_id','year'), suffix = c("_pos", "_neg"))%>%
  mutate(E_city_full = E_city_pos + E_city_neg)

write_parquet(capacity_city_full_df, 'results/UESI_fairshare_capacity_city_emissions_1970_2100_wide_LD15deg.parquet')

full_capacity_city_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% select(uesi_id, year, total_GHG) %>% rename(GHG_e = total_GHG),
  capacity_city_full_df  %>% ungroup() %>% select(uesi_id, year, E_city_full) %>% rename(GHG_e = E_city_full)
)

write_parquet(full_capacity_city_df, 'results/UESI_fairshare_capacity_city_emissions_1970_2100_LD15deg.parquet')

## Target data
## baseline units input: tCO2e
gca_targets_regions_focus_og <- rbind(
  read_csv("data/target_city_focus.csv")
  )%>%
  select(uesi_id, name, iso, baseline_value, baseline_year, target_value, target_year)%>%
  mutate(baseline_value = baseline_value/(10^6))%>%
  mutate(GHG_self = baseline_value - ((target_value/100)*baseline_value))%>%
  mutate(GHG_self = case_when(target_value == 100 ~ 0,
                                TRUE ~ GHG_self))

gca_targets_regions_focus <- rbind(
  read_csv("data/target_city_focus.csv")
)%>%
  select(uesi_id, name, iso, baseline_year, target_value, target_year)%>%
  left_join(edgar_emissions_1970_2022, by = c('baseline_year'='year', 'uesi_id'))%>%
  rename(baseline_value = total_GHG)%>%
  mutate(GHG_self = baseline_value - ((target_value/100)*baseline_value))%>%
  mutate(GHG_self = case_when(target_value == 100 ~ 0,
                              TRUE ~ GHG_self))

#output: Mt CO2e
gca_targets_df <- rbind(gca_targets_regions_focus %>% select(uesi_id, name, iso, baseline_value, baseline_year)%>%
                          rename(year = baseline_year, GHG_self = baseline_value)%>% mutate(target_value = 0)
                        ,
                        gca_targets_regions_focus %>% select(uesi_id, name, iso, GHG_self, target_year, target_value)%>%
                          rename(year = target_year)
                        ) %>% filter(complete.cases(.))%>%distinct()

write_parquet(gca_targets_df, 'results/self_reported_targets_prio_cities.parquet')

