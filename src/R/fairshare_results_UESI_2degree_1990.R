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

## Emissions
emissions <- read_parquet("data/edgar_emissions_TOTAL_UESI.parquet")

pop_zero <- POP_1970_2100_raw %>% mutate(population = round(population,0)) %>% filter(population ==0)%>% group_by(uesi_id)%>%add_count()%>% select(uesi_id, n)%>% distinct()

POP_1970_2100 <- POP_1970_2100_raw 

write_parquet(POP_1970_2100, 'results/UESI_POP_1970_2100.parquet')

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
edgar_emissions_1970_2022 <- emissions %>% filter(year > 1989)%>%
  pivot_wider(names_from = GHG, values_from = total_emissions)%>%
  mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  mutate(total_GHG = total_GHG/(10^6))%>%
  # mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  dplyr::select(-sector, -CO2bio)

str(edgar_emissions_1970_2022)

write_parquet(edgar_emissions_1970_2022, 'results/UESI_emissions_1970_2022.parquet')
# Output units: Mt CO2e

# plot total historical GHG emissions (incl.CO2, CH4, N2O)
edgar_year <- edgar_emissions_1970_2022 %>%
  group_by(year) %>%
  summarise(emiss_total = sum(total_GHG))


# 3. national emission projection dataset ----
# Positive Emissions

emiss_2020_2100 <- read_parquet('results/GBL_emissions_2020_2100_2deg.parquet')
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

B_global <- emiss_2020_2100 %>% filter(year > t) %>%
  mutate(B_global = sum(emiss_pos_2020_2100))%>%
  pull(B_global)%>% unique() # in MtCO2e

Hist_global <- edgar_emissions_1970_2022 %>% ungroup() %>% 
  summarise(Hist_gvt = sum(total_GHG)) %>% pull(Hist_gvt)%>% unique()

Hist_gvt_df <- edgar_emissions_1970_2022 %>% ungroup() %>% 
  group_by(uesi_id) %>% summarise(Hist_gvt = sum(total_GHG))

Pop_global <- POP_1970_2100_GLOBAL %>% ungroup()%>%
  summarise(Pop_global = sum(population))%>% 
  pull(Pop_global)%>% unique()

Pop_gvt_df <-  POP_1970_2100 %>% group_by(uesi_id) %>% summarise(Pop_gvt = sum(population))

E_global_df <- emiss_2020_2100 %>% select(year, emiss_pos_2020_2100)%>%
  filter(year > t) %>% rename(E_global = emiss_pos_2020_2100)

responsability_positive_df <- Hist_gvt_df %>% left_join(Pop_gvt_df, by ='uesi_id')%>%
  expand_grid(E_global_df)%>%
  mutate(B_global = B_global)%>%
  mutate(Hist_global = Hist_global)%>%
  mutate(E_19702100 = (B_global + Hist_global))%>%
  mutate(Pop_global = Pop_global)%>%
  mutate(Pop_prop = Pop_gvt/Pop_global) %>% 
  mutate(B_gvt_t1 = (E_19702100*(Pop_gvt/Pop_global)))%>%
  mutate(B_gvt = B_gvt_t1 - Hist_gvt)%>%
  mutate(E_gvt = (B_gvt/B_global)*E_global)


### Negative Part
B_global <- emiss_2020_2100 %>% filter(year > t) %>%
  mutate(B_global = sum(emiss_neg_2020_2100))%>%
  pull(B_global)%>% unique() # in MtCO2e

Hist_global <- 0

Hist_gvt_df <- edgar_emissions_1970_2022 %>% ungroup() %>% 
  group_by(uesi_id) %>% summarise(Hist_gvt = 0)

Pop_global <- POP_1970_2100_GLOBAL %>% ungroup()%>%
  summarise(Pop_global = sum(population))%>% 
  pull(Pop_global)%>% unique()

Pop_gvt_df <-  POP_1970_2100 %>% group_by(uesi_id) %>% summarise(Pop_gvt = sum(population))

E_global_df <- emiss_2020_2100 %>% select(year, emiss_neg_2020_2100)%>%
  filter(year > t) %>% rename(E_global = emiss_neg_2020_2100)

responsability_negative_df <- Hist_gvt_df %>% left_join(Pop_gvt_df, by ='uesi_id')%>%
  expand_grid(E_global_df)%>%
  mutate(B_global = B_global)%>%
  mutate(Hist_global = Hist_global)%>%
  mutate(E_19702100 = (B_global + Hist_global))%>%
  mutate(Pop_global = Pop_global)%>%
  mutate(Pop_prop = Pop_gvt/Pop_global) %>% 
  mutate(B_gvt_t1 = (E_19702100*(Pop_gvt/Pop_global)))%>%
  mutate(B_gvt = B_gvt_t1 - Hist_gvt)%>%
  mutate(E_gvt = (B_gvt/B_global)*E_global)

resposability_full_df <- responsability_positive_df %>%
  left_join(responsability_negative_df, by = c('uesi_id', 'year'), suffix = c("_pos", "_neg"))%>%
  mutate(E_gvt_full = E_gvt_pos + E_gvt_neg)

# note : historical GHG emissions in 2022: 53.3 GtCO2e; projected GHG emissions in 2022: 47.3 GtCO2e
(edgar_emissions_1970_2022 %>% filter(year == 2022) %>% pull(total_GHG) %>% sum())/(10^3) #tCO2
(emiss_2020_2100 %>% filter(year == 2022) %>% pull(emiss_pos_2020_2100) %>% sum())/(10^3)# tCO2


full_resp_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% select(uesi_id, year, total_GHG) %>% rename(GHG_e = total_GHG),
  resposability_full_df %>% ungroup() %>% select(uesi_id, year, E_gvt_full) %>% rename(GHG_e = E_gvt_full)
)

write_parquet(full_resp_df, 'results/UESI_fairshare_resp_emissions_1970_2100_2deg.parquet')

## Capacity approach
capacity_region_full_df <- read_parquet('results/ADM1_fairshare_capacity_region_emissions_1970_2100_wide_2deg.parquet')%>%
  select(GID_1, year, E_region_full)
  
uesi_gdam <- read_csv('data/uesi_city_gdam.csv')%>% select(uesi_id, GID_1)

capacity_city_full_df <-  uesi_gdam %>%
             left_join(POP_1970_2100_GLOBAL %>% rename(population_region = population), by = 'GID_1')%>%
             left_join(POP_1970_2100,  by = c('uesi_id', 'population_year'))%>%
             rename(year = population_year)%>% 
             left_join(capacity_region_full_df, by = c('GID_0', 'year', 'GID_1'))%>%
             filter(year > t)%>%
             mutate(pop_ratio = population/population_region)%>%
             group_by(uesi_id) %>% mutate(max_ratio = max(pop_ratio))%>%
             filter(max_ratio < 1.05)%>%
             mutate(pop_ratio = case_when(max_ratio>0.95 ~ 1,
                                          TRUE ~ pop_ratio))%>%
             mutate(E_city_full = E_region_full*pop_ratio)
             
# note : historical GHG emissions in 2022: 53.3 GtCO2e; projected GHG emissions in 2022: 47.3 GtCO2e

full_capacity_city_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% select(uesi_id, year, total_GHG) %>% rename(GHG_e = total_GHG),
  capacity_city_full_df %>% ungroup() %>% select(uesi_id, year, E_city_full) %>% rename(GHG_e = E_city_full)
)

write_parquet(full_capacity_city_df, 'results/UESI_fairshare_capacity_city_emissions_1970_2100_2deg.parquet')

## Target data
## baseline units input: tCO2e
gca_targets_regions_focus <- rbind(
  read_csv("data/target_city_focus.csv")
  )%>%
  select(uesi_id, name, iso, baseline_value, baseline_year, target_value, target_year)%>%
  mutate(baseline_value = baseline_value/(10^6))%>%
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

