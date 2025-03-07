# Results for Climate fair share of subnational governments 
# Kaihui Song and Diego Manya
# Last update: May 2024

# setwd("/Users/songkaihui/Documents/ResearchUNC/fair share")
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
  library(sf)
}

# 1. population data ----
# final population data: POP_1970_2100

POP_1970_2000_raw <- arrow::read_parquet("data/POP_1970_2000_GADM_1.parquet") %>%
  dplyr::rename(GID_0 = "iso",
         GID_1 = "GDAM_id")%>%
  filter(population_year != 2000)
  #filter(population_year != 2000)

POP_2000_2020_raw <- read_parquet("data/POP_2000_2020_GADM_1.parquet")%>%
  dplyr::rename(GID_0 = "iso",
                GID_1 = "GDAM_id") 

POP_2020_2100_raw<- read_parquet("data/POP_2020_2100_GADM_1.parquet") %>%
  dplyr::rename(GID_0 = "iso",
         GID_1 = "GDAM_id")%>%
  filter(population_year != 2020)
  
  #filter(population_year != 2020)

POP_1970_2100_raw  <- rbind(POP_1970_2000_raw,POP_2000_2020_raw,POP_2020_2100_raw)

## GDP
GDP_1970_2100_raw <- read_parquet("data/GDP_1970_2100_GADM_1.parquet") %>%
  dplyr::rename(GID_0 = "iso",
                GID_1 = "GDAM_id")

## Emissions
emissions <- read_parquet("data/edgar_emissions_TOTAL_ADM1.parquet")

pop_zero <- POP_1970_2100_raw %>% mutate(population = round(population,0)) %>% filter(population ==0)%>% group_by(GID_0)%>%add_count()%>% select(GID_0, n)%>% distinct()

POP_1970_2100 <- POP_1970_2100_raw %>% mutate(population = round(population,0))

write_parquet(POP_1970_2100, 'results/ADM1_POP_1970_2100.parquet')

# str(POP_1970_2100)

# plot global pop from 1970 to 2100
# POP_1970_2100 %>% group_by(population_year) %>%
#   summarise(total_pop = sum(population)/1000000000)%>%
#   ggplot(aes(x = population_year, y = total_pop))+
#   geom_line() +
#   scale_x_continuous(breaks = c(seq(1970, 2100, by = 20)), limits = c(1960, 2100), expand = c(0,0))+
#   scale_y_continuous(breaks = c(seq(2,10, by = 1)), limits = c(3.5, 10), expand = c(0,0))+
#   ylab("Billion people")+
#   xlab("Year")+
#   theme_ipsum()

# 2. subnational historical emission dataset -----
# final historical emission dataset : edgar_emissions_1970_2022
# Input units: Tons
edgar_emissions_1970_2022 <- emissions %>% filter(year > 1989)%>%
  pivot_wider(names_from = GHG, values_from = total_emissions)%>%
  mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  # mutate(total_GHG = total_GHG/(10^6))%>%
  # mutate(total_GHG = CH4*25 + N2O*298 + CO2, .keep = 'unused')%>% 
  dplyr::select(-sector)%>%
  mutate(total_GHG = total_GHG/10^6)
## Output units: MTC02e

str(edgar_emissions_1970_2022)

write_parquet(edgar_emissions_1970_2022, 'results/ADM1_emissions_1970_2022.parquet')
# Output units: Mt CO2e

# plot total historical GHG emissions (incl.CO2, CH4, N2O)
# edgar_emissions_1970_2022 %>%
#   group_by(year) %>%
#   summarise(emiss_total = sum(total_GHG)/1000000000) %>%
#   ggplot(aes(x = year, y = emiss_total)) +
#   geom_line() +
#   ylab(bquote('GHG missions (Gt' ~CO[2]~ 'e)'))+
#   xlab("Year")+
#   theme_ipsum()

## Checks for Population, GDP and emissions

full_emiss_pop_gdp <- edgar_emissions_1970_2022 %>%
  left_join(POP_1970_2100, by = c('GID_0', 'GID_1', 'year'='population_year'))%>%
  left_join(GDP_1970_2100_raw, by = c('GID_0', 'GID_1', 'year'='gdp_year'))

full_emiss_pop_gdp_iso <- edgar_emissions_1970_2022 %>% 
  left_join(POP_1970_2100, by = c('GID_0', 'GID_1', 'year'='population_year'))%>%
  left_join(GDP_1970_2100_raw, by = c('GID_0', 'GID_1', 'year'='gdp_year'))%>%
  dplyr::group_by(GID_1, GID_0, NAME_1) %>%
  summarise(total_GHG = sum(total_GHG), population = sum(population), gdp = sum(gdp))%>%
  mutate(GHG_pc= total_GHG*(10^6)/population, GHG_usd = total_GHG*(10^6)/gdp, GHG_year = total_GHG*(10^6)/53)%>%
  ungroup()%>%
  mutate(GHG_total_share = total_GHG*100/sum(total_GHG))

full_emiss_pop_gdp_iso_minpop <-  POP_1970_2100 %>% rename(year = population_year)%>%
  left_join(GDP_1970_2100_raw, by = c('GID_0', 'GID_1', 'year'='gdp_year'))%>%
  dplyr::group_by(GID_1, GID_0)%>%
  mutate(min_pop = min(population))%>%
  filter(min_pop < 100)

GID_1_removal_responsability <- full_emiss_pop_gdp_iso %>%
  filter(population == 0)

GID_1_removal_capacity <- full_emiss_pop_gdp_iso %>%
  filter(gdp == 0)

GID_1_removal_iso3 <- full_emiss_pop_gdp_iso %>%
  filter(GID_0 %in% c('XCA','XKO', 'XPI', 'XSP'))

full_emiss_pop_gdp_iso_clean <- full_emiss_pop_gdp_iso %>%
  filter(!GID_1 %in% full_emiss_pop_gdp_iso_minpop$GID_1)%>%
  filter(!GID_1 %in% GID_1_removal_capacity$GID_1)%>%
  filter(!GID_0 %in% GID_1_removal_iso3$GID_1)

full_emiss_pop_gdp_iso_clean$GHG_total_share %>% sum()

ggplot(full_emiss_pop_gdp_iso_clean)+
  geom_point(aes(x=GHG_pc, y = GHG_usd))+
  scale_x_continuous(transform = "log10") + scale_y_continuous(transform = "log10")

ggplot(full_emiss_pop_gdp_iso_clean, aes(x=GHG_pc, y = GHG_usd))+
  geom_point()+
  geom_text(label=paste0(full_emiss_pop_gdp_iso_clean$NAME_1, ", ", full_emiss_pop_gdp_iso_clean$GID_0))

ggplot(full_emiss_pop_gdp_iso_clean, aes(x=GHG_pc, y = GHG_total_share))+
  geom_point()+
  geom_text(label=paste0(full_emiss_pop_gdp_iso_clean$NAME_1, ", ", full_emiss_pop_gdp_iso_clean$GID_0))

ggplot(full_emiss_pop_gdp_iso_clean, aes(x=GHG_usd, y = GHG_total_share))+
  geom_point()+
  geom_text(label=paste0(full_emiss_pop_gdp_iso_clean$NAME_1, ", ", full_emiss_pop_gdp_iso_clean$GID_0))


edgar_emissions_1970_2022 <- edgar_emissions_1970_2022 %>%
  filter(!GID_1 %in% full_emiss_pop_gdp_iso_minpop$GID_1)%>%
  filter(!GID_1 %in% GID_1_removal_capacity$GID_1)%>%
  filter(!GID_0 %in% GID_1_removal_iso3$GID_1)

POP_1970_2100 <- POP_1970_2100 %>% 
  filter(!GID_1 %in% full_emiss_pop_gdp_iso_minpop$GID_1)%>%
  filter(!GID_1 %in% GID_1_removal_capacity$GID_1)%>%
  filter(!GID_0 %in% GID_1_removal_iso3$GID_1)

GDP_1970_2100_raw <- GDP_1970_2100_raw %>% 
  filter(!GID_1 %in% full_emiss_pop_gdp_iso_minpop$GID_1)%>%
  filter(!GID_1 %in% GID_1_removal_capacity$GID_1)%>%
  filter(!GID_0 %in% GID_1_removal_iso3$GID_1)

write_parquet(edgar_emissions_1970_2022, 'results/ADM1_emissions_1970_2022.parquet')

# 3. national emission projection dataset ----
# Positive Emissions
# Input units: Mt CO2/yr, Mt CH4/yr, kt N2O/yr
IMP_C1_Emissions_og <- read_excel("data/WITCH_5_0_CO_Bridge.xlsx", sheet = "data")%>%
  filter(grepl('Emissions', Variable))%>%
  pivot_longer(cols = 6:25, names_to = "year", values_to = "emissions")%>%
  filter(year >= 2020) %>%
  filter(emissions!=0) 
# Output units: Mt CO2/yr, Mt CH4/yr, kt N2O/yr


# Input units: Mt CO2/yr, Mt CH4/yr, kt N2O/yr

IMP_C1_Emissions <- IMP_C1_Emissions_og %>%
  mutate(Model_name = 'IMP GS')%>%
  mutate(emissions_co2e = case_when(Variable == "Emissions|CH4" ~ emissions *25,
                                    Variable == "Emissions|N2O" ~ ((emissions/(10^3))*298), #kT to mt
                                    TRUE ~ emissions)) %>%
  mutate(emissions_co2e = emissions_co2e)%>%
  group_by(Model_name, Model, Scenario,Region, year) %>%
  summarise(total_emissions_co2e = sum(emissions_co2e)) %>%
  ungroup()
#Output =  Mt CO2e/yr

str(IMP_C1_Emissions)


interpolate_func2 <- function(variable, data) {
  data %>% 
    group_by(Model_name, Model, Scenario, Region) %>% 
    # can't interpolate if only one year
    filter(n() >= 2) %>% 
    group_modify(~as_tibble(approx(.x$year, .x[[variable]], 
                                   xout = min(.x$year):max(.x$year)))) %>% 
    set_names("Model_name", "Model", "Scenario", "Region", "year", paste0(variable, "_interpolated")) %>% 
    ungroup()
}

vars_to_interpolate_net <- names(dplyr::select(IMP_C1_Emissions, total_emissions_co2e))
#input =  MtCO2/yr
emiss_pos_2020_2100 <- purrr::map(vars_to_interpolate_net, interpolate_func2, 
    data = IMP_C1_Emissions) %>%
  reduce(full_join, by = c("Model_name", "Model", "Scenario", "Region", "year")) %>%
  dplyr::select(Model_name, Model, Scenario, Region, year, total_emissions_co2e_interpolated) %>%
  mutate(total_emissions_co2e_interpolated = total_emissions_co2e_interpolated) 
#input =  MtCO2/yr

# Negative Emissions
# #input =  MtCO2/yr
negative_emiss <- read_excel("data/WITCH_5_0_CO_Bridge.xlsx", sheet = "data")%>%
  filter(grepl('Emissions', Variable))%>%
  pivot_longer(cols = 6:25, names_to = "year", values_to = "emissions")%>%
  filter(year >= 2020) %>%
  filter(emissions>=0) %>%
  mutate(Model_name = 'IMP GS')%>%
  group_by(Model_name, Model, Scenario,Region, year) %>%
  summarise(neg_emiss_total = sum(emissions)/10000) %>%
  ungroup()
#output =  MtCO2/yr
vars_to_interpolate_neg <- names(dplyr::select(negative_emiss, neg_emiss_total))
#input =  MtCO2/yr
emiss_neg_2020_2100 <- purrr::map(vars_to_interpolate_neg, interpolate_func2, 
    data = negative_emiss) %>%
  reduce(full_join, by = c("Model_name", "Model", "Scenario", "Region", "year")) %>%
  dplyr::select(Model_name, Model, Scenario, Region, year, neg_emiss_total_interpolated) %>%
  mutate(neg_emiss_total_interpolated = -neg_emiss_total_interpolated)
#output =  MtCO2/yr

#input =  MtCO2/yr
LULUCF_emiss <- read_csv("data/LULUCF_per_temperature.csv") %>%
  rename(year = Time) %>%
  filter(Temperature == "2")%>%
  rename(emiss_lulucf = GHG_lulucf)
#output =  MtCO2/yr

emiss_2020_2100 = emiss_pos_2020_2100 %>% 
  left_join(emiss_neg_2020_2100, by = c("Model_name", "Model", "Scenario", "Region", "year")) %>%
  left_join(LULUCF_emiss %>% select(year, emiss_lulucf), by = c("year")) %>%
  mutate(emiss_net_2020_2100_wo_lulucf = total_emissions_co2e_interpolated - emiss_lulucf)%>%
  mutate(emiss_pos_2020_2100 = emiss_net_2020_2100_wo_lulucf - neg_emiss_total_interpolated)%>%
  rename(emiss_neg_2020_2100 = neg_emiss_total_interpolated,
         emiss_net_2020_2100_with_lulucf= total_emissions_co2e_interpolated)

write_parquet(emiss_2020_2100, 'results/GBL_emissions_2020_2100_2deg.parquet')

# plot global emission projection 
# emiss_2020_2100 %>% 
#   ggplot(aes(x = year, y = total_emissions_co2e_interpolated/1000000000, group = 1)) + 
#   geom_line() + 
#   scale_y_continuous(breaks = c(seq(0,50, by = 10)), limits = c(-5,55), expand = c(0,0))+
#   xlab("Year") +
#   ylab(bquote('GHG Budget (Gt' ~CO[2]~ 'e)'))+
#   theme_ipsum()
# str(emiss_2020_2100)

# plot total emission trend (historical + projection)
# # Input: Mt CO2e
proj_emissions <- emiss_2020_2100 %>%
  rename(`Net Emissions with LULUCF` = emiss_net_2020_2100_with_lulucf,
         `Net Emissions without LULUCF` = emiss_net_2020_2100_wo_lulucf,
         `Negative Emissions` = emiss_neg_2020_2100,
         `Positive Emissions` = emiss_pos_2020_2100,
         `LULUCF Emissions` = emiss_lulucf)%>%
  pivot_longer(cols = contains('Emissions'), values_to = 'emission', names_to = 'emission_type')%>%
  select(-Model, -Model_name, -Scenario, -Region)
# Output: MtCO2e

# Input: Mt CO2e
hist_emissions <- edgar_emissions_1970_2022 %>%
  group_by(year) %>%
  summarise(emission = sum(total_GHG))%>%
  mutate(emission_type = 'Historic Emissions')
# Output: MtCO2e


hist_proj_emission <- rbind(hist_emissions, proj_emissions)

ggplot(hist_proj_emission, aes(x = year, y = emission/(10^3), color = emission_type)) +
  geom_line(size = 0.9)+
  ylab(bquote('GHG missions (Gt' ~CO[2]~ 'e)'))+
  xlab("Year")+
  theme_ipsum()+
  theme(legend.position = 'bottom')

# Input: Mt CO2e
hist_emissions <- edgar_emissions_1970_2022 %>%
  group_by(year) %>%
  summarise(emission = sum(total_GHG))%>%
  mutate(emission_type = 'Historic')
# Output: MtCO2e

# Input: Mt CO2e
proj_pos_emissions <- emiss_2020_2100 %>% filter(year > 2020)%>%
  mutate(emiss_poss = emiss_pos_2020_2100) %>%
  select(year, emiss_poss)
# Output: MtCO2e

# Input: Mt CO2e
proj_neg_emissions <- emiss_2020_2100 %>% filter(year > 2020)%>%
  mutate(emiss_neg = emiss_neg_2020_2100) %>%
  select(year, emiss_neg)
# Output: MtCO2e

# Input: Mt CO2e
proj_net_emissions <- emiss_2020_2100 %>% filter(year > 2020)%>%
  mutate(emiss_net = emiss_net_2020_2100_wo_lulucf) %>%
  select(year, emiss_net)
# Output: MtCO2e

ggplot() +
  geom_line(data = hist_emissions, aes(x = year, y = emission/(10^3)), color = "blue") +
  geom_line(data = proj_pos_emissions, aes(x = year, y = emiss_poss/(10^3)), color = "coral") +
  geom_line(data = proj_neg_emissions, aes(x = year, y = emiss_neg/(10^3)), color = "green") +
  geom_line(data = proj_net_emissions, aes(x = year, y = emiss_net/(10^3)), color = "black") +
  ylab(bquote('GHG missions (Gt' ~CO[2]~ 'e)'))+
  xlab("Year")+
  theme_bw()+
  theme(legend.position = 'bottom')





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

### Positive part
# calculation of budget (B_global)
t <- 2022

B_global <- emiss_2020_2100 %>% filter(year > t) %>%
  mutate(B_global = sum(emiss_pos_2020_2100))%>%
  pull(B_global)%>% unique() # in MtCO2e

Hist_global <- edgar_emissions_1970_2022 %>% ungroup() %>% 
  summarise(Hist_gvt = sum(total_GHG)) %>% pull(Hist_gvt)%>% unique()

Hist_gvt_df <- edgar_emissions_1970_2022 %>% ungroup() %>% 
  group_by(GID_1) %>% summarise(Hist_gvt = sum(total_GHG))

Pop_global <- POP_1970_2100 %>% ungroup()%>%
              summarise(Pop_global = sum(population))%>% 
              pull(Pop_global)%>% unique()

Pop_gvt_df <-  POP_1970_2100 %>% group_by(GID_1) %>% summarise(Pop_gvt = sum(population))

E_global_df <- emiss_2020_2100 %>% select(year, emiss_pos_2020_2100)%>%
  filter(year > t) %>% rename(E_global = emiss_pos_2020_2100)

responsability_positive_df <- Hist_gvt_df %>% left_join(Pop_gvt_df, by ='GID_1')%>%
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
  group_by(GID_1) %>% summarise(Hist_gvt = 0)

Pop_global <- POP_1970_2100 %>% ungroup()%>%
              summarise(Pop_global = sum(population))%>% 
              pull(Pop_global)%>% unique()

Pop_gvt_df <-  POP_1970_2100 %>% group_by(GID_1) %>% summarise(Pop_gvt = sum(population))

E_global_df <- emiss_2020_2100 %>% select(year, emiss_neg_2020_2100)%>%
  filter(year > t) %>% rename(E_global = emiss_neg_2020_2100)

responsability_negative_df <- Hist_gvt_df %>% left_join(Pop_gvt_df, by ='GID_1')%>%
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
  left_join(responsability_negative_df, by = c('GID_1', 'year'), suffix = c("_pos", "_neg"))%>%
  mutate(E_gvt_full = E_gvt_pos + E_gvt_neg)

# note : historical GHG emissions in 2022: 53.3 GtCO2e; projected GHG emissions in 2022: 47.3 GtCO2e
(edgar_emissions_1970_2022 %>% filter(year == 2022) %>% pull(total_GHG) %>% sum())/(10^3) #tCO2
(emiss_2020_2100 %>% filter(year == 2022) %>% pull(emiss_pos_2020_2100) %>% sum())/(10^3)# tCO2


full_resp_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% select(GID_1, year, total_GHG) %>% rename(GHG_e = total_GHG),
  resposability_full_df %>% ungroup() %>% select(GID_1, year, E_gvt_full) %>% rename(GHG_e = E_gvt_full)
)

write_parquet(full_resp_df, 'results/ADM1_fairshare_resp_emissions_1970_2100_2deg.parquet')

# 4.1 Capacity Approach
t <- 2022

# Step 1 Country Level
B_positive_global <- emiss_2020_2100 %>% filter(year > t) %>%
  group_by(year)%>% summarise(B_global = sum(emiss_pos_2020_2100))
# in MtCO2e

Pop_gvt_df <-  POP_1970_2100  %>% filter(population_year > t) %>% 
  group_by(GID_0, population_year)%>% summarise(Pop_gvt = sum(population)) %>%
  rename(year = population_year)%>% ungroup()

GDP_gvt_df <-  GDP_1970_2100_raw %>% filter(gdp_year > t)%>% 
  group_by(GID_0, gdp_year) %>% summarise(GDP_gvt = sum(gdp)) %>%
  rename(year = gdp_year)%>%  ungroup()

capacity_positive_country_df <- Pop_gvt_df %>% 
  left_join(GDP_gvt_df, by= c('GID_0', 'year'))%>%
  left_join(B_positive_global, by= c('year'))%>%
  filter(GDP_gvt != 0)%>%
  mutate(POP2_GDP = Pop_gvt^2/GDP_gvt)%>%
  group_by(year)%>%
  mutate(Sum_POP2_GDP = sum(Pop_gvt^2/GDP_gvt))%>%
  mutate(E_country = B_global*POP2_GDP/Sum_POP2_GDP)

B_negative_global <- emiss_2020_2100 %>% filter(year > t) %>%
  group_by(year)%>% summarise(B_global = sum(emiss_neg_2020_2100))

capacity_negative_country_df <- Pop_gvt_df %>% 
  left_join(GDP_gvt_df, by= c('GID_0', 'year'))%>%
  left_join(B_negative_global, by= c('year'))%>%
  filter(GDP_gvt != 0)%>%
  group_by(year)%>%
  mutate(Sum_GDP = sum(GDP_gvt))%>%
  mutate(E_country = B_global*GDP_gvt/Sum_GDP)

capacity_country_full_df <- capacity_positive_country_df %>%
  left_join(capacity_negative_country_df, by = c('GID_0', 'year'), suffix = c("_pos", "_neg"))%>%
  mutate(E_country_full = E_country_pos + E_country_neg)

write_parquet(capacity_country_full_df, 'results/ADM1_fairshare_capacity_country_emissions_1970_2100_wide_2deg.parquet')

full_capacity_country_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% group_by(GID_0, year) %>% summarise(total_GHG = sum(total_GHG)) %>% rename(GHG_e = total_GHG),
  capacity_country_full_df  %>% ungroup() %>% select(GID_0, year, E_country_full) %>% rename(GHG_e = E_country_full)
)
write_parquet(full_capacity_country_df, 'results/ADM1_fairshare_capacity_country_emissions_1970_2100_2deg.parquet')

## Step 2 Regions
B_positive_country <- capacity_country_full_df %>% select(GID_0, year, E_country_pos)
# in MtCO2e

Pop_gvt_df <-  POP_1970_2100  %>% filter(population_year > t) %>% 
  group_by(GID_0, GID_1, population_year)%>% summarise(Pop_gvt = sum(population)) %>%
  rename(year = population_year)%>% ungroup()

GDP_gvt_df <-  GDP_1970_2100_raw %>% filter(gdp_year > t)%>% 
  group_by(GID_0, GID_1, gdp_year) %>% summarise(GDP_gvt = sum(gdp)) %>%
  rename(year = gdp_year)%>%  ungroup()

capacity_positive_region_df <- Pop_gvt_df %>% 
  left_join(GDP_gvt_df, by= c('GID_0', 'GID_1', 'year'))%>%
  left_join(B_positive_country, by= c('GID_0', 'year'))%>%
  filter(GDP_gvt != 0)%>%
  mutate(POP2_GDP = Pop_gvt^2/GDP_gvt)%>%
  group_by(GID_0, year)%>%
  mutate(Sum_POP2_GDP = sum(Pop_gvt^2/GDP_gvt))%>%
  mutate(E_gvt = E_country_pos*POP2_GDP/Sum_POP2_GDP)

B_negative_country <- capacity_country_full_df %>% select(GID_0, year, E_country_neg)

capacity_negative_region_df <- Pop_gvt_df %>% 
  left_join(GDP_gvt_df, by= c('GID_0', 'GID_1', 'year'))%>%
  left_join(B_negative_country, by= c('GID_0', 'year'))%>%
  filter(GDP_gvt != 0)%>%
  group_by(GID_0, year)%>%
  mutate(Sum_GDP = sum(GDP_gvt))%>%
  mutate(E_gvt = E_country_neg*GDP_gvt/Sum_GDP)

capacity_region_full_df <- capacity_positive_region_df %>%
  left_join(capacity_negative_region_df, by = c('GID_0', 'GID_1','year'), suffix = c("_pos", "_neg"))%>%
  mutate(E_region_full = E_gvt_pos + E_gvt_neg)

write_parquet(capacity_region_full_df, 'results/ADM1_fairshare_capacity_region_emissions_1970_2100_wide_2deg.parquet')


full_capacity_region_df <- rbind(
  edgar_emissions_1970_2022 %>% ungroup() %>% select(GID_1, year, total_GHG) %>% rename(GHG_e = total_GHG),
  capacity_region_full_df  %>% ungroup() %>% select(GID_1, year, E_region_full) %>% rename(GHG_e = E_region_full)
)

write_parquet(full_capacity_region_df, 'results/ADM1_fairshare_capacity_region_emissions_1970_2100_2deg.parquet')

## Target data
## baseline units input: tCO2e
gca_targets_regions_focus <- rbind(
  read_csv("data/target_region_focus.csv")
)%>%
  select(GDAM_id, name, iso, baseline_value, baseline_year, target_value, target_year)%>%
  # mutate(baseline_value = baseline_value/(10^6))%>%
  mutate(GHG_self = baseline_value - ((target_value/100)*baseline_value))%>%
  mutate(GHG_self = case_when(target_value == 100 ~ 0,
                              TRUE ~ GHG_self))
#output: t CO2e
gca_targets_df <- rbind(gca_targets_regions_focus %>% select(GDAM_id, name, iso, baseline_value, baseline_year)%>%
                          rename(year = baseline_year, GHG_self = baseline_value),
                        gca_targets_regions_focus %>% select(GDAM_id, name, iso, GHG_self, target_year)%>%
                          rename(year = target_year)
) %>% filter(complete.cases(.))%>%distinct()

write_parquet(gca_targets_df, 'results/self_reported_targets_prio_region.parquet')


iso_lby <- data.frame(
  year = c(1990, 2000, 2005, 2010, 2015, 2020, 2025, 2030),
  GHG_self = c(88.3, 88.8, 99.5, 106.9, 71.6, 66.4, 94.3, 101.0),
  iso = 'LBY'
)%>% mutate(GHG_self = GHG_self*(10^6))

gca_targets_country <- read_csv("data/ndc_final_exclude_hotair.csv") %>% 
  filter(columns.category == "Current")%>%
  rename(iso = columns.region, GHG_self_raw = value)%>%
  select(iso, year, GHG_self_raw, columns.ambition)%>%
  filter(!is.na(GHG_self_raw))%>%
  group_by(iso, year)%>%
  mutate(GHG_self = mean(GHG_self_raw)*(10^6))%>%
  select(iso, year, GHG_self)%>% distinct()%>%
  rbind(iso_lby)
#output: T CO2e
#
write_parquet(gca_targets_country, 'results/ndc_targets_prio_country.parquet')


