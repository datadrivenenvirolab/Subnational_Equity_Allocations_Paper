library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(hrbrthemes)
library(arrow)
library(Cairo)
library(sf)
library(tidyverse)
library(viridis)
library(Cairo)
library(rmapshaper)
library(scales)
# library(plyr)
library(UESIplots)
library(scales)
library(geojsonsf)
library(maps)
library(UESIplots)
library(showtext)
# library(rjson)
library(readxl)
library(psych)

# Import the project font if you haven't done so
font_add_google("Noto Sans", family = "noto-sans")
showtext_auto()

setwd("~/GitHub/Equity_Allocations_Subnationals/")

gadm_path <- "data/gadm_410-levels_ADM_0_1.gpkg"
gadm_path_sov <-"data/GADM_SOV.gpkg"
gadm_country <- st_read(gadm_path_sov)%>%
  filter(!GID_0 %in% c('ATA', 'XCA', 'GRL'))

gadm_sp <- st_read(gadm_path, layer = 'gadm_410-levels — ADM_1') %>%
  left_join(read_csv("data/gadm_sov_state.csv"), by = 'GID_0') %>% select(-GID_0)%>% rename(GID_0= iso_state)%>% 
  filter(!GID_0 %in% c('ATA', 'XCA', 'GRL'))


gap_responsability <- read_parquet('results/ADM1_fairshare_resp_gap_2022_2023_LD15deg.parquet')
full_resp_df <- read_parquet('results/ADM1_fairshare_resp_emissions_1970_2100_LD15deg.parquet')
full_capacity_region_df <- read_parquet('results/ADM1_fairshare_capacity_region_emissions_1970_2100_LD15deg.parquet')
emiss_2020_2100 <- read_parquet('results/GBL_emissions_2020_2100_LD15deg.parquet')
gap_capacity_region <- read_parquet('results/ADM1_fairshare_capacity_region_gap_2022_2023_LD15deg.parquet')
gap_capacity_country <- read_parquet('results/ADM1_fairshare_capacity_country_gap_2022_2023_LD15deg.parquet')
gap_capacity_region_2030 <- read_parquet('results/ADM1_fairshare_capacity_region_gap_2022_2030_LD15deg.parquet')
gap_capacity_country_2030 <- read_parquet('results/ADM1_fairshare_capacity_country_gap_2022_2030_LD15deg.parquet')
gap_responsability_2030 <- read_parquet('results/ADM1_fairshare_resp_gap_2022_2030_LD15deg.parquet')
POP_1970_2100 <- read_parquet('results/ADM1_POP_1970_2100.parquet')
emissions_og <- read_parquet("data/edgar_emissions_TOTAL_ADM1.parquet")%>% ungroup() %>% select(GID_1, NAME_1, GID_0) %>% distinct()
edgar_emissions_1970_2022 <- read_parquet('results/ADM1_emissions_1970_2022.parquet')
edgar_emissions_1970_2022_uesi<- read_parquet('results/UESI_emissions_1970_2022.parquet')


eu_iso3 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)
### 2. TIMESERIES ###
##### 2.1 Global Trend #####

global_resp <- full_resp_df %>% dplyr::group_by(year) %>% summarise(year_sum = sum(GHG_e))
global_cap <- full_capacity_region_df  %>% dplyr::group_by(year) %>% summarise(year_sum = sum(GHG_e))

Cairo("plots/Global_Timeseries_LD15.png", type = "png", res = 100, width = 700, height = 500)
ggplot()+
  geom_line(data=subset(global_resp,year<2023),
            aes(x=year,y=year_sum/(10^3),color="Historical Data"),size=1)+
  geom_line(data=subset(emiss_2020_2100,year>2022),
            aes(x=year,y=emiss_lulucf/(10^3),color="LULUCF Emissions"),
            linetype="dashed",size=0.7)+
  geom_line(data=subset(emiss_2020_2100,year>2022),
            aes(x=year,y=emiss_neg_2020_2100/(10^3),color="Negative Emissions"),
            linetype="dashed",size=0.7)+
  geom_line(data=subset(emiss_2020_2100,year>2022),
            aes(x=year,y=emiss_pos_2020_2100/(10^3),color="Positive Emissions"),
            linetype="dashed",size=0.7)+
  geom_line(data=subset(emiss_2020_2100,year>2022),
            aes(x=year,y=emiss_net_2020_2100_wo_lulucf/(10^3),color="Net Emissions (excl. LULUCF)"),
            size=0.7)+
  ylab("Emissions (GtCO2e)")+ xlab("Year")+
  labs(title="Global Emissions Timeseries",color="Emissions", subtitle = 'Scenario: LowEnergyDemand_1.3_IPCC')+
  scale_color_manual(values=c(
    "Historical Data"="black",
    "LULUCF Emissions"="#66c2a5",
    "Negative Emissions"="#d53e4f",
    "Positive Emissions"="#3288bd",
    "Net Emissions (excl. LULUCF)"="#5e4fa2"
  ))+
  guides(color = guide_legend(nrow = 2))+
  theme_ipsum()+
  theme(legend.position="bottom",
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=10))
dev.off()

### 3 MAPS ###
#### Mapping functions ####
asinh_trans <- scales::trans_new(
  "inverse_hyperbolic_sine",
  transform = function(x) {asinh(x)},
  inverse = function(x) {sinh(x)})

plot_gap_map_asinh <- function(df, variable, scale, Title, output_file , legend_name, res = 250, width = 7000, height = 3000) {
  
  # Calculate min, max, and midpoints
  min_value <- min(df[[variable]] / scale, na.rm = TRUE)
  max_value <- max(df[[variable]] / scale, na.rm = TRUE)
  
  # Transform min, max, and zero using asinh
  trans_min <- asinh(min_value)
  trans_max <- asinh(max_value)
  trans_zero <- asinh(0)
  
  # Create 3 equidistant points between min and 0, and 0 and max in the transformed space
  breaks_trans <- c(
    trans_min, 
    trans_min / 3 * 2,  # 2/3 between min and 0 in transformed space
    trans_min / 3,      # 1/3 between min and 0 in transformed space
    trans_zero,         # 0 in transformed space
    trans_max / 3,      # 1/3 between 0 and max in transformed space
    trans_max / 3 * 2,  # 2/3 between 0 and max in transformed space
    trans_max
  )
  
  # Convert the transformed breaks back to the original space using sinh
  breaks <- sinh(breaks_trans)
  
  # Start the plot with Cairo
  Cairo(output_file, type = "png", res = res, width = width, height = height)
  
  # Create the plot
  print(
    ggplot(df, aes(fill = df[[variable]] / scale)) +
      geom_sf() +
      scale_fill_gradient2(
        low = "#9EB3C2", high = "#21295C", na.value = "#F5F5F5", 
        midpoint = 0, 
        limits = c(min_value, max_value),
        transform = asinh_trans(),
        breaks = breaks,  # Set equidistant breaks in the original scale
        labels = round(breaks, 2)  # Set labels
      ) +
      labs(fill = legend_name, 
           title = Title) +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=24), legend.position="left") +
      guides(fill = guide_colorbar(barwidth = 2, barheight = 50)) +
      theme_UESI() +
      theme(
        legend.position = "left",
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 35, face = "bold"),
        plot.title = element_text(size=22),
        text = element_text(family = "noto-sans")
      )
  )
  
  # Close the Cairo device
  dev.off()
}
plot_gap_map_linear <- function(df, variable, scale, Title, output_file , legend_name, res = 250, width = 7000, height = 3000) {
  
  # Calculate min, max, and midpoints
  min_value <- min(df[[variable]] / scale, na.rm = TRUE)
  max_value <- max(df[[variable]] / scale, na.rm = TRUE)
  
  # Create 3 equidistant points between min and 0, and 0 and max
  breaks <- c(
    min_value, 
    min_value / 3 * 2,  # 2/3 between min and 0
    min_value / 3,      # 1/3 between min and 0
    0,                  # Zero point
    max_value / 3,      # 1/3 between 0 and max
    max_value / 3 * 2,  # 2/3 between 0 and max
    max_value
  )
  
  # Start the plot with Cairo
  Cairo(output_file, type = "png", res = res, width = width, height = height)
  
  # Create the plot
  print(
    ggplot(df, aes(fill = df[[variable]] / scale)) +
      geom_sf() +
      scale_fill_gradient2(
        low = "#9EB3C2", high = "#21295C", 
        na.value = "#F5F5F5", 
        midpoint = 0, 
        limits = c(min_value, max_value),
        breaks = breaks,  # Set equidistant breaks in the linear scale
        labels = round(breaks, 0)  # Set labels
      ) +
      labs(fill = legend_name, 
           title = Title) +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=24), legend.position="left") +
      guides(fill = guide_colorbar(barwidth = 2, barheight = 50)) +
      theme_UESI() +
      theme(
        legend.position = "left",
        legend.text = element_text(size= 45),
        legend.title = element_text(size= 60),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 50, face = "bold"),
        plot.title = element_text(size=80),
        text = element_text(family = "noto-sans")
      )
  )
  
  # Close the Cairo device
  dev.off()
}
plot_gap_map_linear_trunc <- function(df, variable, scale, Title, output_file , min_val, max_val, legend_name, res = 250, width = 7000, height = 3000) {
  
  # Calculate min, max, and midpoints
  min_value <- min_val
  max_value <- max_val
  
  # Create 3 equidistant points between min and 0, and 0 and max
  breaks <- c(
    min_value, 
    min_value / 3 * 2,  # 2/3 between min and 0
    min_value / 3,      # 1/3 between min and 0
    0,                  # Zero point
    max_value / 3,      # 1/3 between 0 and max
    max_value / 3 * 2,  # 2/3 between 0 and max
    max_value
  )
  
  # Start the plot with Cairo
  Cairo(output_file, type = "png", res = res, width = width, height = height)
  
  # Create the plot
  print(
    ggplot(df, aes(fill = df[[variable]] / scale)) +
      geom_sf(colour = '#F2F6F7') +
      scale_fill_gradientn(
        colors = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", 
                   "#e6f598", 
                   "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2"),
        values = scales::rescale(c(-300, -200, -100, -50, 0, 50, 100, 200, 300), to = c(0, 1)),
        limits = c(min_value, max_value),
        breaks = breaks,  # Set equidistant breaks in the linear scale
        labels = round(breaks, 2)# Labels for the breaks
      ) +
      labs(fill = legend_name, 
           title = Title) +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=24), legend.position="left") +
      guides(fill = guide_colorbar(barwidth = 2, barheight = 50, title.position = "left",  # Ensure title is on the left
                                   title.theme = element_text(angle = 90, size = 60, vjust = 0.5,hjust = 0.5))) +
      theme_UESI() +
      theme(
        legend.position = "left",
        legend.text = element_text(size= 45),
        legend.title = element_text(size= 60, angle = 90, vjust = 0.5,hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 50, face = "bold"),
        plot.title = element_text(size=80),
        text = element_text(family = "noto-sans")
      )
  )
  
  # Close the Cairo device
  dev.off()
}

plot_align_map_linear_trunc <- function(df, variable, scale, Title, output_file , min_val, max_val, legend_name, res = 250, width = 7000, height = 3000) {
  
  # Calculate min, max, and midpoints
  min_value <- min_val
  max_value <- max_val
  
  # Create 3 equidistant points between min and 0, and 0 and max
  breaks <- c(
    min_value, 
    min_value / 3 * 2,  # 2/3 between min and 0
    min_value / 3,      # 1/3 between min and 0
    0,                  # Zero point
    max_value / 3,      # 1/3 between 0 and max
    max_value / 3 * 2,  # 2/3 between 0 and max
    max_value
  )
  
  # Start the plot with Cairo
  Cairo(output_file, type = "png", res = res, width = width, height = height)
  
  # Create the plot
  print(
    ggplot(df, aes(fill = df[[variable]])) +
      geom_sf() +
      scale_fill_manual(
        breaks = c("0", "1", "2"),  # Set equidistant breaks in the linear scale
        values = c("#ee6055","#ffd97d","#60d394"),
        na.value = "#F5F5F5"# Set labels
      ) +
      labs(fill = legend_name, 
           title = Title) +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=24), legend.position="bottom") +
      # guides(fill = guide_colorbar(barwidth = 100, barheight = 2)) +
      theme_UESI() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size=45),
        legend.title = element_text(size=60),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 50, face = "bold"),
        plot.title = element_text(size=80),
        text = element_text(family = "noto-sans")
      )
  )
  
  # Close the Cairo device
  dev.off()
}

plot_align_map_linear_integrated <- function(df, variable, scale, Title, output_file , min_val, max_val, legend_name, res = 250, width = 7000, height = 3000) {
  
  # Calculate min, max, and midpoints
  min_value <- min_val
  max_value <- max_val
  
  # Create 3 equidistant points between min and 0, and 0 and max
  breaks <- c(
    min_value, 
    min_value / 3 * 2,  # 2/3 between min and 0
    min_value / 3,      # 1/3 between min and 0
    0,                  # Zero point
    max_value / 3,      # 1/3 between 0 and max
    max_value / 3 * 2,  # 2/3 between 0 and max
    max_value
  )
  
  # Start the plot with Cairo
  Cairo(output_file, type = "png", res = res, width = width, height = height)
  
  # Create the plot
  print(
    ggplot(df, aes(fill = df[[variable]])) +
      geom_sf() +
      scale_fill_manual(
        breaks = c("1.5 °C", "2.0 °C", "> 2.0 °C"),  # Set equidistant breaks in the linear scale
        values = c("#60d394","#ffd97d","#ee6055"),
        na.value = "#F5F5F5"# Set labels
      ) +
      labs(fill = legend_name, 
           title = Title) +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=24), legend.position="bottom") +
      # guides(fill = guide_colorbar(barwidth = 100, barheight = 2)) +
      theme_UESI() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size=45),
        legend.title = element_text(size=60),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 50, face = "bold"),
        plot.title = element_text(size=80),
        text = element_text(family = "noto-sans")
      )
  )
  
  # Close the Cairo device
  dev.off()
}


#### Region gap map ####
Byear_resp_sp <- gadm_sp %>% left_join(
                             gap_responsability %>% 
                             filter(year %in% c(2022, 2023)) %>% select(GID_1, GHG_e, gap_E_gvt) %>%
                             mutate(gap_E_gvt_prop = gap_E_gvt*100 / lag(GHG_e)) %>% select(GID_1, gap_E_gvt, gap_E_gvt_prop), 
                             by = "GID_1")%>% filter(is.finite(gap_E_gvt_prop))%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100 ,
                                    TRUE ~ gap_E_gvt_prop))
  
Byear_cap_sp <- gadm_sp %>% left_join(gap_capacity_region %>%                              
                                        filter(year %in% c(2022, 2023)) %>% select(GID_1, GHG_e, gap_E_gvt) %>%
                                        mutate(gap_E_gvt_prop = gap_E_gvt*100  / lag(GHG_e)) %>% select(GID_1,gap_E_gvt, gap_E_gvt_prop), 
                                      by = "GID_1")%>% filter(is.finite(gap_E_gvt_prop))%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100,
                                    TRUE ~ gap_E_gvt_prop))

Byear_resp_sp_2030 <- gadm_sp %>% left_join(
              gap_responsability_2030 %>% 
              filter(year %in% c(2022, 2030)) %>% select(GID_1, GHG_e, gap_E_gvt) %>%
              mutate(gap_E_gvt_prop = gap_E_gvt*100  / lag(GHG_e)) %>% select(GID_1, gap_E_gvt, gap_E_gvt_prop), 
              by = "GID_1")%>% filter(is.finite(gap_E_gvt_prop))%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100 ,
                                    TRUE ~ gap_E_gvt_prop))


Byear_cap_sp_2030 <- gadm_sp %>% left_join(gap_capacity_region_2030 %>%                              
                                        filter(year %in% c(2022, 2030)) %>% select(GID_1, GHG_e, gap_E_gvt) %>%
                                        mutate(gap_E_gvt_prop = gap_E_gvt*100  / lag(GHG_e)) %>% select(GID_1,gap_E_gvt, gap_E_gvt_prop), 
                                      by = "GID_1")%>% filter(is.finite(gap_E_gvt_prop))%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100 ,
                                    TRUE ~ gap_E_gvt_prop))


plot_gap_map_linear_trunc(df = Byear_resp_sp_2030, variable = "gap_E_gvt_prop", scale = 1, min_val = -300, max_val = 300, 
                    Title = 'Responsibility approach', 
                    output_file = "plots/Map_fairshare_resp_Bg_gap_2030_prop_LD15deg.png",
                    legend_name = "2030 Allocation as percent change to 2022 levels")

plot_gap_map_linear_trunc(df = Byear_cap_sp_2030, variable = "gap_E_gvt_prop", scale = 1, min_val = -300, max_val = 300, 
                    Title = 'Capacity approach', 
                    output_file = "plots/Map_fairshare_cap_reg_Bg_gap_2030_prop_LD15deg.png",
                    legend_name = "2030 Allocation as percent change to 2022 levels")

#### Country gap map ####

Byear_resp_sp_country <- gadm_country %>% left_join(gap_responsability %>% mutate(GID_0 = str_sub(GID_1, 1, 3)) %>% group_by(GID_0, year) %>% summarise(gap_E_gvt=sum(gap_E_gvt, na.rm = TRUE)) , by = "GID_0")
Byear_cap_sp_country <- gadm_country %>% left_join(gap_capacity_country %>% select(GID_0, gap_E_gvt), by = "GID_0")

Byear_resp_sp_country_2030 <- gadm_country %>%
              left_join(  
              gap_responsability_2030 %>% 
              mutate(GID_0 = str_sub(GID_1, 1, 3)) %>% filter(!is.na(GID_0))%>%
              group_by(GID_0, year) %>% 
              summarise(gap_E_gvt=sum(gap_E_gvt, na.rm = TRUE), GHG_e=sum(GHG_e, na.rm = TRUE)) %>%
              filter(year %in% c(2022, 2030))%>% 
              select(GID_0, GHG_e, gap_E_gvt) %>%
              mutate(gap_E_gvt_prop = gap_E_gvt*100  / lag(GHG_e)) %>% 
              select(GID_0, gap_E_gvt, gap_E_gvt_prop) %>% filter(is.finite(gap_E_gvt_prop)),
              by = "GID_0")%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100 ,
                                    TRUE ~ gap_E_gvt_prop))

Byear_cap_sp_country_2030 <- gadm_country %>% left_join(gap_capacity_country_2030 %>%                              
                                                     filter(year %in% c(2022, 2030)) %>% select(GID_0, GHG_e, gap_E_gvt) %>%
                                                     mutate(gap_E_gvt_prop = gap_E_gvt*100  / lag(GHG_e)) %>% select(GID_0,gap_E_gvt, gap_E_gvt_prop), 
                                                   by = "GID_0")%>% filter(is.finite(gap_E_gvt_prop))%>%
  mutate(gap_E_gvt_prop_og = gap_E_gvt_prop)%>%
  mutate(gap_E_gvt_prop = case_when(gap_E_gvt_prop > 3*100  ~ 3*100 ,
                                    gap_E_gvt_prop < -3*100  ~ -3*100 ,
                                    TRUE ~ gap_E_gvt_prop))

plot_gap_map_linear_trunc(df = Byear_resp_sp_country_2030, variable = "gap_E_gvt_prop", scale = 1, min_val = -300, max_val = 300, 
                    Title = 'Responsibility approach', 
                    output_file = "plots/Map_fairshare_resp_Bg_gap_2030_prop_country_LD15deg.png",
                    legend_name = "2030 Allocation as percent change to 2022 levels")

plot_gap_map_linear_trunc(df = Byear_cap_sp_country_2030, variable = "gap_E_gvt_prop", scale = 1, min_val = -300, max_val = 300,
                    Title = 'Capacity approach', 
                    output_file = "plots/Map_fairshare_cap_Bg_gap_2030_prop_country_LD15deg.png",
                    legend_name = "2030 Allocation as percent change to 2022 levels")

#### Comparative analysis ####

wb_context <- read_csv('data/WB_class.csv')
oecd_df <- data.frame(
  iso = c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST",
           "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
           "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
           "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR",
           "USA"),
  oecd = "OECD"
)


full_resp_cap_df <- full_resp_df %>% rename(GHG_e_resp = GHG_e) %>%
  left_join(emissions_og, by = 'GID_1')%>%
  left_join(full_capacity_region_df %>% rename(GHG_e_capc = GHG_e), by = c('GID_1', 'year'))%>%
  mutate(GID_0 = str_sub(GID_1, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(POP_1970_2100, by = c('GID_0', 'GID_1', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))
  

full_resp_cap_df_iso <- full_resp_cap_df %>%
  group_by(GID_0, year) %>%
  summarise(GHG_e_capc=sum(GHG_e_capc, na.rm = TRUE), GHG_e_resp=sum(GHG_e_resp, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)

full_resp_cap_df_region <- full_resp_cap_df %>%
  group_by(region, year) %>%
  summarise(GHG_e_capc=sum(GHG_e_capc, na.rm = TRUE), GHG_e_resp=sum(GHG_e_resp, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)

full_resp_cap_city_df <- read_parquet('results/UESI_fairshare_resp_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_resp = GHG_e)%>%
  left_join(read_parquet('results/UESI_fairshare_capacity_city_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_capc = GHG_e), by = c('uesi_id', 'year'))%>%
  mutate(GID_0 = str_sub(uesi_id, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(read_parquet('results/UESI_POP_1970_2100.parquet'), by = c('uesi_id', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))

full_resp_cap_region_agg <- full_resp_cap_df %>% 
  group_by(year, region_wb)%>% summarize(GHG_e_resp_agg = sum(GHG_e_resp, na.rm=TRUE),
                                   GHG_e_capc_agg = sum(GHG_e_capc, na.rm=TRUE))%>%
  filter(!is.na(region_wb))
                                
Cairo("plots/Region_Timeseries_Resp_LD15.png", type = "png", res = 100, width = 1400, height = 1400)
ggplot()+
  geom_line(data=subset(full_resp_cap_region_agg,year<2023),
            aes(x=year,y=GHG_e_resp_agg/(10^3),color=region_wb),
            linetype="dashed",size=1.2)+
  geom_line(data=subset(full_resp_cap_region_agg,year>2023),
            aes(x=year,y=GHG_e_resp_agg/(10^3),color=region_wb),
            size=1.2)+
  ylab("Emissions (GtCO2e)")+ xlab("Year")+
  labs(title="Regional Emissions Timeseries",color="Region", subtitle = 'Responsability Allocation')+
  guides(color = guide_legend(nrow = 2))+
  facet_wrap(~region_wb, scales = "free_y")+
  theme_ipsum()+
  theme(legend.position="bottom",
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))
dev.off()

Cairo("plots/Region_Timeseries_Cap_LD15.png", type = "png", res = 100, width = 1400, height = 1400)
ggplot()+
  geom_line(data=subset(full_resp_cap_region_agg,year<2023),
            aes(x=year,y=GHG_e_capc_agg/(10^3),color=region_wb),
            linetype="dashed",size=1.2)+
  geom_line(data=subset(full_resp_cap_region_agg,year>2023),
            aes(x=year,y=GHG_e_capc_agg/(10^3),color=region_wb),
            size=1.2)+
  ylab("Emissions (GtCO2e)")+ xlab("Year")+
  labs(title="Regional Emissions Timeseries",color="Region", subtitle = 'Capacity Allocation')+
  guides(color = guide_legend(nrow = 2))+
  facet_wrap(~region_wb, scales = "free_y")+
  theme_ipsum()+
  theme(legend.position="bottom",
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12))
dev.off()


Cairo(paste0("plots/fairshare_resp_vs_cap_region_2030_region_pop_LD15deg.png"), type = "png", res=400, width = 4000, height = 4000)
full_resp_cap_df_2030 <- full_resp_cap_df %>% filter(year == 2030) %>% filter(!is.na(region))
full_resp_cap_df_region_2030 <- full_resp_cap_df_region %>% filter(year == 2030)%>% filter(!is.na(region))
full_resp_cap_city_df_2030 <- full_resp_cap_city_df %>% filter(year == 2030)%>% filter(!is.na(region))

(ggplot()+ 
    geom_point(data = full_resp_cap_df_2030, aes(x = GHG_e_resp_pc, y=GHG_e_capc_pc, fill = region, shape = 'State', color = region), stroke = 0, size = 3.0, alpha = 0.3)+
    geom_point(data = full_resp_cap_df_region_2030, aes(x = GHG_e_resp_pc, y=GHG_e_capc_pc, fill = region, shape = 'Region'),colour="black", alpha = 0.4, size = 7)+
    geom_point(data = full_resp_cap_city_df_2030, aes(x = GHG_e_resp_pc, y=GHG_e_capc_pc, color = region, shape = 'City'), size = 3.0, alpha = 0.3)+
    # geom_text(label=paste0(full_resp_cap_df_2030$NAME_1, ", ", full_resp_cap_df_2030$GID_0))+
    labs(color = 'Region', shape = 'Entity', x= 'Responsibility Allocation per capita (TCO2eq)', y = 'Capacity Allocation per capita (TCO2eq)', 
         title = 'Responsibility vs Capacity Allocation per capita for 2030') + 
    scale_x_continuous(transform = asinh_trans, breaks = c(-1600, -1400, -1200, -1000, -800, -600, -400, -200, -100, -10, 0, 10, 100)) + 
    scale_y_continuous(transform = asinh_trans, breaks = c(-100, -10, 0, 10, 100, 200, 400, 800, 1200, 1600, 2000, 2400, 2800))+
    scale_shape_manual(values = c("Region" = 21, "State" = 16, "City" = 8)) +
    guides(fill = 'none', color = guide_legend(order = 1), shape = guide_legend(order = 2))+
    theme_UESI()+
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.text = element_text(size=45),
      legend.title = element_text(size=60),
      axis.text.x = element_text(size = 30, angle = 90),
      axis.title.x = element_text(size = 50),
      axis.ticks.x = element_blank(), 
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(size = 50),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 50, face = "bold"),
      plot.title = element_text(size=80),
      text = element_text(family = "noto-sans")
    ))%>% print()
dev.off()

## Comparative plots
full_resp_cap_df_2022_comp <- full_resp_cap_df  %>% group_by(GID_1) %>%
  mutate(ref_cap_2022 = GHG_e_capc_pc[year == 2022])%>%
  mutate(GHG_e_capc_pc_ref = GHG_e_capc_pc/ref_cap_2022)%>%
  mutate(ref_resp_2022 = GHG_e_resp_pc[year == 2022])%>%
  mutate(GHG_e_resp_pc_ref = GHG_e_resp_pc/ref_resp_2022)

full_resp_cap_df_iso_2022_comp  <- full_resp_cap_df_iso %>% group_by(GID_0) %>%
  mutate(ref_cap_2022 = GHG_e_capc_pc[year == 2022])%>%
  mutate(GHG_e_capc_pc_ref = GHG_e_capc_pc/ref_cap_2022)%>%
  mutate(ref_resp_2022 = GHG_e_resp_pc[year == 2022])%>%
  mutate(GHG_e_resp_pc_ref = GHG_e_resp_pc/ref_resp_2022)


full_resp_cap_city_df <- read_parquet('results/UESI_fairshare_resp_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_resp = GHG_e)%>%
  left_join(read_parquet('results/UESI_fairshare_capacity_city_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_capc = GHG_e), by = c('uesi_id', 'year'))%>%
  mutate(GID_0 = str_sub(uesi_id, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(read_parquet('results/UESI_POP_1970_2100.parquet'), by = c('uesi_id', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))

full_resp_cap_df_city_2022_comp  <- full_resp_cap_city_df%>%
  group_by(uesi_id) %>%
  mutate(ref_cap_2022 = GHG_e_capc_pc[year == 2022])%>%
  mutate(GHG_e_capc_pc_ref = GHG_e_capc_pc/ref_cap_2022)%>%
  mutate(ref_resp_2022 = GHG_e_resp_pc[year == 2022])%>%
  mutate(GHG_e_resp_pc_ref = GHG_e_resp_pc/ref_resp_2022)

countries <- c("USA", "CHN", "IND", "KEN")

full_resp_cap_df_city_2022_comp <- full_resp_cap_df_city_2022_comp %>% filter(GID_0 %in% countries) %>% 
                                   mutate(GID_0 = factor(GID_0, levels = c("USA", "CHN", "IND", "KEN")))

full_resp_cap_df_2022_comp<- full_resp_cap_df_2022_comp %>% filter(GID_0 %in% countries) %>% 
  mutate(GID_0 = factor(GID_0, levels = c("USA", "CHN", "IND", "KEN")))
         
full_resp_cap_df_iso_2022_comp<- full_resp_cap_df_iso_2022_comp %>% filter(GID_0 %in% countries) %>% 
  mutate(GID_0 = factor(GID_0, levels = c("USA", "CHN", "IND", "KEN")))

Cairo(paste0("plots/fairshare_cap_country_region_timeseries_abs_LD15deg.png"), type = "png", res=250, width = 3000, height = 3000)
(ggplot()+ 
    geom_line(data = full_resp_cap_df_city_2022_comp %>% filter(GID_0 %in% countries) %>% filter(year > 2022), aes(x = year, y=GHG_e_capc_pc, group = uesi_id, color = 'Cities'), alpha = 0.3)+
    geom_line(data = full_resp_cap_df_2022_comp %>% filter(GID_0 %in% countries)%>% filter(year > 2022), aes(x = year, y=GHG_e_capc_pc, group = GID_1, color = 'Regions'), alpha = 0.3, size = 0.5)+
    geom_line(data = full_resp_cap_df_iso_2022_comp%>% filter(GID_0 %in% countries)%>% filter(year > 2022), aes(x = year, y=GHG_e_capc_pc, group = GID_0, color = 'Country'), size = 0.6)+
    geom_line(data = full_resp_cap_df_city_2022_comp %>% filter(GID_0 %in% countries)%>% filter(!year > 2022), aes(x = year, y=GHG_e_capc_pc, group = uesi_id, color = 'Cities'), alpha = 0.3)+
    geom_line(data = full_resp_cap_df_2022_comp %>% filter(GID_0 %in% countries)%>% filter(!year > 2022), aes(x = year, y=GHG_e_capc_pc, group = GID_1,color = 'Regions'), alpha = 0.3, size = 0.5)+
    geom_line(data = full_resp_cap_df_iso_2022_comp%>% filter(GID_0 %in% countries)%>% filter(!year > 2022), aes(x = year, y=GHG_e_capc_pc, group = GID_0, color = 'Country'), size = 0.6)+
    # geom_point(data = full_resp_cap_df_region %>% filter(GID_0 == 'USA'), aes(x = year, y=GHG_e_capc_pc), size = 7)+
    # geom_abline(intercept = 0)+
    # geom_text(label=paste0(full_resp_cap_df_2030$NAME_1, ", ", full_resp_cap_df_2030$GID_0))+
    scale_color_manual(
      name = "Entities",
      values = c(
        "Cities" = "#949D43",
        "Regions" = "#57879E",
        "Country" = "#E84855"
      )) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)+
    scale_y_continuous(transform = asinh_trans)+
    scale_x_continuous(
      breaks = c(setdiff(seq(1990, 2100, by = 10), 2020), 2022), # Exclude 2020, add 2022
      labels = c(setdiff(seq(1990, 2100, by = 10), 2020), "2022") # Optional: Custom labels
    ) +
    labs(x= 'Year', y = 'Capacity Allocation per capita (TCO2eq)', 
         title = 'Capacity Allocation Time Series') + 
    facet_wrap(~GID_0, scales = 'free_y')+
    theme_UESI()+
    theme(
      legend.position = "bottom",
      legend.text = element_text(size=45),
      legend.title = element_text(size=60),
      axis.text.x = element_text(size = 30, angle = 90),
      axis.title.x = element_text(size = 50),
      axis.ticks.x = element_blank(), 
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(size = 50),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 50, face = "bold"),
      plot.title = element_text(size=80),
      text = element_text(family = "noto-sans")
    ))%>% print()
dev.off()

Cairo(paste0("plots/fairshare_resp_country_region_timeseries_abs_LD15deg.png"), type = "png", res=250, width = 3000, height = 3000)
(ggplot()+ 
    geom_line(data = full_resp_cap_df_city_2022_comp %>% filter(GID_0 %in% countries) %>% filter(year > 2022), aes(x = year, y=GHG_e_resp_pc, group = uesi_id, color = 'Cities'), alpha = 0.3)+
    geom_line(data = full_resp_cap_df_2022_comp %>% filter(GID_0 %in% countries) %>% filter(year > 2022), aes(x = year, y=GHG_e_resp_pc, group = GID_1, color = 'Regions'), alpha = 0.3, size = 0.5)+
    geom_line(data = full_resp_cap_df_iso_2022_comp%>% filter(GID_0 %in% countries) %>% filter(year > 2022), aes(x = year, y=GHG_e_resp_pc, group = GID_0, color = 'Country'), size = 0.6)+
    geom_line(data = full_resp_cap_df_city_2022_comp %>% filter(GID_0 %in% countries) %>% filter(!year > 2022), aes(x = year, y=GHG_e_resp_pc, group = uesi_id, color = 'Cities'), alpha = 0.3)+
    geom_line(data = full_resp_cap_df_2022_comp %>% filter(GID_0 %in% countries) %>% filter(!year > 2022), aes(x = year, y=GHG_e_resp_pc, group = GID_1, color = 'Regions'), alpha = 0.3, size = 0.5)+
    geom_line(data = full_resp_cap_df_iso_2022_comp%>% filter(GID_0 %in% countries) %>% filter(!year > 2022), aes(x = year, y=GHG_e_resp_pc, group = GID_0, color = 'Country'), size = 0.6)+
    scale_color_manual(
      name = "Entities",
      values = c(
        "Cities" = "#949D43",
        "Regions" = "#57879E",
        "Country" = "#E84855"
      )) +
    # geom_point(data = full_resp_cap_df_region %>% filter(GID_0 == 'USA'), aes(x = year, y=GHG_e_capc_pc), size = 7)+
    # geom_abline(intercept = 0)+
    # geom_text(label=paste0(full_resp_cap_df_2030$NAME_1, ", ", full_resp_cap_df_2030$GID_0))+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)+
    scale_y_continuous(transform = asinh_trans)+
    scale_x_continuous(
      breaks = c(setdiff(seq(1990, 2100, by = 10), 2020), 2022), # Exclude 2020, add 2022
      labels = c(setdiff(seq(1990, 2100, by = 10), 2020), "2022") # Optional: Custom labels
    ) +
    labs(x= 'Year', y = 'Responsibility Allocation per capita (TCO2eq)', 
         title = 'Responsibility Allocation Time Series') + 
    facet_wrap(~GID_0, scales = 'free_y')+
    theme_UESI()+
    theme(
      legend.position = "bottom",
      legend.text = element_text(size=45),
      legend.title = element_text(size=60),
      axis.text.x = element_text(size = 30, angle = 90),
      axis.title.x = element_text(size = 50),
      axis.ticks.x = element_blank(), 
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(size = 50),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 50, face = "bold"),
      plot.title = element_text(size=80),
      text = element_text(family = "noto-sans")
    ))%>% print()
dev.off()

#### Target Comparison ####
### 1.5 degree ###
gca_targets_region_df_baseline<- read_parquet('results/self_reported_targets_prio_region.parquet')%>%
  group_by(GDAM_id, iso, name)%>% add_count()%>%
  filter(n==1)%>%
  mutate(year = 2022)%>%
  left_join(edgar_emissions_1970_2022, by = c('GDAM_id'='GID_1', 'year'))%>%
  select(GDAM_id, name, iso, GHG_self, year)
  
gca_targets_region_df_int<- read_parquet('results/self_reported_targets_prio_region.parquet')%>%
  rbind(gca_targets_region_df_baseline)%>%
  group_by(GDAM_id, iso, name)%>% add_count()%>%
  filter(n>=2)%>%
  select(-n) %>% distinct()%>% ungroup()%>%
  group_by(GDAM_id, iso, name, year)%>%
  slice_min(GHG_self)%>% ungroup()%>% 
  group_by(GDAM_id, iso, name)%>%
  complete(year = full_seq(year, 1)) %>%
  # Linearly interpolate GHG_self values within each group
  mutate(GHG_self = approx(year, GHG_self, year)$y) %>%
  mutate(GHG_self = GHG_self/(10^6))%>%
  ungroup()

full_resp_cap_target_df <- full_resp_cap_df%>%
  right_join(gca_targets_region_df_int, by = c('GID_1'='GDAM_id', 'year'))%>%
  filter(GID_1 %in% gca_targets_region_df_int$GDAM_id)%>%
  mutate(resp_fair = case_when(GHG_e_resp > GHG_self ~ 1,
                               GHG_e_resp < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(capc_fair = case_when(GHG_e_capc > GHG_self ~ 1,
                               GHG_e_capc < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(sum_approach = resp_fair + capc_fair)%>%
  filter(year == 2030)%>%
  mutate(sum_approach = as.character(sum_approach))

full_resp_cap_target <- gadm_sp %>% 
  left_join(full_resp_cap_target_df, by = c('GID_1', 'GID_0', 'NAME_1'))
  
gca_targets_country_df<- read_parquet('results/ndc_targets_prio_country.parquet')%>%
  mutate(GHG_self = GHG_self/(10^6))%>%
  ungroup()

full_resp_cap_target_iso_df <- full_resp_cap_df_iso%>% 
  right_join(gca_targets_country_df, by = c('GID_0'='iso', 'year'))%>%
  mutate(resp_fair = case_when(GHG_e_resp > GHG_self ~ 1,
                               GHG_e_resp < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(capc_fair = case_when(GHG_e_capc > GHG_self ~ 1,
                               GHG_e_capc < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(sum_approach = resp_fair + capc_fair)%>%
  filter(year == 2030)%>% 
  mutate(sum_approach = as.character(sum_approach))

full_resp_cap_target_iso <- gadm_country %>% 
  left_join(full_resp_cap_target_iso_df, by = c('GID_0'))


### 2 degree ###

full_resp_df_2deg <- read_parquet('results/ADM1_fairshare_resp_emissions_1970_2100_2deg.parquet')
full_capacity_country_df_2deg <- read_parquet('results/ADM1_fairshare_capacity_country_emissions_1970_2100_2deg.parquet')
full_capacity_region_df_2deg <- read_parquet('results/ADM1_fairshare_capacity_region_emissions_1970_2100_2deg.parquet')

wb_context <- read_csv('data/WB_class.csv')
oecd_df <- data.frame(
  iso = c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST",
          "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
          "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
          "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR",
          "USA"),
  oecd = "OECD"
)

full_resp_cap_df_2deg <- full_resp_df_2deg %>% rename(GHG_e_resp = GHG_e) %>%
  left_join(emissions_og, by = 'GID_1')%>%
  left_join(full_capacity_region_df_2deg %>% rename(GHG_e_capc = GHG_e), by = c('GID_1', 'year'))%>%
  mutate(GID_0 = str_sub(GID_1, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(POP_1970_2100, by = c('GID_0', 'GID_1', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))

full_resp_cap_df_iso_2deg <- full_resp_cap_df_2deg %>%
  group_by(GID_0, year) %>%
  summarise(GHG_e_capc=sum(GHG_e_capc, na.rm = TRUE), GHG_e_resp=sum(GHG_e_resp, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)

gca_targets_region_df_baseline<- read_parquet('results/self_reported_targets_prio_region.parquet')%>%
  group_by(GDAM_id, iso, name)%>% add_count()%>%
  filter(n==1)%>%
  mutate(year = 2022)%>%
  left_join(edgar_emissions_1970_2022, by = c('GDAM_id'='GID_1', 'year'))%>%
  select(GDAM_id, name, iso, GHG_self, year)

gca_targets_region_df_int<- read_parquet('results/self_reported_targets_prio_region.parquet')%>%
  rbind(gca_targets_region_df_baseline)%>%
  group_by(GDAM_id, iso, name)%>% add_count()%>%
  filter(n>=2)%>%
  select(-n) %>% distinct()%>% ungroup()%>%
  group_by(GDAM_id, iso, name, year)%>%
  slice_min(GHG_self)%>% ungroup()%>% 
  group_by(GDAM_id, iso, name)%>%
  complete(year = full_seq(year, 1)) %>%
  # Linearly interpolate GHG_self values within each group
  mutate(GHG_self = approx(year, GHG_self, year)$y) %>%
  mutate(GHG_self = GHG_self/(10^6))%>%
  ungroup()

full_resp_cap_target_df_2deg <- full_resp_cap_df_2deg%>%
  right_join(gca_targets_region_df_int, by = c('GID_1'='GDAM_id', 'year'))%>%
  filter(GID_1 %in% gca_targets_region_df_int$GDAM_id)%>%
  mutate(resp_fair = case_when(GHG_e_resp > GHG_self ~ 1,
                               GHG_e_resp < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(capc_fair = case_when(GHG_e_capc > GHG_self ~ 1,
                               GHG_e_capc < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(sum_approach = resp_fair + capc_fair)%>%
  filter(year == 2030)%>%
  mutate(sum_approach = as.character(sum_approach))

full_resp_cap_target_2deg <- gadm_sp %>% 
  left_join(full_resp_cap_target_df_2deg, by = c('GID_1', 'GID_0', 'NAME_1'))


gca_targets_country_df<- read_parquet('results/ndc_targets_prio_country.parquet')%>%
  mutate(GHG_self = GHG_self/(10^6))%>%
  ungroup()

full_resp_cap_target_iso_df_2deg <- full_resp_cap_df_iso_2deg%>% 
  right_join(gca_targets_country_df, by = c('GID_0'='iso', 'year'))%>%
  mutate(resp_fair = case_when(GHG_e_resp > GHG_self ~ 1,
                               GHG_e_resp < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(capc_fair = case_when(GHG_e_capc > GHG_self ~ 1,
                               GHG_e_capc < GHG_self ~ 0,
                               TRUE ~ NA))%>%
  mutate(sum_approach = resp_fair + capc_fair)%>%
  filter(year == 2030)%>%
  mutate(sum_approach = as.character(sum_approach))

full_resp_cap_target_iso_2deg <- gadm_country %>% 
  left_join(full_resp_cap_target_iso_df_2deg, by = c('GID_0'))


### Join 1.5 and 2 degree ####

full_resp_cap_df_15_2deg <- full_resp_cap_df_2deg %>%
  left_join(full_resp_cap_df,
            by = c("GID_1", "year", "NAME_1", "GID_0", "country", "region",
                   "wb_income_group", "oecd", "region_wb", "population", "pop_size"),
            suffix = c("_2deg", "_15deg"))

full_resp_cap_df_iso_15_2deg <- full_resp_cap_df_iso_2deg %>%
  left_join(full_resp_cap_df_iso,
            by = c("GID_0", "year", "population"),
            suffix = c("_2deg", "_15deg"))


full_resp_cap_target_df_15_2deg <- full_resp_cap_df_15_2deg%>%
  right_join(gca_targets_region_df_int, by = c('GID_1'='GDAM_id', 'year'))%>%
  filter(GID_1 %in% gca_targets_region_df_int$GDAM_id)%>%
  mutate(GHG_e_int_15deg = (GHG_e_resp_15deg + GHG_e_capc_15deg)/2,
         GHG_e_int_2deg = (GHG_e_resp_2deg + GHG_e_capc_2deg)/2)%>%
  mutate(align15= case_when(GHG_e_int_15deg > GHG_self ~ "1.5 °C",
                            GHG_e_int_15deg < GHG_self ~ NA,
                               TRUE ~ NA))%>%
  mutate(align2 = case_when(GHG_e_int_2deg > GHG_self ~ "2.0 °C",
                            GHG_e_int_2deg < GHG_self ~ "> 2.0 °C",
                               TRUE ~ NA))%>%
  mutate(align_final = coalesce(align15, align2))%>% 
  filter(year == 2030)

full_resp_cap_target_15_2deg <- gadm_sp %>% 
  left_join(full_resp_cap_target_df_15_2deg, by = c('GID_1', 'GID_0', 'NAME_1'))


full_resp_cap_target_iso_df_15_2deg <- full_resp_cap_df_iso_15_2deg%>% 
  right_join(gca_targets_country_df, by = c('GID_0'='iso', 'year'))%>%
  mutate(GHG_e_int_15deg = (GHG_e_resp_15deg + GHG_e_capc_15deg)/2,
         GHG_e_int_2deg = (GHG_e_resp_2deg + GHG_e_capc_2deg)/2)%>%
  mutate(align15= case_when(GHG_e_int_15deg > GHG_self ~ "1.5 °C",
                            GHG_e_int_15deg < GHG_self ~ NA,
                            TRUE ~ NA))%>%
  mutate(align2 = case_when(GHG_e_int_2deg > GHG_self ~ "2.0 °C",
                            GHG_e_int_2deg < GHG_self ~ "> 2.0 °C",
                            TRUE ~ NA))%>%
  mutate(align_final = coalesce(align15, align2))%>% filter(!is.na(align_final))%>%
  filter(year == 2030)

full_resp_cap_target_iso_15_2deg <- gadm_country %>% 
  left_join(full_resp_cap_target_iso_df_15_2deg, by = c('GID_0'))


plot_align_map_linear_integrated(df = full_resp_cap_target_iso_15_2deg, variable = "align_final", scale = 1, min_val = 0, max_val = 2,
                            Title = 'Comparison of pledges with averaged fairshare allocations',
                            output_file = "plots/Map_fairshare_assessment_country_LD15_2deg.png",
                            legend_name = "Pledged aligned with allocation to stay below:")

## City Alignment
area_centroid_Diss_raw  <- read_csv('data/area_D_cities.csv')
area_centroid_Diss <- st_as_sf(data.frame(area_centroid_Diss_raw, geom= geojson_sf(area_centroid_Diss_raw$centroid)))%>%
  mutate(long_city = unlist(purrr::map(.$geometry,1)),
         lat_city = unlist(purrr::map(.$geometry,2)))%>% as.data.frame()%>%
  rename(area_city = area)%>%
  select(area_city, uesi_id, lat_city, long_city)

 
gca_targets_city_df_baseline<- read_parquet('results/self_reported_targets_prio_cities.parquet')%>%
  group_by(uesi_id, iso, name)%>% add_count()%>%
  filter(n==1)%>%
  mutate(year = 2022)%>%
  left_join(edgar_emissions_1970_2022_uesi, by = c('uesi_id', 'year'))%>%
  select(uesi_id, name, iso, GHG_self, year)


gca_targets_city_df<- read_parquet('results/self_reported_targets_prio_cities.parquet')%>%
  select(uesi_id, name, iso, GHG_self, year)%>%
  rbind(gca_targets_city_df_baseline)%>%
  group_by(uesi_id, iso, name)%>% add_count()%>%
  filter(n>=2)%>% select(-n) %>% distinct()%>% ungroup()%>%
  group_by(uesi_id, iso, name, year)%>%
  slice_min(GHG_self)%>% ungroup()%>% 
  group_by(uesi_id, iso, name)%>%
  complete(year = full_seq(year, 1)) %>%
  # Linearly interpolate GHG_self values within each group
  mutate(GHG_self = approx(year, GHG_self, year)$y) %>%
  ungroup()


full_resp_cap_city_df <- read_parquet('results/UESI_fairshare_resp_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_resp = GHG_e)%>%
  left_join(read_parquet('results/UESI_fairshare_capacity_city_emissions_1970_2100_LD15deg.parquet') %>% rename(GHG_e_capc = GHG_e), by = c('uesi_id', 'year'))%>%
  mutate(GID_0 = str_sub(uesi_id, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(read_parquet('results/UESI_POP_1970_2100.parquet'), by = c('uesi_id', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))

full_resp_cap_city_df_2deg <- read_parquet('results/UESI_fairshare_resp_emissions_1970_2100_2deg.parquet') %>% rename(GHG_e_resp = GHG_e)%>%
  left_join(read_parquet('results/UESI_fairshare_capacity_city_emissions_1970_2100_2deg.parquet') %>% rename(GHG_e_capc = GHG_e), by = c('uesi_id', 'year'))%>%
  mutate(GID_0 = str_sub(uesi_id, 1, 3))%>%
  left_join(wb_context, by = c('GID_0'='iso'))%>%
  left_join(oecd_df, by = c('GID_0'='iso'))%>%
  mutate(region_wb = region)%>%
  mutate(region = coalesce(oecd, region_wb))%>%
  left_join(read_parquet('results/UESI_POP_1970_2100.parquet'), by = c('uesi_id', 'year'='population_year'))%>%
  mutate(GHG_e_resp_pc = GHG_e_resp*(10^6)/population, GHG_e_capc_pc = GHG_e_capc*(10^6)/population)%>%
  group_by(region) %>%
  mutate(pop_size = mean(population, na.rm = TRUE))

full_resp_cap_city_df_15_2deg <- full_resp_cap_city_df_2deg %>%
  left_join(full_resp_cap_city_df,
            by = c("uesi_id", "year", "GID_0", "country", "region", "wb_income_group",
                   "oecd", "region_wb", "population"),
            suffix = c("_2deg", "_15deg"))


full_resp_cap_city_target_df_15_2deg <- full_resp_cap_city_df_15_2deg%>%
  right_join(gca_targets_city_df, by = c('uesi_id', 'year'))%>%
  filter(uesi_id %in% gca_targets_city_df$uesi_id)%>%
  mutate(GHG_e_int_15deg = (GHG_e_resp_15deg + GHG_e_capc_15deg)/2,
         GHG_e_int_2deg = (GHG_e_resp_2deg + GHG_e_capc_2deg)/2)%>%
  mutate(align15= case_when(GHG_e_int_15deg > GHG_self ~ "1.5 °C",
                            GHG_e_int_15deg < GHG_self ~ NA,
                            TRUE ~ NA))%>%
  mutate(align2 = case_when(GHG_e_int_2deg > GHG_self ~ "2.0 °C",
                            GHG_e_int_2deg < GHG_self ~ "> 2.0 °C",
                            TRUE ~ NA))%>%
  mutate(align_final = coalesce(align15, align2))%>%
  filter(year == 2030)%>% filter(!is.na(align_final))%>%
  left_join(area_centroid_Diss, by = 'uesi_id')


## Summarize table
summary_entity_df <- rbind(
  full_resp_cap_target_df_15_2deg %>%
  group_by(align_final) %>%
  summarise(count = n(), .groups = "drop")%>% mutate(entity_type = 'Region'),
  full_resp_cap_target_iso_df_15_2deg %>%
  group_by(align_final) %>%
  summarise(count = n(), .groups = "drop")%>% mutate(entity_type = 'Country'),
  full_resp_cap_city_target_df_15_2deg %>%
  group_by(align_final) %>%
  summarise(count = n(), .groups = "drop")%>% mutate(entity_type = 'City'))%>%
  select(entity_type, align_final, count)%>%
  group_by(entity_type) %>%
  mutate(percent = round((count / sum(count)) * 100,2))

full_resp_cap_target_df_15_2deg_country_city <- rbind(
  full_resp_cap_target_df_15_2deg %>% 
  ungroup()%>% select(GID_0, GID_1, year, align_final) %>% 
    mutate(entity_type = 'Region') %>% rename(entity_id = GID_1),
  full_resp_cap_city_target_df_15_2deg %>% 
  ungroup()%>% select(GID_0, uesi_id, year, align_final)%>% 
    mutate(entity_type = 'City') %>% rename(entity_id = uesi_id)
  )%>%
  left_join(full_resp_cap_target_iso_df_15_2deg %>% select(GID_0, year, align_final), 
            by = c('GID_0', 'year'), suffix = c("_entity", "_country"),
  )%>%
  mutate(ambition_country = case_when(align_final_entity == '1.5 °C' & align_final_country == '> 2.0 °C' ~ 'More Ambitious',
                                      align_final_entity == '2.0 °C' & align_final_country == '> 2.0 °C' ~ 'More Ambitious',
                                      align_final_entity == '> 2.0 °C' & align_final_country == '> 2.0 °C' ~ 'As Ambitious',
                                      align_final_entity == '2.0 °C' & align_final_country == '2.0 °C' ~ 'As Ambitious',
                                      align_final_entity == '1.5 °C' & align_final_country == '1.5 °C' ~ 'As Ambitious',
                                      TRUE ~ 'Less Ambitious'))

summary_df <- full_resp_cap_target_df_15_2deg_country_city %>%
  group_by(entity_type, ambition_country) %>%
  summarise(count = n(), .groups = "drop")%>%
  group_by(entity_type) %>%
  mutate(percent = round((count / sum(count)) * 100,2))

### Region and city alignment ####

map_region_city_int_align <- ggplot(full_resp_cap_target_15_2deg) +
      geom_sf(aes(fill = align_final))+
      scale_fill_manual(
        breaks = c("1.5 °C", "2.0 °C", "> 2.0 °C"),  # Set equidistant breaks in the linear scale
        values = c("#60d394","#ffd97d","#ee6055"),
        na.value = "#F5F5F5"# Set labels
      ) +
      geom_point(data = full_resp_cap_city_target_df_15_2deg, aes(x = long_city, y = lat_city, fill = align_final), colour="black",pch=21, size = 5)+
      # scale_color_manual(
      #   breaks = c("1.5 °C", "2.0 °C", "> 2.0 °C"),  # Set equidistant breaks in the linear scale
      #   values = c("green", "yellow", "orange")  # Set labels
      # )
      labs(fill = "Pledged aligned with allocation to stay below:", 
           title = "Comparison of pledges with averaged fairshare allocations", 
           x = "", y="") +  # Custom legend title with full subscript "2e"
      theme(legend.text = element_text(face = "bold", size=35), legend.position="bottom") +
      # guides(fill = guide_colorbar(barwidth = 100, barheight = 2)) +
      theme_UESI() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size=45),
        legend.title = element_text(size=60),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 50, face = "bold"),
        plot.title = element_text(size=80),
        text = element_text(family = "noto-sans")
      )

Cairo("plots/Map_fairshare_assessment_region_city_LD15_2deg.png", type = "png", res = 250, width = 7000, height = 3000)
print(map_region_city_int_align)
dev.off()
