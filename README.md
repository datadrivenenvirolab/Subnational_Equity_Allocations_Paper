# From countries to cities: assessing climate ambition with a multi-level fair-share allocation framework

Authors: Yann Robiou du Pont, Diego Manya, Kaihui Song,Håvard Haarstad, Angel Hsu

## Instructions
1) Run the following scripts to replicate fair-share allocation:
- "src\R\fairshare_results_2degree_1970.R": Administrative 1 Level allocation for 2 degree scenario
- "src\R\fairshare_results_LD15degree_1970.R": Administrative 1 Level allocation for 1.5 degree scenario
- "src\R\fairshare_results_UESI_2degree_1970.R": City level allocation for 2 degree scenario
- "src\R\fairshare_results_UESI_LD15degree_1970.R": City Level allocation for 1.5 degree scenario

2) Run Script "src\R\fairshare_LD_plots.R" for plot replication.

## Data
### Input data
1) POP_YYYY_YYYY_GADM_1.parquet and POP_YYYY_YYYY_UESI.parquet : Files with that structure include Population data derived from raster sources for Regions (GDAM_1) and cities (UESI)<sup>45,46,47</sup>.
2) GDP_1970_2100_GADM_1.parquet: GDP data from raster sources for Regions (GDAM_1)<sup>48</sup>.
3) LD15_scenario.xlsx and WITCH_5_0_CO_Bridge.xlsx: Correspond to global scenario for 1.5 and 2.0 degree IPCC-AR6 report and hosted online (https://data.ece.iiasa.ac.at/ar6).
4) 13) LULUCF_per_temperature.csv: Average Land Use, Land-Use Change and Forestry emissions for 1.5 and 2 degree scenarios
5) edgar_emissions_TOTAL_ADM1.parquet and edgar_emissions_TOTAL_UESI.parquet: Correspond to the territorial emissions timeseries extracted from EDGAR for Regions (GDAM_1) and cities (UESI)<sup>42,43</sup>.
6) ndc_final_exclude_hotair.csv: Country mitigation targets based on the NDC<sup>49</sup>
7) target_city_focus.csv and target_region_focus.csv: Target mititgation data for subnational entities<sup>1</sup>.
8) WB_class.csv: World Bank classification for income and geographical regions
