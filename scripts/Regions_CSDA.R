library(tidyverse)
library(sf)

BUFF_DIS = 2000

calfire = st_read('data/fire_boundaries/California_Fire_Perimeters_(all).shp')
historic_fp = st_read('output/historical/historical_fire_perimeters_candidates.gpkg')
historic_sites = st_read('output/historical/historical_sampled_sites.gpkg')

recent_fp = st_read('output/recent/fire_perimeters_candidates.gpkg')
recent_sites = st_read('output/recent/sampled_sites.gpkg')


fires = bind_rows(historic_fp, recent_fp) %>%
  st_buffer(BUFF_DIS)

fires_very_recent = fires %>%
  filter(year> 2021) 

st_write(fires_very_recent, 'output/CSDA/fires_2022_2024.shp', append = F)

sites = bind_rows(historic_sites, recent_sites) 

sites_very_recent = sites %>%
  filter(fire_year > 2021)

st_write(fires_very_recent, 'output/CSDA/sites_2022_2024.shp', append = F)

fire_long_record = st_read('output/historical/historical_fire_perimeters_candidates_long_record.gpkg') %>%
  bind_rows(historic_fp) %>%
  bind_rows(recent_fp) %>%
  st_transform(crs = st_crs(calfire)) %>%
  unique() %>%
  st_buffer(BUFF_DIS) %>%
  st_union()

st_write(fire_long_record, 'output/CSDA/fires_1912_2024.shp', append = F)
