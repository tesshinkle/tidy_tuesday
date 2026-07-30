#Tidy Tuesday Challenge 7/28/2026
#Ecoturism

require(tidytuesdayR)
library(tidyverse)
require(ecotourism)

tuesdata = tidytuesdayR::tt_load('2026-07-28')

occurrences = tuesdata$occurrences
tourism = tuesdata$tourism
weather = tuesdata$weather

# Option 2: Read directly from GitHub
#
#occurrences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/occurrences.csv')
#tourism <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/tourism.csv')
#weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/weather.csv')


manta_rays = ecotourism::manta_rays |>
  mutate(organism_name = "Manta ray", .after = "sci_name")
gouldian_finch = ecotourism::gouldian_finch |>
  mutate(organism_name = "Gouldian finch", .after = "sci_name")
orchids = ecotourism::orchids |>
  mutate(organism_name = "Orchid", .after = "sci_name")
glowworms = ecotourism::glowworms |>
  mutate(organism_name = "Glowworm", .after = "sci_name")
occurrences = bind_rows(
  manta_rays, gouldian_finch, orchids, glowworms
)

weather = ecotourism::weather

tourism = ecotourism::tourism_quarterly |>
  left_join(tourism_region, by = "region_id") |>
  rename(ws_id = ws_id.x) |>
  select(-ws_id.y)

#the three data sets can be joined by ws_id

