#Tidy Tuesday Challenge 7/28/2026
#Ecoturism
#Challenge questions
## Under which weather conditions are you most likely to observe a Gouldian finch?
## How does weather affect tourism numbers in each region?
## How do observations of the different animals relate to numbers of tourists?

require(tidytuesdayR)
library(tidyverse)
require(ecotourism)
require(mosaic)

theme_set(theme_bw())

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

ecotourism_data = right_join(weather, occurrences, by = c("ws_id", "date", "year",
                                                          "month", "day", "weekday", "dayofyear"),
                             relationship = "many-to-many")

#simple statisitcs
str(ecotourism_data)
summary(ecotourism_data)

#looking at the weather that gouldian finches are most often observed
favstats(temp~organism_name, data = ecotourism_data)
favstats(dewp~organism_name, data = ecotourism_data)
favstats(rh~organism_name, data = ecotourism_data)
favstats(prcp~organism_name, data = ecotourism_data)
favstats(wind_speed~organism_name, data = ecotourism_data)

#sleceting just the weather columns and the organism name

gouldian_weather_data = ecotourism_data |>
  select(temp, dewp, rh, prcp, wind_speed, organism_name) |>
  filter(organism_name == "Gouldian finch")

#correlation matrix of numeric columns
cor( gouldian_weather_data[ ,c(1:5) ], use = "complete.obs" )

gouldian_weather_data |> 
  ggplot(aes(prcp)) +
  geom_density()

gouldian_weather_data |> 
  ggplot(aes(wind_speed)) +
  geom_density() 

gouldian_weather_data |> 
  ggplot(aes(temp)) +
  geom_density()

gouldian_weather_data |> 
  ggplot(aes(dewp)) +
  geom_density()



    