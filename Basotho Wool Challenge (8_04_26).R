#Tidy Tuesday CHallenge 8/4/2026
#Basotho Wool
# Challenge Questions:

##Does Winter (being in the Southern hemisphere, these are the months June, July and August) 
#impact Basotho wool quantity/volume exported and revenue?

##Which country imports the most Basotho wool at time on average?

basotho_wool = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-04/basotho_wool.csv')

library(dplyr)
library(ggplot2)
library(tidyplots)
library(ggtext) 
library(scales)


# Plot from Tidy Tuesday challenge repo
basotho_wool_2023 <- basotho_wool |>
  filter(ref_year == 2023) |>
  group_by(reporter_desc) |>
  summarise(total_primary_value = sum(primary_value, na.rm = TRUE)) |>
  ungroup() |>
  arrange(total_primary_value) |>
  mutate(reporter_desc = factor(reporter_desc, levels = reporter_desc))

# generating the horizontal bar chart
basotho_wool_plot <- basotho_wool_2023 |>
  tidyplot(x = total_primary_value, y = reporter_desc, color = reporter_desc) |>
  add_sum_bar(alpha = 0.8) |>
  add_title("Global Basotho Wool Imports (2023)") |>
  add_caption("Using mirror trade statistics, this plot shows the principal monetary value of wool items recorded by global importing partners from Lesotho in **2023**.  
              By focusing on **primary_value**, the dataset captures the main statistical focus value standardized by the UN Comtrade database, bypassing  
              inconsistencies between CIF and FOB declarations across different regions.") |>
  adjust_x_axis(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) |> 
  adjust_x_axis_title("Total Import Value (Primary Value USD)") |>
  adjust_size(width = 370, height = 60) |> 
  theme_tidyplot() +
  geom_text(
    aes(label = scales::dollar(total_primary_value, scale_cut = scales::cut_short_scale())),
    hjust = -0.15,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  theme(
    plot.title = element_text(family = "Plus Jakarta Sans", size = 26, face = "bold", margin = margin(b = 20)),
    axis.title.x = element_text(size = 16, margin = margin(t = 18)),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 14, margin = margin(r = 10)),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.caption = ggtext::element_markdown(size = 14, hjust = 0, margin = margin(t = 40), lineheight = 1.6),
    legend.position = "none" 
  )
basotho_wool_plot
