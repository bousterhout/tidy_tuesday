######################
# Tidy Tuesday
# 4 January 2021
# Transit Costs
# Britt Ousterhout
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-05/readme.md
######################

#Objective of project is to determine how to decrease costs of mass transit projects in 

#Libraries
require(dplyr)
require(ggplot2)
require(countrycode)
require(RColorBrewer)
require(cowplot)

#Read in data
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

#Cost by country
cost_by_country <- df %>% 
  group_by(country) %>% 
  filter(n() > 10) %>% 
  summarize(mean_cost_per_km = mean(cost_km_millions, na.rm = TRUE),
            median_cost_per_km = median(cost_km_millions, na.rm = TRUE),
            iqr_cost_per_km = IQR(cost_km_millions, na.rm = TRUE),
            mean_tunnel_per = mean(tunnel / length, na.rm = TRUE),
            mean_stations_per = mean(stations/length, na.rm = TRUE))

cost_by_country$country_name <- countrycode(cost_by_country$country,
                                            origin = "iso2c",
                                            destination = "country.name")

cost_by_country$country_name <- ifelse(cost_by_country$country == "UK", "UK",
                                       ifelse(cost_by_country$country == "US", "USA", cost_by_country$country_name))

base_map_data <- map_data("world")

cost_mapped <- full_join(base_map_data,
                         cost_by_country,
                         by = c("region" = "country_name"))

base_map <- ggplot(data = base_map_data, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + 
  geom_polygon() +
  theme_light()

country <- 
base_map + geom_polygon(data = cost_mapped, aes(fill = median_cost_per_km)) +
  geom_polygon(fill = NA) +
  scale_fill_distiller(palette = 'Reds',
                       direction = 2,
                       na.value = "grey90",
                       name = "",
                       guide = guide_colourbar(direction = "horizontal",
                                               location = "bottom",
                                               barwidth = 20,
                                               barheight = 0.5))+
  ggtitle("Avg cost of Urban Rail Projects \n (millions USD per km)") +
  theme_light() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  
  )

#Cost doesn't appear to be driven by stations of number of tunnels
tunnels <-   
base_map + geom_polygon(data = cost_mapped, aes(fill = mean_tunnel_per)) +
  geom_polygon(fill = NA) +
  scale_fill_distiller(palette = 'Blues',
                       direction = 2,
                       na.value = "grey90",
                       name = "",
                       guide = guide_colourbar(direction = "horizontal",
                                               location = "bottom",
                                               barwidth = 10,
                                               barheight = 0.5))+
  ggtitle("Tunnels per km") +
  theme_light() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  
  )

stations <-
  base_map + geom_polygon(data = cost_mapped, aes(fill = mean_stations_per)) +
  geom_polygon(fill = NA) +
  scale_fill_distiller(palette = 'Blues',
                       direction = 2,
                       na.value = "grey90",
                       name = "",
                       guide = guide_colourbar(direction = "horizontal",
                                               location = "bottom",
                                               barwidth = 10,
                                               barheight = 0.5))+
  ggtitle("Stations per km") +
  theme_light() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  
  )

#Print
bottom_row <- plot_grid(tunnels, stations, align = 'v', nrow = 1)

final_plot <- plot_grid(country, bottom_row, nrow = 2, rel_heights = c(1.75,1))

ragg::agg_png("figures/04_Jan_2021_TransitCosts.png",
              width = 1200, height = 1600,
              res = 300)
print(final_plot)
dev.off()
