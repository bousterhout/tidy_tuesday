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
require(rworldmap)
require(passport)

#Read in data
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

#Summarize

#Unique cities: 141
length(unique(df$city))

#Unique countries: 57
length(unique(df$country))

#Cities with more than 1 project: 86. Cities with more than a handful of projects generally in China

mult_project_cities <- df %>% 
  group_by(city) %>% 
  mutate(n_projects = n()) %>% 
  filter(n_projects > 1) %>% 
  count(city)

length(unique(mult_project_cities$city))
hist(mult_project_cities$n)

#Do costs increase or decrease with time? Modestly ($23,000 / year)
hist(df$cost_km_millions)
year_mod <- lm(log(cost_km_millions) ~ as.numeric(start_year), data = df)
plot(year_mod)
summary(year_mod)
ggplot(data = df) +
  geom_jitter(aes(x = cost_km_millions, y = start_year))


#Cost by country
cost_by_country <- df %>% 
  group_by(country) %>% 
  summarize(mean_cost_per_km = mean(cost_km_millions, na.rm = TRUE),
            median_cost_per_km = median(cost_km_millions, na.rm = TRUE),
            iqr_cost_per_km = IQR(cost_km_millions, na.rm = TRUE),
            mean_tunnel_per = mean(tunnel / length, na.rm = TRUE),
            mean_stations_per = mean(stations/length, na.rm = TRUE))

cost_by_country$country_name <- as_country_name(cost_by_country$country,
                                                short = TRUE,
                                                variant = TRUE)

cost_by_country$country_name <- ifelse(cost_by_country$country == "UK", "UK",
                                       ifelse(cost_by_country$country == "US", "USA", cost_by_country$country_name))

base_map_data <- map_data("world")

cost_mapped <- full_join(base_map_data,
                         cost_by_country,
                         by = c("region" = "country_name"))

base_map <- ggplot(data = base_map_data, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "gray")

base_map + geom_polygon(data = cost_mapped, aes(fill = median_cost_per_km), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

#Cost doesn't appear to be driv
base_map + geom_polygon(data = cost_mapped, aes(fill = mean_stations_per), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

#Some small impact of stations per km but seems minor
base_map + geom_polygon(data = cost_mapped, aes(fill = mean_tunnel_per), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
