######################
# Tidy Tuesday
# 12 January 2021
# Tate Modern Collection
# Britt Ousterhout
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md
######################

#Objective of project is to determine how to decrease costs of mass transit projects in 

#Libraries
require(dplyr)
require(ggplot2)
#require(countrycode)
require(RColorBrewer)
require(cowplot)

#Helper functions
round_to_decade = function(value){ return(round(value / 10) * 10) }
round_to_20 = function(value){return(ifelse(value %% 20 < 10, value - (value %% 20), value - (value %% 20) + 20))}

#Read in data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

df <- merge(x = artwork,
            y = artists,
            by.x = "artistId", 
            by.y = "id")

df %>% group_by(artist) %>% 
  count(sort = TRUE)

by_year_acquired <- df %>% 
  filter(artist != 'Turner, Joseph Mallord William') %>% 
  mutate(acquisitionYear = round_to_20(acquisitionYear)) %>% 
  group_by(gender, acquisitionYear) %>% 
  count() %>% 
  filter(!is.na(gender))

split_dfs <- split(by_year_acquired, by_year_acquired$gender)
spline_df <- lapply(split_dfs, function(x)spline(x$acquisitionYear, x$n))
spline_df$Female$y <- ifelse(spline_df$Female$y < 0, 0, spline_df$Female$y)
spline_df$Male$y <- ifelse(spline_df$Male$y < 0, 0, spline_df$Male$y)

year_acquired <-
  ggplot() + 
  geom_line(data = as.data.frame(spline_df$Male), aes(x = x, y = y), lwd = 1, color = '#241571') +
  geom_line(data = as.data.frame(spline_df$Female), aes(x = x, y = y), lwd = 1, color = '#FF1493') +
  scale_y_continuous("Count", limits = c(0,11000), breaks = seq(0,9000,3000)) +
  scale_x_continuous("Year acquired")

  

by_year_created <- df %>% 
  filter(artist != 'Turner, Joseph Mallord William') %>% 
  mutate(year = round_to_20(year)) %>% 
  group_by(gender, year) %>% 
  count() %>% 
  filter(!is.na(gender))

split_dfs <- split(by_year_created, by_year_created$gender)
spline_df <- lapply(split_dfs, function(x)spline(x$year, x$n))
spline_df$Female$y <- ifelse(spline_df$Female$y < 0, 0, spline_df$Female$y)
spline_df$Male$y <- ifelse(spline_df$Male$y < 0, 0, spline_df$Male$y)

year_created <- ggplot() + 
  geom_line(data = as.data.frame(spline_df$Male), aes(x = x, y = y), lwd = 1, color = '#241571') +
  geom_line(data = as.data.frame(spline_df$Female), aes(x = x, y = y), lwd = 1, color = '#FF1493') +
  scale_y_continuous("Count", limits = c(0,11000), breaks = seq(0,9000,3000)) +
  scale_x_continuous("Year created")
  

ragg::agg_png("figures/13_Jan_2021_TateModern.png",
              width = 1200, height = 1600,
              res = 300)
print(plot_grid(year_created, year_acquired, ncol = 1))
dev.off()


