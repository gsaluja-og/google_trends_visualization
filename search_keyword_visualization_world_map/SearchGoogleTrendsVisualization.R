# install.packages(c("gtrendsR", "countrycode", "tidyverse", "sf", "lwgeom", "rnaturalearth", "rnaturalearthdata", "gganimate", "transformr"))
# install.packages("gifski")

library(gtrendsR)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(gganimate)
library(transformr)
library(lwgeom)
library(magick)
library(countrycode)
library(dplyr)
library(stringr)

# ADD KEYWORD TO SEARCH HERE <- "NFT"
keyword = "NFT"
# SET THE TOTAL TIME FRAME HERE
trend_start_date = "2021-06-01"
trend_end_date = "2023-03-01"

# Define a threshold for visibility on the map.
# If a country's interest surpasses this value, it's plotted.
# This value is the "hits" value returned by google trends
# which is a normalized data point. 100 is peak interest in the keyword, 0 is none.
threshold_value = 30

# Don't use spherical geometry. (Known issues with plotting countries like Russia)
# See https://stackoverflow.com/questions/72352070/unexpected-result-when-plotting-sf-object-from-rnaturalearth-after-having-remo
sf::sf_use_s2(FALSE)

# Get a shapefile for countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Rename 'iso_a2' in the world dataset to match 'geo' from google trends dataset
world <- world %>% rename(geo = iso_a2)

# Retrieve a list of all country codes to loop through
all_country_codes <- world$geo

# Define a function to get trends data for a specific keyword in a specific country
get_trends <- function(keyword, country) {
  tryCatch({
    trends_data <- gtrends(keyword, geo = country, time = paste(trend_start_date, trend_end_date, sep = " "))$interest_over_time
    return(trends_data)
  }, error = function(e) {
    return(NULL)  # Return NULL if there's an error (e.g., no data for that country)
  })
}

all_data <- list()

# Loop through each country to get its trends data (TAKES SOME TIME)
for(country in all_country_codes) {
  
  trends_data <- get_trends(keyword, country)
  
  # Only proceed if trends_data isn't NULL (meaning no error occurred)
  if(!is.null(trends_data)) {
    # Convert blanks to NA
    trends_data$hits[trends_data$hits == ""] <- NA
    
    # Convert hits to numeric
    trends_data$hits <- as.numeric(trends_data$hits)
    
    # Handle NA values
    trends_data$hits[is.na(trends_data$hits)] <- 0  # Replace with zero, but adjust as needed
    
    all_data <- append(all_data, list(trends_data))
  }
}

# Convert the list of trends data into a single dataframe
all_data_df <- bind_rows(all_data)

# Merge the world shapefile data with the Google Trends data
merged_data <- left_join(world, all_data_df, by = "geo")

# Calculate the centroid (center point) of each country to represent it as a single point on the map
coords <- st_coordinates(st_centroid(merged_data$geometry))
merged_data$longitude <- coords[, "X"]
merged_data$latitude <- coords[, "Y"]

# Some filtering
merged_data <- merged_data %>% rename(trend_date = date)
merged_data$trend_date <- as.Date(merged_data$trend_date)
merged_data <- merged_data %>% filter(!is.na(trend_date))
merged_data <- merged_data %>% filter(pop_est >= 200000)

# Add a new column for the progress bar
merged_data <- merged_data %>%
  group_by(geo) %>%
  arrange(trend_date) %>%
  mutate(progress = as.numeric(trend_date - min(trend_date)) / as.numeric(max(trend_date) - min(trend_date)))

# Can be calculated using the below code
peak_hype_date <- as.Date("2022-01-23")
# peak_hype_date <- merged_data %>%
#   group_by(trend_date) %>%
#   summarize(total_hits = sum(hits, na.rm = TRUE)) %>%
#   arrange(-total_hits) %>%
#   top_n(1, wt = total_hits) %>%
#   pull(trend_date)

# Generate the map visualization using ggplot2 and gganimate
# geom_rect is used to add a progress bar
# Also add a text when peak hype date is reached
plt <- ggplot() +
  geom_sf(data = world, fill = "#586a8a", color = "#F5F5F5", lwd = 0.1) +  # Fill countries blue and outline white
  geom_point(data = merged_data, 
             aes(longitude, latitude, size = ifelse(hits > threshold_value, hits, NA)), 
             color = "white", alpha = 0.9) +
  scale_size_continuous(range = c(1, 7)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 90), expand = FALSE) +
  labs(title = 'Search interest in "NFT" over time: {frame_time}', subtitle = "Source: Google Trends (Country level)\nData Viz by gsaluja") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#3b475b"),  # Fill plot background black
    panel.grid.major = element_blank(),  # Hide major grid lines
    panel.grid.minor = element_blank(),   # Hide minor grid lines
    plot.title = element_text(color = "white", size = 27, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 22),
    axis.title = element_text(color = "white", size = 16),
    axis.text = element_text(color = "white", size = 20)     
  ) +
  geom_rect(aes(xmin = -180, xmax = 180, 
                  ymin = -78, ymax = -75), fill = "grey50") +
  geom_rect(data = merged_data, aes(xmin = -180, xmax = -180 + (360 * progress), 
                                    ymin = -78, ymax = -75), fill = "#E63B60") +
  geom_text(data = merged_data, 
            aes(label = ifelse(trend_date >= (peak_hype_date[1] - 30) & trend_date <= (peak_hype_date[1] + 30),
                                                   "Peak Hype Time", NA),
            x = 0, y = 65),
            size = 15, vjust = -2, color = "#E63B60") +
  transition_time(trend_date)


# Create mp4 output
# ADJUST THE nframes and codec options to adjust the quality of the output.
final_animation <- animate(plt,
                           nframes = 300,
                           duration = 15,
                           width = 1920,
                           height = 1080,
                           renderer = ffmpeg_renderer(format = 'mp4', options = list(vcodec = "libx264",
                                                                                     crf = "18")))

anim_save("NFTTrend15sec.mp4", animation=final_animation)

# Create gif output
final_animation <- animate(plt, nframes = 200, duration = 15, width = 1920, height = 1080, 
                     renderer = gifski_renderer(loop = TRUE))

# Save as GIF
anim_save("NFTTrend15sec2.gif", final_animation)
