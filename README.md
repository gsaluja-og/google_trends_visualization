Animated visualizations using data from google trends

1. World map showing search interest for a keyword over time. The circle size denotes search interest in the keyword at the country level. When the "hits" value from google trends data falls below a threshold value (30 out of 100), the countries point is not plotted on the map (to denote the keyword is not "hyped" anymore in that country's search result) - Created in R using ggplot and gganimate. Data was fetched using gtrendsR (Google Trends API wrapper for R). Includes code to save it as a gif file with a progress bar.

![NFT Search Trend worldwide over time](https://github.com/gsaluja-og/google_trends_visualization/blob/main/search_keyword_visualization_world_map/NFTTrend15sec.gif?raw=true)
