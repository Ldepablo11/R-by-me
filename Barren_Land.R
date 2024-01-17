# Setting working directoy, sourcing functions, and loading libraries.
setwd('~/Downloads')
source("Helper_Functions (1).R")
library(dplyr)
library(ggplot2)
library(cowplot)

# Using the filepath_join function to create the dataframes containing air
# quality and land use data from their file locations.
aqi <- filepath_join('~/Downloads/AQI Data')
ldu <- filepath_join('~/Downloads/Land Use', toggle_year = TRUE)

# Cleaning the ldu dataframe to make it compatible to work with the aqi
# data.
ldu_names <- c('Total Land Area', 'Open Water', 'Ice/Snow', 'Dev. Open Space', 
               'Low Int. Dev.', 'Med. Int. Dev.', 'High int. Dev.',
               'Barren Land', 'Decid. Forest', 'Ever. Forest', 'Mixed Forest',
               'Shrub', 'Grassland', 'Pasture/Hay', 'Cultivated Crops',
               'Woody Wetlands', 'Emer. Wetlands')

ldu_cleaner <- function(dataframe, name_list) {
  total <- dataframe$SE_T001_001
  col_names <- names(dataframe)[8:24]
  colnames(dataframe)[8:24] <- name_list
  col_names <- names(dataframe)[9:24]
  
  for (col in col_names) {
    dataframe[[col]] <- numeric_percent(dataframe[[col]], total)
  }
  
  colnames(dataframe)[2] <- "County"
  dataframe$County <- gsub(' County', '', dataframe$County)
  colnames(dataframe)[3] <- "State"
  dataframe$State <- sub('.+,\\s(.+)$', '\\1', dataframe$State)
  return(dataframe)
}

ldu <- ldu_cleaner(ldu, ldu_names)

# Filtering the data by specific states.
midwest_states <- c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin')

aqi <- list_filter(aqi, 'State', midwest_states)
ldu <- list_filter(ldu, 'State', midwest_states)

# Creating scatter plots of for AQI across the years.
plot_list <- list()
for (state in midwest_states) {
  plot <- mean_scatter(aqi, state, aqi$Median.AQI, main_lab = state)
  plot_list <- append(plot_list, list(plot))
}
combined_plots <- plot_grid(plotlist = plot_list)

combined_plots +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.margin = margin(t = 15)) +
  ggtitle('The average countey-level median AQI in different US States between 
          1993 and 2021.')

# Creating bar plot for land cover in Illinois across the years.
mean_bar(ldu, 'Illinois', 2011)
