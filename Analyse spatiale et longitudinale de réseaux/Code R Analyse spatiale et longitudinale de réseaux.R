library(dplyr)
library(readr)
library(ggplot2)

nodegeo <- read.csv('caen_nodegeo.csv', sep = ';')

#Ex1

# Extract wave and id information
nodegeo <- nodegeo %>%
  mutate(
    wave = stringr::str_extract(Idw, "[A-Z]"),
    ego_id = as.numeric(stringr::str_extract(Idw, "[0-9]+"))
  )

# Count the number of distinct waves for each individual
individual_wave_counts <- nodegeo %>%
  group_by(ego_id) %>%
  summarize(n_wave = n_distinct(wave), .groups = 'drop')

# Filter for individuals who responded to at least 3 waves
trivague_individuals <- individual_wave_counts %>%
  filter(n_wave >= 3) %>%
  select(ego_id)

# Filter the main dataframe to include only these individuals
trivague_nodegeo <- nodegeo %>%
  semi_join(trivague_individuals, by = "ego_id")


# Calculate network metrics
network_metrics <- trivague_nodegeo %>%
  group_by(ego_id, wave) %>%
  summarize(
    n_places = n_distinct(urban_area),
    n_links = sum(nb_strong),
    .groups = 'drop'
  )

# Calculate network density
network_metrics <- network_metrics %>%
  mutate(
    density = ifelse(n_places > 1, 
                     2 * n_links / (n_places * (n_places - 1)), 
                     0)
  )

# Calculate mean and variance for each wave
wave_metrics <- network_metrics %>%
  group_by(wave) %>%
  summarize(
    mean_n_places = mean(n_places),
    var_n_places = var(n_places),
    mean_n_links = mean(n_links),
    var_n_links = var(n_links),
    mean_density = mean(density),
    var_density = var(density)
  )

print(wave_metrics)


# Plotting with explicit group aesthetic
ggplot(wave_metrics, aes(x = wave, group = 1)) +  # Add 'group = 1' to ensure that all data is considered as one group for line plotting
  geom_line(aes(y = mean_n_places), color = 'blue') +
  geom_ribbon(aes(ymin = mean_n_places - sqrt(var_n_places), ymax = mean_n_places + sqrt(var_n_places)), alpha = 0.2, fill = 'blue') +
  ggtitle('Evolution of Number of Places') +
  ylab('Number of Places') +
  xlab('Wave') +
  theme_minimal()

# Repeat the same for other plots
ggplot(wave_metrics, aes(x = wave, group = 1)) +
  geom_line(aes(y = mean_n_links), color = 'green') +
  geom_ribbon(aes(ymin = mean_n_links - sqrt(var_n_links), ymax = mean_n_links + sqrt(var_n_links)), alpha = 0.2, fill = 'green') +
  ggtitle('Evolution of Number of Links') +
  ylab('Number of Links') +
  xlab('Wave') +
  theme_minimal()

ggplot(wave_metrics, aes(x = wave, group = 1)) +
  geom_line(aes(y = mean_density), color = 'red') +
  geom_ribbon(aes(ymin = mean_density - sqrt(var_density), ymax = mean_density + sqrt(var_density)), alpha = 0.2, fill = 'red') +
  ggtitle('Evolution of Network Density') +
  ylab('Density') +
  xlab('Wave') +
  theme_minimal()



#Ex2 

library(dplyr)
library(tidyr)
library(readr)

# Load the data
nodegeo <- read.csv('caen_nodegeo.csv', sep = ';')


# Extract wave and ego_id information
nodegeo <- nodegeo %>%
  mutate(
    wave = as.character(sub(".*([A-Z]).*", "\\1", Idw)),
    ego_id = as.numeric(sub(".*?(\\d+).*", "\\1", Idw))  # Revised pattern for ego_id
  )

# Count the number of distinct waves for each individual
individual_wave_counts <- nodegeo %>% 
  group_by(ego_id) %>%
  summarise(n_wave = n_distinct(wave)) %>%
  filter(n_wave >= 3) %>%
  select(ego_id)

# Filter the main dataframe to include only these individuals
trivague_nodegeo <- nodegeo %>% 
  filter(ego_id %in% individual_wave_counts$ego_id)

# Calculate the number of places per wave and ego_id
places_per_wave <- trivague_nodegeo %>%
  group_by(ego_id, wave) %>%
  summarise(n_places = n_distinct(urban_area), .groups = 'drop')

# Reshape the data to have one row per ego_id and columns for each wave's number of places
pivot_places <- places_per_wave %>%
  pivot_wider(names_from = wave, values_from = n_places)

# Define the categorization function
categorize <- function(row) {
  row <- na.omit(row) # Remove NA values
  if(length(row) < 2) return("Insufficient data") # Check if there are at least two data points
  
  if(all(diff(row) == 0, na.rm = TRUE)) {
    return('Stable')
  } else if(all(diff(row) >= 0, na.rm = TRUE) && row[length(row)] > row[1]) {
    return('Increasing')
  } else if(all(diff(row) <= 0, na.rm = TRUE) && row[length(row)] < row[1]) {
    return('Decreasing')
  } else if(any(diff(row) != 0, na.rm = TRUE)) {
    if(row[length(row)] > row[1]) {
      return('Fluctuating Positive')
    } else if(row[length(row)] < row[1]) {
      return('Fluctuating Negative')
    } else if(row[length(row)] == row[1]) {
      return('Fluctuating Neutral')
    }
  } else {
    return('Undefined')
  }
}

# Apply the categorization to each row
pivot_places$category <- apply(pivot_places[, -1], 1, categorize)

# Count the number of egos in each category
category_counts <- pivot_places %>%
  count(category)

print(category_counts)


# Use pivot_wider to create a column for each wave with the number of locations (nb de lieux)
pivot_places <- places_per_wave %>%
  pivot_wider(names_from = wave, values_from = n_places)

# Apply categorization feature for each individual
pivot_places$category <- apply(pivot_places[, -1], 1, categorize)

# Separate individuals by category and list the identifiers (IDs) for each category
individuals_by_category <- pivot_places %>%
  group_by(category) %>%
  summarize(ids = toString(ego_id)) # Utilisez toString pour convertir les listes en chaînes de caractères

# Show IDs for each category
print(individuals_by_category)