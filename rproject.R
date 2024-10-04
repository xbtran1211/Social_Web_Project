# Prompt: detailed steps to perform kmeans clustering for songs genre in a playlist based on 
# their audio features with Spotify API using R

## Initiating working environment 

Sys.setenv(SPOTIFY_CLIENT_ID = '3a5930bcdbb744079f18ee5b827e52f5') # Spotify client ID
Sys.setenv(SPOTIFY_CLIENT_SECRET = '52b9bdfead98433c8d5169c10ed6ea1a') # Spotify client secret ID"

access_token <- get_spotify_access_token() # Get access token


# Data collection and exploration -----------------------------------------

library(spotifyr)
library(dplyr)
library(FactoMineR)
library(factoextra)

my_playlist_id <- "5Sc2esIc6s0VsJTE5GgfDg"

audio_features <- get_playlist_audio_features(playlist_uris = "5Sc2esIc6s0VsJTE5GgfDg")

print(audio_features)

# Getting Tracks' Audio Features ------------------------------------------

# Select relevant audio features for clustering
audio_features_selected <- audio_features %>%
  select(track.name, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, tempo)

audio_features_selected <- na.omit(audio_features_selected)

print(audio_features_selected, n = 200)

# Normalization function: rescales data to the range [0, 1]
minmax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the numeric features
audio_features_normalised <- as.data.frame(lapply(audio_features_selected[, c("danceability", "energy", "valence","liveness","tempo", 
                                                              "instrumentalness", "acousticness", "speechiness")], minmax))

# Inspect normalized features
head(audio_features_normalised)

# K-Means Clustering ------------------------------------------------------

## Finding the optimal number of clusters (k)

# Elbow Method

set.seed(123)  # For reproducibility

# Define a range of cluster numbers
k <- 1:10

# Initialize a vector to hold the WSS values
ssw <- numeric(length(k))

# Loop through the range of k values and calculate WSS
for (i in k) {
  kmeans_result <- kmeans(audio_features_selected[,-1], centers = i, nstart = 25) #nstart = 25 
  ssw[i] <- kmeans_result$tot.withinss  # Store the WSS
}

# Plotting the result
plot(1:10, ssw, type = "b", pch = 19, 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method")

## We can see that the SSW declined steeply as the number of clusters went from 1 to 3. However, at k = 4 onward,
# The decline starts to flatten out. Therefore, 4 should be the most optimal number of clusters.


# Performing K-means Clustering with the optimal k

k <- 4 # Number of clusters

kmeans_result <- kmeans(audio_features_normalised, centers = k, nstart = 25)  # Exclude the ID column

audio_features_normalised$cluster <- kmeans_result$cluster

fviz_cluster(kmeans_result, data = audio_features_selected[,-1], 
             geom = "point", stand = FALSE, 
             ellipse.type = "convex", ggtheme = theme_minimal())

ggplot(audio_features_normalised, mapping = aes(x = energy , y = valence, color = factor(cluster))) +
  geom_point() +
  theme_minimal()

ggpairs(audio_features_normalised, 
        aes(color = as.factor(cluster), alpha = 0.5),
        columns = 1:8) # Select the columns representing your features (1:7)

