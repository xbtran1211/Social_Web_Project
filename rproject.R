# Prompt: detailed steps to perform kmeans clustering for songs genre in a playlist based on 
# their audio features with Spotify API using R

## Initiating working environment

library(spotifyr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(GGally)

Sys.setenv(SPOTIFY_CLIENT_ID = '3a5930bcdbb744079f18ee5b827e52f5') # Spotify client ID
Sys.setenv(SPOTIFY_CLIENT_SECRET = '52b9bdfead98433c8d5169c10ed6ea1a') # Spotify client secret ID"

access_token <- get_spotify_access_token() # Get access token


# Data collection and exploration -----------------------------------------

my_playlist_id <- "5Sc2esIc6s0VsJTE5GgfDg"

audio_features <- get_playlist_audio_features(playlist_uris = "5Sc2esIc6s0VsJTE5GgfDg")

print(audio_features)

# Getting Tracks' Audio Features ------------------------------------------

# Select relevant audio features for clustering
audio_features_selected <- audio_features %>%
  select(track.name, danceability, energy, speechiness, acousticness, 
         instrumentalness, liveness, valence, tempo)

# Interpretation of the audio features:

# danceability: Measures howsuitable a track is for dancing based on tempo, rhythm, beat strength,..
# Danceability ranges from 0.0 being the least danceable and 1.0 being the most danceable.

# energy: Energetic tracks feel loud, fast, upbeat, and fun. Energy ranges from 0.0 to 1.0

# instrumentalness: Level of vocal presence in a track. For example, rap or hip-hop tracks are
# clearly not very instrumental, whereas orchestral play scores very high on the scale. The
# measurement ranges from 0.0 to 1.0

# speechiness: Measures the presence of spoken words in a track.

# liveness: Detects whether there's audience in the track. If the tracks scores high in the feature,
# there's an increased probability that the track was performed live.

# valence: Measures the positivity or emotional tone of a track.

# acousticness: Reflects how much of the track is acoustic.

# tempo: Measured in beats per minute (BPM), it is the estimated tempo of a track.

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


# Heat Map ----------------------------------------------------------------
first_50_tracks <- audio_features_normalised[1:50,]
heatmap(as.matrix(first_50_tracks),
        Colv = NA,
        Rowv = NA,
        cexRow = 0.75,
        cexCol = 0.75)

# Based on the customised layout of the heatmap above, we can inspect the audio features of 
# the first 50 tracks within the playlist. We used the color saturation of orange to indicate
# the level of each feature. For example, it seemed that the first 50 tracks all have high level
# of danceability, which is clearly obvious because most the tracks belong to pop/rock genre. 
# Furthermore, almost all of them have very low level of instrumentalness, meaning very few
# of them are free of vocals and speech.

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

table(audio_features_normalised$cluster)

audio_features_normalised_2d <- cmdscale(dist(audio_features_normalised)) # reduce to 2 dimensions

plot(audio_features_normalised_2d, col = kmeans_result$cluster,
     pch = as.numeric(kmeans_result$cluster))
legend("topright", legend = c(1,2,3,4), pch = c(1,2,3,4))

# The ggpairs() function will provide us with a plotting matrix from every possible 
# pairs of dimensions

ggpairs(audio_features_normalised, 
        aes(color = as.factor(cluster), alpha = 0.5),  #controls the transparency (opacity) of the points in the scatter plots
        columns = 1:8) # Select the columns representing your features (1:8)

# Based on the current matrix, we can determine which pair of dimension have the best separation of the
# clusters based on the selected audio features.

ggplot(audio_features_normalised, mapping = aes(x = acousticness , y = valence, color = factor(cluster))) +
  geom_point() +
  theme_minimal()


# Interpretation of the result:

# Based on the classification of music genres through their audio features, the K-Means clusters
# can be identified as below:

# KM1: The tracks that have membership with this cluster is observed to have relatively high
# acousticness and valence; therefore, they are likely to be Pop/Indie Pop.

# KM2: We find that tracks which belong to this cluster have medium to high level of valence, and most
# of them have relatively low acousticness nature. Hence, this can be Hip-hop/Melodic Rap .

# KM3: Low valence and high acousticness can be observed in tracks within this cluster. Our best
# estimation of the genre is Blues.

# KM4: The level of valence and acousticness both range from low to medium, which can be seen
# most commonly in R&B/Soul.


ggplot(audio_features_normalised, mapping = aes(x = energy , y = danceability, color = factor(cluster))) +
  geom_point() +
  theme_minimal()


