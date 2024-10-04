## Initiating working environment 

Sys.setenv(SPOTIFY_CLIENT_ID = '3a5930bcdbb744079f18ee5b827e52f5') # Spotify client ID
Sys.setenv(SPOTIFY_CLIENT_SECRET = '52b9bdfead98433c8d5169c10ed6ea1a') # Spotify client secret ID"

access_token <- get_spotify_access_token() # Get access token


# Data collection and exploration -----------------------------------------

my_playlist_id <- "5Sc2esIc6s0VsJTE5GgfDg"

my_playlist_tracks <- get_playlist_tracks("5Sc2esIc6s0VsJTE5GgfDg")

head(my_playlist_tracks)

tracks_id <- my_playlist_tracks$track.id

audio_features <- get_playlist_audio_features(playlist_uris = '5Sc2esIc6s0VsJTE5GgfDg')

head(audio_features)


# Getting Tracks' Audio Features ------------------------------------------

audio_features <- audio_features %>%
  select(track.name, danceability, energy, loudness, tempo, valence, acousticness, instrumentalness)


# Dropping tracks that are locally imported by user because they are not recognised by R 
audio_features <- na.omit(audio_features)

# Print the clean version of the playlist
print(audio_features)

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
  kmeans_result <- kmeans(scaled_audio_features, centers = i, nstart = 25) #nstart = 25 
  ssw[i] <- kmeans_result$tot.withinss  # Store the WSS
}

# Plotting the result
plot(1:10, wss, type = "b", pch = 19, 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method")

## We can see that the SSW declined steeply as the number of clusters went from 1 to 4. However, at k = 5 onward,
# The decline starts to flatten out. Therefore, 5 should be the most optimal number of clusters.


# Normalising the data
selected_features <- audio_features %>%
  select(2,3,4,5,6,7,8)

normalised_features <- selected_features %>%
  mutate_if(is.numeric, function(x) (x - min(x)) / (max(x) - min(x)))

# Performing K-means Clustering with the optimal k
k <- 5  # Number of clusters
kmeans_result <- kmeans(normalised_features, centers = k, nstart = 25)  # Exclude the ID column and the data is not scaled
normalised_features$cluster <- kmeans_result$cluster

pca_result <- PCA(normalised_features, graph = FALSE)
df_pca <- data.frame(PCA = pca_result$ind, cluster = kmeans_result$cluster)

df_pca

# Plot clusters
ggplot(df_pca, aes(x = PCA.contrib.Dim.1, y = PCA.contrib.Dim.5, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.75) +
  labs(title = "Clustering Result", x = "PC1", y = "PC2")

scatterplot3d(normalised_features$danceability, normalised_features$energy,
              color = kmeans_result$cluster, pch = 16, angle = 60, scale.y = 0.7,
              main = "K-Means Clustering in 3D",
              xlab = "Danceability", ylab = "Energy")

# Add cluster centers
scatterplot3d(km$centers[,1], km$centers[,2], km$centers[,3],
              color = "red", pch = 18, scale.y = 0.7, add = TRUE)