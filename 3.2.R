### Opgave 3 – Clustering ###

# Opgave 3.2 – Afleveringer, skud og spillere

library(dplyr)
library(caret)
library(plotly)

c <- cong$count('{ "type.primary": { "$in": ["pass", "shot"] } }')
s <- 20000

all_data2 <- list()

i <- 0
while (i < c) {
  print(paste("Henter data fra række:", i, "til", min(i + s, c)))
  
  # Hent en batch af data
  batch <- cong$find('{ "type.primary": { "$in": ["pass", "shot"] } }', limit = s, skip = i)
  
  # Konverter batch til JSON og flad det ud
  if (length(batch) > 0) {
    batch_flat <- fromJSON(toJSON(batch), flatten = TRUE)
    all_data2 <- append(all_data2, list(batch_flat))
  }
  
  # Opdater `i` for at hente næste batch
  i <- i + s
  
  # Kort pause for at undgå overload
  Sys.sleep(2) 
}

shot_pass <- bind_rows(all_data2)
colnames(shot_pass)

unique_player_shot_pass <- shot_pass %>% 
  group_by(player.name) %>% 
  summarise(
    total_passes = sum(type.primary == "pass"),
    avg_pass_angle = round(mean(pass.angle[type.primary == "pass"], na.rm = TRUE)),
    avg_pass_length = round(mean(pass.length[type.primary == "pass"], na.rm = TRUE), digits = 1),
    pass_accuracy = round(sum(pass.accurate[type.primary == "pass"], na.rm = TRUE) / total_passes, digits = 2),
    total_shots = sum(type.primary == "shot"),
    avg_shot_xg = round(mean(shot.xg[type.primary == "shot"], na.rm = TRUE), digits = 2)
  )

scaled_shot_pass <- as.data.frame(scale(select(unique_player_shot_pass, -player.name)))
scaled_shot_pass[is.na(scaled_shot_pass)] <- 0

shotpassCor <- cor(scaled_shot_pass)
corrplot::corrplot(shotpassCor, addCoef.col = "black",method = "square",type = "lower")



k <- 8
kmeans_result <- kmeans(scaled_shot_pass, centers = k, nstart = 20)
kmeans_result$tot.withinss
scaled_shot_pass$cluster <- sample(1:k, nrow(scaled_shot_pass), replace = TRUE)

ggplot(scaled_shot_pass, aes(x=avg_pass_angle, y=avg_shot_xg,colour = as.factor(cluster)))+
  geom_point()

isChanged <- TRUE
roundcounter <- 0

while (isChanged) {
  cat("Iteration:", roundcounter + 1, "\n")
  changecounter <- 0
  roundcounter <- roundcounter + 1
  
  # 2. Compute new centroids
  centroids <- scaled_shot_pass %>%
    group_by(cluster) %>%
    summarise(X = mean(avg_shot_xg), Y = mean(avg_pass_length), Z = mean(pass_accuracy)) %>%
    ungroup()
  
  # 3D Plot with Plotly
  p <- plot_ly() %>%
    add_trace(data = scaled_shot_pass, x = ~avg_shot_xg, y = ~avg_pass_length, z = ~pass_accuracy, 
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster),
              marker = list(size = 5)) %>%
    add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z, 
              type = "scatter3d", mode = "markers",
              marker = list(size = 10, symbol = "diamond", color = "black"),
              name = "Centroids") %>%
    
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "avg_shot_xg"),
                        yaxis = list(title = "avg_pass_length"),
                        zaxis = list(title = "pass_accuracy")))
  
  print(p)
  Sys.sleep(2)  # Pause for visualization
  
  # 3. Assign each observation to the closest centroid
  for (i in 1:nrow(scaled_shot_pass)) {
    distances <- sapply(1:k, function(j) { sqrt((centroids$X[j] - scaled_shot_pass$avg_shot_xg[i])^2 + (centroids$Y[j] - scaled_shot_pass$avg_pass_length[i])^2 + (centroids$Z[j] - scaled_shot_pass$pass_accuracy[i])^2) })
    new_cluster <- which.min(distances)
    
    if (scaled_shot_pass$cluster[i] != new_cluster) {
      scaled_shot_pass$cluster[i] <- new_cluster
      changecounter <- changecounter + 1
    }
  }
  
  # Check if clusters have changed
  isChanged <- changecounter > 0
  cat("Changes:", changecounter, "\n")
}

cat("Clustering complete!\n")

scaled_shot_pass$player.name <- unique_player_shot_pass$player.name
p <- plot_ly() %>%
  add_trace(data = scaled_shot_pass, 
            x = ~avg_shot_xg, 
            y = ~avg_pass_length, 
            z = ~pass_accuracy, 
            type = "scatter3d", 
            mode = "markers",
            color = ~as.factor(cluster),
            text = ~paste("Name:", unique_player_shot_pass$player.name, "<br>",
                          "AverageShotXG:", unique_player_shot_pass$avg_shot_xg, "<br>",
                          "AveragePassLength:", unique_player_shot_pass$avg_pass_length, "<br>",
                          "PassAccuracy:", unique_player_shot_pass$pass_accuracy),
            hoverinfo = "text",
            marker = list(size = 5)) %>%
  layout(title = paste("Iteration:", roundcounter),
         scene = list(xaxis = list(title = "avg_shot_xg"),
                      yaxis = list(title = "avg_pass_length"),
                      zaxis = list(title = "pass_accuracy")))

p


# Beregn WSS for forskellige værdier af k
wss <- sapply(1:15, function(k) kmeans(scaled_shot_pass[1:6], centers = k, nstart = 10)$tot.withinss)

# Plot Elbow-metoden
plot(1:15, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Antal Clusters (k)", ylab = "WSS (Within Sum of Squares)",
     main = "Elbow-metoden for optimal k")






