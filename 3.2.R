library(dplyr)
library(tidyr)
library(ggplot2)

  ### Opgave 3.2 – Afleveringer, skud og spillere ###

  # Lav en Clustering model af spillere fra den polske liga baseret på deres 
  # afleveringer og skud. (Hint: én spiller kan have flere end én aflevering 
  # og mere end ét skud i data. I skal sørge for spilleren kun er én gang
  # i data – fx tag gennemsnittet af længden på én spillers afleveringer)

shotPass <- f_shot_pass_polish %>% 
  filter(player.position != "GK") %>%  # Fjerner målmændene
  group_by(player.id, player.name) %>% 
  filter(n() > 100) %>%
  summarise(
    avg_pass_angle = round(mean(pass.angle, na.rm = TRUE)),
    avg_pass_length = round(mean(pass.length, na.rm = TRUE), digits = 1),
    avg_x_location = round(mean(location.x, na.rm = TRUE)),
    avg_y_location = round(mean(location.y, na.rm = TRUE)),
    avg_end_x_location = round(mean(pass.endLocation.x, na.rm = TRUE)),
    avg_end_y_location = round(mean(pass.endLocation.y, na.rm = TRUE)),
    avg_shotXg = round(mean(shot.xg, na.rm = TRUE), digits = 2)
  ) %>% 
  filter(!is.na(player.name)) %>% 
  filter(player.id != 0)

summary(shotPass)
nrow(shotPass)

# Opret nyt dataframe med standardafvigelser
shotPass_sd <- f_shot_pass_polish %>%
  filter(player.position != "GK") %>%  # Fjerner målmændene
  group_by(player.id, player.name) %>% 
  filter(n() > 100) %>%
  summarise(
    sd_pass_angle = round(sd(pass.angle, na.rm = TRUE), digits = 2),
    sd_pass_length = round(sd(pass.length, na.rm = TRUE), digits = 2),
    sd_x_location = round(sd(location.x, na.rm = TRUE), digits = 2),
    sd_y_location = round(sd(location.y, na.rm = TRUE), digits = 2),
    sd_end_x_location = round(sd(pass.endLocation.x, na.rm = TRUE), digits = 2),
    sd_end_y_location = round(sd(pass.endLocation.y, na.rm = TRUE), digits = 2),
    sd_shotXg = ifelse(is.na(sd(shot.xg, na.rm = TRUE)), 0, round(sd(shot.xg, na.rm = TRUE), digits = 2)),
  ) %>%
  filter(!is.na(player.name))

summary(shotPass_sd)

min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Anvend skalering på standardafvigelser
shotPass_sd_norm <- shotPass_sd[3:9] %>%
  mutate(
    sd_pass_angle = min_max_norm(sd_pass_angle),
    sd_pass_length = min_max_norm(sd_pass_length),
    sd_x_location = min_max_norm(sd_x_location),
    sd_y_location = min_max_norm(sd_y_location),
    sd_end_x_location = min_max_norm(sd_end_x_location),
    sd_end_y_location = min_max_norm(sd_end_y_location),
    sd_shotXg = min_max_norm(sd_shotXg)
  )

summary(shotPass_sd_norm)

# Beregn standardafvigelsen for hver numerisk kolonne i datasættet
sd_values <- sapply(shotPass_sd_norm, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)

# Fjern NA-værdier (hvis der er kolonner, der ikke er numeriske)
sd_values <- sd_values[!is.na(sd_values)]

# Sorter standardafvigelserne i faldende rækkefølge
sorted_sd_values <- sort(sd_values, decreasing = TRUE)

# Vis de top 10 kolonner med højeste standardafvigelse
head(sorted_sd_values, 10)

scale_data <- data.frame(scale(shotPass_sd_norm))

k <- 4
kmeans_result_shotPass <- kmeans(scale_data, centers = k, nstart = 20)
shotPass <- cbind(shotPass, kmeans_result_shotPass$cluster)
colnames(shotPass) <- c("player.id","player.name","avg_pass_angle","avg_pass_length","avg_x_location","avg_y_location","avg_end_x_location","avg_end_y_location","avg_shotXg","clusters" )
summary(shotPass)
shotPass <- shotPass %>% drop_na()

# Beregn WSS for forskellige værdier af k
wss <- sapply(1:7, function(k) kmeans(scale_data, centers = k, nstart = 20)$tot.withinss)

# Plot Elbow-metoden
plot(1:7, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Antal Clusters (k)", ylab = "WSS (Within Sum of Squares)",
     main = "Elbow-metoden for optimal k")

shotPass_data <- shotPass %>% 
  group_by(clusters) %>% 
  summarise(
    location.x = mean(avg_x_location),
    location.y = mean(avg_y_location),
    pass.angle = mean(avg_pass_angle),
    pass.length = mean(avg_pass_length),
    pass.endLocation.x = mean(avg_end_x_location),
    pass.endLocation.y = mean(avg_end_y_location),
    shotXg = mean(avg_shotXg)
  )

shotPass_data1 <- as.data.frame(shotPass_data[c(1,4,5,8)])
colnames(shotPass_data1) <- c("clusters", "pass.angle", "pass.length", "shotXg")
#klargør til plot
long_data <- shotPass_data1 %>% 
  pivot_longer(cols = c(pass.angle, pass.length, shotXg), names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = clusters, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ variable, scales = "free_y", 
             labeller = labeller(variable = c(
               pass.angle = "Afleveringsvinkel, %",
               pass.length = "Afleveringslængde, meter",
               shotXg = "Expected Goals (xG), sandsynlighed"
             ))) +
  labs(x = "Cluster", y = "Værdi", title = "Cluster 1 har den højeste xG") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 14))

#opret datasæt for hvert cluster
filter_cluster <- function(data, cluster_number) {
  data %>%
    filter(clusters == cluster_number)
}

# Brug funktionen til at oprette datasæt for hvert cluster
cluster1 <- filter_cluster(shotPass, 1)
cluster2 <- filter_cluster(shotPass, 2)
cluster3 <- filter_cluster(shotPass, 3)
cluster4 <- filter_cluster(shotPass, 4)

# lokation på banen
# Saml dataene til et samlet datasæt med en kolonne for 'clusters'
combined_data <- rbind(
  mutate(cluster1, clusters = "Cluster 1"),
  mutate(cluster2, clusters = "Cluster 2"),
  mutate(cluster3, clusters = "Cluster 3"),
  mutate(cluster4, clusters = "Cluster 4")
)

mean_positions <- combined_data %>%
  group_by(clusters) %>%
  summarise(
    mean_x = mean(avg_x_location, na.rm = TRUE),
    mean_y = mean(avg_y_location, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(combined_data, aes(x = avg_x_location, y = avg_end_y_location)) +
  geom_point(color = "blue", size = 2) +  
  labs(x = "Location X", y = "Location Y", title = "") +
  xlim(0, 100) +  # Sæt x-aksen fra 0 til 100
  ylim(100, 0) +  # Sæt y-aksen fra 0 til 100
  theme_minimal() +  # Minimalistisk tema
  theme(
    axis.text = element_text(size = 12, face = "bold"),  # Ændr skrifttype for aksetekst
    axis.title = element_text(size = 14, face = "bold"),  # Ændr skrifttype for aksetitler
    strip.text = element_text(size = 14, face = "bold")  # Skift skrifttype for facet labels
  ) +
  # Tilføj en grøn fodboldbane
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, alpha = 0.1, fill = "green") +  
  # Tilføj midterlinje
  geom_vline(xintercept = 50, color = "white", size = 1) +
  geom_hline(yintercept = 50, color = "white", size = 1) +
  # Tilføj målområder (målmål)
  annotate("rect", xmin = 0, xmax = 5, ymin = 30, ymax = 70, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = 95, xmax = 100, ymin = 30, ymax = 70, alpha = 0.2, fill = "red") +
  # Tilføj gennemsnitspunkterne med en anden farve (f.eks. rød)
  geom_point(data = mean_positions, aes(x = mean_x, y = mean_y), color = "red", size = 6, shape = 19) + 
  facet_wrap(~ clusters)  # Del plottene op efter cluster

  #team.name 
team_name <- f_shot_pass_polish %>% 
  distinct(player.id, .keep_all = TRUE) %>%  # Fjern duplikater, beholde første observation per player.id
  select(player.id, team.name)

cluster1_with_team <- cluster1 %>%
  left_join(team_name %>% select(player.id, team.name), by = "player.id")

cluster2_with_team <- cluster2 %>%
  left_join(team_name %>% select(player.id, team.name), by = "player.id")

cluster3_with_team <- cluster3 %>%
  left_join(team_name %>% select(player.id, team.name), by = "player.id")

cluster4_with_team <- cluster4 %>%
  left_join(team_name %>% select(player.id, team.name), by = "player.id")

table_cluster1 <- data.frame(table(cluster1_with_team$team.name))
table_cluster2 <- data.frame(table(cluster2_with_team$team.name))
