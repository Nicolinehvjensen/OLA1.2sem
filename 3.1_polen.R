##### OLA 1 #####

### Opgave 3 – Clustering ###

library(mongolite)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggplot2)

  #connect til mongodb
cong=mongo(
  collection = "games",
  db = "soccer",
  url= "mongodb://localhost"
)

conm=mongo(
  collection = "matches",
  db = "soccer",
  url= "mongodb://localhost"
)

conp=mongo(
  collection = "players",
  db = "soccer",
  url= "mongodb://localhost"
)

  ### Opgave 3.1 – Afleveringer og spillere ###

  #indhent Id for polen og holland
allMatches <- conm$find(query = '{}', fields = '{}')
allPolish <- allMatches %>% filter(competitionId == "692")
allDutch <- allMatches %>% filter(competitionId == "635")
polishId <- allPolish$`_id`
dutchId <- allDutch$`_id`

# indhent data for polen
queryP <- jsonlite::toJSON(
  list(
    "matchId" = list("$in" = polishId),
    "type.primary" = list("$in" = c("pass", "shot"))
  ), 
  auto_unbox = TRUE
)

#shot_pass_polish <- cong$find(query = queryP, fields = '{"_id": 0}')
#save(shot_pass_polish, file = "shot_pass_polish.RData")
#load("shot_pass_polish.RData")
f_shot_pass_polish <- jsonlite::flatten(shot_pass_polish)

# indhent data for holland
queryD <- jsonlite::toJSON(
  list(
    "matchId" = list("$in" = dutchId),
    "type.primary" = list("$in" = c("pass", "shot"))
  ), 
  auto_unbox = TRUE
)

#shot_pass_dutch <- cong$find(query = queryD, fields = '{"_id": 0}')
#save(shot_pass_dutch, file = "shot_pass_dutch.RData") 
#load("shot_pass_dutch.RData")
f_shot_pass_dutch <- jsonlite::flatten(shot_pass_dutch)

  # metode 1: gns. pr spiller 

pass_data <- data.frame(cbind(f_shot_pass_polish$location.x, f_shot_pass_polish$location.y, f_shot_pass_polish$pass.angle, f_shot_pass_polish$pass.length, f_shot_pass_polish$pass.endLocation.x, f_shot_pass_polish$pass.endLocation.y))
# tilføj player.id
player.id <- as.numeric(f_shot_pass_polish$player.id)
player.name <- f_shot_pass_polish$player.name
player.position <- f_shot_pass_polish$player.position
pass_data <- data.frame(cbind(pass_data, player.id, player.name, player.position))

summary(pass_data)
nrow(pass_data)
pass_data <- na.omit(pass_data)

#kmeans
set.seed(123)
k <- 4
kmeans_result_pass_polish <- kmeans(pass_data[1:6], centers = k, nstart = 20)
pass_data1 <- data.frame(cbind(pass_data, kmeans_result_pass_polish$cluster))

# Beregn WSS for forskellige værdier af k
wss <- sapply(1:7, function(k) kmeans(pass_data, centers = k, nstart = 20)$tot.withinss)

# Plot Elbow-metoden
plot(1:7, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Antal Clusters (k)", ylab = "WSS (Within Sum of Squares)",
     main = "Elbow-metoden for optimal k")

colnames(pass_data1) <- c("location.x", "location.y", "pass.angle", "pass.length", "pass.endLocation.x", "pass.endLocation.y", "player.id", "player.name", "player.position", "clusters")
class(pass_data1$clusters)
summary(pass_data1)

pass_data2 <- pass_data1 %>% 
  group_by(clusters) %>% 
  summarise(
    location.x = mean(location.x),
    location.y = mean(location.y),
    pass.angle = mean(pass.angle),
    pass.length = mean(pass.length),
    pass.endLocation.x = mean(pass.endLocation.x),
    pass.endLocation.y = mean(pass.endLocation.y)
  )

pass_data3 <- as.data.frame(pass_data2[c(1,4,5)])
colnames(pass_data3) <- c("clusters", "pass.angle", "pass.length")
#klargør til plot
long_data <- pass_data3 %>% 
  pivot_longer(cols = c(pass.angle, pass.length), names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = clusters, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ variable, scales = "free_y", 
             labeller = labeller(variable = c(
               pass.angle = "Afleveringsvinkel i %",
               pass.length = "Afleveringslængde i meter",
             ))) +
  labs(x = "Cluster", y = "Værdi", title = "Afleveringsvinkel har mest variation mellem klyngerne") +
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
cluster1 <- filter_cluster(pass_data1, 1)
cluster2 <- filter_cluster(pass_data1, 2)
cluster3 <- filter_cluster(pass_data1, 3)
cluster4 <- filter_cluster(pass_data1, 4)


player_positions <- pass_data1 %>%
  mutate(player_category = case_when(
    # Goalkeeper
    player.position %in% c("GK") ~ "Goalkeeper",
    
    # Defense
    player.position %in% c("CB", "LCB", "RCB", "LCB3", "RCB3", "LB", "RB", "LB5", "RB5", "LWB", "RWB") ~ "Defense",
    
    # Midfield
    player.position %in% c("DMF", "LDMF", "RDMF", "AMF", "RAMF", "LAMF", "LCMF", "RCMF", "LCMF3", "RCMF3") ~ "Midfield",
    
    # Attack
    player.position %in% c("CF", "SS", "LW", "RW", "LWF", "RWF") ~ "Attack",
    
    TRUE ~ "Other"  # ukendte positioner
  ))

table_positions_c2 <- data.frame(table(cluster2$player.position))
table_positions_c3 <- data.frame(table(cluster3$player.position))
table_positions_c4 <- data.frame(table(cluster4$player.position))

position_counts <- player_positions %>%
  group_by(clusters) %>% 
  count(player_category) %>%
  arrange(desc(n)) %>% 
  pivot_wider(names_from = clusters, values_from = n)

plot_data_positions <- player_positions %>%
  group_by(clusters) %>% 
  count(player_category) %>%
  arrange(desc(n))

ggplot(plot_data_positions, aes(x = player_category, y = n, fill = as.factor(clusters))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  labs(x = "Spillerkategori", y = "Antal spillere", fill = "Clusters",
       title = "Fordeling af spillerkategorier på tværs af klynger") +
  scale_fill_brewer(palette = "Greens") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

calculate_cluster_percentage <- function(data) {
  total_count <- nrow(data %>% filter(player.id != 0))  # Total antal spillere (uden player.id = 0)
  
  data %>%
    filter(player.id != 0) %>%  # Fjern player.id == 0
    group_by(player.id, player.name) %>%
    summarise(count = n(), .groups = "drop") %>%  # Tæl antallet per player.id
    mutate(percentage = count / total_count * 100)  # Beregn procentandel af totalen
}

cluster_percentage1 <- calculate_cluster_percentage(cluster1)
cluster_percentage2 <- calculate_cluster_percentage(cluster2)
cluster_percentage3 <- calculate_cluster_percentage(cluster3)
cluster_percentage4 <- calculate_cluster_percentage(cluster4)

# Funktion til at generere top 10 spillere plot for et cluster
generate_cluster_plot <- function(cluster_data, cluster_name) {
  # Filtrer top 10 spillere baseret på procentandelen
  top_5_players <- cluster_data %>%
    arrange(desc(percentage)) %>%
    slice_head(n = 5)
  
  # Opret ggplot 
  plot <- ggplot(top_5_players, aes(x = reorder(player.name, percentage), y = percentage, fill = cluster_name)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(x = "", y = "Procentandel", fill = cluster_name, 
         title = paste("Top 5 spillere i", cluster_name)) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "none")
  
  return(plot)
}

# Generer plot for hver cluster
plot1 <- generate_cluster_plot(cluster_percentage1, "Cluster 1")
plot2 <- generate_cluster_plot(cluster_percentage2, "Cluster 2")
plot3 <- generate_cluster_plot(cluster_percentage3, "Cluster 3")
plot4 <- generate_cluster_plot(cluster_percentage4, "Cluster 4")

# Vis plottene
print(plot1)
print(plot2)
print(plot3)
print(plot4)

library(patchwork)
plot1 | plot4



pass_player_percent <- pass_data1 %>%
  filter(player.id != 0) %>%  # Fjern spiller.id == 0
  group_by(player.id, player.name, clusters) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(player.id) %>% 
  mutate(total_count = sum(count)) %>%  
  ungroup() %>%  # Fjern gruppering for at arbejde med hele datasættet
  mutate(percentage = (count / total_count) * 100) %>%  # Beregn procentandel per spiller i hvert cluster
  select(player.id, player.name, clusters, percentage) %>% 
  pivot_wider(names_from = clusters, values_from = percentage) %>%  # Omform data til bredt format
  mutate(total_percentage = rowSums(select(., -player.id, -player.name), na.rm = TRUE)) 

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
    mean_x = mean(location.x, na.rm = TRUE),
    mean_y = mean(location.y, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(combined_data, aes(x = location.x, y = location.y)) +
  geom_point(color = "blue", size = 2) +  
  labs(x = "Location X", y = "Location Y", title = "Cluster 2 + 3 viser afleveringsmønstre") +
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

#hej



