# OLA Opgave 2 - Beskrivende statistik og visualisering

library(tidyverse)
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(factoextra)
library(purrr)

# Opgave 2.1 – Afleveringer
{
# Lav beskrivende statistik på afleveringer i henholdsvis den polske og hollandske liga. 
# I repræsenterer IKKE et unikt hold, men er alene ekstern analytiker ansat hos fx Wyscout. 
# Der er minimum fem elementer som det ønskes I kigger nærmere på:
# 1.	Hvordan er afleveringerne for henholdsvis top og bund 5 holdene for sæsonen 2021/2022 i den polske og hollandske liga?
# 2.	Hvordan er afleveringerne sidst i kampene i sæsonen 2021/2022, efter minut 80, sammenlignet med resten af kampen for henholdsvis den polske og hollandske liga?
# 3.	Hvordan er længde på afleveringerne, der er ”assist” sammenlignet med afleveringerne, der ikke er ”assist” for henholdsvis den polske og hollandske liga?
# 4.	Hvor en del af afleveringerne, der er ”assist” kommer fra henholdsvis forsvars-, midtbane- og angrebsspillere? (Hint: husk at præsentere jeres definition af de tre grupper)
# 5.	Hvilke 10 spillere lavede flest assist i henholdsvis den polske og hollandske liga? (Sammenlign fx med spillernes xA)

# Eredivisie  
{
# Vi laver df'er som viser ligaernes stilling i sæsonen 2021/22
{
library(dplyr)
Eredivisie_top5 <- tibble(Team = c("Ajax", "PSV", "Feyenoord", "Twente", "AZ")) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Team)

Eredivisie_bot5 <- tibble(Team = c("Sparta Rotterdam","Fortuna Sittard","Heracles","Willem II","PEC Zwolle")) %>% 
  mutate(Rank = row_number()) %>% 
  select(Rank, Team)
}

# Vi henter alle afleveringer fra Eredivisie
{
  
  # Vi connecter til MongoDB
  con_matches = mongo(
    collection = "matches",
    db = "fodbold",
    url= "mongodb://localhost"
  )
  
  con_games = mongo(
    collection = "games",
    db = "fodbold",
    url= "mongodb://localhost"
  )
  
  # Hent alle matches med competitionId og seasonId
  all_compID_SeasonID <- con_matches$find(query = '{}', fields = '{"_id": 1, "competitionId": 1, "seasonId": 1}')
  
  # Filtrer for Hollandsk liga (competitionId == "692")
  Eredivisie_ID <- all_compID_SeasonID %>% filter(competitionId == "635")
  
  # Hent matchId og seasonId
  netherlandData <- Eredivisie_ID %>% select("_id", seasonId)
  
  # Konverter matchId til liste
  netherlandId <- netherlandData$`_id`
  
  # Opret query for at hente pass-events fra mongo_games
  query2 <- toJSON(
    list(
      "matchId" = list("$in" = netherlandId),  
      "type.primary" = "pass"
    ), 
    auto_unbox = TRUE
  )
  
  # Hent kampdata med ALLE felter fra mongo_games
  test8 <- con_games$find(query = query2, fields = '{}')  # Henter alt
  
  # Flet seasonId ind i test8-data ved at matche på matchId
  Eredivisie_pass <- test8 %>%
    left_join(netherlandData, by = c("matchId" = "_id"))
  
  # Vi flattener vores df Ekstraklasa_pass
  library(jsonlite)
  
  # Hvis Ekstraklasa_pass er en liste (fra MongoDB), flatten den
  Eredivisie_pass <- as.data.frame(flatten(Eredivisie_pass))
  
}

# Stats for Top 5 hold i Eredivisie

# Afleveringspræcision
{
  # Vi definere vores top 5 hold
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Vi bruger table function til at tælle succesfulde afleveringer
  Eredivisie_top5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Top5_Hold_Eredivisie)
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_top5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)

}

# Afleveringspræcision efter 80. minut
{
  # Vi definere vores top 5 hold
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_top5_Pass_acc_80 <- Eredivisie_pass %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    filter(minute > 80) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Stats for Bottom 5 hold i Eredivisie

# Afleveringspræcision
{
  # Vi definere vores top 5 hold
  Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
  
  # Vi bruger table function til at tælle succesfulde afleveringer
  Eredivisie_bot5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie)
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_bot5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Afleveringspræcision efter 80. minut
{
  # Vi definere vores top 5 hold
  Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_bot5_Pass_acc_80 <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    filter(minute > 80) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Afleveringslængde på assist
{
# Vi starter med at finde alle rækker, som giver en assist 
library(dplyr)
library(purrr)

Eredivisie_pass_assist <- Eredivisie_pass %>%
  filter(map_lgl(type.secondary, ~ any(grepl("assist", .))))

# Nu finder vi gennemsnitslængden på en assist
Eredivisie_pass_assist_mean <- mean(Eredivisie_pass_assist$pass.length)
}

# Assistmagere på positionsniveau
{
# Opret en ny kolonne 'position_group' baseret på player.position
Eredivisie_pass_assist_pos <- Eredivisie_pass_assist %>%
  mutate(position_group = case_when(
    player.position %in% Goalkeeper ~ "Goalkeeper",
    player.position %in% Defender   ~ "Defender",
    player.position %in% Midfielder ~ "Midfielder",
    player.position %in% Attacker   ~ "Attacker",
    TRUE ~ "Other"  # Eventuelle positioner, der ikke matcher nogen af de definerede grupper
  ))

# Gruppér efter position_group og tæl antallet af assists
Eredivisie_pass_assist_pos_count <- Eredivisie_pass_assist_pos %>%
  group_by(position_group) %>%
  summarise(antal_assists = n())
}

# Assistmagere på spillerniveau
{
Eredivisie_pass_assist_player_count <- Eredivisie_pass_assist %>% 
  group_by(player.name, team.name) %>% 
  summarise(Eredivisie_pass_assist = n()) %>% 
  arrange(desc(Eredivisie_pass_assist))
}
}  
  
# Ekstraklasa
  {
    # Vi laver df'er som viser ligaernes stilling i sæsonen 2021/22
    {
      library(dplyr)
      Ekstraklasa_top5 <- tibble(Team = c("Lech Poznań", "Raków Częstochowa",
                                         "Pogoń Szczecin", "Lechia Gdańsk", "Piast Gliwice")) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, Team)
      
      # Eredivisie_bot5 <- tibble(Team = c("Stal Mielec","Śląsk Wrocław",
                                         # "Nieciecza","Cracovia Kraków","PEC Zwolle")) %>% 
        #mutate(Rank = row_number()) %>% 
        #select(Rank, Team)
    }
    
    # Vi henter alle afleveringer fra Ekstraklasa
    {
      
      # Vi connecter til MongoDB
      con_matches = mongo(
        collection = "matches",
        db = "fodbold",
        url= "mongodb://localhost"
      )
      
      con_games = mongo(
        collection = "games",
        db = "fodbold",
        url= "mongodb://localhost"
      )
      
      # Hent alle matches med competitionId og seasonId
      all_compID_SeasonID <- con_matches$find(query = '{}', fields = '{"_id": 1, "competitionId": 1, "seasonId": 1}')
      
      # Filtrer for Polish liga (competitionId == "692")
      Ekstraklasa_ID <- all_compID_SeasonID %>% filter(competitionId == "692")
      
      # Hent matchId og seasonId
      polishData <- Ekstraklasa_ID %>% select("_id", seasonId)
      
      # Konverter matchId til liste
      polishId <- polishData$`_id`
      
      # Opret query for at hente pass-events fra mongo_games
      query <- toJSON(
        list(
          "matchId" = list("$in" = polishId),  
          "type.primary" = "pass"
        ), 
        auto_unbox = TRUE
      )
      
      # Hent kampdata med ALLE felter fra mongo_games
      test5 <- con_games$find(query = query, fields = '{}')  # Henter alt
      
      # Flet seasonId ind i test-data ved at matche på matchId
      Ekstraklasa_pass <- test5 %>%
        left_join(polishData, by = c("matchId" = "_id"))
      
      # Vi flattener vores df Ekstraklasa_pass
      library(jsonlite)
      
      # Hvis Ekstraklasa_pass er en liste (fra MongoDB), flatten den
      Ekstraklasa_pass <- as.data.frame(flatten(Ekstraklasa_pass))
      
    }
    
    # Stats for Top 5 hold i Ekstraklasa
    
    # Afleveringspræcision
    {
      # Vi definere vores top 5 hold
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Vi bruger table function til at tælle succesfulde afleveringer
      Ekstraklasa_top5_Pass_acc <- Ekstraklasa_pass %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa)
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_top5_Pass_acc <- Ekstraklasa_pass %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        group_by(team.name) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # Afleveringspræcision efter 80. minut
    {
      # Vi definere vores top 5 hold
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_top5_Pass_acc_80 <- Ekstraklasa_pass %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        filter(minute > 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # Stats for Bottom 5 hold i Ekstraklasa (Er ikke lavet grundet manlende data.)
    
    # Afleveringspræcision
    {
      # Vi definerer vores bot 5 hold
      Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
      
      # Vi bruger table function til at tælle succesfulde afleveringer
      Ekstraklasa_bot5_Pass_acc <- Ekstraklasa_pass %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa)
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_bot5_Pass_acc <- Ekstraklasa_pass %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        group_by(team.name) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # Afleveringspræcision efter 80. minut
    {
      # Vi definerer vores top 5 hold
      Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_bot5_Pass_acc_80 <- Ekstraklasa_pass %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        filter(minute > 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # Afleveringslængde på assist
    {
      # Vi starter med at finde alle rækker, som giver en assist 
      library(dplyr)
      library(purrr)
      
      Ekstraklasa_pass_assist <- Ekstraklasa_pass %>%
        filter(map_lgl(type.secondary, ~ any(grepl("assist", .))))
      
      # Nu finder vi gennemsnitslængden på en assist
      Ekstraklasa_pass_assist_mean <- mean(Ekstraklasa_pass_assist$pass.length)
    }
    
    # Assistmagere på positionsniveau
    {
      # Opret en ny kolonne 'position_group' baseret på player.position
      Ekstraklasa_pass_assist_pos <- Ekstraklasa_pass_assist %>%
        mutate(position_group = case_when(
          player.position %in% Goalkeeper ~ "Goalkeeper",
          player.position %in% Defender   ~ "Defender",
          player.position %in% Midfielder ~ "Midfielder",
          player.position %in% Attacker   ~ "Attacker",
          TRUE ~ "Other"  # Eventuelle positioner, der ikke matcher nogen af de definerede grupper
        ))
      
      # Gruppér efter position_group og tæl antallet af assists
      Ekstraklasa_pass_assist_pos_count <- Ekstraklasa_pass_assist_pos %>%
        group_by(position_group) %>%
        summarise(antal_assists = n())
    }
    
    # Assistmagere på spillerniveau
    {
      Ekstraklasa_pass_assist_player_count <- Ekstraklasa_pass_assist %>% 
        group_by(player.name, team.name) %>% 
        summarise(Ekstraklasa_pass_assist = n()) %>% 
        arrange(desc(Ekstraklasa_pass_assist))
    }
  }    
}
  
# Opgave 2.2 – Skud

# Lav beskrivende statistik på skud i henholdsvis den polske og hollandske liga. 
# I repræsenterer IKKE et unikt hold, men er alene ekstern analytiker ansat hos fx Wyscout. 
# Der er minimum fem elementer som det ønskes I kigger nærmere på:
# 1.	Hvordan er skuddene for henholdsvis top og bund 5 holdene for sæsonen 2021/2022 i den polske og hollandske liga?
# 2.	Hvordan er Wyscout xG sidst i kampene i sæsonen 2021/2022, efter minut 80, sammenlignet med resten af kampen for henholdsvis den polske og hollandske liga?
# 3.	Hvordan er længde på skuddene, der er ”mål” sammenlignet med skud, der ikke er ”mål” for henholdsvis den polske og hollandske liga?
# 4.	Hvor en del af skuddene, der er ”mål” kommer fra henholdsvis forsvars-, midtbane- og angrebsspillere? (Hint: husk at præsentere jeres definition af de tre grupper)
# 5.	Hvilke 10 spillere lavede flest mål i henholdsvis den polske og hollandske liga? (Sammenlign med spillernes xG)

# Vi starter med at hente alle skud fra Eredivisie
  {
    
    # Vi connecter til MongoDB
    con_matches = mongo(
      collection = "matches",
      db = "fodbold",
      url= "mongodb://localhost"
    )
    
    con_games = mongo(
      collection = "games",
      db = "fodbold",
      url= "mongodb://localhost"
    )
    
    # Hent alle matches med competitionId og seasonId
    all_compID_SeasonID <- con_matches$find(query = '{}', fields = '{"_id": 1, "competitionId": 1, "seasonId": 1}')
    
    # Filtrer for Hollandsk liga (competitionId == "635")
    Eredivisie_ID <- all_compID_SeasonID %>% filter(competitionId == "635")
    
    # Hent matchId og seasonId
    netherlandData <- Eredivisie_ID %>% select("_id", seasonId)
    
    # Konverter matchId til liste
    netherlandId <- netherlandData$`_id`
    
    # Opret query for at hente shot-events fra mongo_games
    query3 <- toJSON(
      list(
        "matchId" = list("$in" = netherlandId),  
        "type.primary" = "shot"
      ), 
      auto_unbox = TRUE
    )
    
    # Hent kampdata med ALLE felter fra mongo_games
    Eredivisie_shot <- con_games$find(query = query3, fields = '{}')  # Henter alt
    
    # Flet seasonId ind i test8-data ved at matche på matchId
    Eredivisie_shot <- Eredivisie_shot %>%
      left_join(netherlandData, by = c("matchId" = "_id"))
    
    # Vi flattener vores df Ekstraklasa_pass
    library(jsonlite)
    
    # Hvis Ekstraklasa_pass er en liste (fra MongoDB), flatten den
    Eredivisie_shot <- as.data.frame(flatten(Eredivisie_shot))
    
  }
    
# Stats for Top 5 hold i Eredivisie

# Skudpræcision
{
  # Vi definere vores top 5 hold
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Vi bruger table function til at tælle succesfulde afleveringer
  Eredivisie_top5_Shot_acc <- Eredivisie_shot %>% 
    filter(team.name %in% Top5_Hold_Eredivisie)
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_top5_Shot_acc <- Eredivisie_shot %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    group_by(team.name) %>% 
    summarise(
      total_shots = n(),
      shot_on_target = sum(shot.onTarget, na.rm = TRUE),
      accuracy = round(shot_on_target / total_shots, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Skudpræcision efter 80. minut
{
  # Vi definere vores top 5 hold
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn skudpræcision og arranger efter ranking
  Eredivisie_top5_Shot_acc_80 <- Eredivisie_shot %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    filter(minute > 80) %>% 
    group_by(team.name) %>% 
    summarise(
      total_shots = n(),
      shot_on_target = sum(shot.onTarget, na.rm = TRUE),
      accuracy = round(shot_on_target / total_shots, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# xG før og efter 80. minut  
  
  
# Stats for Bottom 5 hold i Eredivisie

# Afleveringspræcision
{
  # Vi definere vores top 5 hold
  Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
  
  # Vi bruger table function til at tælle succesfulde afleveringer
  Eredivisie_bot5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie)
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_bot5_Pass_acc <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Afleveringspræcision efter 80. minut
{
  # Vi definere vores top 5 hold
  Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_bot5_Pass_acc_80 <- Eredivisie_pass %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    filter(minute > 80) %>% 
    group_by(team.name) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# Afleveringslængde på assist
{
  # Vi starter med at finde alle rækker, som giver en assist 
  library(dplyr)
  library(purrr)
  
  Eredivisie_pass_assist <- Eredivisie_pass %>%
    filter(map_lgl(type.secondary, ~ any(grepl("assist", .))))
  
  # Nu finder vi gennemsnitslængden på en assist
  Eredivisie_pass_assist_mean <- mean(Eredivisie_pass_assist$pass.length)
}

# Assistmagere på positionsniveau
{
  # Opret en ny kolonne 'position_group' baseret på player.position
  Eredivisie_pass_assist_pos <- Eredivisie_pass_assist %>%
    mutate(position_group = case_when(
      player.position %in% Goalkeeper ~ "Goalkeeper",
      player.position %in% Defender   ~ "Defender",
      player.position %in% Midfielder ~ "Midfielder",
      player.position %in% Attacker   ~ "Attacker",
      TRUE ~ "Other"  # Eventuelle positioner, der ikke matcher nogen af de definerede grupper
    ))
  
  # Gruppér efter position_group og tæl antallet af assists
  Eredivisie_pass_assist_pos_count <- Eredivisie_pass_assist_pos %>%
    group_by(position_group) %>%
    summarise(antal_assists = n())
}

# Assistmagere på spillerniveau
{
  Eredivisie_pass_assist_player_count <- Eredivisie_pass_assist %>% 
    group_by(player.name, team.name) %>% 
    summarise(Eredivisie_pass_assist = n()) %>% 
    arrange(desc(Eredivisie_pass_assist))
}  
















