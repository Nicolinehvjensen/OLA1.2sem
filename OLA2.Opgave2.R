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

# Afleveringspræcision hver 10. minut
  {
    # Load nødvendige pakker
    library(ggplot2)
    library(dplyr)
    
    # Tilføj en ny variabel for 10-minutters intervaller
    Eredivisie_pass_10 <- Eredivisie_pass %>%
      mutate(minute_group = ifelse(minute >= 90, "90+", floor(minute / 10) * 10))
    
    # Filtrer pass-data for kun at inkludere de top 5 hold og beregn afleveringspræcisionen samlet for alle hold per 10 min
    Eredivisie_Pass_top5_acc_by_10min <- Eredivisie_pass_10 %>% 
      filter(team.name %in% Top5_Hold_Eredivisie) %>% 
      group_by(minute_group) %>% 
      summarise(
        total_passes = n(),
        successful_passes = sum(pass.accurate, na.rm = TRUE),
        accuracy = round(successful_passes / total_passes, 2)
      ) %>%
      arrange(minute_group)
    
    # Sørg for, at minute_group er en faktor med korrekt sortering
    Eredivisie_Pass_top5_acc_by_10min$minute_group <- factor(
      Eredivisie_Pass_top5_acc_by_10min$minute_group, 
      levels = c(seq(0, 80, by = 10), "90+")
    )
    
    # Vis resultatet
    print(Eredivisie_Pass_top5_acc_by_10min)
    
    # Opret barplot med ggplot2
    ggplot(Eredivisie_Pass_top5_acc_by_10min, aes(x = minute_group)) +
      geom_bar(aes(y = total_passes, fill = "Antal afleveringer"), stat = "identity", color = "black") +
      geom_line(aes(y = accuracy * max(total_passes), group = 1, color = "Afleveringspræcision"), size = 1.2) +
      geom_point(aes(y = accuracy * max(total_passes), color = "Afleveringspræcision"), size = 3) +
      scale_y_continuous(
        name = "Antal afleveringer",
        sec.axis = sec_axis(~ . / max(Eredivisie_Pass_top5_acc_by_10min$total_passes), name = "Afleveringspræcision (%)", labels = scales::percent)
      ) +
      scale_fill_manual(values = c("Antal afleveringer" = "steelblue")) +
      scale_color_manual(values = c("Afleveringspræcision" = "red")) +
      labs(
        title = "Afleveringspræcision og antal afleveringer per 10 minutter",
        x = "Minutgruppe",
        fill = "",
        color = ""
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    
  
  
  
}
  
# Stats for Bottom 5 hold i Eredivisie

# Afleveringspræcision
{
  # Vi definere vores bot 5 hold
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

# Afleveringspræcision pr. 10 minut
{
  # Load nødvendige pakker
  library(ggplot2)
  library(dplyr)
  
  # Tilføj en ny variabel for 10-minutters intervaller
  Eredivisie_pass_10 <- Eredivisie_pass %>%
    mutate(minute_group = ifelse(minute >= 90, "90+", floor(minute / 10) * 10))
  
  # Filtrer pass-data for kun at inkludere de bund 5 hold og beregn afleveringspræcisionen samlet for alle hold per 10 min
  Eredivisie_Pass_bot5_acc_by_10min <- Eredivisie_pass_10 %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    group_by(minute_group) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    ) %>%
    arrange(minute_group)
  
  # Sørg for, at minute_group er en faktor med korrekt sortering
  Eredivisie_Pass_bot5_acc_by_10min$minute_group <- factor(
    Eredivisie_Pass_bot5_acc_by_10min$minute_group, 
    levels = c(seq(0, 80, by = 10), "90+")
  )
  
  # Vis resultatet
  print(Eredivisie_Pass_bot5_acc_by_10min)
  
  # Opret barplot med ggplot2
  ggplot(Eredivisie_Pass_bot5_acc_by_10min, aes(x = minute_group)) +
    geom_bar(aes(y = total_passes, fill = "Antal afleveringer"), stat = "identity", color = "black") +
    geom_line(aes(y = accuracy * max(total_passes), group = 1, color = "Afleveringspræcision"), size = 1.2) +
    geom_point(aes(y = accuracy * max(total_passes), color = "Afleveringspræcision"), size = 3) +
    scale_y_continuous(
      name = "Antal afleveringer",
      sec.axis = sec_axis(~ . / max(Eredivisie_Pass_bot5_acc_by_10min$total_passes), name = "Afleveringspræcision (%)", labels = scales::percent)
    ) +
    scale_fill_manual(values = c("Antal afleveringer" = "steelblue")) +
    scale_color_manual(values = c("Afleveringspræcision" = "red")) +
    labs(
      title = "Afleveringspræcision og antal afleveringer per 10 minutter (Bund 5 hold)",
      x = "Minutgruppe",
      fill = "",
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
}
  
  # Kode for både top og bund
  # Afleveringspræcision pr. 10 minut
  {
    # Load nødvendige pakker
    library(ggplot2)
    library(dplyr)
    library(gridExtra) # Bruges til at vise plots ved siden af hinanden
    
    # Tilføj en ny variabel for 10-minutters intervaller
    Eredivisie_pass_10 <- Eredivisie_pass %>%
      mutate(minute_group = ifelse(minute >= 90, "90+", floor(minute / 10) * 10))
    
    # Filtrer pass-data for top 5 hold og bund 5 hold
    Eredivisie_Pass_top5_acc_by_10min <- Eredivisie_pass_10 %>% 
      filter(team.name %in% Top5_Hold_Eredivisie) %>% 
      group_by(minute_group) %>% 
      summarise(
        total_passes = n(),
        successful_passes = sum(pass.accurate, na.rm = TRUE),
        accuracy = round(successful_passes / total_passes, 2)
      ) %>%
      arrange(minute_group)
    
    Eredivisie_Pass_bot5_acc_by_10min <- Eredivisie_pass_10 %>% 
      filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
      group_by(minute_group) %>% 
      summarise(
        total_passes = n(),
        successful_passes = sum(pass.accurate, na.rm = TRUE),
        accuracy = round(successful_passes / total_passes, 2)
      ) %>%
      arrange(minute_group)
    
    # Sørg for, at minute_group er en faktor med korrekt sortering
    Eredivisie_Pass_top5_acc_by_10min$minute_group <- factor(
      Eredivisie_Pass_top5_acc_by_10min$minute_group, 
      levels = c(seq(0, 80, by = 10), "90+")
    )
    
    Eredivisie_Pass_bot5_acc_by_10min$minute_group <- factor(
      Eredivisie_Pass_bot5_acc_by_10min$minute_group, 
      levels = c(seq(0, 80, by = 10), "90+")
    )
    
    # Find det maksimale antal afleveringer i begge datasæt for at ensrette y-aksen
    max_passes <- max(max(Eredivisie_Pass_top5_acc_by_10min$total_passes, na.rm = TRUE),
                      max(Eredivisie_Pass_bot5_acc_by_10min$total_passes, na.rm = TRUE))
    
    # Funktion til at lave plottet
    create_plot <- function(data, title) {
      ggplot(data, aes(x = minute_group)) +
        geom_bar(aes(y = total_passes, fill = "Antal afleveringer"), stat = "identity", color = "black") +
        geom_line(aes(y = (accuracy - 0.6) * (max_passes / 0.4), group = 1, color = "Afleveringspræcision"), size = 1.2) +
        geom_point(aes(y = (accuracy - 0.6) * (max_passes / 0.4), color = "Afleveringspræcision"), size = 3) +
        scale_y_continuous(
          limits = c(0, max_passes),
          name = "Antal afleveringer",
          sec.axis = sec_axis(~ . * 0.4 / max_passes + 0.6, name = "Afleveringspræcision (%)", labels = scales::percent, breaks = seq(0.6, 1.0, by = 0.1))
        ) +
        scale_fill_manual(values = c("Antal afleveringer" = "steelblue")) +
        scale_color_manual(values = c("Afleveringspræcision" = "red")) +
        labs(
          title = title,
          x = "Minutgruppe",
          fill = "",
          color = ""
        ) +
        theme_minimal() +
        theme(legend.position = "top")
    }
    
    # Opret de to plots
    plot_top5 <- create_plot(Eredivisie_Pass_top5_acc_by_10min, "Top 5 hold")
    plot_bot5 <- create_plot(Eredivisie_Pass_bot5_acc_by_10min, "Bund 5 hold")
    
    # Vis plottene ved siden af hinanden
    grid.arrange(plot_top5, plot_bot5, ncol = 2)
    
  
  # Vis plottene ved siden af hinanden
  grid.arrange(plot_top5, plot_bot5, ncol = 2)
  }
# Alle hold Eredivisie
  
# Afleveringspræcision pr. 10 minut
  {
  # Tilføj en ny variabel for 10-minutters intervaller
  Eredivisie_pass_10 <- Eredivisie_pass %>%
    mutate(minute_group = floor(minute / 10) * 10)
  
  # Filtrer pass-data for kun at inkludere de top 5 hold, beregn afleveringspræcisionen for hver 10-minutters periode
  Eredivisie_Pass_acc_by_10min <- Eredivisie_pass_10 %>% 
    group_by(minute_group) %>% 
    summarise(
      total_passes = n(),
      successful_passes = sum(pass.accurate, na.rm = TRUE),
      accuracy = round(successful_passes / total_passes, 2)
    )
  
  # Vis resultatet
  print(Eredivisie_top5_Pass_acc_by_10min)
  }
  
# Afleveringslængde på assist
{
# Vi starter med at finde alle rækker, som giver en assist 
library(dplyr)
library(purrr)

Eredivisie_pass_assist <- Eredivisie_pass %>%
  filter(map_lgl(type.secondary, ~ any(. == "assist")))

# Nu finder vi gennemsnitslængden på en assist
Eredivisie_pass_assist_mean <- mean(Eredivisie_pass_assist$pass.length)
}

  # Position med flest afleveringer samt pass accuracy
  {
    Eredivisie_pass_sum <- Eredivisie_pass[, c(14,26:27)]
    
    # Vi grupperer vores positioner, så vi kan lave et plot.
    library(dplyr)
    
    # Definér bredere positioner med labels til fodboldbanen
    position_groups <- list(
      "Right Back" = c("RB", "RWB", "RB5"),
      "Left Back" = c("LB", "LWB", "LB5"),
      "Center Back" = c("CB","RCB", "RCB3","LCB", "LCB3"),
      "Defensive Midfielder" = c("DMF", "LDMF", "RDMF"),
      "Right Central Midfielder" = c("RCMF", "RCMF3"),
      "Left Central Midfielder" = c("LCMF", "LCMF3"),
      "Attacking Midfielder" = c("AMF", "RAMF", "LAMF"),
      "Right Winger" = c("RW", "RWF"),
      "Left Winger" = c("LW", "LWF"),
      "Striker" = c("SS", "CF"),
      "Goalkeeper" = c("GK")
    )
    
    # Funktion til at matche positioner med den nye kategori
    assign_position_group <- function(pos) {
      for (group in names(position_groups)) {
        if (pos %in% position_groups[[group]]) {
          return(group)
        }
      }
      return(NA) # Hvis position ikke matcher, returner NA
    }
    
    # Anvend funktionen til at skabe en ny kolonne med bredere positioner
    Eredivisie_pass_sum <- Eredivisie_pass_sum %>%
      mutate(broad_position = sapply(player.position, assign_position_group))
    
    # Vi beregner antal afleveringer samt pass accuracy for hver position.
    Eredivisie_pass_acc_pos <- Eredivisie_pass_sum %>%
      group_by(broad_position) %>%
      summarise(
        total_passes = n(),
        accurate_passes = sum(pass.accurate, na.rm = TRUE),
        pass_accuracy = round((accurate_passes / total_passes) * 100, 2)
      ) %>%
      arrange(desc(pass_accuracy))
    
    Eredivisie_pass_acc_pos <- na.omit(Eredivisie_pass_acc_pos)
    
    # Se resultatet
    print(Eredivisie_pass_acc_pos)
    
    # Til sidst plotter vi vores accuracy
    
    
    # ikke pænt ggplot
    {    
    library(ggplot2)
    
    # Definér koordinater for spillernes positioner på banen
    position_coords <- data.frame(
      broad_position = c("Goalkeeper", "Right Back", "Left Back", "Center Back",
                         "Defensive Midfielder", "Right Central Midfielder", "Left Central Midfielder",
                         "Attacking Midfielder", "Right Winger", "Left Winger", "Striker"),
      x = c(5, 20, 20, 20, 40, 50, 50, 60, 70, 70, 85),  # Juster X-koordinater for realistisk placering
      y = c(50, 20, 80, 50, 50, 35, 65, 50, 20, 80, 50)   # Juster Y-koordinater
    )
    
    # Merge positioner med accuracy data
    plot_data <- merge(Ekstraklasa_pass_acc_pos, position_coords, by = "broad_position", all.y = TRUE)
    
    # Tilføj labels med både antal afleveringer og pass accuracy
    plot_data$label <- paste0(plot_data$broad_position, "\n",
                              "Passes: ", plot_data$total_passes, "\n",
                              "Accuracy: ", plot_data$pass_accuracy, "%")
    
    # Tegn fodboldbanen
    ggplot(plot_data, aes(x = x, y = y)) +
      # Tegn punkter for hver spiller
      geom_point(aes(size = pass_accuracy, color = pass_accuracy), alpha = 0.8) +  
      # Tilføj labels med position, antal afleveringer og accuracy
      geom_text(aes(label = label), vjust = -1, size = 2.5) +  
      # Skala for størrelse af prikker
      scale_size_continuous(range = c(5, 15)) +  
      # Farveskala fra rød (lav accuracy) til grøn (høj accuracy)
      scale_color_gradient(low = "red", high = "green") +  
      # Baggrund, der ligner en fodboldbane
      annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "green", alpha = 0.3) +  # Banen
      annotate("rect", xmin = 0, xmax = 10, ymin = 30, ymax = 70, color = "white", fill = NA, size = 1) +  # Målområde venstre
      annotate("rect", xmin = 90, xmax = 100, ymin = 30, ymax = 70, color = "white", fill = NA, size = 1) +  # Målområde højre
      annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, color = "white", fill = NA, size = 1) +  # Yderste linjer
      # Titler og layout
      labs(title = "CB og DM med størst afleveringspræcision",
           x = "", y = "", color = "Pass Accuracy (%)") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    }
    }
  view(Ekstraklasa_pass_acc_pos)
  
  # Position med flest modtagelser af afleveringer
  {
    # Vi beregner antal afleveringer samt pass accuracy for hver position.
    
    Eredivisie_pass_receive <- Eredivisie_pass[, c(14,27,32:33)]
    
    # Anvend funktionen til at skabe en ny kolonne med bredere positioner
    Eredivisie_pass_receive <- Eredivisie_pass_receive %>%
      mutate(broad_position = sapply(pass.recipient.position, assign_position_group))
    
    # # Vi beregner antal receives for hver position.
    Eredivisie_Top_recievers <- Eredivisie_pass_receive %>% 
      group_by(pass.recipient.position, pass.recipient.name) %>% 
      summarise(total_receives = n()) %>% 
      arrange(desc(total_receives)) %>% 
      na.omit()   
    
    Eredivisie_Top_recievers_pos <- Eredivisie_pass_receive %>% 
      group_by(broad_position) %>% 
      summarise(total_receives = n()) %>% 
      arrange(desc(total_receives)) %>% 
      na.omit()
    
    
    # Plot
    
    {
      # Load nødvendige pakker
      library(ggplot2)
      library(dplyr)
      library(ggsoccer)
      
      # Definér koordinater for spillernes positioner på banen
      position_coords <- data.frame(
        broad_position = c("Goalkeeper", "Right Back", "Left Back", "Center Back",
                           "Defensive Midfielder", "Right Central Midfielder", "Left Central Midfielder",
                           "Attacking Midfielder", "Right Winger", "Left Winger", "Striker"),
        x = c(5, 20, 20, 20, 40, 50, 50, 60, 70, 70, 85),  # X-koordinater for realistisk placering
        y = c(50, 20, 80, 50, 50, 35, 65, 50, 20, 80, 50)   # Y-koordinater
      )
      
      # Merge positioner med antal modtagne afleveringer
      plot_data2 <- merge(Eredivisie_Top_recievers_pos, position_coords, by = "broad_position", all.y = TRUE)
      
      # Tilføj labels med antal modtagne afleveringer
      plot_data2$label <- paste0(plot_data2$broad_position, "\nReceives: ", plot_data2$total_receives)
      
      # Lav fodboldbane-plot
      ggplot(plot_data2, aes(x = x, y = y)) +
        annotate_pitch(colour = "white", fill = "green") +  # Fodboldbanen
        theme_pitch() +  # Fjerner akse-markeringer
        geom_point(aes(size = total_receives, color = total_receives), alpha = 0.8) +  # Spillernes positioner
        geom_text(aes(label = label), vjust = -1, size = 3, color = "Black") +  # Labels
        scale_size_continuous(range = c(5, 15)) +  # Justér prikstørrelse
        scale_color_gradient(low = "blue", high = "yellow") +  # Farveskala fra få (blå) til mange (gul) afleveringer
        labs(
          title = "Forsvar, Defensiv midt og Offensiv midt med flest modtagne afleveringer",
          x = "", y = "", color = "Total Receives"
        ) +
        theme_minimal() +
        theme(
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "White"),  # Sort baggrund for kontrast
          plot.title = element_text(color = "Black", hjust = 0.5, size = 14)  # Hvid titel
        )
      
      
      }
    
  }
  view(Eredivisie_Top_recievers)
  
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

  # Load nødvendige pakker
  library(ggplot2)
  library(dplyr)
  
  # Beregn antal assists per position og sorter faldende
  Eredivisie_pass_assist_pos_count <- Eredivisie_pass_assist_pos %>%
    group_by(position_group) %>%
    summarise(antal_assists = n()) %>%
    arrange(desc(antal_assists))  # Sorter fra høj til lav
  
  # Find maks antal assists for at tilføje luft over den højeste søjle
  max_assists <- max(Eredivisie_pass_assist_pos_count$antal_assists)
  
  # Opret barplot med tal ovenover søjlerne og udvidet y-akse
  ggplot(Eredivisie_pass_assist_pos_count, aes(x = reorder(position_group, -antal_assists), y = antal_assists)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    geom_text(aes(label = antal_assists), vjust = -0.5, size = 5, fontface = "bold") +  # Tilføj tal over søjlerne
    scale_y_continuous(limits = c(0, max_assists * 1.1), expand = expansion(mult = 0.1)) +  # Udvid y-aksen med 10%
    labs(
      title = "Midtbanen skaber flest assists",
      x = "Position",
      y = "Antal assists"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter labels for bedre læsbarhed
  
  
  
}

# Assistmagere på spillerniveau
{
Eredivisie_pass_assist_player_count <- Eredivisie_pass_assist %>% 
  group_by(player.name, team.name) %>% 
  summarise(Eredivisie_pass_assist = n()) %>% 
  arrange(desc(Eredivisie_pass_assist))

# Nyt m. plot
  # Load nødvendige pakker
  library(ggplot2)
  library(dplyr)
  
  # Beregn assists per spiller og vælg top 10
  top_10_assist_players_Eredivisie <- Eredivisie_pass_assist %>%
    group_by(player.name, team.name) %>%
    summarise(
      antal_assists = n(),
      primary_position = names(which.max(table(player.position))),  # Finder hyppigste position
      .groups = 'drop'
    ) %>%
    arrange(desc(antal_assists)) %>%
    slice_head(n = 10)
  
  # Opret en ny kolonne med en samlet label (Spiller - Hold - Position)
  top_10_assist_players_Eredivisie <- top_10_assist_players_Eredivisie %>%
    mutate(spiller_info = paste0(player.name, " (", team.name, ") - ", primary_position))
  
  # Opret et horisontalt søjlediagram
  ggplot(top_10_assist_players_Eredivisie, aes(x = reorder(spiller_info, antal_assists), y = antal_assists)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.6) +  # Smalle søjler for tabel-look
    geom_text(aes(label = antal_assists), hjust = -0.3, size = 5, fontface = "bold") +  # Tilføj tal
    labs(
      title = "Top 10 assistspillere i Eredivisie",
      x = "", 
      y = "Antal assists"
    ) +
    coord_flip() +  # Roter grafen for tabel-stil
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, hjust = 1),  # Større tekst for spillernavne
      plot.margin = margin(10, 30, 10, 10)  # Ekstra margin for at give plads
    )
  
  
  
  
  }
  
# Shiny app m. plots  
  {
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(DT)
  library(scales)
  
  ui_Eredivisie_pass <- fluidPage(
    titlePanel("Eredivisie Statistik Plots"),
    sidebarLayout(
      sidebarPanel(
        selectInput("plotType", "Vælg plot:",
                    choices = list(
                      "Top 5: Afleveringspræcision" = "top_pass",
                      "Top 5: Afleveringspræcision efter 80. minut" = "top_pass_80",
                      "Bot 5: Afleveringspræcision" = "bot_pass",
                      "Bot 5: Afleveringspræcision efter 80. minut" = "bot_pass_80",
                      "Assistmagere (positionsniveau)" = "assist_pos",
                      "Assistmagere (spillerniveau)" = "assist_player"
                    ))
      ),
      mainPanel(
        # Bruger et dynamisk output, så vi kan skifte mellem plot og tabel
        uiOutput("selectedUI")
      )
    )
  )
  
  server_Eredivisie_pass <- function(input, output, session) {
    
    output$selectedUI <- renderUI({
      if (input$plotType == "assist_player") {
        DT::dataTableOutput("assist_table")
      } else {
        plotOutput("selectedPlot")
      }
    })
    
    output$selectedPlot <- renderPlot({
      if(input$plotType == "top_pass") {
        ggplot(Eredivisie_top5_Pass_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - Afleveringspræcision", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "top_pass_80") {
        ggplot(Eredivisie_top5_Pass_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - Afleveringspræcision efter 80. minut", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "bot_pass") {
        ggplot(Eredivisie_bot5_Pass_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Bot 5 hold - Afleveringspræcision", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "bot_pass_80") {
        ggplot(Eredivisie_bot5_Pass_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Bot 5 hold - Afleveringspræcision efter 80. minut", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "assist_pos") {
        ggplot(Eredivisie_pass_assist_pos_count, 
               aes(x = reorder(position_group, -antal_assists), y = antal_assists, fill = position_group)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = antal_assists), vjust = -0.5) +
          labs(title = "Assistmagere på positionsniveau", x = "Position", y = "Antal assists") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    })
    
    output$assist_table <- DT::renderDataTable({
      # Vælger top 10 spillere med flest assists
      top_players <- Eredivisie_pass_assist_player_count %>% 
        top_n(10, wt = Eredivisie_pass_assist)
      
      DT::datatable(top_players, options = list(
        scrollY = "400px",   # Gør tabellen scrollbar på y-aksen
        paging = FALSE,      # Fjern paging, så hele tabellen er synlig i scrollbaren
        searching = FALSE,   # Fjern søgefelt, hvis det ønskes
        dom = 't'            # Fjern ekstra elementer som fx info tekst
      ))
    })
  }
  }
  shinyApp(ui_Eredivisie_pass, server_Eredivisie_pass)
  
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
      
      Ekstraklasa_bot5 <- tibble(Team = c("Stal Mielec","Śląsk Wrocław",
                                          "Nieciecza","Cracovia Kraków","Górnik Łęczna")) %>% 
        mutate(Rank = row_number()) %>% 
        select(Rank, Team)
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
    
    # Afleveringspræcision hver 10. minut
    {
      # Tilføj en ny variabel for 10-minutters intervaller
      
      Ekstraklasa_pass_10 <- Ekstraklasa_pass %>%
        mutate(minute_group = floor(minute / 10) * 10)
      
      # Filtrer pass-data for kun at inkludere de top 5 hold og beregn afleveringspræcisionen samlet for alle hold per 10 min
      Ekstraklasa_Pass_top5_acc_by_10min <- Ekstraklasa_pass_10 %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        group_by(minute_group) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        arrange(minute_group)
      
      # Vis resultatet
      print(Eredivisie_Pass_acc_by_10min)
    }
    
    # Stats for Bottom 5 hold i Ekstraklasa
    
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
    
    # Afleveringspræcision hver 10. minut
    {
      # Tilføj en ny variabel for 10-minutters intervaller
      
      Ekstraklasa_pass_10 <- Ekstraklasa_pass %>%
        mutate(minute_group = floor(minute / 10) * 10)
      
      # Filtrer pass-data for kun at inkludere de top 5 hold og beregn afleveringspræcisionen samlet for alle hold per 10 min
      Ekstraklasa_Pass_bot5_acc_by_10min <- Ekstraklasa_pass_10 %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        group_by(minute_group) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        arrange(minute_group)
      
      # Vis resultatet
      print(Eredivisie_Pass_acc_by_10min)
    }
    
    # Stats på alle hold
    
    # Afleveringspræcision hver 10. minut
    # Kode for både top og bund
    # Afleveringspræcision pr. 10 minut
    {
      # Load nødvendige pakker
      library(ggplot2)
      library(dplyr)
      library(gridExtra) # Bruges til at vise plots ved siden af hinanden
      
      # Tilføj en ny variabel for 10-minutters intervaller
      Ekstraklasa_pass_10 <- Ekstraklasa_pass %>%
        mutate(minute_group = ifelse(minute >= 90, "90+", floor(minute / 10) * 10))
      
      # Filtrer pass-data for top 5 hold og bund 5 hold
      Ekstraklasa_Pass_top5_acc_by_10min <- Ekstraklasa_pass_10 %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        group_by(minute_group) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        arrange(minute_group)
      
      Ekstraklasa_Pass_bot5_acc_by_10min <- Ekstraklasa_pass_10 %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        group_by(minute_group) %>% 
        summarise(
          total_passes = n(),
          successful_passes = sum(pass.accurate, na.rm = TRUE),
          accuracy = round(successful_passes / total_passes, 2)
        ) %>%
        arrange(minute_group)
      
      # Sørg for, at minute_group er en faktor med korrekt sortering
      Ekstraklasa_Pass_top5_acc_by_10min$minute_group <- factor(
        Eredivisie_Pass_top5_acc_by_10min$minute_group, 
        levels = c(seq(0, 80, by = 10), "90+")
      )
      
      Ekstraklasa_Pass_bot5_acc_by_10min$minute_group <- factor(
        Eredivisie_Pass_bot5_acc_by_10min$minute_group, 
        levels = c(seq(0, 80, by = 10), "90+")
      )
      
      # Find det maksimale antal afleveringer i begge datasæt for at ensrette y-aksen
      max_passes_Ekstraklasa <- max(max(Ekstraklasa_Pass_top5_acc_by_10min$total_passes, na.rm = TRUE),
                        max(Ekstraklasa_Pass_bot5_acc_by_10min$total_passes, na.rm = TRUE))
      
      # Funktion til at lave plottet
      create_plot_Ekstraklasa <- function(data, title) {
        ggplot(data, aes(x = minute_group)) +
          geom_bar(aes(y = total_passes, fill = "Antal afleveringer"), stat = "identity", color = "black") +
          geom_line(aes(y = (accuracy - 0.6) * (max_passes_Ekstraklasa / 0.4), group = 1, color = "Afleveringspræcision"), size = 1.2) +
          geom_point(aes(y = (accuracy - 0.6) * (max_passes_Ekstraklasa / 0.4), color = "Afleveringspræcision"), size = 3) +
          scale_y_continuous(
            limits = c(0, max_passes_Ekstraklasa),
            name = "Antal afleveringer",
            sec.axis = sec_axis(~ . * 0.4 / max_passes_Ekstraklasa + 0.6, name = "Afleveringspræcision (%)", labels = scales::percent, breaks = seq(0.6, 1.0, by = 0.1))
          ) +
          scale_fill_manual(values = c("Antal afleveringer" = "steelblue")) +
          scale_color_manual(values = c("Afleveringspræcision" = "red")) +
          labs(
            title = title,
            x = "Minutgruppe",
            fill = "",
            color = ""
          ) +
          theme_minimal() +
          theme(legend.position = "top")
      }
      
      # Opret de to plots
      plot_top5_Ekstraklasa <- create_plot_Ekstraklasa(Ekstraklasa_Pass_top5_acc_by_10min, "Top 5 hold")
      plot_bot5_Ekstraklasa <- create_plot_Ekstraklasa(Ekstraklasa_Pass_bot5_acc_by_10min, "Bund 5 hold")
      
      # Vis plottene ved siden af hinanden
      grid.arrange(plot_top5_Ekstraklasa, plot_bot5_Ekstraklasa, ncol = 2)
      
      
      # Vis plottene ved siden af hinanden
      grid.arrange(plot_top5_Ekstraklasa, plot_bot5_Ekstraklasa, ncol = 2)
    }
    
    # Afleveringslængde på assist
    {
      # Vi starter med at finde alle rækker, som giver en assist 
      library(dplyr)
      library(purrr)
      Ekstraklasa_pass_assist <- Ekstraklasa_pass %>%
        filter(map_lgl(type.secondary, ~ any(. == "assist")))
      
      # Nu finder vi gennemsnitslængden på en assist
      Ekstraklasa_pass_assist_mean <- mean(Ekstraklasa_pass_assist$pass.length)
    }
    
    # Position med flest afleveringer samt pass accuracy
    {
      Ekstraklasa_pass_sum <- Ekstraklasa_pass[, c(14,26:27)]
      
      # Vi grupperer vores positioner, så vi kan lave et plot.
      library(dplyr)
      
      # Definér bredere positioner med labels til fodboldbanen
      position_groups <- list(
        "Right Back" = c("RB", "RWB", "RB5"),
        "Left Back" = c("LB", "LWB", "LB5"),
        "Center Back" = c("CB","RCB", "RCB3","LCB", "LCB3"),
        "Defensive Midfielder" = c("DMF", "LDMF", "RDMF"),
        "Right Central Midfielder" = c("RCMF", "RCMF3"),
        "Left Central Midfielder" = c("LCMF", "LCMF3"),
        "Attacking Midfielder" = c("AMF", "RAMF", "LAMF"),
        "Right Winger" = c("RW", "RWF"),
        "Left Winger" = c("LW", "LWF"),
        "Striker" = c("SS", "CF"),
        "Goalkeeper" = c("GK")
      )
      
      # Funktion til at matche positioner med den nye kategori
      assign_position_group <- function(pos) {
        for (group in names(position_groups)) {
          if (pos %in% position_groups[[group]]) {
            return(group)
          }
        }
        return(NA) # Hvis position ikke matcher, returner NA
      }
      
      # Anvend funktionen til at skabe en ny kolonne med bredere positioner
      Ekstraklasa_pass_sum <- Ekstraklasa_pass_sum %>%
        mutate(broad_position = sapply(player.position, assign_position_group))
      
      # Vi beregner antal afleveringer samt pass accuracy for hver position.
      Ekstraklasa_pass_acc_pos <- Ekstraklasa_pass_sum %>%
        group_by(broad_position) %>%
        summarise(
          total_passes = n(),
          accurate_passes = sum(pass.accurate, na.rm = TRUE),
          pass_accuracy = round((accurate_passes / total_passes) * 100, 2)
        ) %>%
        arrange(desc(pass_accuracy))
      
      Ekstraklasa_pass_acc_pos <- na.omit(Ekstraklasa_pass_acc_pos)
      
      # Se resultatet
      print(Ekstraklasa_pass_acc_pos)
      
      # Til sidst plotter vi vores accuracy
      library(ggplot2)
      
      # Definér koordinater for spillernes positioner på banen
      position_coords <- data.frame(
        broad_position = c("Goalkeeper", "Right Back", "Left Back", "Center Back",
                           "Defensive Midfielder", "Right Central Midfielder", "Left Central Midfielder",
                           "Attacking Midfielder", "Right Winger", "Left Winger", "Striker"),
        x = c(5, 20, 20, 20, 40, 50, 50, 60, 70, 70, 85),  # Juster X-koordinater for realistisk placering
        y = c(50, 80, 20, 50, 50, 60, 40, 50, 80, 20, 50)   # Juster Y-koordinater
      )
      
      # Merge positioner med accuracy data
      plot_data <- merge(Ekstraklasa_pass_acc_pos, position_coords, by = "broad_position", all.y = TRUE)
      
      # Tilføj labels med både antal afleveringer og pass accuracy
      plot_data$label <- paste0(plot_data$broad_position, "\n",
                                "Passes: ", plot_data$total_passes, "\n",
                                "Accuracy: ", plot_data$pass_accuracy, "%")
      
      # Tegn fodboldbanen
      ggplot(plot_data, aes(x = x, y = y)) +
        # Tegn punkter for hver spiller
        geom_point(aes(size = pass_accuracy, color = pass_accuracy), alpha = 0.8) +  
        # Tilføj labels med position, antal afleveringer og accuracy
        geom_text(aes(label = label), vjust = -1, size = 2.5) +  
        # Skala for størrelse af prikker
        scale_size_continuous(range = c(5, 15)) +  
        # Farveskala fra rød (lav accuracy) til grøn (høj accuracy)
        scale_color_gradient(low = "red", high = "green") +  
        # Baggrund, der ligner en fodboldbane
        annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "green", alpha = 0.3) +  # Banen
        annotate("rect", xmin = 0, xmax = 10, ymin = 30, ymax = 70, color = "white", fill = NA, size = 1) +  # Målområde venstre
        annotate("rect", xmin = 90, xmax = 100, ymin = 30, ymax = 70, color = "white", fill = NA, size = 1) +  # Målområde højre
        annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, color = "white", fill = NA, size = 1) +  # Yderste linjer
        annotate("point", x = 50, y = 50, color = "white", size = 3) +  # Midtercirkel
        annotate("rect", xmin = 40, xmax = 60, ymin = 40, ymax = 60, color = "white", fill = NA, size = 1) +  # Midtercirkel
        # Titler og layout
        labs(title = "Pass Accuracy per Position on a Football Pitch",
             x = "", y = "", color = "Pass Accuracy (%)") +
        theme_minimal() +
        theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    }
    view(Ekstraklasa_pass_acc_pos)
    
    # Position med flest modtagelser af afleveringer
    {
      # Vi beregner antal afleveringer samt pass accuracy for hver position.
      
      Ekstraklasa_pass_receive <- Ekstraklasa_pass[, c(14,27,32:33)]
      
      # Anvend funktionen til at skabe en ny kolonne med bredere positioner
      Ekstraklasa_pass_receive <- Ekstraklasa_pass_receive %>%
        mutate(broad_position = sapply(pass.recipient.position, assign_position_group))
      
      # # Vi beregner antal receives for hver position.
      Ekstraklasa_Top_recievers <- Ekstraklasa_pass_receive %>% 
        group_by(pass.recipient.position, pass.recipient.name) %>% 
        summarise(total_receives = n()) %>% 
        arrange(desc(total_receives)) %>% 
        na.omit()
    }
    view(Ekstraklasa_Top_recievers)
    
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
      
      # Load nødvendige pakker
      library(ggplot2)
      library(dplyr)
      
      # Beregn antal assists per position og sorter faldende
      Ekstraklasa_pass_assist_pos_count <- Ekstraklasa_pass_assist_pos %>%
        group_by(position_group) %>%
        summarise(antal_assists = n()) %>%
        arrange(desc(antal_assists))  # Sorter fra høj til lav
      
      # Find maks antal assists for at tilføje luft over den højeste søjle
      max_assists <- max(Ekstraklasa_pass_assist_pos_count$antal_assists)
      
      # Opret barplot med tal ovenover søjlerne og udvidet y-akse
      ggplot(Ekstraklasa_pass_assist_pos_count, aes(x = reorder(position_group, -antal_assists), y = antal_assists)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +
        geom_text(aes(label = antal_assists), vjust = -0.5, size = 5, fontface = "bold") +  # Tilføj tal over søjlerne
        scale_y_continuous(limits = c(0, max_assists * 1.1), expand = expansion(mult = 0.1)) +  # Udvid y-aksen med 10%
        labs(
          title = "Angribere skaber flest assists",
          x = "Position",
          y = "Antal assists"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter labels for bedre læsbarhed
      
      
      
    }
    
    # Assistmagere på spillerniveau
    {
      Ekstraklasa_pass_assist_player_count <- Ekstraklasa_pass_assist %>% 
        group_by(player.name, team.name) %>% 
        summarise(Ekstraklasa_pass_assist = n()) %>% 
        arrange(desc(Ekstraklasa_pass_assist))
   
      # Nyt m. plot
      # Load nødvendige pakker
      library(ggplot2)
      library(dplyr)
      
      # Beregn assists per spiller og vælg top 10
      top_10_assist_players_Ekstraklasa <- Ekstraklasa_pass_assist %>%
        group_by(player.name, team.name) %>%
        summarise(
          antal_assists = n(),
          .groups = 'drop'
        ) %>%
        arrange(desc(antal_assists)) %>%
        slice_head(n = 10)
      
      # Opret en ny kolonne med kun spillerens navn og hold (uden position)
      top_10_assist_players_Ekstraklasa <- top_10_assist_players_Ekstraklasa %>%
        mutate(spiller_info = paste0(player.name, " (", team.name, ")"))  # Ingen position her
      
      # Opret et horisontalt søjlediagram
      ggplot(top_10_assist_players_Ekstraklasa, aes(x = reorder(spiller_info, antal_assists), y = antal_assists)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.6) +  # Smalle søjler for tabel-look
        geom_text(aes(label = antal_assists), hjust = -0.3, size = 5, fontface = "bold") +  # Tilføj tal
        labs(
          title = "Top 10 assistspillere i Ekstraklasa",
          x = "", 
          y = "Antal assists"
        ) +
        coord_flip() +  # Roter grafen for tabel-stil
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 10, hjust = 1),  # Større tekst for spillernavne
          plot.margin = margin(10, 30, 10, 10)  # Ekstra margin for at give plads
        )
      
      }
    
    # Shiny app m. plots  
    {
    library(shiny)
    library(ggplot2)
    library(dplyr)
    library(DT)
    library(scales)
    
    ui_Ekstraklasa_pass <- fluidPage(
      titlePanel("Ekstraklasa Statistik Plots"),
      sidebarLayout(
        sidebarPanel(
          selectInput("plotType", "Vælg plot:",
                      choices = list(
                        "Top 5: Afleveringspræcision" = "top_pass",
                        "Top 5: Afleveringspræcision efter 80. minut" = "top_pass_80",
                        "Bot 5: Afleveringspræcision" = "bot_pass",
                        "Bot 5: Afleveringspræcision efter 80. minut" = "bot_pass_80",
                        "Assistmagere (positionsniveau)" = "assist_pos",
                        "Assistmagere (spillerniveau)" = "assist_player"
                      ))
        ),
        mainPanel(
          # Bruger et dynamisk output, så vi kan skifte mellem plot og tabel
          uiOutput("selectedUI")
        )
      )
    )
    
    server_Ekstraklasa_pass <- function(input, output, session) {
      
      output$selectedUI <- renderUI({
        if (input$plotType == "assist_player") {
          DT::dataTableOutput("assist_table")
        } else {
          plotOutput("selectedPlot")
        }
      })
      
      output$selectedPlot <- renderPlot({
        if(input$plotType == "top_pass") {
          ggplot(Ekstraklasa_top5_Pass_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - Afleveringspræcision", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "top_pass_80") {
          ggplot(Ekstraklasa_top5_Pass_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - Afleveringspræcision efter 80. minut", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "bot_pass") {
          ggplot(Ekstraklasa_bot5_Pass_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Bot 5 hold - Afleveringspræcision", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "bot_pass_80") {
          ggplot(Ekstraklasa_bot5_Pass_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Bot 5 hold - Afleveringspræcision efter 80. minut", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "assist_pos") {
          ggplot(Ekstraklasa_pass_assist_pos_count, 
                 aes(x = reorder(position_group, -antal_assists), y = antal_assists, fill = position_group)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = antal_assists), vjust = -0.5) +
            labs(title = "Assistmagere på positionsniveau", x = "Position", y = "Antal assists") +
            theme_minimal() +
            theme(legend.position = "none")
        }
      })
      
      output$assist_table <- DT::renderDataTable({
        # Vælger top 10 spillere med flest assists
        top_players <- Ekstraklasa_pass_assist_player_count %>% 
          top_n(10, wt = Ekstraklasa_pass_assist)
        
        DT::datatable(top_players, options = list(
          scrollY = "400px",   # Gør tabellen scrollbar på y-aksen
          paging = FALSE,      # Fjern paging, så hele tabellen er synlig i scrollbaren
          searching = FALSE,   # Fjern søgefelt, hvis det ønskes
          dom = 't'            # Fjern ekstra elementer som fx info tekst
        ))
      })
    }
    
    shinyApp(ui_Ekstraklasa_pass, server_Ekstraklasa_pass)
    
    }    
  }
}
  
# Opgave 2.2 – Skud
{
# Lav beskrivende statistik på skud i henholdsvis den polske og hollandske liga. 
# I repræsenterer IKKE et unikt hold, men er alene ekstern analytiker ansat hos fx Wyscout. 
# Der er minimum fem elementer som det ønskes I kigger nærmere på:
# 1.	Hvordan er skuddene for henholdsvis top og bund 5 holdene for sæsonen 2021/2022 i den polske og hollandske liga?
# 2.	Hvordan er Wyscout xG sidst i kampene i sæsonen 2021/2022, efter minut 80, sammenlignet med resten af kampen for henholdsvis den polske og hollandske liga?
# 3.	Hvordan er længde på skuddene, der er ”mål” sammenlignet med skud, der ikke er ”mål” for henholdsvis den polske og hollandske liga?
# 4.	Hvor en del af skuddene, der er ”mål” kommer fra henholdsvis forsvars-, midtbane- og angrebsspillere? (Hint: husk at præsentere jeres definition af de tre grupper)
# 5.	Hvilke 10 spillere lavede flest mål i henholdsvis den polske og hollandske liga? (Sammenlign med spillernes xG)

# Eredivisie
{
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
  
  # Ggplot
  # Kombiner data
  Eredivisie_shot_acc <- bind_rows(
    Eredivisie_top5_Shot_acc %>% mutate(Group = "Top 5"),
    Eredivisie_bot5_Shot_acc %>% mutate(Group = "Bottom 5")
  )
  
  # Lav barplot med ggplot
  ggplot(Eredivisie_shot_acc, aes(x = team.name, y = total_shots, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    geom_text(aes(label = paste0(accuracy * 100, "%")), vjust = -0.5, size = 5) +
    scale_y_continuous(limits = c(0, max(Eredivisie_shot_acc$total_shots) + 20)) +
    labs(title = "Top5 hold skyder mere på mål med bedre præcision",
         x = "Hold",
         y = "Total Skud",
         fill = "Gruppe") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
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

# goal før og efter 80. minut  
{
# goal før 80. min
# Vi definere vores top 5 hold
Top5_Hold_Eredivisie <- Eredivisie_top5$Team

# Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
Eredivisie_top5_goal <- Eredivisie_shot %>% 
  filter(team.name %in% Top5_Hold_Eredivisie) %>% 
  filter(minute < 80) %>% 
  group_by(team.name) %>% 
  summarise(
    total_shots = n(),
    shotisGoal = sum(shot.isGoal, na.rm = TRUE),
    accuracy = round(shotisGoal / total_shots, 2)
  ) %>%
  # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
  mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
  arrange(team.name)

# goal efter 80. minut
Top5_Hold_Eredivisie <- Eredivisie_top5$Team

# Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
Eredivisie_top5_goal_80 <- Eredivisie_shot %>% 
  filter(team.name %in% Top5_Hold_Eredivisie) %>% 
  filter(minute > 80) %>% 
  group_by(team.name) %>% 
  summarise(
    total_shots = n(),
    shotisGoal = sum(shot.isGoal, na.rm = TRUE),
    accuracy = round(shotisGoal / total_shots, 2)
  ) %>%
  # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
  mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
  arrange(team.name)

# ggplot 
library(ggplot2)
library(dplyr)

# Kombiner xG data for top5 og bund5 hold før og efter 80. minut
Eredivisie_goal <- bind_rows(
  Eredivisie_top5_goal %>% mutate(Time = "Before 80 min", Group = "Top 5"),
  Eredivisie_top5_goal_80 %>% mutate(Time = "After 80 min", Group = "Top 5"),
  Eredivisie_bot5_goal %>% mutate(Time = "Before 80 min", Group = "Bottom 5"),
  Eredivisie_bot5_goal_80 %>% mutate(Time = "After 80 min", Group = "Bottom 5")
)

# Beregn den samlede antal mål for hvert hold
Eredivisie_goal <- Eredivisie_goal %>%
  group_by(team.name) %>%
  mutate(total_goals = sum(shotisGoal),
         goal_percentage = round((shotisGoal / total_goals) * 100, 1))

# Lav barplot med ggplot
ggplot(Eredivisie_goal, aes(x = team.name, y = shotisGoal, fill = Time)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = paste0(goal_percentage, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  labs(title = "Mål scoret før og efter 80. minut for Top og Bund 5 Hold",
       x = "Hold",
       y = "Mål scoret",
       fill = "Tidspunkt") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



}
  
  # xg før og efter 80.minut
  {
  # xG før 80. min
  # Vi definere vores top 5 hold
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn gennemsnitlig xG før 80. minut
  Eredivisie_top5_xG <- Eredivisie_shot %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    filter(minute < 80) %>% 
    group_by(team.name) %>% 
    summarise(
      avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
      total_shots = n(),
      shotisGoal = sum(shot.isGoal, na.rm = TRUE),
      accuracy = round(shotisGoal / total_shots, 2)
    ) %>%
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
  # xG efter 80. minut
  Top5_Hold_Eredivisie <- Eredivisie_top5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_top5_xG_80 <- Eredivisie_shot %>% 
    filter(team.name %in% Top5_Hold_Eredivisie) %>% 
    filter(minute > 80) %>% 
    group_by(team.name) %>% 
    summarise(
      avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
      total_shots = n(),
      shotisGoal = sum(shot.isGoal, na.rm = TRUE),
      accuracy = round(shotisGoal / total_shots, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Top5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
  # ggplot
  library(ggplot2)
  library(dplyr)
  
  # Kombiner xG data for top5 og bund5 hold før og efter 80. minut
  Eredivisie_xG <- bind_rows(
    Eredivisie_top5_xG %>% mutate(Time = "After 80 min", Group = "Top 5"),
    Eredivisie_top5_xG_80 %>% mutate(Time = "Before 80 min", Group = "Top 5"),
    Eredivisie_bot5_xG %>% mutate(Time = "After 80 min", Group = "Bottom 5"),
    Eredivisie_bot5_xG_80 %>% mutate(Time = "Before 80 min", Group = "Bottom 5")
  )
  
  # Beregn den samlede xG for hvert hold
  Eredivisie_xG <- Eredivisie_xG %>%
    group_by(team.name) %>%
    mutate(total_xG = sum(avg_xG),
           xG_percentage = round((avg_xG / total_xG) * 100, 1))
  
  # Lav barplot med ggplot
  ggplot(Eredivisie_xG, aes(x = team.name, y = avg_xG, fill = Time)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    geom_text(aes(label = paste0(xG_percentage, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    scale_y_continuous(limits = c(0, max(Eredivisie_xG$avg_xG) + 0.2)) +
    labs(title = "Gennemsnitlig xG før og efter 80. minut for Top og Bund 5 Hold",
         x = "Hold",
         y = "Gennemsnitlig xG",
         fill = "Tidspunkt") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
# Stats for Bottom 5 hold i Eredivisie

# Skudpræcision
{
  # Vi definere vores bot 5 hold
  Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
  
  # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
  Eredivisie_bot5_Shot_acc <- Eredivisie_shot %>% 
    filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
    group_by(team.name) %>% 
    summarise(
      total_shots = n(),
      shot_on_target = sum(shot.onTarget, na.rm = TRUE),
      accuracy = round(shot_on_target / total_shots, 2)
    ) %>%
    # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
    mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
    arrange(team.name)
  
}

# goal før og efter 80. minut  
{
# goal før 80. min
# Vi definere vores bot 5 hold
Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team

# Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
Eredivisie_bot5_goal <- Eredivisie_shot %>% 
  filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
  filter(minute < 80) %>% 
  group_by(team.name) %>% 
  summarise(
    total_shots = n(),
    shotisGoal = sum(shot.isGoal, na.rm = TRUE),
    accuracy = round(shotisGoal / total_shots, 2)
  ) %>%
  # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
  mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
  arrange(team.name)

# goal efter 80. minut

# Vi definere vores bot 5 hold
Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team

# Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
Eredivisie_bot5_goal_80 <- Eredivisie_shot %>% 
  filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
  filter(minute > 80) %>% 
  group_by(team.name) %>% 
  summarise(
    total_shots = n(),
    shotisGoal = sum(shot.isGoal, na.rm = TRUE),
    accuracy = round(shotisGoal / total_shots, 2)
  ) %>%
  # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
  mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
  arrange(team.name)
}

# xG før og efter 80. minut
  {
    # xG før 80. min
    # Vi definere vores top 5 hold
    Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn gennemsnitlig xG før 80. minut
    Eredivisie_bot5_xG <- Eredivisie_shot %>% 
      filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
      filter(minute < 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
      arrange(team.name)
    
    # xG efter 80. minut
    Bot5_Hold_Eredivisie <- Eredivisie_bot5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
    Eredivisie_bot5_xG_80 <- Eredivisie_shot %>% 
      filter(team.name %in% Bot5_Hold_Eredivisie) %>% 
      filter(minute > 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
      mutate(team.name = factor(team.name, levels = Bot5_Hold_Eredivisie)) %>%
      arrange(team.name)
  }
  
# Skudposition på mål kontra ikke mål
{
  # Vi starter med at finde alle rækker, som giver en assist 
  library(dplyr)
  library(purrr)
  
  # Shot is Goal
  Eredivisie_shotIsGoal <- Eredivisie_shot %>%
    filter(shot.isGoal == TRUE)
  
  # Nu finder vi x og y på et mål
  Eredivisie_shotIsGoal_mean <- data.frame(
    location.x = mean(Eredivisie_shotIsGoal$location.x),
    location.y = mean(Eredivisie_shotIsGoal$location.y)
  )
  
  
  
  # Shot is not Goal
  Eredivisie_shotIsNotGoal <- Eredivisie_shot %>%
    filter(shot.isGoal == FALSE)
  
  Eredivisie_shotIsNotGoal_mean <- data.frame(
    location.x = mean(Eredivisie_shotIsNotGoal$location.x),
    location.y = mean(Eredivisie_shotIsNotGoal$location.y)
  )
  
}

# Målmaskiner på positionsniveau
{
  # Opret en ny kolonne 'position_group' baseret på player.position
  Eredivisie_shotIsGoal_pos <- Eredivisie_shotIsGoal %>%
    mutate(position_group = case_when(
      player.position %in% Goalkeeper ~ "Goalkeeper",
      player.position %in% Defender   ~ "Defender",
      player.position %in% Midfielder ~ "Midfielder",
      player.position %in% Attacker   ~ "Attacker",
      TRUE ~ "Other"  # Eventuelle positioner, der ikke matcher nogen af de definerede grupper
    ))
  
  # Gruppér efter position_group og tæl antallet af assists
  Eredivisie_shotIsGoal_pos_count <- Eredivisie_shotIsGoal_pos %>%
    group_by(position_group) %>%
    summarise(antal_mål = n())

  # ggplot
  # Kombiner data med liga-kolonne
  combined_goal_data <- bind_rows(
    Eredivisie_shotIsGoal_pos_count %>% mutate(League = "Eredivisie"),
    Ekstraklasa_shotIsGoal_pos_count %>% mutate(League = "Ekstraklasa")
  )
  
  # Sortér positionerne efter faldende antal mål inden for hver liga
  combined_goal_data <- combined_goal_data %>%
    group_by(League) %>%
    arrange(League, desc(antal_mål)) %>%
    mutate(position_group = factor(position_group, levels = unique(position_group)))
  
  # Lav barplot med ggplot - Facet Grid til at vise begge ligaer side om side
  ggplot(combined_goal_data, aes(x = position_group, y = antal_mål, fill = position_group)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = antal_mål), vjust = -0.5, size = 5) +
    facet_wrap(~ League) +
    scale_y_continuous(limits = c(0, max(combined_goal_data$antal_mål) + 25)) +
    labs(title = "Angriberer scorer flest mål",
         x = "Position",
         y = "Antal mål",
         fill = "Position") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  
  }

# Målmaskiner på spillerniveau
{
  # Data for topscorerne i Eredivisie
  Eredivisie_shotIsGoal_player_count <- Eredivisie_shotIsGoal %>% 
    group_by(player.name, team.name) %>% 
    summarise(goals_scored = n(), .groups = "drop") %>% 
    arrange(desc(goals_scored)) %>%
    slice_head(n = 10)  # Behold kun de 10 øverste
  
  # Lav liggende barplot med ggplot
ggplot(Eredivisie_shotIsGoal_player_count, aes(x = reorder(player.name, goals_scored), y = goals_scored, fill = team.name)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = goals_scored), hjust = -0.2, size = 5) +
    coord_flip() +
    labs(title = "Topscorere i Eredivisie",
         x = "Spiller",
         y = "Antal mål",
         fill = "Hold") +
    theme_minimal()
  
  
}  

  # Shiny app
  {
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(DT)
  
  ui_Eredivisie_shot <- fluidPage(
    titlePanel("Skudstatistik"),  # En lettere overskrift end før
    sidebarLayout(
      sidebarPanel(
        selectInput("plotType", "Vælg plot:",
                    choices = list(
                      "Top 5: Skudpræcision" = "top_shot",
                      "Top 5: Skudpræcision efter 80. minut" = "top_shot_80",
                      "Top 5: xG før 80. minut" = "top_xG",
                      "Top 5: xG efter 80. minut" = "top_xG_80",
                      "Bot 5: Skudpræcision" = "bot_shot",
                      "Bot 5: xG før 80. minut" = "bot_xG",
                      "Bot 5: xG efter 80. minut" = "bot_xG_80",
                      "Skudposition: Mål vs. Ikke mål" = "shot_position",
                      "Målmaskiner (positionsniveau)" = "shot_pos_count",
                      "Målmaskiner (spillerniveau)" = "shot_player"
                    ))
      ),
      mainPanel(
        uiOutput("selectedUI")  # Dynamisk UI: viser enten plot eller tabel
      )
    )
  )
  
  server_Eredivisie_shot <- function(input, output, session) {
    
    # Dynamisk UI: hvis "shot_player" vælges, vises tabellen, ellers et plot
    output$selectedUI <- renderUI({
      if(input$plotType == "shot_player"){
        DT::dataTableOutput("shot_table")
      } else {
        plotOutput("selectedPlot")
      }
    })
    
    output$selectedPlot <- renderPlot({
      if(input$plotType == "top_shot") {
        ggplot(Eredivisie_top5_Shot_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - Skudpræcision", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "top_shot_80") {
        ggplot(Eredivisie_top5_Shot_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - Skudpræcision efter 80. minut", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "top_xG") {
        ggplot(Eredivisie_top5_xG, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - xG før 80. minut", x = "Hold", y = "xG") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "top_xG_80") {
        ggplot(Eredivisie_top5_xG_80, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Top 5 hold - xG efter 80. minut", x = "Hold", y = "xG") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "bot_shot") {
        ggplot(Eredivisie_bot5_Shot_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Bot 5 hold - Skudpræcision", x = "Hold", y = "Præcision") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "bot_xG") {
        ggplot(Eredivisie_bot5_xG, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Bot 5 hold - xG før 80. minut", x = "Hold", y = "xG") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "bot_xG_80") {
        ggplot(Eredivisie_bot5_xG_80, aes(x = team.name, y = accuracy, fill = team.name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
          labs(title = "Bot 5 hold - xG efter 80. minut", x = "Hold", y = "xG") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if(input$plotType == "shot_position") {
        ggplot(Eredivisie_shot, aes(x = location.x, y = location.y, color = shot.isGoal)) +
          geom_jitter(alpha = 0.5) +
          labs(title = "Skudposition: Mål vs. Ikke mål", x = "Location X", y = "Location Y") +
          theme_minimal()
        
      } else if(input$plotType == "shot_pos_count") {
        ggplot(Eredivisie_shotIsGoal_pos_count, 
               aes(x = reorder(position_group, -antal_assists), y = antal_assists, fill = position_group)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = antal_assists), vjust = -0.5) +
          labs(title = "Målmaskiner på positionsniveau", x = "Position", y = "Antal mål") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    })
    
    output$shot_table <- DT::renderDataTable({
      # Top 10 målmaskiner på spillerniveau som en scrollable tabel
      top_players <- Eredivisie_shotIsGoal_player_count %>% 
        top_n(10, wt = Eredivisie_shotIsGoal)
      
      DT::datatable(top_players, options = list(
        scrollY = "400px",
        paging = FALSE,
        searching = FALSE,
        dom = 't'
      ))
    })
  }
  
  shinyApp(ui_Eredivisie_shot, server_Eredivisie_shot)
}
  
}

# Ekstraklasa
{
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
      
      # Filtrer for Polish liga (competitionId == "692")
      Ekstraklasa_ID <- all_compID_SeasonID %>% filter(competitionId == "692")
      
      # Hent matchId og seasonId
      polishData <- Ekstraklasa_ID %>% select("_id", seasonId)
      
      # Konverter matchId til liste
      polishId <- polishData$`_id`
      
      # Opret query for at hente shot-events fra mongo_games
      query4 <- toJSON(
        list(
          "matchId" = list("$in" = polishId),  
          "type.primary" = "shot"
        ), 
        auto_unbox = TRUE
      )
      
      # Hent kampdata med ALLE felter fra mongo_games
      Ekstraklasa_shot <- con_games$find(query = query4, fields = '{}')  # Henter alt
      
      # Flet seasonId ind i test-data ved at matche på matchId
      Ekstraklasa_shot <- Ekstraklasa_shot %>%
        left_join(polishData, by = c("matchId" = "_id"))
      
      # Vi flattener vores df Ekstraklasa_shot
      library(jsonlite)
      
      # Hvis Ekstraklasa_shot er en liste (fra MongoDB), flatten den
      Ekstraklasa_shot <- as.data.frame(flatten(Ekstraklasa_shot))
      
    }
    
    # Stats for Top 5 hold i Ekstraklasa
    
    # Skudpræcision
    {
      # Vi definere vores top 5 hold
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Vi bruger table function til at tælle succesfulde afleveringer
      Ekstraklasa_top5_Shot_acc <- Ekstraklasa_shot %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa)
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_top5_Shot_acc <- Ekstraklasa_shot %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shot_on_target = sum(shot.onTarget, na.rm = TRUE),
          accuracy = round(shot_on_target / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
      # ggplot
      # Ggplot
      # Kombiner data
      Ekstraklasa_shot_acc <- bind_rows(
        Ekstraklasa_top5_Shot_acc %>% mutate(Group = "Top 5"),
        Ekstraklasa_bot5_Shot_acc %>% mutate(Group = "Bottom 5")
      )
      
      # Lav barplot med ggplot
      ggplot(Ekstraklasa_shot_acc, aes(x = team.name, y = total_shots, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = paste0(accuracy * 100, "%")), vjust = -0.5, size = 5) +
        scale_y_continuous(limits = c(0, max(Ekstraklasa_shot_acc$total_shots) + 20)) +
        labs(title = "Top5 hold og bund5 hold ligner hinanden",
             x = "Hold",
             y = "Total Skud",
             fill = "Gruppe") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    
    # Skudpræcision efter 80. minut
    {
      # Vi definere vores top 5 hold
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn skudpræcision og arranger efter ranking
      Ekstraklasa_top5_Shot_acc_80 <- Ekstraklasa_shot %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        filter(minute > 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shot_on_target = sum(shot.onTarget, na.rm = TRUE),
          accuracy = round(shot_on_target / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # goal før og efter 80. minut  
    {
      # goal før 80. min
      # Vi definere vores top 5 hold
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_top5_goal <- Ekstraklasa_shot %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        filter(minute < 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shotisGoal = sum(shot.isGoal, na.rm = TRUE),
          accuracy = round(shotisGoal / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
      # goal efter 80. minut
      Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_top5_goal_80 <- Ekstraklasa_shot %>% 
        filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
        filter(minute > 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shotisGoal = sum(shot.isGoal, na.rm = TRUE),
          accuracy = round(shotisGoal / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
    
      # ggplot 
      library(ggplot2)
      library(dplyr)
      
      # Kombiner xG data for top5 og bund5 hold før og efter 80. minut
      Ekstraklasa_goal <- bind_rows(
        Ekstraklasa_top5_goal %>% mutate(Time = "Before 80 min", Group = "Top 5"),
        Ekstraklasa_top5_goal_80 %>% mutate(Time = "After 80 min", Group = "Top 5"),
        Ekstraklasa_bot5_goal %>% mutate(Time = "Before 80 min", Group = "Bottom 5"),
        Ekstraklasa_bot5_goal_80 %>% mutate(Time = "After 80 min", Group = "Bottom 5")
      )
      
      # Beregn den samlede antal mål for hvert hold
      Ekstraklasa_goal <- Ekstraklasa_goal %>%
        group_by(team.name) %>%
        mutate(total_goals = sum(shotisGoal),
               goal_percentage = round((shotisGoal / total_goals) * 100, 1))
      
      # Lav barplot med ggplot
      ggplot(Ekstraklasa_goal, aes(x = team.name, y = shotisGoal, fill = Time)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = paste0(goal_percentage, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
        labs(title = "Mål scoret før og efter 80. minut for Top og Bund 5 Hold",
             x = "Hold",
             y = "Mål scoret",
             fill = "Tidspunkt") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      }
  
  # xg før og efter 80.minut
  {
    # xG før 80. min
    # Vi definere vores top 5 hold
    Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn gennemsnitlig xG før 80. minut
    Ekstraklasa_top5_xG <- Ekstraklasa_shot %>% 
      filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
      filter(minute < 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
      arrange(team.name)
    
    # xG efter 80. minut
    Top5_Hold_Ekstraklasa <- Ekstraklasa_top5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
    Ekstraklasa_top5_xG_80 <- Ekstraklasa_shot %>% 
      filter(team.name %in% Top5_Hold_Ekstraklasa) %>% 
      filter(minute > 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
      mutate(team.name = factor(team.name, levels = Top5_Hold_Ekstraklasa)) %>%
      arrange(team.name)
    
    # ggplot
    library(ggplot2)
    library(dplyr)
    
    # Kombiner xG data for top5 og bund5 hold før og efter 80. minut
    Ekstraklasa_xG <- bind_rows(
      Ekstraklasa_top5_xG %>% mutate(Time = "After 80 min", Group = "Top 5"),
      Ekstraklasa_top5_xG_80 %>% mutate(Time = "Before 80 min", Group = "Top 5"),
      Ekstraklasa_bot5_xG %>% mutate(Time = "After 80 min", Group = "Bottom 5"),
      Ekstraklasa_bot5_xG_80 %>% mutate(Time = "Before 80 min", Group = "Bottom 5")
    )
    
    # Beregn den samlede xG for hvert hold
    Ekstraklasa_xG <- Ekstraklasa_xG %>%
      group_by(team.name) %>%
      mutate(total_xG = sum(avg_xG),
             xG_percentage = round((avg_xG / total_xG) * 100, 1))
    
    # Lav barplot med ggplot
    ggplot(Ekstraklasa_xG, aes(x = team.name, y = avg_xG, fill = Time)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      geom_text(aes(label = paste0(xG_percentage, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
      scale_y_continuous(limits = c(0, max(Eredivisie_xG$avg_xG) + 0.2)) +
      labs(title = "Gennemsnitlig xG før og efter 80. minut for Top og Bund 5 Hold",
           x = "Hold",
           y = "Gennemsnitlig xG",
           fill = "Tidspunkt") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
    # Stats for Bottom 5 hold i Ekstraklasa
    
    # Skudpræcision
    {
      # Vi definere vores bot 5 hold
      Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_bot5_Shot_acc <- Ekstraklasa_shot %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shot_on_target = sum(shot.onTarget, na.rm = TRUE),
          accuracy = round(shot_on_target / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
    }
    
    # goal før og efter 80. minut  
    {
      # goal før 80. min
      # Vi definere vores bot 5 hold
      Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_bot5_goal <- Ekstraklasa_shot %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        filter(minute < 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shotisGoal = sum(shot.isGoal, na.rm = TRUE),
          accuracy = round(shotisGoal / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
      
      # goal efter 80. minut
      
      # Vi definere vores bot 5 hold
      Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
      
      # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
      Ekstraklasa_bot5_goal_80 <- Ekstraklasa_shot %>% 
        filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
        filter(minute > 80) %>% 
        group_by(team.name) %>% 
        summarise(
          total_shots = n(),
          shotisGoal = sum(shot.isGoal, na.rm = TRUE),
          accuracy = round(shotisGoal / total_shots, 2)
        ) %>%
        # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
        mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
        arrange(team.name)
    }
    
  # xG før og efter 80. minut
  {
    # xG før 80. min
    # Vi definere vores top 5 hold
    Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn gennemsnitlig xG før 80. minut
    Ekstraklasa_bot5_xG <- Ekstraklasa_shot %>% 
      filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
      filter(minute < 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
      arrange(team.name)
    
    # xG efter 80. minut
    Bot5_Hold_Ekstraklasa <- Ekstraklasa_bot5$Team
    
    # Filtrer pass-data for kun at inkludere de top5 hold, beregn afleveringspræcisionen og arranger efter ranking
    Ekstraklasa_bot5_xG_80 <- Ekstraklasa_shot %>% 
      filter(team.name %in% Bot5_Hold_Ekstraklasa) %>% 
      filter(minute > 80) %>% 
      group_by(team.name) %>% 
      summarise(
        avg_xG = round(mean(shot.xg, na.rm = TRUE),2),  # Beregn gennemsnitlig xG
        total_shots = n(),
        shotisGoal = sum(shot.isGoal, na.rm = TRUE),
        accuracy = round(shotisGoal / total_shots, 2)
      ) %>%
      # Gør team-navnet til en faktor med niveauer i den rækkefølge, du har defineret
      mutate(team.name = factor(team.name, levels = Bot5_Hold_Ekstraklasa)) %>%
      arrange(team.name)
  }
  
    # Skudposition på mål kontra ikke mål
    {
      # Vi starter med at finde alle rækker, som giver en assist 
      library(dplyr)
      library(purrr)
      
      # Shot is Goal
      Ekstraklasa_shotIsGoal <- Ekstraklasa_shot %>%
        filter(shot.isGoal == TRUE)
      
      # Nu finder vi x og y på et mål
      Ekstraklasa_shotIsGoal_mean <- data.frame(
        location.x = mean(Ekstraklasa_shotIsGoal$location.x),
        location.y = mean(Ekstraklasa_shotIsGoal$location.y)
      )
      
      
      
      # Shot is not Goal
      Ekstraklasa_shotIsNotGoal <- Ekstraklasa_shot %>%
        filter(shot.isGoal == FALSE)
      
      Ekstraklasa_shotIsNotGoal_mean <- data.frame(
        location.x = mean(Ekstraklasa_shotIsNotGoal$location.x),
        location.y = mean(Ekstraklasa_shotIsNotGoal$location.y)
      )
      
    }
    
    # Målmaskiner på positionsniveau
    {
      # Opret en ny kolonne 'position_group' baseret på player.position
      Ekstraklasa_shotIsGoal_pos <- Ekstraklasa_shotIsGoal %>%
        mutate(position_group = case_when(
          player.position %in% Goalkeeper ~ "Goalkeeper",
          player.position %in% Defender   ~ "Defender",
          player.position %in% Midfielder ~ "Midfielder",
          player.position %in% Attacker   ~ "Attacker",
          TRUE ~ "Other"  # Eventuelle positioner, der ikke matcher nogen af de definerede grupper
        ))
      
      # Gruppér efter position_group og tæl antallet af assists
      Ekstraklasa_shotIsGoal_pos_count <- Ekstraklasa_shotIsGoal_pos %>%
        group_by(position_group) %>%
        summarise(antal_mål = n())
    }
    
    # Målmaskiner på spillerniveau
    {
      # Data for topscorerne i Ekstraklasa
      Ekstraklasa_shotIsGoal_player_count <- Ekstraklasa_shotIsGoal %>% 
        group_by(player.name, team.name) %>% 
        summarise(goals_scored = n(), .groups = "drop") %>% 
        arrange(desc(goals_scored)) %>%
        slice_head(n = 10)  # Behold kun de 10 øverste
      
      # Lav liggende barplot med ggplot
      ggplot(Ekstraklasa_shotIsGoal_player_count, aes(x = reorder(player.name, goals_scored), y = goals_scored, fill = team.name)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        geom_text(aes(label = goals_scored), hjust = -0.2, size = 5) +
        coord_flip() +
        labs(title = "Topscorere i Ekstraklasa",
             x = "Spiller",
             y = "Antal mål",
             fill = "Hold") +
        theme_minimal()
      
    }  
    
  }
  
  # Shiny app
  {
    # Shiny app
    library(shiny)
    library(ggplot2)
    library(dplyr)
    library(scales)
    library(DT)
    
    ui_Ekstraklasa_shot <- fluidPage(
      titlePanel("Skudstatistik"),  # En lettere overskrift end før
      sidebarLayout(
        sidebarPanel(
          selectInput("plotType", "Vælg plot:",
                      choices = list(
                        "Top 5: Skudpræcision" = "top_shot",
                        "Top 5: Skudpræcision efter 80. minut" = "top_shot_80",
                        "Top 5: xG før 80. minut" = "top_xG",
                        "Top 5: xG efter 80. minut" = "top_xG_80",
                        "Bot 5: Skudpræcision" = "bot_shot",
                        "Bot 5: xG før 80. minut" = "bot_xG",
                        "Bot 5: xG efter 80. minut" = "bot_xG_80",
                        "Skudposition: Mål vs. Ikke mål" = "shot_position",
                        "Målmaskiner (positionsniveau)" = "shot_pos_count",
                        "Målmaskiner (spillerniveau)" = "shot_player"
                      ))
        ),
        mainPanel(
          uiOutput("selectedUI")  # Dynamisk UI: viser enten plot eller tabel
        )
      )
    )
    
    server_Ekstraklasa_shot <- function(input, output, session) {
      
      # Dynamisk UI: hvis "shot_player" vælges, vises tabellen, ellers et plot
      output$selectedUI <- renderUI({
        if(input$plotType == "shot_player"){
          DT::dataTableOutput("shot_table")
        } else {
          plotOutput("selectedPlot")
        }
      })
      
      output$selectedPlot <- renderPlot({
        if(input$plotType == "top_shot") {
          ggplot(Ekstraklasa_top5_Shot_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - Skudpræcision", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "top_shot_80") {
          ggplot(Ekstraklasa_top5_Shot_acc_80, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - Skudpræcision efter 80. minut", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "top_xG") {
          ggplot(Ekstraklasa_top5_xG, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - xG før 80. minut", x = "Hold", y = "xG") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "top_xG_80") {
          ggplot(Ekstraklasa_top5_xG_80, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Top 5 hold - xG efter 80. minut", x = "Hold", y = "xG") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "bot_shot") {
          ggplot(Ekstraklasa_bot5_Shot_acc, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Bot 5 hold - Skudpræcision", x = "Hold", y = "Præcision") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "bot_xG") {
          ggplot(Ekstraklasa_bot5_xG, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Bot 5 hold - xG før 80. minut", x = "Hold", y = "xG") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "bot_xG_80") {
          ggplot(Ekstraklasa_bot5_xG_80, aes(x = team.name, y = accuracy, fill = team.name)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = percent(accuracy)), vjust = -0.5) +
            labs(title = "Bot 5 hold - xG efter 80. minut", x = "Hold", y = "xG") +
            theme_minimal() +
            theme(legend.position = "none")
          
        } else if(input$plotType == "shot_position") {
          ggplot(Ekstraklasa_shot, aes(x = location.x, y = location.y, color = shot.isGoal)) +
            geom_jitter(alpha = 0.5) +
            labs(title = "Skudposition: Mål vs. Ikke mål", x = "Location X", y = "Location Y") +
            theme_minimal()
          
        } else if(input$plotType == "shot_pos_count") {
          ggplot(Ekstraklasa_shotIsGoal_pos_count, 
                 aes(x = reorder(position_group, -antal_assists), y = antal_assists, fill = position_group)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = antal_assists), vjust = -0.5) +
            labs(title = "Målmaskiner på positionsniveau", x = "Position", y = "Antal mål") +
            theme_minimal() +
            theme(legend.position = "none")
        }
      })
      
      output$shot_table <- DT::renderDataTable({
        # Top 10 målmaskiner på spillerniveau som en scrollable tabel
        top_players <- Ekstraklasa_shotIsGoal_player_count %>% 
          top_n(10, wt = Ekstraklasa_shotIsGoal)
        
        DT::datatable(top_players, options = list(
          scrollY = "400px",
          paging = FALSE,
          searching = FALSE,
          dom = 't'
        ))
      })
    }
    
    shinyApp(ui_Ekstraklasa_shot, server_Ekstraklasa_shot)
    
  }
    
    
  }
  













