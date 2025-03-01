###OLA1 OPGAVE 4 - SHINY

# OPG 4.2 - Afleveringer og spillere 
#Shiny webapp med barplot af metrik fra polske og hollandske afleveringer 

#Opretter et sammensat indeks på "type.primary" og "matchId" i "games"-samlingen
#Indekset gør forespørgsler, der filtrerer på "type.primary" og "matchId" hurtigere

#Dataindhentning 

library(mongolite)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

#Dataindhentning 

games_connection <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
matches_connection <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

query_games <- '{}'
fields_games <- '{"pass.accurate": 1, "player.name": 1, "matchId": 1, "team.name": 1, "_id": 0}'
games_data <- games_connection$find(query_games, fields_games)

query_matches <- '{}'
fields_matches <- '{"_id": 1, "label": 1, "competitionId": 1, "matchId": 1}'
matches_data <- matches_connection$find(query_matches, fields_matches)

#Unnesting 

gamesdataflat <- games_data %>%
  mutate(
    player_name = player$name,   # Ekstraher spillernavn
    pass_accurate = pass$accurate,  # Ekstraher afleveringens præcision
    team_name = team$name         # Ekstraher holdnavn
  ) %>%
  select(matchId, player_name, pass_accurate, team_name)  # Behold kun relevante kolonner

#Merger df 
passesgamesandplayers <- gamesdataflat %>%
  left_join(matches_data, by = c("matchId" = "_id"))

###ShinyApp
# 📌 Opret valgmuligheder for kamp
match_choices <- passesgamesandplayers %>%
  select(matchId, label) %>%
  distinct() %>%
  arrange(label) %>%
  pull(matchId, name = label)

# 📌 UI 
ui <- fluidPage(
  titlePanel("Passes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("matchId", "Vælg kamp:", choices = match_choices)
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# 📌 Server (Backend-funktionalitet)
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    # 📌 Filtrér data for den valgte kamp
    filtered_data <- passesgamesandplayers %>%
      filter(matchId == input$matchId)
    
    # 📌 Beregn afleveringsmetrics per hold
    pass_summary <- filtered_data %>%
      group_by(team_name) %>%
      summarise(
        good = sum(pass_accurate, na.rm = TRUE),  
        inaccurate = sum(pass_accurate == FALSE, na.rm = TRUE),  # Eksplicit tælling af FALSE
        total = good + inaccurate,  # Brug summen af good og inaccurate
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(good, inaccurate, total), names_to = "pass_type", values_to = "vals")
    
    # 📌 Farver til barplot
    colors <- c("good" = "seagreen", "inaccurate" = "indianred", "total" = "lightblue")
    
    # 📌 Lav barplot
    ggplot(pass_summary, aes(x = team_name, y = vals, fill = pass_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = colors, name = "Metrics for Passes") +
      labs(x = "Hold", y = "Antal afleveringer") +
      theme_minimal()
  })
}

# 📌 Kør appen
shinyApp(ui = ui, server = server)
