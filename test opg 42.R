library(shiny)
library(mongolite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

### OLA1 OPGAVE 4 - SHINY

# Dataindhentning 
mongo_games <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
mongo_matches <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

# Henter data om alle kampe og afleveringer til lokal filtrering 
query_games <- '{}'
fields_games <- '{"pass.accurate": 1, "player.name": 1, "matchId": 1, "team.name": 1, "_id": 0}'
games_data <- mongo_games$find(query_games, fields_games)

query_matches <- '{}'
fields_matches <- '{"_id": 1, "label": 1, "competitionId": 1, "matchId": 1, "seasonId": 1}'  # Tilføj seasonId
matches_data <- mongo_matches$find(query_matches, fields_matches)

# Unnester og merger
gamesdataflat <- games_data %>%
  mutate(
    player_name = player$name,   
    pass_accurate = pass$accurate,  
    team_name = team$name        
  ) %>%
  select(matchId, player_name, pass_accurate, team_name)

passesgamesandplayers <- gamesdataflat %>%
  left_join(matches_data, by = c("matchId" = "_id"))

# Opretter ligaid
passesgamesandplayers$liga <- ifelse(passesgamesandplayers$competitionId == 635, "Holland", "Polen")

# Valgmuligheder for dropdown 
match_choices <- passesgamesandplayers %>%
  select(matchId, label) %>%
  distinct() %>%
  arrange(label) %>%
  pull(matchId, name = label)

# UI
ui <- fluidPage(
  titlePanel("MongoDB vs. Lokal filtrering"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("matchId", "Vælg kamp:", choices = match_choices),
      actionButton("compare", "Sammenlign metoder"),
      verbatimTextOutput("time_output")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server 
server <- function(input, output) {
  
  observeEvent(input$compare, { # Bruger trykker sammenlign metoder 
    
    match_id <- as.numeric(input$matchId)
    
    # Metode 1: Lokal filtrering
    time_local <- system.time({
      filtered_data_local <- passesgamesandplayers %>%
        filter(matchId == match_id)
    })
    
    # Metode 2: MongoDB-filtrering
    time_mongo <- system.time({
      query <- sprintf('{"matchId": %d}', match_id)
      fields <- '{"pass.accurate": 1, "team.name": 1, "_id": 0}'
      filtered_data_mongo <- mongo_games$find(query, fields)
      
      # Converting to a list to avoid the named vector issue with jsonlite
      filtered_data_mongo <- lapply(filtered_data_mongo, as.list)
    })
    
    # Sammenligning
    output$time_output <- renderText({ ### Sammenligner responstid
      sprintf("Lokal filtrering: %.3f sek | MongoDB forespørgsel: %.3f sek",
              time_local["elapsed"], time_mongo["elapsed"])
    })
    
    # Bruger mongodb data til plot 
    output$barPlot <- renderPlot({
      
      filtered_data <- filtered_data_mongo %>%
        mutate(
          player_name = player$name,
          pass_accurate = pass$accurate,
          team_name = team$name
        ) %>%
        select(team_name, pass_accurate)
      
      # Beregninger metrics 
      pass_summary <- filtered_data %>%
        group_by(team_name) %>%
        summarise(
          good = sum(pass_accurate, na.rm = TRUE),  
          inaccurate = sum(pass_accurate == FALSE, na.rm = TRUE),  
          total = good + inaccurate,  
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(good, inaccurate, total), names_to = "pass_type", values_to = "vals")
      
      # Barplot 
      ggplot(pass_summary, aes(x = team_name, y = vals, fill = pass_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("good" = "seagreen", "inaccurate" = "indianred", "total" = "lightblue"), name = "Pass Metrics") +
        labs(x = "Hold", y = "Antal afleveringer") +
        theme_minimal()
    })
    
  })
}

shinyApp(ui = ui, server = server)
