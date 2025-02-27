library(shiny)
library(mongolite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(shinythemes)

###OLA1 OPGAVE 4 - SHINY

# OPG 4.2 - Afleveringer og spillere 
#Shiny webapp med barplot af metrik fra polske og hollandske afleveringer 

#Dataindhentning 

mongo_games <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
mongo_matches <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

#Henter data om alle kampe og afleveringer til lokal filtrereing 
query_games <- '{}'
fields_games <- '{"pass.accurate": 1, "player.name": 1, "matchId": 1, "team.name": 1, "_id": 0}'
games_data <- mongo_games$find(query_games, fields_games)

query_matches <- '{}'
fields_matches <- '{"_id": 1, "label": 1, "competitionId": 1, "matchId": 1, "seasonId": 1}'
matches_data <- mongo_matches$find(query_matches, fields_matches)

# Unnester og merger
gamesdataflat <- games_data %>%
  mutate(
    team_name = team$name,
    player_name = player$name,
    pass_accurate = pass$accurate
  ) %>%
  select(-team, -player, -pass)  

passesgamesandplayers <- gamesdataflat %>%
  left_join(matches_data, by = c("matchId" = "_id"))

#Opretter ligaid og sæsonid
passesgamesandplayers$liga <- ifelse(passesgamesandplayers$competitionId == 635, "Holland", "Polen")

passesgamesandplayers <- passesgamesandplayers %>%
  mutate(sæson = case_when(
    seasonId %in% c(186215, 187502) ~ "2021/22",
    seasonId %in% c(188088, 188125) ~ "2022/23"
  ))

# UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),  
  titlePanel("MongoDB vs. lokal filtrering af afleveringer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("liga", "Vælg liga:", choices = NULL),
      selectInput("team_name", "Vælg hold:", choices = NULL),
      selectInput("matchId", "Vælg kamp:", choices = NULL),
      actionButton("compare", "Sammenlign metoder"),
      verbatimTextOutput("time_output")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server 
server <- function(input, output, session) {  
  
  # Opdater liga-dropdown ved opstart
  observe({
    liga_choices <- unique(passesgamesandplayers$liga)
    updateSelectInput(session, "liga", choices = liga_choices)
  })
  
  # Når en liga vælges -> Opdater hold-dropdown
  observeEvent(input$liga, {
    hold_choices <- passesgamesandplayers %>%
      filter(liga == input$liga) %>%
      pull(team_name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "team_name", choices = hold_choices, selected = NULL)
  })
  
  # Når et hold vælges -> Opdater kamp-dropdown
  observeEvent(input$team_name, {
    kamp_choices <- passesgamesandplayers %>%
      filter(team_name == input$team_name) %>%
      select(matchId, label) %>%
      distinct() %>%
      arrange(label) %>%
      pull(matchId, name = label)
    
    updateSelectInput(session, "matchId", choices = kamp_choices, selected = NULL)
  })
  
  # Når brugeren trykker på sammenlign metoder
  observeEvent(input$compare, { 
    
    match_id <- as.numeric(input$matchId)
    
    #Metode 1: Lokal filtrering
    time_local <- system.time({
      filtered_data_local <- passesgamesandplayers %>%
        filter(matchId == match_id)
    })
    
    #Metode 2: MongoDB-filtrering
    time_mongo <- system.time({
      query <- sprintf('{"matchId": %d}', match_id)
      fields <- '{"pass.accurate": 1, "player.name": 1, "team.name": 1, "_id": 0}'
      filtered_data_mongo <- mongo_games$find(query, fields)
    })
    
    #Sammenligning
    output$time_output <- renderText({ 
      sprintf("Lokal filtrering: %.3f sek | MongoDB forespørgsel: %.3f sek",
              time_local["elapsed"], time_mongo["elapsed"])
    })
    
    # Bruger MongoDB-data til plot
    output$barPlot <- renderPlot({
      
      filtered_data <- filtered_data_mongo %>%
        mutate(
          player_name = player$name,
          pass_accurate = pass$accurate,
          team_name = team$name
        ) %>%
        select(team_name, pass_accurate)
      
      # Beregning af metrics
      pass_summary <- filtered_data %>%
        group_by(team_name) %>%
        summarise(
          good = sum(pass_accurate, na.rm = TRUE),  
          inaccurate = sum(pass_accurate == FALSE, na.rm = TRUE),  
          total = good + inaccurate,  
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(good, inaccurate), names_to = "pass_type", values_to = "vals") %>%
        mutate(percentage = vals / total * 100)  
      
      # Beregn y-positioner for labels (midt i hver sektion)
      pass_summary <- pass_summary %>%
        arrange(team_name, desc(pass_type)) %>%  
        group_by(team_name) %>%
        mutate(y_position = cumsum(vals) - (vals / 2))  
      
      # Stacked barplot med absolutte tal og procenter i midten
      ggplot(pass_summary, aes(x = team_name, y = vals, fill = pass_type)) +
        geom_bar(stat = "identity", position = "stack") +  
        geom_text(aes(label = sprintf("%.0f %%", percentage), y = y_position), 
                  color = "black", size = 6, fontface = "bold") + 
        scale_fill_manual(values = c("good" = "darkseagreen", "inaccurate" = "coral2"), 
                          name = "Afleveringer", labels = c("Præcis", "Upræcis")) +
        labs(title = "Afleveringer pr. hold pr. kamp",
             x = "Hold", y = "Antal afleveringer") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),  
          panel.background = element_rect(fill = "white", color = NA),
          text = element_text(color = "black"),  
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black", face = "bold"),
          plot.title = element_text(size = 18, face = "bold", color = "black"),
          legend.background = element_rect(fill = "white"),  
          legend.key = element_rect(fill = "white"),
          legend.title = element_text(color = "black", face = "bold"),
          legend.text = element_text(color = "black")
        )
    })
  })  
}


shinyApp(ui = ui, server = server)
