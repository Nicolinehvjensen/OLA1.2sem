###OLA1 OPGAVE 4 - SHINY

# OPG 4.1 - Skud og spillere 
#Shiny webapp med barplot and spillers samlede målscore og XG-sum med regulering af antal af spillere

#Dataindhentning 

library(mongolite)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

mongo_connection <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
      
#Hent kun skud-events
query <- '{"type.primary": "shot"}'
      
# Vælger de relevante felter
fields <- '{"player.name": 1, "shot.isGoal": 1, "shot.xg": 1, "_id": 0}'
      
shotdata <- mongo_connection$find(query, fields)

#Unnester

shotdataflat <- shotdata %>%
  unnest_wider(player, names_sep = "_") %>%  
  unnest_wider(shot, names_sep = "_")  

#ShinyApp

#Total målscore og xG per spiller
dfsummary <- shotdataflat %>%
  group_by(player_name) %>%
  summarise(
    total_goals = sum(as.numeric(shot_isGoal), na.rm = TRUE),  
    total_xG = sum(shot_xg, na.rm = TRUE)  
  ) %>%
  arrange(desc(total_goals))  

# UI 
ui <- fluidPage(
  titlePanel("Spilleres XG vs. shotisGoal"),
  sidebarLayout(
    sidebarPanel(
      numericInput("min_goals", "Flere mål end ..", 
                   min = min(dfsummary$total_goals, na.rm = TRUE), 
                   max = max(dfsummary$total_goals, na.rm = TRUE), 
                   value = 5)
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server 
server <- function(input, output) {
  output$barPlot <- renderPlot({
    filtered_players <- dfsummary %>%
      filter(total_goals >= input$min_goals)
    
    #Barplot
    ggplot(filtered_players, aes(x = reorder(player_name, total_goals), y = total_goals)) +
      geom_bar(stat = "identity", fill = "gray30") +  
      geom_point(aes(y = total_xG, color = "xG sum"), size = 3) +  
      scale_color_manual(name = "", values = c("xG sum" = "red")) +  
      coord_flip() +
      labs(title = "Top spillere efter målscore", 
           x = "Spillere", y = "Antal mål") +
      theme_minimal()
  })
}

# Kør appen
shinyApp(ui = ui, server = server)

