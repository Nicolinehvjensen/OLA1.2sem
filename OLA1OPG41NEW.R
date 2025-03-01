###OLA1 OPGAVE 4 - SHINY

# OPG 4.1 - Skud og spillere 
#Shiny webapp med barplot and spillers samlede målscore og XG-sum med regulering af antal af spillere

#Dataindhentning 

library(shinythemes)
library(mongolite)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

#Forbindelser til mine collections 
mongo_games <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
mongo_matches <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

#Henter skudevents og matchId ind fra games samt team-navn
query <- '{ "type.primary": "shot" }'
fields <- '{ "player.name": 1, "shot.isGoal": 1, "shot.xg": 1, "matchId": 1, "team.name": 1, "_id": 0 }'
skuddata <- mongo_games$find(query, fields)
                             
#Henter kampid, competitionId, seasonId og date fra matches
kampdata <- mongo_matches$find('{}', '{ "_id": 1, "competitionId": 1, "seasonId": 1, "date": 1 }')

# Unnest skuddata og adskiller kolonnenavne 
skuddata_flat <- skuddata %>%
  unnest(team, names_sep = "_") %>%  
  unnest(player, names_sep = "_") %>%  
  unnest(shot, names_sep = "_")  

#Merger skuddata med kampdata baseret på matchid

# Ømdøber _id til matchid i kampdata
kampdata <- kampdata %>%
  rename(matchId = `_id`)

merged_data <- skuddata_flat %>%
  left_join(kampdata, by = "matchId")

#Opretter ligaid og sæsonid
merged_data$liga <- ifelse(merged_data$competitionId == 635, "Holland", "Polen")

merged_data <- merged_data %>%
  mutate(sæson = case_when(
    seasonId %in% c(186215, 187502) ~ "2021/22",
    seasonId %in% c(188088, 188125) ~ "2022/23"
  ))

#ShinyApp

# UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Spilleres samlede XG-sum vs. samlede målscore opdelt pr. liga, sæson og hold"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_league", "Vælg liga:", choices = c("Alle", unique(merged_data$liga))),
      selectInput("selected_season", "Vælg sæson:", choices = c("Alle", unique(merged_data$sæson))),
      selectInput("selected_team", "Vælg hold:", choices = "Alle"),  
      sliderInput("num_players", "Antal spillere:", min = 5, max = 50, value = 20, step = 1)  
    ),
    
    mainPanel(plotOutput("barPlot"))
  )
)

# Server
server <- function(input, output, session) {
  
  #Filtrerer data baseret på valg
  filtered_data <- reactive({
    merged_data %>%
      filter((input$selected_league == "Alle" | liga == input$selected_league),
             (input$selected_season == "Alle" | sæson == input$selected_season))
  })
  
  #Opdaterer hold-dropdown baseret på filtreret data
  observe({
    available_teams <- unique(filtered_data()$team_name)
    updateSelectInput(session, "selected_team",
                      choices = c("Alle", available_teams),
                      selected = ifelse(input$selected_team %in% available_teams, input$selected_team, "Alle"))
  })
  
  #Beregner spillernes mål og XG
  dfsummary_reactive <- reactive({
    filtered_data() %>%
      filter(input$selected_team == "Alle" | team_name == input$selected_team) %>%
      group_by(player_name) %>%
      summarise(total_goals = sum(shot_isGoal, na.rm = TRUE),
                total_xG = sum(shot_xg, na.rm = TRUE)) %>%
      arrange(desc(total_goals)) %>%
      head(input$num_players)  
  })
  
  # Plot
  output$barPlot <- renderPlot({
    ggplot(dfsummary_reactive(), aes(x = reorder(player_name, total_goals), y = total_goals)) +
      geom_bar(stat = "identity", fill = "gray", color = "gray40", width = 0.6) +  
      geom_point(aes(y = total_xG, color = factor("xG sum")), size = 4) +  
      scale_color_manual(name = "", values = c("xG sum" = "seagreen")) +  
      coord_flip() +  
      labs(x = "Spillere", y = "Antal mål") +
      theme_minimal(base_size = 14) +  
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "right",
            axis.text.y = element_text(size = 10, face = "bold", angle = 30, hjust = 1))
  })
}

# Kør appen
shinyApp(ui = ui, server = server)


