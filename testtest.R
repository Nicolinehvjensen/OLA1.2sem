#WIP

library(shiny)
library(ggplot2)
library(ggsoccer)
library(mongolite)
library(dplyr)

###OLA1 OPGAVE 4 - SHINY

# OPG 4.3 - Skud i en kamp
#Lav en dynamisk webapp vha ggsoccer, hvor man kan vælge en kamp og derpå se et plot af skud og
#hvor fordelingen mellem de to hold vises vha farve. Desuden skal man vha farven kunne se hvilke
#skud der gik i mål. 

#Dataindhentning

# Opretter forbindelse til mongodb 
mongo_conn <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
matches_conn <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

# Henter skuddata
query <- '{
    "$or": [
        {"type.primary": "shot"},
        {"type.secondary": "shot"}
    ]
}'
fields <- '{"matchId": 1, "location": 1, "team": 1, "shot": 1, "type": 1, "_id": 0}'
kamp_data <- mongo_conn$find(query, fields = fields)


# Henter matchinformation
matches_data <- matches_conn$find('{}', fields = '{"_id": 1, "label": 1, "competitionId": 1, "seasonId": 1}')

#Datarensning 

# Omdøbber og merger matchinfo med gamesinfo 
matches_data <- matches_data %>%
  rename(matchId = `_id`, kamp_label = label)

kamp_data_merged <- left_join(kamp_data, matches_data, by = "matchId")

#Unnesting 
kamp_data_merged <- kamp_data_merged %>%
  mutate(
    location_x = location$x,
    location_y = location$y,
    team_name = team$name,
    shot_isGoal = shot$isGoal,
    shot_onTarget = shot$onTarget
  ) %>%
  select(-location, -team, -shot)  

#Opretter ligaid og sæsonid
kamp_data_merged$liga <- ifelse(kamp_data_merged$competitionId == 635, "Holland", "Polen")

# UI
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Skudfordeling i Fodboldkampe"),
  
  fluidRow(
    column(3, 
           wellPanel(
             selectInput("liga", "Vælg Liga:", choices = unique(kamp_data_merged$liga)),
             selectInput("hold", "Vælg Hold:", choices = NULL),
             selectInput("kamp", "Vælg Kamp:", choices = NULL)
           )
    ),
    
    column(9, 
           plotOutput("skudPlot", width = "100%", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Opdater hold baseret på valgt liga
  observeEvent(input$liga, {
    hold_choices <- kamp_data_merged %>%
      filter(liga == input$liga) %>%
      pull(team_name) %>%
      unique()
    
    updateSelectInput(session, "hold", choices = hold_choices)
  })
  
  # Opdater kampe baseret på valgt hold
  observeEvent(input$hold, {
    kamp_choices <- kamp_data_merged %>%
      filter(team_name == input$hold) %>%
      pull(kamp_label) %>%
      unique()
    
    updateSelectInput(session, "kamp", choices = kamp_choices)
  })
  
output$skudPlot <- renderPlot({
  
  req(input$kamp) # Sikrer at en kamp er valgt
  
  data_filtered <- kamp_data_merged %>%
    filter(kamp_label == input$kamp)
  
  # Identificer de to hold i kampen
  hold1 <- unique(data_filtered$team_name)[1]
  hold2 <- unique(data_filtered$team_name)[2]
  
  # Spejlvend x-koordinaterne for ét af holdene (så de skyder mod modsatte mål)
  data_filtered <- data_filtered %>%
    mutate(location_x = ifelse(team_name == hold1, location_x, 100 - location_x))
  
  ggplot(data_filtered, aes(x = location_x, y = location_y, color = team_name, 
                            shape = shot_isGoal, size = shot_isGoal)) +
    annotate_pitch(fill = "seagreen", colour = "white") + # Græsbaggrund
    geom_point(alpha = 0.8) +
    scale_color_manual(values = c("red", "blue"), labels = c(hold1, hold2)) + # Farver til holdene
    scale_shape_manual(values = c(4, 16), labels = c("Ikke mål", "Mål")) +  # 4 = kryds (ikke på mål), 16 = cirkel (på mål)
    scale_size_manual(values = c(2, 4), labels = c("Ikke mål", "Mål")) +    # 2 = ikke mål, 4 = mål
    coord_equal(ratio = 0.6) +
    theme_minimal() +
    ggtitle(paste("Skud i kampen:", input$kamp))
})

# Kør app
shinyApp(ui, server)
