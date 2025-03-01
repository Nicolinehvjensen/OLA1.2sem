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
query <- '{"type.primary": "shot"}'
fields <- '{"matchId": 1, "location.x": 1, "location.y": 1, "team.name": 1, "shot.isGoal": 1, "_id": 0}'
kamp_data <- mongo_conn$find(query, fields = fields)

# Henter matchinformation
matches_data <- matches_conn$find('{}', fields = '{"_id": 1, "label": 1}')

#Datarensning 

# Omdøbber og merger matchinfo med gamesinfo 
matches_data <- matches_data %>%
  rename(matchId = `_id`, kamp_label = label)

kamp_data <- left_join(kamp_data, matches_data, by = "matchId")

#Unnesting 
kamp_data <- kamp_data %>%
  mutate(
    x = location$x,
    y = location$y,
    hold = team$name,
    mål = ifelse(shot$isGoal, "Ja", "Nej")
  ) %>%
  select(matchId, kamp_label, x, y, hold, mål)

#Shiny 

# UI

# Unikke kombinationer af label og kampid og fjerner dubletter
kamp_choices <- kamp_data %>%
  select(matchId, kamp_label) %>%
  distinct()  

ui <- fluidPage(
  titlePanel("Skudvisualisering af fodboldkampe"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("kamp", "Vælg en kamp:", 
                  choices = setNames(kamp_choices$matchId, kamp_choices$kamp_label))
    ),
    mainPanel(
      plotOutput("shotPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    print(paste("Valgt kamp:", input$kamp)) 
  })
  
  output$shotPlot <- renderPlot({
    req(input$kamp)  
    
    # FILTRÉR DATA FOR VALGT KAMP 
    kamp_plot_data <- kamp_data %>% filter(matchId == input$kamp)
    
    # DYNAMISK FARVEVALG FOR HOLD
    hold_farver <- setNames(rainbow(length(unique(kamp_plot_data$hold))), unique(kamp_plot_data$hold))
    
    # Plot
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, fill = "darkseagreen") +
      geom_point(data = kamp_plot_data, aes(x = x, y = y, color = hold, shape = mål), size = 4) +
      scale_color_manual(values = hold_farver) +
      scale_shape_manual(values = c("Ja" = 19, "Nej" = 1)) +
      coord_fixed() +
      theme_minimal() +
      labs(title = paste("Skudvisualisering for:", unique(kamp_plot_data$kamp_label)),
           color = "Hold",
           shape = "Mål")
  })
}

shinyApp(ui, server)
