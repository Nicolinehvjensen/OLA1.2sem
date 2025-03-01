library(shiny)
library(ggplot2)
library(ggsoccer)
library(mongolite)
library(dplyr)
library(jsonlite)

### OLA1 OPGAVE 4 - SHINY

# OPG 4.3 - Skud i en kamp
# Lav en dynamisk webapp vha ggsoccer, hvor man kan vælge en kamp og derpå se et plot af skud og
# hvor fordelingen mellem de to hold vises vha farve. Desuden skal man vha farven kunne se hvilke
# skud der gik i mål. 

# Dataindhentning

# Opretter forbindelse til mongodb 
mongo_conn <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
matches_conn <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

# Henter skuddata
query <- '{"type.primary": "shot"}'
fields <- '{"matchId": 1, "location.x": 1, "location.y": 1, "team.name": 1, "shot.isGoal": 1, "_id": 0, "shot.onTarget": 1}'
kamp_data_raw <- mongo_conn$find(query, fields = fields)

# Flader dataene
kamp_data <- flatten(kamp_data_raw)

# Henter matchinformation
matches_data <- matches_conn$find('{}', fields = '{"_id": 1, "label": 1, "competitionId": 1, "seasonId": 1}')

# Datarensning 

# Omdøbber og merger matchinfo med gamesinfo 
matches_data <- matches_data %>%
  rename(matchId = `_id`, kamp_label = label)

kamp_data <- left_join(kamp_data, matches_data, by = "matchId")

#Competition id 
kamp_data$liga <- ifelse(kamp_data$competitionId == 635, "Holland", "Polen")

# Shiny UI

# Unikke kombinationer af label og kampid og fjerner dubletter
kamp_choices <- kamp_data %>%
  select(matchId, kamp_label) %>%
  distinct()  

# Unikke ligaer og hold
liga_choices <- unique(kamp_data$liga)
# Unikke hold (ændret fra kamp_data$team$name til kamp_data$team.name)
hold_choices <- unique(kamp_data$team.name)

ui <- fluidPage(
  titlePanel("Skudvisualisering af fodboldkampe"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("liga", "Vælg liga:", choices = c("Alle", liga_choices)),
      selectInput("hold", "Vælg hold:", choices = c("Alle", hold_choices)),
      selectInput("kamp", "Vælg en kamp:", choices = as.list(setNames(kamp_choices$matchId, kamp_choices$kamp_label))),
      actionButton("compare", "Opdater skud")
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
    
    # Find hjemmehold og udehold
    home_team <- unique(kamp_plot_data$homeTeamId)
    away_team <- unique(kamp_plot_data$awayTeamId)
    
    # Skift x-koordinaterne for hjemmehold (venstre) og udehold (højre)
    kamp_plot_data <- kamp_plot_data %>%
      mutate(
        x = ifelse(team$name == home_team, x, 100 - x)  # Skift x-koordinater for udeholdet
      )
    
    # Skaler x og y, hvis nødvendigt
    kamp_plot_data <- kamp_plot_data %>%
      mutate(
        x = ifelse(x < 0, 0, ifelse(x > 100, 100, x)),
        y = ifelse(y < 0, 0, ifelse(y > 100, 100, y))
      )
    
    # Farvekodning for mål og forsøg
    shape_values <- c("Ja" = 19, "Nej" = 1)
    color_values <- c("Ja" = "green", "Nej" = "red")
    
    # Plot
    ggplot() +
      annotate_pitch(fill = "darkseagreen") +
      geom_point(data = kamp_plot_data, aes(x = x, y = y, shape = faktor(mål), color = faktor(mål)), size = 4) +
      scale_shape_manual(values = shape_values) +
      scale_color_manual(values = color_values) +
      coord_fixed() +
      theme_minimal() +
      labs(title = paste("Skudvisualisering for:", unique(kamp_plot_data$kamp_label)),
           color = "Mål", shape = "Mål") +
      annotate("text", x = 10, y = 50, label = home_team, color = "cyan", size = 6) +
      annotate("text", x = 90, y = 50, label = away_team, color = "red", size = 6)
  })
}

shinyApp(ui, server)
