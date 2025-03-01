### test 2 


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
fields <- '{"matchId": 1, "location.x": 1, "location.y": 1, "team.name": 1, "shot.isGoal": 1, "shot.onTarget": 1, "_id": 0}'
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

kamp_data_merged <- kamp_data_merged %>%
  mutate(sæson = case_when(
    seasonId %in% c(186215, 187502) ~ "2021/22",
    seasonId %in% c(188088, 188125) ~ "2022/23"
  ))

# UI
ui <- fluidPage(
  titlePanel("Skudfordeling i Fodboldkampe"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("liga", "Vælg Liga:", choices = unique(kamp_data_merged$liga)),
      selectInput("hold", "Vælg Hold:", choices = NULL),
      selectInput("kamp", "Vælg Kamp:", choices = NULL)
    ),
    
    mainPanel(
      plotOutput("skudPlot")
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
  
  # Plot af skud
  output$skudPlot <- renderPlot({
    
    req(input$kamp) # Sikrer at en kamp er valgt
    
    kamp_plot_data <- kamp_data_merged %>%
      filter(kamp_label == input$kamp) %>%
      mutate(
        mål = case_when(
          shot_isGoal == TRUE ~ "Mål",          # Hvis skuddet er mål
          shot_onTarget == TRUE ~ "På mål",     # Hvis skuddet er på mål, men ikke mål
          TRUE ~ "Ikke på mål"                  # Hvis det ikke er på mål
        )
      )
    
    # Dynamisk farvetildeling til holdene
    unikke_hold <- unique(kamp_plot_data$team_name)
    hold_farver <- setNames(c("red", "blue", "green", "purple", "orange")[1:length(unikke_hold)], unikke_hold)
    
    # Definerer formater for de tre typer af skud
    skud_shapes <- c("Mål" = 24, "På mål" = 16, "Ikke på mål" = 1) # Trekant, fyldt cirkel, tom cirkel
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, fill = "darkseagreen") + # Bane baggrund
      geom_point(data = kamp_plot_data, aes(x = location_x, y = location_y, 
                                            color = team_name, shape = mål, size = mål), 
                 stroke = 1.2, alpha = 0.9) + # Gør punkterne skarpere
      scale_color_manual(values = hold_farver) + # Farver til holdene
      scale_shape_manual(values = skud_shapes) + # Former til skudforsøg
      scale_size_manual(values = c("Mål" = 4, "På mål" = 3, "Ikke på mål" = 2)) + # Justerede størrelser
      coord_fixed() + # Sørger for, at banen ikke forvrænges
      theme_minimal() +
      labs(title = paste("Skudvisualisering for:", unique(kamp_plot_data$kamp_label)),
           color = "Hold",
           shape = "Skudtype",
           size = "Skudtype") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right"
      )
  })
}  

  
# Kør app
shinyApp(ui, server)
