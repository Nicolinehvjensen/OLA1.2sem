# 📌 Load nødvendige pakker
library(shiny)
library(ggplot2)
library(mongolite)
library(dplyr)
library(tidyr)
library(tibble)

# 📌 Opret MongoDB-forbindelser
games_connection <- mongo(collection = "games", db = "test", url = "mongodb://localhost:27017")
matches_connection <- mongo(collection = "matches", db = "test", url = "mongodb://localhost:27017")

# 📌 Opret et index på `matchId` for hurtigere søgninger
games_connection$index(add = '{"matchId": 1}')

# 📌 Hent kampinformation fra `matches`
query_matches <- '{}'
fields_matches <- '{"_id": 1, "label": 1, "competitionId": 1, "matchId": 1}'
matches_data <- matches_connection$find(query_matches, fields_matches)

# 📌 UI
ui <- fluidPage(
  titlePanel("Afleveringsanalyse: Hollandske & Polske Ligaer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("matchId", "Vælg kamp:", choices = setNames(matches_data$`_id`, matches_data$label)),
      actionButton("load_df", "Brug lokal DataFrame"),
      actionButton("load_mongo", "Brug MongoDB"),
      verbatimTextOutput("time_output")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# 📌 Server
server <- function(input, output, session) {
  
  selected_data <- reactiveVal(NULL)
  
  # 📌 Hent data fra lokal DataFrame (hele `games`)
  observeEvent(input$load_df, {
    start_time <- Sys.time()
    
    query_games <- '{}'
    fields_games <- '{"pass.accurate": 1, "player.name": 1, "team.name": 1, "matchId": 1, "_id": 0}'
    games_data <- games_connection$find(query_games, fields_games)
    
    # Unnest nested data
    if ("team" %in% colnames(games_data)) {
      games_data <- games_data %>%
        unnest_wider(team, names_sep = "_") %>%
        rename(team_name = team_name)
    }
    if ("player" %in% colnames(games_data)) {
      games_data <- games_data %>%
        unnest_wider(player, names_sep = "_") %>%
        rename(player_name = player_name)
    }
    if ("pass" %in% colnames(games_data)) {
      games_data <- games_data %>%
        unnest_wider(pass, names_sep = "_") %>%
        rename(pass_accurate = pass_accurate)
    }
    
    filtered_data <- games_data %>%
      filter(matchId == as.integer(input$matchId))
    
    selected_data(filtered_data)
    
    end_time <- Sys.time()
    output$time_output <- renderText(paste("Lokal DataFrame tid:", round(difftime(end_time, start_time, units = "secs"), 4), "sek"))
  })
  
  # 📌 Hent data fra MongoDB (kun afleveringer for valgte `matchId`)
  observeEvent(input$load_mongo, {
    start_time <- Sys.time()
    
    match_id <- as.integer(input$matchId)
    query <- sprintf('{"matchId": %d}', match_id)
    fields_games <- '{"pass.accurate": 1, "player.name": 1, "team.name": 1, "matchId": 1, "_id": 0}'
    mongo_result <- games_connection$find(query, fields_games)
    
    # Unnest nested data
    if ("team" %in% colnames(mongo_result)) {
      mongo_result <- mongo_result %>%
        unnest_wider(team, names_sep = "_") %>%
        rename(team_name = team_name)
    }
    if ("player" %in% colnames(mongo_result)) {
      mongo_result <- mongo_result %>%
        unnest_wider(player, names_sep = "_") %>%
        rename(player_name = player_name)
    }
    if ("pass" %in% colnames(mongo_result)) {
      mongo_result <- mongo_result %>%
        unnest_wider(pass, names_sep = "_") %>%
        rename(pass_accurate = pass_accurate)
    }
    
    end_time <- Sys.time()
    
    if (nrow(mongo_result) > 0) {
      selected_data(mongo_result)
      output$time_output <- renderText(paste("MongoDB tid:", round(difftime(end_time, start_time, units = "secs"), 4), "sek"))
    } else {
      output$time_output <- renderText("Ingen data fundet i MongoDB.")
    }
  })
  
  # 📌 Generer barplot
  output$barPlot <- renderPlot({
    data <- selected_data()
    if (is.null(data)) return()
    
    pass_summary <- data %>%
      group_by(team_name) %>%
      summarise(
        good = sum(pass_accurate, na.rm = TRUE),
        inaccurate = sum(pass_accurate == FALSE, na.rm = TRUE),
        total = n(),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = c(good, inaccurate), names_to = "pass_type", values_to = "vals")
    
    # 📌 Lav barplot
    ggplot(pass_summary, aes(x = team_name, y = vals, fill = pass_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("good" = "seagreen", "inaccurate" = "indianred")) +
      labs(title = paste("Passes i kamp", unique(data$matchId)),
           x = "Hold", y = "Antal afleveringer") +
      theme_minimal()
  })
}

# 📌 Kør appen
shinyApp(ui = ui, server = server)

