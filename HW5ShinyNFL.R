# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)

# Load data
team_stats <- read_csv("https://raw.githubusercontent.com/Shahv03/nfl-pca-shiny-app/refs/heads/main/team_stats_2003_2023.csv")

# Data Preparation
tidy_team_stats <- team_stats %>%
  select(year, team, win_loss_perc, points, points_opp, rush_td, rush_yds_per_att,
         pass_yds, pass_td, pass_net_yds_per_att, penalties, penalties_yds,
         score_pct, turnover_pct, exp_pts_tot) %>%
  drop_na()

# Shiny App UI
ui <- fluidPage(
  titlePanel("NFL Team Performance PCA (2003-2023)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = min(tidy_team_stats$year),
                  max = max(tidy_team_stats$year),
                  value = c(min(tidy_team_stats$year), max(tidy_team_stats$year)),
                  sep = ""),
      
      selectInput("color_var", "Color Points By:",
                  choices = c("Year" = "year",
                              "Win-Loss %" = "win_loss_perc",
                              "Team" = "team")),
      
      checkboxGroupInput("selected_teams", "Select Teams to Display:",
                         choices = sort(unique(tidy_team_stats$team)),
                         selected = unique(tidy_team_stats$team)),
      
      helpText("\nPrincipal Component 1: Overall team strength based on scoring, turnovers, and penalties.",
               "\nPrincipal Component 2: Offensive style differences, especially passing vs rushing efficiency.")
    ),
    
    mainPanel(
      plotlyOutput("pcaPlot")
    )
  )
)

# Shiny App Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    tidy_team_stats %>%
      filter(year >= input$year_range[1], year <= input$year_range[2],
             team %in% input$selected_teams)
  })
  
  pca_results <- reactive({
    filtered_data() %>%
      select(-year, -team) %>%
      scale() %>%
      prcomp()
  })
  
  output$pcaPlot <- renderPlotly({
    pca_data <- predict(pca_results(), newdata = filtered_data() %>% select(-year, -team))
    pca_df <- as.data.frame(pca_data[, 1:2])
    pca_df$team <- filtered_data()$team
    pca_df$year <- filtered_data()$year
    pca_df$win_loss_perc <- filtered_data()$win_loss_perc
    
    p <- ggplot(pca_df, aes(x = -PC1, y = -PC2,
                            color = .data[[input$color_var]],
                            text = paste0("Team: ", team,
                                          "<br>Year: ", year,
                                          "<br>Win %: ", win_loss_perc))) +
      geom_point(size = 2) +
      labs(x = "Principal Component 1", y = "Principal Component 2",
           title = "PCA of NFL Team Stats",
           color = input$color_var) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
