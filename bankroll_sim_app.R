# Bankroll Management Simulator
# This Shiny app simulates bankroll management strategies for sports betting.
# Adam Wickwire - Bettor Analysis
# 2025 

library(shiny)
library(ggplot2)
library(mc2d)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(reshape2)

american_to_decimal <- function(american_odds) {
  if (american_odds > 0) {
    return(1 + (american_odds / 100))
  } else {
    return(1 + (100 / abs(american_odds)))
  }
}

simulate_bankroll <- function(starting_bankroll, num_bets, ev_min, ev_mode, ev_max, strategy, kelly_frac, pct_risk, odds_min_am, odds_mode_am, odds_max_am, sims = 1000, flat_bet = NULL) {
  results <- matrix(NA, nrow = num_bets, ncol = sims)
  odds_min <- american_to_decimal(odds_min_am)
  odds_mode <- american_to_decimal(odds_mode_am)
  odds_max <- american_to_decimal(odds_max_am)
  
  for (s in 1:sims) {
    bankroll <- starting_bankroll
    odds <- rtriang(num_bets, min = odds_min, mode = odds_mode, max = odds_max)
    evs <- rtriang(num_bets, min = ev_min / 100, mode = ev_mode / 100, max = ev_max / 100)
    
    for (i in 1:num_bets) {
      ev <- evs[i]
      win_prob <- (ev + 1) / odds[i]
      
      if (strategy == "Flat") {
        stake <- flat_bet
      } else if (strategy == "Kelly") {
        b <- odds[i] - 1
        kelly_stake <- ((win_prob * (b + 1)) - 1) / b
        stake <- bankroll * kelly_stake * kelly_frac
      } else if (strategy == "% Risk") {
        stake <- bankroll * (pct_risk / 100)
      }
      
      stake <- max(stake, 0)
      win <- runif(1) < win_prob
      bankroll <- bankroll + if (win) stake * (odds[i] - 1) else -stake
      results[i, s] <- bankroll
    }
  }
  return(results)
}

ui <- navbarPage("Bankroll Simulator App", theme = shinytheme("flatly"),
                 tabPanel("Simulator",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         numericInput("starting_bankroll", "Starting Bankroll ($):", 1000, min = 1),
                                         numericInput("num_bets", "Number of Bets:", 1000, min = 10),
                                         
                                         h4("Expected Value Distribution"),
                                         numericInput("ev_min", "Minimum EV (%):", 1),
                                         numericInput("ev_mode", "Most Likely EV (%):", 5),
                                         numericInput("ev_max", "Maximum EV (%):", 10),
                                         
                                         h4("Bet Sizing Strategy"),
                                         selectInput("strategy", "Strategy:", c("Flat", "Kelly", "% Risk")),
                                         conditionalPanel("input.strategy == 'Flat'",
                                                          numericInput("flat_bet", "Flat Bet Size ($):", 10, min = 1)),
                                         conditionalPanel("input.strategy == 'Kelly'",
                                                          numericInput("kelly_frac", "Kelly Fraction (0-100%):", 100, min = 0, max = 100)),
                                         conditionalPanel("input.strategy == '% Risk'",
                                                          numericInput("pct_risk", "% of Bankroll per Bet:", 1, min = 0, max = 100)),
                                         
                                         h4("Odds Distribution (American)"),
                                         numericInput("odds_min_am", "Minimum Odds:", -200),
                                         numericInput("odds_mode_am", "Most Likely Odds:", 120),
                                         numericInput("odds_max_am", "Maximum Odds:", 200),
                                         
                                         numericInput("sims", "Simulations to Run:", 1000, min = 10),
                                         checkboxInput("show_mean", "Show Mean Trajectory", TRUE),
                                         checkboxInput("show_median", "Show Median Trajectory", TRUE),
                                         
                                         downloadButton("downloadData", "Download CSV"),
                                         actionButton("run_sim", "Run Simulation", class = "btn-primary")
                            ),
                            
                            mainPanel(
                              fluidRow(
                                column(12,
                                       plotOutput("bankrollPlot", height = "400px"),
                                       conditionalPanel(
                                         condition = "output.showSlider",
                                         sliderInput("plot_sample", "Displayed Lines on Chart:", min = 10, max = 1000, value = 100, step = 10)
                                       ),
                                       plotOutput("finalDistribution", height = "300px"),
                                       verbatimTextOutput("summaryText")
                                )
                              )
                            )
                          )
                 ),
                 
                 tabPanel("About",
                          fluidPage(
                            h3("How to Use This App"),
                            p("This app simulates long-term bankroll trajectories using different bet sizing strategies."),
                            p("Input your EV range, odds range, and number of bets, then view simulation results."),
                            p("You can filter how many lines are plotted and overlay summary lines to better understand variance.")
                          )
                 )
)

server <- function(input, output, session) {
  sim_data <- eventReactive(input$run_sim, {
    simulate_bankroll(
      starting_bankroll = input$starting_bankroll,
      num_bets = input$num_bets,
      ev_min = input$ev_min,
      ev_mode = input$ev_mode,
      ev_max = input$ev_max,
      strategy = input$strategy,
      kelly_frac = input$kelly_frac / 100,
      pct_risk = input$pct_risk,
      odds_min_am = input$odds_min_am,
      odds_mode_am = input$odds_mode_am,
      odds_max_am = input$odds_max_am,
      sims = input$sims,
      flat_bet = input$flat_bet
    )
  })
  
  output$bankrollPlot <- renderPlot({
    data <- sim_data()
    if (is.null(data)) return(NULL)
    
    df <- as.data.frame(data)
    sim_cols <- paste0("Sim", 1:ncol(df))
    colnames(df) <- sim_cols
    df$Bet <- 1:nrow(df)
    df_long <- melt(df, id.vars = "Bet", variable.name = "Simulation")
    
    sample_sims <- unique(df_long$Simulation)[1:min(input$plot_sample, length(unique(df_long$Simulation)))]
    df_filtered <- df_long[df_long$Simulation %in% sample_sims, ]
    
    mean_traj <- rowMeans(df[, sim_cols[1:min(input$plot_sample, length(sim_cols))]])
    median_traj <- apply(df[, sim_cols[1:min(input$plot_sample, length(sim_cols))]], 1, median)
    
    p <- ggplot(df_filtered, aes(x = Bet, y = value, group = Simulation)) +
      geom_line(color = "lightblue", alpha = 0.3) +
      labs(title = "Simulated Bankroll Trajectories", x = "Bet Number", y = "Bankroll") +
      theme_minimal()
    
    if (input$show_mean) {
      p <- p + geom_line(data = data.frame(Bet = 1:nrow(data), value = mean_traj),
                         mapping = aes(x = Bet, y = value), inherit.aes = FALSE,
                         color = "darkgreen", size = 1)
    }
    if (input$show_median) {
      p <- p + geom_line(data = data.frame(Bet = 1:nrow(data), value = median_traj),
                         mapping = aes(x = Bet, y = value), inherit.aes = FALSE,
                         color = "darkorange", size = 1)
    }
    
    p
  })
  
  output$showSlider <- reactive({
    !is.null(sim_data())
  })
  outputOptions(output, "showSlider", suspendWhenHidden = FALSE)
  
  output$finalDistribution <- renderPlot({
    data <- sim_data()
    if (is.null(data)) return(NULL)
    finals <- data[nrow(data), ]
    ggplot(data.frame(FinalBankroll = finals), aes(x = FinalBankroll)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.5) +
      geom_density(color = "darkblue", size = 1) +
      labs(title = "Final Bankroll Distribution", x = "Final Bankroll", y = "Density") +
      theme_minimal()
  })
  
  output$summaryText <- renderText({
    data <- sim_data()
    if (is.null(data)) return("")
    finals <- data[nrow(data), ]
    bankruptcies <- sum(finals <= input$starting_bankroll * 0.1)
    losses <- sum(finals < input$starting_bankroll)
    drawdowns <- apply(data, 2, function(x) min(x) / x[1] - 1)
    volatility <- apply(data, 2, sd)
    
    paste0(
      "Median Final Bankroll: $", round(median(finals), 2), "\n",
      "Mean Final Bankroll: $", round(mean(finals), 2), "\n",
      "% Ruin (Bankroll < 10% of start): ", round(100 * bankruptcies / input$sims, 1), "%\n",
      "% Lost Money: ", round(100 * losses / input$sims, 1), "%\n",
      "Avg Max Drawdown: ", round(100 * mean(drawdowns), 1), "%\n",
      "Avg Volatility: $", round(mean(volatility), 2)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0("final_bankrolls_", Sys.Date(), ".csv") },
    content = function(file) {
      data <- sim_data()
      if (!is.null(data)) {
        finals <- data[nrow(data), ]
        write.csv(data.frame(FinalBankroll = finals), file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
