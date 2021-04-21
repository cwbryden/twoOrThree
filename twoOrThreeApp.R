#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(ggplot2)

data <- read_csv("data.csv")
data$X1 <- c("Kevin Durant",
             "Steph Curry",
             "Damian Lillard",
             "LeBron James",
             "Saddiq Bey",
             "James Harden",
             "Kyrie Irving",
             "Trae Young",
             "Lamelo Ball",
             "Luka Doncic")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Points by Two Or Three by Player"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("shots",
                  "Number of shots:",
                min = 1,
                max = 100,
                value = 20),
      
      selectInput(inputId = "player",
                  label = "What player would you like to simmulate?",
                  choices = c("Kevin Durant",
                              "Steph Curry",
                              "Damian Lillard",
                              "LeBron James",
                              "Saddiq Bey",
                              "James Harden",
                              "Kyrie Irving",
                              "Trae Young",
                              "Lamelo Ball",
                              "Luka Doncic"))),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot"),
      textOutput("selected_var")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shot2 <- c(2, 0)
  shot3 <- c(3, 0)
  
  shot_input <- reactive(input$shots)
  player <- reactive(input$player)
  
  number_of_shots <- shot_input
  
  shot2_prob <- c(0, 0)
  shot3_prob <- c(0, 0)
  
  shot2_samp <- 0
  shot3_samp <- 0
  
  
  shot_attempt <- seq(1, 1000, by = 1)
  
  if(player == "Kevin Durant"){
    shot2_prob <- c(0.576, 0.424)
    shot3_prob <- c(0.448, 0.552)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Kevin Durant 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Steph Curry"){
    shot2_prob <- c(0.58, 0.42)
    shot3_prob <- c(0.422, 0.578)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Steph Curry 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  
  if(player == "Damian Lillard"){
    shot2_prob <- c(0.51, 0.49)
    shot3_prob <- c(0.379, 0.621)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Damian Lillard 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "LeBron James"){
    shot2_prob <- c(0.592, 0.408)
    shot3_prob <- c(0.386, 0.614)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "LeBron James 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Saddiq Bey"){
    shot2_prob <- c(0.473, 0.527)
    shot3_prob <- c(0.386, 0.614)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Saddiq Bey 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "James Harden"){
    shot2_prob <- c(0.548, 0.452)
    shot3_prob <- c(0.361, 0.639)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "James Harden 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Kyrie Irving"){
    shot2_prob <- c(0.575, 0.425)
    shot3_prob <- c(0.398, 0.602)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Kyrie Irving 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Trae Young"){
    shot2_prob <- c(0.472, 0.528)
    shot3_prob <- c(0.358, 0.642)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Trae Young 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Luka Doncic"){
    shot2_prob <- c(0.564, 0.436)
    shot3_prob <- c(0.364, 0.636)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Luka Doncic 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
  
  if(player == "Lamelo Ball"){
    shot2_prob <- c(0.503, 0.497)
    shot3_prob <- c(0.375, 0.625)
    
    for(i in 1:number_of_shots){
      shot2_samp <- sample(shot2, size = number_of_shots, replace = TRUE, prob = shot2_prob)
      shot3_samp <- sample(shot3, size = number_of_shots, replace = TRUE, prob = shot3_prob)
    }
    
    shot2_cumulative <- cumsum(shot2_samp)
    shot3_cumulative <- cumsum(shot3_samp)
    
    player_df <- data.frame(shot_attempt, shot2_cumulative, shot3_cumulative)
    print(player_df)
    
    player_plot <- ggplot()+
      geom_line(data = player_df, aes(x = shot_attempt, 
                                      y = shot2_cumulative,
                                      col = "2 point")) +
      geom_line(data = player_df, aes(x = shot_attempt,
                                      y = shot3_cumulative,
                                      col = "3 point")) +
      labs(title = "Lamelo Ball 2 Pointer v. 3 Pointer",
           subtitle = "By 2020-21 Shooting Percentages",
           y = "Points Scored",
           x = "Shots Attempted") +
      theme_minimal()
    player_plot
    
  }
}
    

# Run the application 
shinyApp(ui = ui, server = server)
