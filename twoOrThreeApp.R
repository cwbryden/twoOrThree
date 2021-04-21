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
  
 shot_input <- reactive(input$shots)
 player <- reactive(input$player)

 score <- function(shot_input, player){

   shot2 <- c(2, 0)
   shot3 <- c(3, 0)

   kd2_prob <- c(data$twoptmake[which(data$X1==player)], data$twoptmiss[which(data$X1==player)])
   kd3_prob <- c(data$threeptmake[which(data$X1==player)], data$threeptmiss[which(data$X1==player)])


   kd2samp <- sample(shot2, size = shot_input, replace = TRUE, prob = kd2_prob)
   kd3samp <- sample(shot3, size = shot_input, replace = TRUE, prob = kd3_prob)

   kd2cumulative <- cumsum(kd2samp)
   kd3cumulative <- cumsum(kd3samp)

   # kd2cumulative
   # kd3cumulative

   shot_attempt <- seq(1, number_of_shots, by = 1)

   kd_df <- data.frame(shot_attempt, kd2cumulative, kd3cumulative)
   #kd_df
 }
}
   output$plot <- renderPlot({
       ggplot()+
       geom_line(data = kd_df, aes(x = kd_df$shot_attempt,
                                   y = kd_df$kd2cumulative),
                 col = "red") +
       geom_line(data = kd_df, aes(x = kd_df$shot_attempt,
                                   y = kd_df$kd3cumulative),
                 col = "blue") +
       labs(title = "2 Pointer v. 3 Pointer",
            subtitle = "By 2020-21 Shooting Percentages",
            y = "Points Scored",
            x = "Shots Attempted")
     theme_minimal()
   })

    

# Run the application 
shinyApp(ui = ui, server = server)
