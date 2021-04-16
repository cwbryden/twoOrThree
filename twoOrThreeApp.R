#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
                        max = 100000,
                        value = 50),
        
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
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
