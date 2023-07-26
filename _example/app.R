#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinySlab)

# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
    titlePanel("Old Faithful Geyser Data"),

    verbatimTextOutput("queryText"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {


    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    output$queryText <- renderText({
        query <- parseQueryString(session$clientData$url_search)

        paste("Querystring: ", paste(names(query), query, sep = "=", collapse=", "),sep = "")

    })
}

# Run the application
shinySlab::shinySlabApp(ui = ui, server = server,roles=c("86d24bd7-3a5f-4f70-a864-c8d3560a1d02","08c397d4-b4ab-482e-b354-5a35606f3aee"))
#shinyApp
