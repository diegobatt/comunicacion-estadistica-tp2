#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)

tcl <- function(n, N, distribution) {
    print(n)
    print(N)
    means <- numeric(length=N)
    for(i in seq(N)) {
        means[i] = mean(distribution(n=n))
    }
    print(means)
    return(means)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualizacion TCL"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "distribution_name",
                "Distribucion",
                c("Poisson", "Binomial", "Exponential", "Chi2")
            ),
            sliderInput(
                "n",
                "Cantidad de muestras:",
                min = 10,
                max = 10000,
                value = 10
            ),
            sliderInput(
                "N",
                "Repeticiones:",
                min = 10,
                max = 10000,
                value = 10
            ),
            # numericInput(
            #     "loc",
            #     "Parametro de centralidad",
            #     0
            # ),
            # numericInput(
            #     "scale",
            #     "Parametro de escala",
            #     1
            # ),
            uiOutput("moreControls")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$moreControls <- renderUI({
        name = input$distribution_name
        if (name == "Exponencial") {
            tagList(
                sliderInput("rate", "Taza", 0.001, 1000, 1)
            )
        } else if (name == "Bernoulli") {
            tagList(
                sliderInput("p", "p", 0, 1, 0.5),
                sliderInput("size", "Tiradas", 1, 100, 1)
            )
        } else if (name == "Poisson") {
            tagList(
                sliderInput("lambda", "Lambda", 0.001, 1000, 1)
            )
        } else if (name == "Chi2") {
            tagList(
                sliderInput("df", "Grados de libertad", 1, 1000, 10)
            )
        }
    })
    
    output$distPlot <- renderPlot({
        name = input$distribution_name
        if (name == "Exponencial") {
            rate = if (is.null(input$rate)) 1 else input$rate
            f = partial(rexp, rate=rate)
        } else if (name == "Bernoulli") {
            p = if (is.null(input$p)) 0.5 else input$p
            size = if (is.null(input$size)) 1 else input$size
            f = partial(rbinom, size=size, p=p)
        } else if (name == "Poisson") {
            lambda = if (is.null(input$lambda)) 1 else input$lambda
            f = partial(rpois, lambda=lambda)
        } else if (name == "Chi2") {
            df = if (is.null(input$df)) 10 else input$df
            f = partial(rchisq, rate=df)
        }
        means = tcl(input$n, input$N, f)
        hist(means, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
