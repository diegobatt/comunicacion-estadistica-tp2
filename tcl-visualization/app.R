library(shiny)
library(tidyverse)

tcl <- function(n, N, distribution) {
    means <- numeric(length=N)
    for(i in seq(N)) {
        means[i] = mean(distribution(n=n))
    }
    return(means)
}

ui <- fluidPage(

    titlePanel("Visualizacion TCL"),

    sidebarLayout(
        sidebarPanel(
            selectInput(
                "distribution_name",
                "Distribucion",
                c("Poisson", "Binomial", "Exponencial", "Chi2", "Gaussiana")
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
            uiOutput("moreControls")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("qqPlot")
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
        } else if (name == "Binomial") {
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
        } else if (name == "Gaussiana") {
          tagList(
            sliderInput("mu", "Media", -30, 30, 0),
            sliderInput("sigma", "Desvio", 0.1, 50, 1)
          )
        }
    })
    
    output_tcl <- reactive({
      name = input$distribution_name
      if (name == "Exponencial") {
        rate = if (is.null(input$rate)) 1 else input$rate
        f = partial(rexp, rate=rate)
      } else if (name == "Binomial") {
        p = if (is.null(input$p)) 0.5 else input$p
        size = if (is.null(input$size)) 1 else input$size
        f = partial(rbinom, size=size, p=p)
      } else if (name == "Poisson") {
        lambda = if (is.null(input$lambda)) 1 else input$lambda
        f = partial(rpois, lambda=lambda)
      } else if (name == "Chi2") {
        df = if (is.null(input$df)) 10 else input$df
        f = partial(rchisq, df=df)
      } else if (name == "Gaussiana") {
        mu = if (is.null(input$mu)) 0 else input$mu
        sigma = if (is.null(input$sigma)) 0 else input$sigma
        f = partial(rnorm, mean=mu, sd=sigma)
      }
      data.frame(tcl=tcl(input$n, input$N, f))
    })
    
    output$distPlot <- renderPlot({
      output_tcl() %>% ggplot(aes(x=tcl)) +
          geom_histogram(aes(y=..density..), colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666") 
    })
    
    output$qqPlot <- renderPlot({
      output_tcl() %>% ggplot(aes(sample=tcl)) +
        geom_qq() + geom_qq_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
