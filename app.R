library(shiny)

ui <- bootstrapPage(
  titlePanel("When does the Central Limit Theorem apply?"),
  sidebarPanel(
    numericInput(inputId = 'nx', label = 'Uniform random number population size', 
                 value = 1000),
    sliderInput(inputId = 'sampsize',
                label = "Choose the sample size",
                value = 100, min = 1, max = 500),
    sliderInput(inputId = 'nsamp', label='Choose number of samples',
                value = 100, min=1, max = 500),
    radioButtons("dist", "Distribution type:",
                 c("Normal" = "norm",
                   "Uniform" = "unif",
                   "Log-normal" = "lnorm",
                   "Exponential" = "exp"))
  ), 
  mainPanel(
    fluidRow(
      column(6, plotOutput(outputId = "hist")), 
      column(6, plotOutput(outputId = "pop"))
    )
    
  )
)

server <- function(input, output, session) {
  observeEvent(input$nx, {
    updateSliderInput(session, "sampsize", max = input$nx)
  })
  
  dist <- reactive( {switch(input$dist,
                            norm = rnorm,
                            unif = runif,
                            lnorm = rlnorm,
                            exp = rexp,
                            rnorm)} )
  x <- reactive( {dist()(input$nx)} )
  output$hist <- renderPlot({ 
    means <- c()
    for (i in 1:input$nsamp){
      samp <- sample(x(), input$sampsize)
      means[i] <- mean(samp)
    }
    
    hist(means, col='skyblue3', 
         xlab='mean of x', freq=FALSE, main='Distribution of Means')
    
  }, width=350, height=350)
  output$pop <- renderPlot( {
    hist(dist()(x()), xlab="x", main="Population Distribution", 
         col='skyblue3')
  }, width = 350, height = 350)
}

shinyApp(ui=ui, server = server)
