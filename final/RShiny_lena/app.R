source("helper.R")

library(shiny)

ui <- fluidPage(
  headerPanel('Matrix Completion----LENA'),
  sidebarPanel(
    sliderInput(inputId = "miss", 
                label = "Miss Percentage", 
                value = 0, min = 0, max = 1),
  
  sliderInput(inputId = "lambda", 
              label = "Choose a lambda", 
              value = 100, min = 0, max = 1000)
  ),
  mainPanel(
    splitLayout(
      plotOutput('plot1'),
      plotOutput('plot2')
    )
  )
)

server <- function(input, output) {
  
  
  miss <- reactive({sample(1:N)[1:round(input$miss*N)]})
  
  output$plot1 <- renderPlot({
    lena1[miss()]<-NA
    image(z=lena1, col=gray(1:256/256), axes=FALSE)
  })
  output$plot2 <- renderPlot({
    lena1[miss()]<-NA
    lena2 <- SoftImpute.np(lena1, input$lambda, svd.method = 1)[[1]]
    image(z=lena2, col=gray(1:256/256), axes=FALSE)
  })

}

shinyApp(ui = ui, server = server)
