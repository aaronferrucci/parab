
library(shiny)
library(ggplot2)

ui <- shinyUI(fluidPage(
   fluidRow(
    column(12, h2("Parabolas and Tangent Lines", align="center"))
   ),
   plotOutput("distPlot"),
   hr(),
    fluidRow(
      column(4,
         h5("1) Parameters of the Quadratic"),
         hr(),
         numericInput("param_a", "a", value = 1, step=0.1),
         numericInput("param_b", "b", value = 1, step=0.1),
         numericInput("param_c", "c", value = 1, step=0.1)
      ),
      column(4,
         h5("2) Set a tangent line."),
         hr(),
         sliderInput("tangent_x", "Tangent Line X value", value=0, step=0.1, min = -10, max = 10)
      ),
      column(4,
         hr()
      )
   ),
   fluidRow(
     hr(),
     column(12, 
            h5(a("Source code, github", href="https://github.com/aaronferrucci/mortgage_vs_proptax"))
     )
   )
))

server <- shinyServer(function(input, output) {

   output$distPlot <- renderPlot({
      a <- as.numeric(input$param_a)
      b <- as.numeric(input$param_b)
      c <- as.numeric(input$param_c)
      tangent_x <- as.numeric(input$tangent_x)
      
      vertex_x <- -b / (2 * a) 
      domain <- seq(vertex_x - 5, vertex_x + 5, length.out=300)
      range <- sapply(domain, function(x) a * x^2 + b * x + c)
      df <- data.frame(x = domain, y = range)
      
      tangent_y = a * tangent_x^2 + b * tangent_x + c
      # line_domain = tangent_x + c(-2, 0, 2)
      # line_range = tangent_y + c(-2, 0, 2) * c(2 * a * tangent_x + b)
      line_domain = tangent_x + seq(-2, 2, length.out=100)
      line_range = tangent_y + seq(-2, 2, length.out=100) * c(2 * a * tangent_x + b)
      line <- data.frame(x = line_domain, y = line_range)      
      ggplot(df, aes(x)) +
        geom_line(aes(y = y)) +
        geom_line(data=line, aes(x=x, y=y), color="red") +
        ylab("y") +
        scale_x_continuous(limits = c(-10, 10)) +
        scale_y_continuous(limits = c(-1, 30)) +
        xlab("x")
   })
})

# Run the application
shinyApp(ui = ui, server = server)

