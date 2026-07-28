library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estadistica Computacional RLC"),
  
  # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(),
    # Show a plot of the generated distribution
      mainPanel(
        h2("Aceptación-Rechazo"),
        textInput(inputId="expresion1", label="Escribe una función:",
                  value="funcion(x) 2*x"
      ),
      numericInput("testFun1", "Ingresa un valor", 3),
      textOutput("evaluacion"),
      numericInput("minimoGraf", "xmin", 0),
      numericInput("maximoGraf", "xmax", 10),
      plotOutput("Grafica")
    )
  )  
))