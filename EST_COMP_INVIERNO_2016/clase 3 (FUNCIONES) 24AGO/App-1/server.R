library(shiny)

shinyServer(function(input, output) {
  fun1 <- reactive({
    texto <- paste("aux <- ", input$expresion1)
    # print(texto)
    eval(parse(text=texto))
    # print (aux)
    aux
  })  
  
  output$evaluacion <- renderText({
    #fun1 es un reactiveValue, para llamarlo se llama con parentisis, pero como es una funcion
    #la estoy evaluando en el segundo parentesis
    print(fun1())
    print(input$expresion1)
    (fun1())(input$testFun1)
  })
  
  output$Grafica <- renderPlot({
    x <- seq(input$minimoGraf, input$maximoGraf, length.out = 100)
    #Tarea ver que hace sapply y todo lo apply jejeje
    y <- sapply(x, fun1())
    plot(x, y, type="l", col="blue", main="Gráfica 1")
  })
  
})