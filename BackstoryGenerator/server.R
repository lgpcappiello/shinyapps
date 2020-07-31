library(shiny)
library(DT)

shinyServer(function(input, output) {
  
  sentence <- eventReactive(input$submit, {
    paste0('R-ladies is a ', input$adjective1, ' organization dedicated to ', 
           input$verb1, ' R, and ', input$verb2, ' a network among ', 
           input$noun1, '.')
  })
  sentence2 <- eventReactive(input$submit2, {
    paste0('Test ', input$adjective2, ' organization dedicated to ', 
           input$verb3, ' R, and ', input$verb4, ' a network among ', 
           input$noun2, '.')
  })
  
  output$backstory <- renderText({
    sentence()
  })
  
  output$backstory2 <- renderText({
    sentence2()
  })
})