#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    set.seed(0)
    output$distPlot <- renderPlot({

        # generate samples based on input$n from ui.R
        if(input$dist == "norm"){
            samples <- sapply(rep(input$n, 1000), rnorm, input$mu, input$sig, simplify='array')
        }else if(input$dist == "expo"){
            samples <- sapply(rep(input$n, 1000), rexp, input$rate, simplify='array')
        }else if(input$dist == "chisq"){
            samples <- sapply(rep(input$n, 1000), rchisq, input$df, simplify='array')
        }else if(input$dist == "fdist"){
            samples <- sapply(rep(input$n, 1000), rf, input$df1, input$df2, simplify='array')
        }else if(input$dist == "binom"){
            samples <- sapply(rep(input$n, 1000), rbinom, input$ntrials, input$prob, simplify='array')
        }
        
        means <- colMeans(samples)
        

        # draw the histogram with the specified number of bins
        hist(means, col = 'darkgray', border = 'white')

    })

})
