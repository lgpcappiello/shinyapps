#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$plot <- renderPlot({
        set.seed(0)
        simbin <- rbinom(1000,input$n, input$p)
        binom.dat <- as.data.frame(simbin)
        scalenorm <- function(x, mean = 0, sd = 1, log = FALSE){
            1000*dnorm(x, mean, sd, log)
        }
        # if(input$target == "between"){
            sub <- simbin[simbin > (input$a-1) & simbin < (input$b+1)]
            subset.dat <- as.data.frame(sub)
        # }else if(input$target == "less"){
        #     sub <- simbin[simbin < (input$a+1)]
        #     subset.dat <- as.data.frame(sub)
        # }else if(input$target == "greater"){
        #     sub <- simbin[simbin > (input$b-1)]
        #     subset.dat <- as.data.frame(sub)
        # }
        mu <- input$n*input$p
        sigma <- sqrt(input$n*input$p*(1-input$p))
        ggplot(binom.dat, 
               aes(x = simbin), 
               environment=environment()) + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) + 
            geom_histogram(binwidth=1, 
                           colour="black", 
                           fill="lightgrey") + 
            scale_x_continuous(breaks=1:input$n) +
            geom_histogram(data=subset.dat, 
                           aes(x = sub), 
                           environment=environment(), 
                           binwidth=1, 
                           color="black", 
                           fill="purple") +
            scale_y_continuous(name = NULL, labels=NULL) +
            stat_function(fun = scalenorm,
                          args = list(mean = mu,
                                      sd = sigma)
            )
    })
    output$binom <- renderText({
        probbin <- round(pbinom(input$b, input$n, input$p)-pbinom(input$a-1, input$n, input$p),4)
        paste("Exact Binomial Probability: P( ", input$a," ≤ X ≤ ", input$b, ") = ", probbin)
    })
    output$normdef <- renderText({
        mu <- input$n*input$p
        sigma <- sqrt(input$n*input$p*(1-input$p))
        probnorm.nc <- round(pnorm(input$b, mu, sigma) - pnorm(input$a, mu, sigma), 4)
        paste("Normal Approximation Without Correction: P( ", input$a," ≤ X ≤ ", input$b, ") = ", probnorm.nc)
    })
    output$normcor <- renderText({
        mu <- input$n*input$p
        sigma <- sqrt(input$n*input$p*(1-input$p))
        probnorm.c <- round(pnorm(input$b+0.5, mu, sigma) - pnorm(input$a-0.5, mu, sigma), 4)
        paste("Normal Approximation With Correction: P( ", input$a," ≤ X ≤ ", input$b, ") = ", probnorm.c)
    })
    
})
