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
    set.seed(0)
    output$distPlot <- renderPlot({

        # generate samples based on input$n from ui.R
        if(input$dist == "norm"){
            samples <- sapply(rep(input$n, 1000), rnorm, input$mu, input$sig, simplify='array')
            mu <- input$mu
            sig <- input$sig/sqrt(input$n)
        }else if(input$dist == "expo"){
            samples <- sapply(rep(input$n, 1000), rexp, input$rate, simplify='array')
            mu <- 1/input$rate
            sig <- 1/(input$rate*sqrt(input$n))
        }else if(input$dist == "chisq"){
            samples <- sapply(rep(input$n, 1000), rchisq, input$df, simplify='array')
            mu <- input$df
            sig <- sqrt(2*input$df/input$n)
        }else if(input$dist == "fdist"){
            samples <- sapply(rep(input$n, 1000), rf, input$df1, input$df2, simplify='array')
            mu <- input$df2/(input$df2-2)
            sig <- sqrt((2*input$df2^2*(input$df1+input$df2-2))/(input$df1*(input$df2-2)^2*(input$df2-4)*input$n))
        }else if(input$dist == "binom"){
            samples <- sapply(rep(input$n, 1000), rbinom, input$ntrials, input$prob, simplify='array')
            mu <- input$ntrials*input$prob
            sig <- sqrt((input$ntrials*input$prob*(1-input$prob))/input$n)
        }
        
        means <- colMeans(samples)
        meandat <- as.data.frame(means)
        
        # ggplot(meandat, aes(x = means)) +
        #     geom_histogram(aes(y =..density..),
        #                    breaks = seq(min(means), max(means), 0.1),
        #                    colour = "black",
        #                    fill = "white") +
        #     stat_function(fun = dnorm, args = list(mean = mu, sd = sd(df$PF)))
        ggplot(meandat,
               aes(x = means),
               environment=environment()) +
            theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            geom_histogram(aes(y = ..density..),
                           bins=20,
                           colour="black",
                           fill="lightblue") +
            stat_function(fun = dnorm,
                          args = list(mean = mu,
                                      sd = sig)
            )

        # draw the histogram
        #hist(means, col = 'darkgray', border = 'white')

    })
    output$text1 <- renderText({
        if(input$dist == "norm"){
            mu <- input$mu
            sig <- input$sig/sqrt(input$n)
        }else if(input$dist == "expo"){
            mu <- 1/input$rate
            sig <- 1/(input$rate*sqrt(input$n))
        }else if(input$dist == "chisq"){
            mu <- input$df
            sig <- sqrt(2*input$df/input$n)
        }else if(input$dist == "fdist"){
            mu <- input$df2/(input$df2-2)
            sig <- sqrt((2*input$df2^2*(input$df1+input$df2-2))/(input$df1*(input$df2-2)^2*(input$df2-4)*input$n))
        }else if(input$dist == "binom"){
            mu <- input$ntrials*input$prob
            sig <- sqrt((input$ntrials*input$prob*(1-input$prob))/input$n)
        }
        paste("Histogram of means from from 1000 simulated samples, overlaid with the asymptotic Normal(", round(mu,3), ",", round(sig,3), ") distribution.")
    })

})
