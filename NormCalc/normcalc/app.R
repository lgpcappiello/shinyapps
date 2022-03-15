#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Normal Distribution Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("mu1",
                        "Mean:",
                        value = 0),
            numericInput("sigma1",
                         "Standard Deviation:",
                         min=0,
                         value = 1),
            selectInput("calc", "Calculate:",
                        c("Nothing" = "none",
                          "Probability" = "pr",
                          "Z Score" = "z")
            ),
            conditionalPanel(
                condition = "input.calc == 'pr'",
                selectInput("prob", "Probability:",
                            c("P(a < X < b)" = "bw",
                              "P(X > a)" = "grtr",
                              "P(X < b)" = "less")
                ),
                conditionalPanel(
                    condition = "input.prob == 'bw' | input.prob=='grtr'",
                    numericInput("a","a:",value=-1)
                ),
                conditionalPanel(
                    condition = "input.prob == 'bw' | input.prob == 'less'",
                    numericInput("b","b:",value=1)
                )
            ),
            conditionalPanel(
                condition = "input.calc == 'z'",
                numericInput("pval",
                             "P(Z < z):",
                             value = 0.5, min=0, max=1, step=0.1),
            ),
            radioButtons("second","Toggle Second Distribution", 
                         choiceNames = list("Off","On"),
                         choiceValues = list("n","y")
            ),
            conditionalPanel(
                 condition = "input.second == 'y'",
                 numericInput("mu2",
                              "Mean:",
                              value = 1),
                 numericInput("sigma2",
                              "Standard Deviation:",
                              value = 2)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"),
           conditionalPanel(
               condition = "input.calc == 'pr'",
               textOutput("prob"),
               textOutput("info")
           ),
           conditionalPanel(
               condition = "input.calc == 'z'",
               textOutput("zscore")
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    scalenorm <- function(x, mean = input$mu2, sd = input$sigma2, log = FALSE){
        1000*dnorm(x, mean, sd, log)
    }
    output$plot <- renderPlot({
        if(input$calc=="pr"){
            limmin <- ifelse(input$prob == "bw" | input$prob == "grtr", input$a, input$mu1-3.5*input$sigma1)
            limmax <- ifelse(input$prob == "bw" | input$prob == "less", input$b, input$mu1+3.5*input$sigma1)
        }else{
            limmax <- ifelse(input$calc == "z", input$mu1 + input$sigma1*round(qnorm(input$pval),4), input$mu1+3.5*input$sigma1)
            limmin <- input$mu1-3.5*input$sigma1
        }
        
        if(input$second == 'y'){
            ggplot(data = data.frame(x = c(min(input$mu1-3.5*input$sigma1,input$mu2-3.5*input$sigma2), 
                                           max(input$mu1+3.5*input$sigma1,input$mu2+3.5*input$sigma2))), 
                   aes(x)) + theme_bw() + 
                stat_function(fun = dnorm, n = 101, args = list(mean = input$mu1, sd = input$sigma1)) + ylab("") +
                scale_y_continuous(breaks = NULL) + 
                stat_function(fun = dnorm, n = 101, args = list(mean = input$mu2, sd = input$sigma2)) +
                geom_area(stat = "function", fun = dnorm, args = list(mean = input$mu2, sd = input$sigma2), 
                          fill = "purple", alpha=0.5,
                          xlim = c(input$mu2-3.5*input$sigma2, input$mu2+3.5*input$sigma2)) + 
                geom_area(stat = "function", fun = dnorm, args = list(mean = input$mu1, sd = input$sigma1), 
                              fill = "darkgreen", alpha=0.5, xlim = c(limmin, limmax))
        }else{
            ggplot(data = data.frame(x = c(input$mu1-3.5*input$sigma1, input$mu1+3.5*input$sigma1)), 
                   aes(x)) + theme_bw() + 
                stat_function(fun = dnorm, n = 101, args = list(mean = input$mu1, sd = input$sigma1)) + ylab("") +
                scale_y_continuous(breaks = NULL) +
                geom_area(stat = "function", fun = dnorm, args = list(mean = input$mu1, sd = input$sigma1), 
                              fill = "darkgreen", alpha=0.5, xlim = c(limmin, limmax))
        }
    })
    output$prob <- renderText({
        if(input$prob == "bw"){
            p <- pnorm(input$b, input$mu1, input$sigma1) - pnorm(input$a, input$mu1, input$sigma1)
            if(input$a < input$b){
                paste("P(",input$a," < X < ", input$b, ") = ", round(p,4))
            }else if(input$a > input$b){
                paste("Probability error: attempt to set b < a.")
            }else{
                paste("Probability error: attempt to set a = b.")
            }
        }else if(input$prob == "grtr"){
            p <- pnorm(input$a, input$mu1, input$sigma1, lower.tail=FALSE)
            paste("P( X >",input$a, ") = ", round(p,4))
        }else{
            p <- pnorm(input$b, input$mu1, input$sigma1, lower.tail=TRUE)
            paste("P( X <",input$b, ") = ", round(p,4))
        }
    })
    output$zscore <- renderText({
        if(0 < input$pval & input$pval < 1){
            paste("P(Z < ", round(qnorm(input$pval),4), ") = ", input$pval)
        }else{
            paste("Probability error: probabilities must be between 0 and 1.")
        }
    })
    output$info <- renderText({
        if(input$a == input$b){
            paste("Note: Setting a=b may break the graphics. Change either value to fix the plot.")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
