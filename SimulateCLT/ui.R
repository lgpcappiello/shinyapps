#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Simulate The Central Limit Theorem"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                         "Sample Size:",
                         min = 2,
                         max = 1000,
                         value = 30),
            selectInput("dist", "Distribution:", 
                        c("Normal" = "norm",
                          "Exponential" = "expo",
                          "Chi-Square" = "chisq", 
                          "F" = "fdist",
                          "Binomial" = 'binom')),
            conditionalPanel(
                condition = "input.dist == 'norm'",
                numericInput("mu",
                             "Mean (mu):",
                             value = 0),
                numericInput("sig",
                             "Standard Deviation (sigma):",
                             value = 1,
                             min = 0)
                ),
            conditionalPanel(
                condition = "input.dist == 'expo'",
                numericInput("rate",
                             "Rate (1/lambda):",
                             value = 1,
                             min = 0),
            ),
            conditionalPanel(
                condition = "input.dist == 'chisq'",
                numericInput("df",
                             "Degrees of Freedom:",
                             value = 5,
                             min = 1,
                             step = 1),
                
            ),
            conditionalPanel(
                condition = "input.dist == 'fdist'",
                numericInput("df1",
                             "Numerator Degrees of Freedom:",
                             value = 5,
                             min = 1,
                             step = 1),
                numericInput("df2",
                             "Denominator Degrees of Freedom:",
                             value = 6,
                             min = 1,
                             step = 1),
            ),
            conditionalPanel(
                condition = "input.dist == 'binom'",
                numericInput("ntrials",
                             "Number of Trials:",
                             value = 10,
                             min = 1,
                             step = 1),
                sliderInput("prob",
                             "Probability:",
                             min = 0,
                             max = 1,
                             value = 0.1)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            textOutput("text1")
        )
    )
))
