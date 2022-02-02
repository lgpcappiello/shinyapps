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
    titlePanel("Normal Approximation to the Binomial Distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Distribution Parameters"),
            numericInput("n",
                        "Sample size, n:",
                        min = 1,
                        value = 10),
            numericInput("p",
                         "Probability, p:",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step=0.1),
            h4("Visualize P(a ≤ X ≤ b)"),
            numericInput("a",
                         "a:",
                         value = 2),
            numericInput("b",
                         "b:",
                         value = 5)
            # selectInput("target", "Probability to Calculate",
            #             c("P(a ≤ X ≤ b)" = "between",
            #               "P(X ≤ a)"="less",
            #               "P(X ≥ b)" = "greater"
            #               )
            #     
            # ),
            # conditionalPanel(
            #     condition = "input.target == 'between'",
            #     numericInput("a",
            #                  "a:",
            #                  value = 2),
            #     numericInput("b",
            #                  "b:",
            #                  value = 5)
            # ),
            # conditionalPanel(
            #     condition = "input.target == 'less'",
            #     numericInput("a",
            #                  "a:",
            #                  value = 3)
            # ),
            # conditionalPanel(
            #     condition = "input.target == 'greater'",
            #     numericInput("b",
            #                  "b:",
            #                  value = 1),
            # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
))
