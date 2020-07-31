library(shiny)
library(DT)

#rsconnect::deployApp('~/Documents/shinyApps/BackstoryGenerator')

fluidPage(
  fluidRow(
    column(width = 12,
           tags$h1('Character Backstory Generator'))
  ),
  tabsetPanel(
    tabPanel("Test1",
             fluidRow(
               column(width = 4,
                      textInput(inputId = 'adjective1', label = 'adjective'),
                      textInput('verb1', 'verb ending in "ing"'),
                      textInput('verb2', 'verb ending in "ing"'),
                      textInput('noun1', 'plural noun'),
                      actionButton('submit', 'Submit'),
                      br(),br(),
                      a('Home', href='http://lgpcappiello.github.io'),
                      br(),
                      a('Character Generator', href='https://lgpcappiello.shinyapps.io/charactergenerator/')),
               column(width = 8,
                      tags$h2('Backstory Pt 1'),
                      textOutput('backstory'))
             )
    ),
    tabPanel("Test1",
             fluidRow(
               column(width = 4,
                      textInput(inputId = 'adjective2', label = 'adjective'),
                      textInput('verb3', 'verb ending in "ing"'),
                      textInput('verb4', 'verb ending in "ing"'),
                      textInput('noun2', 'plural noun'),
                      actionButton('submit2', 'Submit'),
                      br(),br(),
                      a('Home', href='http://lgpcappiello.github.io'),
                      br(),
                      a('Character Generator', href='https://lgpcappiello.shinyapps.io/charactergenerator/')),
               column(width = 8,
                      tags$h2('Backstory Pt 1'),
                      textOutput('backstory2'))
             )
    )
  )
)