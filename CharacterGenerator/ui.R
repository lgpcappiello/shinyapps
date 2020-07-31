library(shiny)
library(DT)

#rsconnect::deployApp('~/Documents/shinyApps/CharacterGenerator')

fluidPage(
  titlePanel("Character Generator"),
  sidebarLayout(
    sidebarPanel(
      h4("Change seed to refresh character stats."),
      numericInput("seed", "Seed:", 
                   value  = sample(1:1000, 1)),
      a('Home', href='http://lgpcappiello.github.io'),
      br(),
      a('Character Generator', href='https://lgpcappiello.shinyapps.io/charactergenerator/')
    ),
    
    mainPanel(
      h2("Character Stats", align='center'),
      tabsetPanel(
        tabPanel("Player Stats", tableOutput("stats"),
                 br(), br(),
                 p("Character stats are based on 4d6 with the lowest roll dropped.", 
                  strong("Player Stats"), "shows the rolls assigned (in order) to character stats. 
                  Stat values may also be pulled from the list and assigned to stats as desired. 
                  The stats total is shown at the bottom and may be used for party balancing purposes.", 
                  strong("Class Recs"), "gives some class and race ideas based on player stats.
                  Select", strong("All Rolls"), "to see each set of 4d6 rolls."),
                 br(),
                  p(em("Note:"), "the seed sets the random number generator used to simulate rolls of the 
                    dice. The same seed will always yield the same rolls."),),
        tabPanel("Class Recs", textOutput("raceclass"),
                 br(), br(),
                 p(em("Note:"), "this applet pulls recommendations based on the highest two stats. The third 
                  highest value is included only when there is no good recommendation for the highest two 
                  alone. Currently, it does not have support for the possibility that the second and third 
                  highest stats might be equal.")),
        tabPanel("All Rolls", tableOutput("rolls"))
      ),
    )
    
  )
)