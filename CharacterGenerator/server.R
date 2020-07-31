library(shiny)
library(DT)

source('getRec.R')

function(input, output) {
  statroll <- reactive({
    set.seed(input$seed)
    statroll <- sapply(rep(6,6), sample, 4, replace=TRUE)
    statroll
  })
  
  output$rolls <- renderTable({
    rolls <- data.frame(Roll = statroll())
    names(rolls) <- paste("Roll", 1:6)
    rolls
  })
  
  output$stats <- renderTable({
    stats <- rep(NA,6)
    for(i in 1:6){
      stats[i] <- sum(statroll()[-which.min(statroll()[,i]),i])
    }
    stats <- append(stats, sum(stats))
    name <- c("Strength","Dexterity","Constitution","Intelligence","Wisdom","Charisma","Values Total")
    data.frame(Stat=name,Value=stats)
  })
  
  output$raceclass <- renderText({
    stat <- numeric(6)
    for(i in 1:6){
      stat[i] <- sum(statroll()[-which.min(statroll()[,i]),i])
    }
    names(stat) <- c("Strength","Dexterity","Constitution","Intelligence","Wisdom","Charisma")
    sorted <- sort(stat, decreasing = TRUE)
    recs <- getRec(sorted)
    paste("With high ",names(sorted[1])," and ",names(sorted[2]),", 
          you might consider one of the following classes: ",recs,
          sep="")
  })
  
}