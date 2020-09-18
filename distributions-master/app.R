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
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage("Probability Distributions",id = "dist",
                           tabPanel("Normal",
                                    sidebarPanel(selectInput("norm_val", "Value",
                                                             c("Probability"="prob",
                                                               "Quantile"="quant")),
                                                 textInput("norm_z", "Z-Value", 0),
                                                 textInput("qq_z", "Quantile", 0.5),
                                                 textInput("mean", "Mean", 0),
                                                 textInput("sd", "Standard Deviation", 1)),
                                    mainPanel(tabsetPanel(
                                        tabPanel("Value", tableOutput("normTable")),
                                        tabPanel("Plot", plotOutput("normPlot"))
                                    ))
                           ),
                           tabPanel("Binomial",
                                    sidebarPanel(selectInput("bin_val", "Value",
                                                             c("Probability"="prob",
                                                               "Quantile"="quant")),
                                                 textInput("bin_v", "Value", 0),
                                                 textInput("bin_q", "Quantile", 0.5),
                                                 textInput("n", "N", 10),
                                                 textInput("p", "Proportion", 0.5)
                                    ),
                                    mainPanel(tabsetPanel(
                                        tabPanel("Value", tableOutput("binTable")),
                                        tabPanel("Plot", plotOutput("binPlot"))
                                    ))
                           ),
                           tabPanel("Poisson",
                                    sidebarPanel(selectInput("pois_val", "Value",
                                                             c("Probability"="prob",
                                                               "Quantile"="quant")),
                                                 textInput("pois_v", "Value", 0),
                                                 textInput("pois_q", "Quantile", 0.5),
                                                 textInput("rate", "Rate", 1)
                                    ),
                                    mainPanel(tabsetPanel(
                                        tabPanel("Value", tableOutput("poisTable")),
                                        tabPanel("Plot", plotOutput("poisPlot"))
                                    ))
                           ),
                           tabPanel("t",
                                    sidebarPanel(selectInput("t_val", "Value",
                                                             c("Probability"="prob",
                                                               "Quantile"="quant")),
                                                 textInput("t_v", "Value", 0),
                                                 textInput("t_q", "Quantile", 0.5),
                                                 textInput("df","Degrees of Freedom",1)
                                    ),
                                    mainPanel(tabsetPanel(
                                        tabPanel("Value", tableOutput("tTable")),
                                        tabPanel("Plot", plotOutput("tPlot"))
                                    ))
                           )
                )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    normTable <- reactive({
        qq<-as.numeric(input$qq_z)
        zz<-as.numeric(input$norm_z)
        mm<-as.numeric(input$mean)
        ss<-as.numeric(input$sd)
        diff<-abs(mm-zz)
        if (zz < mm){
            greater_zz<-zz+2*diff
        } else {
            greater_zz<-zz
        }

        if (input$norm_val=="prob"){
            normTable <- data.frame(Probability=c("Lower Tail","Upper Tail","Two Tail"),
                                    Value=c(pnorm(zz,mm,ss),pnorm(zz,mm,ss,lower.tail = FALSE),2*pnorm(greater_zz,mm,ss,lower.tail = FALSE)))
        } else {

            normTable <- data.frame(Probability=c("Quantile"),
                                    Value=c(qnorm(qq,mm,ss)))
        }}
    )
    
    binTable <- reactive({
        qq<-as.numeric(input$bin_q)
        vv<-as.numeric(input$bin_v)
        pp<-as.numeric(input$p)
        nn<-as.numeric(input$n)
        if (input$bin_val=="prob"){
            binTable <- data.frame(Probability=c("Probability","Lower Tail","Upper Tail"),
                                    Value=c(dbinom(vv,nn,pp),pbinom(vv,nn,pp),pbinom(vv,nn,pp,lower.tail = FALSE)))
        } else {
            
            binTable <- data.frame(Probability=c("Quantile"),
                                    Value=c(qbinom(qq,nn,pp)))
        }}
    )
    
    poisTable <- reactive({
        qq<-as.numeric(input$pois_q)
        vv<-as.numeric(input$pois_v)
        rr<-as.numeric(input$rate)
        if (input$pois_val=="prob"){
            poisTable <- data.frame(Probability=c("Probability","Lower Tail","Upper Tail"),
                                   Value=c(dpois(vv,rr),ppois(vv,rr),ppois(vv,rr,lower.tail = FALSE)))
        } else {
            
            poisTable <- data.frame(Probability=c("Quantile"),
                                   Value=c(qpois(qq,rr)))
        }}
    )

    tTable <- reactive({
        qq<-as.numeric(input$t_q)
        vv<-as.numeric(input$t_v)
        dd<-as.numeric(input$df)

        if (input$t_val=="prob"){
            tTable <- data.frame(Probability=c("Lower Tail","Upper Tail","Two Tail"),
                                   Value=c(pt(vv,dd),pt(vv,dd,lower.tail = FALSE),2*pt(abs(vv),dd,lower.tail = FALSE)))
        } else {
            
            tTable <- data.frame(Probability=c("Quantile"),
                                   Value=c(qt(qq,dd)))
        }}
    )

    normPlot <- reactive({
        qq<-as.numeric(input$qq_z)
        zz<-as.numeric(input$norm_z)
        mm<-as.numeric(input$mean)
        ss<-as.numeric(input$sd)
        ee<-qnorm(qq,mm,ss)
        lower<-mm-6*ss
        upper<-mm+6*ss
        diff<-abs(mm-zz)
        if (zz < mm){
            greater_zz<-zz+2*diff
            lower_zz<-zz
        } else {
            greater_zz<-zz
            lower_zz<-zz-2*diff
        }
        gg_df_norm<-data.frame(x=seq(lower,upper,length.out = 600),
                               y=dnorm(seq(lower,upper,length.out = 600),mm,ss))
        dnorm_upper<-function(x){
            y <-dnorm(x,mm,ss)
            y[x<zz] <-NA
            return(y)
        }
        dnorm_lower<-function(x){
            y <-dnorm(x,mm,ss)
            y[x>zz] <-NA
            return(y)
        }
        dnorm_lower_qq<-function(x){
            y <-dnorm(x,mm,ss)
            y[x>ee] <-NA
            return(y)
        }
        dnorm_two<-function(x){
            y <- dnorm(x,mm,ss)
            y[x>lower_zz & x<greater_zz] <-NA
            return(y)
        }
        if (input$norm_val=="prob"){
            gg_1<-ggplot(data=gg_df_norm,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dnorm_lower,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("Normal Density Function: Lower Tail")+ylab("density")
            gg_2<-ggplot(data=gg_df_norm,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dnorm_upper,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("Normal Density Function: Upper Tail")+ylab("density")
            gg_3<-ggplot(data=gg_df_norm,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dnorm_two,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("Normal Density Function:Two Tail")+ylab("density")
            normPlot<-gridExtra::grid.arrange(gg_1,gg_2,gg_3)
            
        } else {
            normPlot<-plot(ggplot(data=gg_df_norm,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dnorm_lower_qq,geom="area", fill="green", alpha=0.2)+
                geom_segment(x=ee,xend=ee,y=0,yend=max(gg_df_norm$y),size=3,col="red")+
                theme_bw()+ggtitle("Normal Density Function: Quantile")+ylab("density"))
        }}
    )

    binPlot <- reactive({
        qq <- as.numeric(input$bin_q)
        vv <- as.numeric(input$bin_v)
        nn <- as.numeric(input$n)
        pp <- as.numeric(input$p)
        ee <- qbinom(qq,nn,pp)

        gg_df_bin<-data.frame(x=c(0:nn),
                               y=dbinom(c(0:nn),nn,pp))

        if (input$bin_val=="prob"){
            gg_1_bin<-ggplot(data=gg_df_bin,aes(x=x,y=y))+geom_step()+
                geom_rect(xmin=vv,xmax=vv+1,ymin=0,ymax=gg_df_bin$y[vv+1])+
                theme_bw()+ggtitle("Binomial Density Function")+ylab("density")
            gg_2_bin<-ggplot(data=gg_df_bin,aes(x=x,y=y))+geom_step()+
                theme_bw()+ggtitle("Binomial Density Function: Lower Tail")+ylab("density")
            for (i in 1:(vv+1)){
                gg_2_bin <- gg_2_bin + geom_rect(xmin=gg_df_bin$x[i],xmax=gg_df_bin$x[i+1],ymin=0,ymax=gg_df_bin$y[i])
            }
            gg_3_bin<-ggplot(data=gg_df_bin,aes(x=x,y=y))+geom_step()+
                theme_bw()+ggtitle("Binomial Density Function: Upper Tail")+ylab("density")
            for (i in (vv+1):nn){
                gg_3_bin <- gg_3_bin + geom_rect(xmin=gg_df_bin$x[i],xmax=gg_df_bin$x[i+1],ymin=0,ymax=gg_df_bin$y[i])
            }
            binPlot<-gridExtra::grid.arrange(gg_1_bin,gg_2_bin,gg_3_bin)
            
        } else {
            gg_bin_q<-ggplot(data=gg_df_bin,aes(x=x,y=y))+geom_step()+
                geom_rect(xmin=ee,xmax=ee+1,ymin=0,ymax=gg_df_bin$y[ee+1],fill="red")+
                theme_bw()+ggtitle("Binomial Density Function")+ylab("density")
            for (i in 1:ee){
                gg_bin_q <- gg_bin_q + geom_rect(xmin=gg_df_bin$x[i],xmax=gg_df_bin$x[i+1],ymin=0,ymax=gg_df_bin$y[i])
            }
            
            binPlot<-plot(gg_bin_q)
        }
        }
    )
    
    poisPlot <- reactive({
        qq <- as.numeric(input$pois_q)
        vv <- as.numeric(input$pois_v)
        rr <- as.numeric(input$rate)
        ee <- qpois(qq,rr)
        nn <- rr + 7 * sqrt(rr)
        gg_df_pois<-data.frame(x=c(0:nn),
                              y=dpois(c(0:nn),rr))
        
        if (input$pois_val=="prob"){
            gg_1_pois<-ggplot(data=gg_df_pois,aes(x=x,y=y))+geom_step()+
                geom_rect(xmin=vv,xmax=vv+1,ymin=0,ymax=gg_df_pois$y[vv+1])+
                theme_bw()+ggtitle("Poisson Density Function")+ylab("density")
            gg_2_pois<-ggplot(data=gg_df_pois,aes(x=x,y=y))+geom_step()+
                theme_bw()+ggtitle("Poisson Density Function: Lower Tail")+ylab("density")
            for (i in 1:(vv+1)){
                gg_2_pois <- gg_2_pois + geom_rect(xmin=gg_df_pois$x[i],xmax=gg_df_pois$x[i+1],ymin=0,ymax=gg_df_pois$y[i])
            }
            gg_3_pois<-ggplot(data=gg_df_pois,aes(x=x,y=y))+geom_step()+
                theme_bw()+ggtitle("Poisson Density Function: Upper Tail")+ylab("density")
            for (i in (vv+1):nn){
                gg_3_pois <- gg_3_pois + geom_rect(xmin=gg_df_pois$x[i],xmax=gg_df_pois$x[i+1],ymin=0,ymax=gg_df_pois$y[i])
            }
            binPlot<-gridExtra::grid.arrange(gg_1_pois,gg_2_pois,gg_3_pois)
            
        } else {
            gg_pois_q<-ggplot(data=gg_df_pois,aes(x=x,y=y))+geom_step()+
                geom_rect(xmin=ee,xmax=ee+1,ymin=0,ymax=gg_df_pois$y[ee+1],fill="red")+
                theme_bw()+ggtitle("Poisson Density Function")+ylab("density")
            for (i in 1:ee){
                gg_pois_q <- gg_pois_q + geom_rect(xmin=gg_df_pois$x[i],xmax=gg_df_pois$x[i+1],ymin=0,ymax=gg_df_pois$y[i])
            }
            
            poisPlot<-plot(gg_pois_q)
        }
    }
    )
    
    tPlot <- reactive({
        qq<-as.numeric(input$t_q)
        vv<-as.numeric(input$t_v)
        dd<-as.numeric(input$df)
        ee<-qt(qq,dd)
        aa<-abs(vv)
        gg_df_t<-data.frame(x=seq(-7,7,length.out = 500),
                               y=dt(seq(-7,7,length.out = 500),dd))
        dt_upper<-function(x){
            y <-dt(x,dd)
            y[x<vv] <-NA
            return(y)
        }
        dt_lower<-function(x){
            y <-dt(x,dd)
            y[x>vv+.01] <-NA
            return(y)
        }
        dt_lower_qq<-function(x){
            y <-dt(x,dd)
            y[x>ee+.01] <-NA
            return(y)
        }
        dt_two<-function(x){
            y <- dt(x,dd)
            y[x>aa & x<aa] <-NA
            return(y)
        }
        if (input$t_val=="prob"){
            gg_1_t<-ggplot(data=gg_df_t,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dt_lower,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("t Density Function: Lower Tail")+ylab("density")
            gg_2_t<-ggplot(data=gg_df_t,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dt_upper,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("t Density Function: Upper Tail")+ylab("density")
            gg_3_t<-ggplot(data=gg_df_t,aes(x=x,y=y))+geom_line()+
                stat_function(fun=dt_two,geom="area", fill="green", alpha=0.2)+
                theme_bw()+ggtitle("t Density Function:Two Tail")+ylab("density")
            tPlot<-gridExtra::grid.arrange(gg_1_t,gg_2_t,gg_3_t)
            
        } else {
            tPlot<-plot(ggplot(data=gg_df_t,aes(x=x,y=y))+geom_line()+
                               stat_function(fun=dt_lower_qq,geom="area", fill="green", alpha=0.2)+
                               geom_segment(x=ee,xend=ee,y=0,yend=max(gg_df_t$y),size=3,col="red")+
                               theme_bw()+ggtitle("t Density Function: Quantile")+ylab("density"))
        }}
    )
    output$normTable <- renderTable(normTable(), digits = 4)
    output$normPlot <- renderPlot(normPlot())
    output$binTable <- renderTable(binTable(), digits = 4)
    output$binPlot <- renderPlot(binPlot())
    output$poisTable <- renderTable(poisTable(), digits = 4)
    output$poisPlot <- renderPlot(poisPlot())
    output$tTable <- renderTable(tTable(), digits = 4)
    output$tPlot <- renderPlot(tPlot())

}

# Run the application 
shinyApp(ui = ui, server = server)
