#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
load('/Users/jinjiahuang/fmBS.Rdata')
load('/Users/jinjiahuang/fmIS.Rdata')
load('/Users/jinjiahuang/fmRatio.Rdata')
am <- read_excel("/Users/jinjiahuang/Desktop/FS/Business/Assumption.xlsx",sheet = "m")
ab <- read_excel("/Users/jinjiahuang/Desktop/FS/Business/Assumption.xlsx",sheet = "b")
aa <- read_excel("/Users/jinjiahuang/Desktop/FS/Business/Assumption.xlsx",sheet = "a")
am[,-1] <- round(am[,-1],2)
ab[,-1] <- round(ab[,-1],2)
aa[,-1] <- round(aa[,-1],2)
colorBlindBlack8  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colortwo <- c("#666600","#CCCC00","#FFFF00","#FFCC33","#FF3333")
# Define UI for application
ui <- fluidPage(
  fluidRow(
    column(12,DTOutput("Assumption"))
  ),
  fluidRow(
    column(6,selectInput('scenario','Scenario to select for valuation',c("Moderate","Aggresive","Bearish"), selected = "Moderate" )),
    column(6,selectInput('overview','Select a report for an overview',c('Balance Sheet(Asset)',
                                                                      'Balance Sheet(Liabilities and Equity)',
                                                                      'Income Statement',
                                                                      'Ratios')))
  ),
fluidRow(
  column(12,plotOutput('Overview'))
),
  fluidRow(
    column(10,actionButton("value","The company valuation is")),
    column(2,textOutput("Value"))
  )

)


# Define server
server <- function(input, output) {
  
output$Assumption <- DT::renderDT({
  if(input$scenario == "Moderate"){am}else if(input$scenario == "Bearish"){ab
  } else{aa}
},options=list(pageLength = 5))

output$Overview <- renderPlot({
  if(input$overview == "Balance Sheet(Asset)"){
  assetdata <- fmBS %>% filter(scenario==input$scenario) %>% melt(id.vars="Year")
  assetdata <- assetdata[1:30,]
  assetdata$Year <- as.factor(assetdata$Year)
  assetdata$value <- as.numeric(assetdata$value)
  ggplot(data = assetdata,aes(Year,value,fill=variable))+geom_bar(stat='identity',position = 'Stack')+
    theme(panel.grid = element_blank())+
    scale_fill_brewer()+scale_y_continuous(breaks = seq(0,200000,50000))
}else if (input$overview == "Balance Sheet(Liabilities and Equity)"){
  lieq <- fmBS %>% filter(scenario==input$scenario) %>% melt(id.vars="Year")
  lieq <- lieq[c(36:55,71:75),]
  lieq$Year <- as.factor(lieq$Year)
  lieq$value <- as.numeric(lieq$value)
  ggplot(data = lieq,aes(Year,value,fill=variable))+geom_bar(stat='identity',position = 'Stack')+
    theme(panel.grid = element_blank())+
    scale_fill_manual(values=colortwo)+scale_y_continuous(breaks = seq(0,200000,50000))
}else if (input$overview == "Income Statement"){
  isdata <- fmIS[,c(1,2,4,6,10,11)] %>% filter(scenario == input$scenario) %>% melt(id.vars="Year")
  isdata$Year <- as.factor(isdata$Year)
  isdata$value <- as.numeric(isdata$value)
  isdata<- isdata[1:20,]
  ggplot(data = isdata,aes(Year,value,fill=variable))+geom_bar(stat='identity',position = 'dodge')+
    theme(panel.grid = element_blank())+scale_fill_manual(values=colorBlindBlack8)
}else{
  ratio <- fmRatio %>% filter(scenario == input$scenario) %>% melt(id.vars="Year")
  ratio$Year <- as.factor(ratio$Year)
  ratio$value <- as.numeric(ratio$value)
  ratio <- ratio[1:35,]
  ggplot(data = ratio,aes(Year,value,group=variable,color=variable))+geom_point()+geom_line()
}
  })

output$Value <- renderText({
  if (input$scenario == "Moderate"){307396}else if(input$scenario == "Aggresive"){392408}else{164462}
})

}

# Run the application 
shinyApp(ui = ui, server = server)
