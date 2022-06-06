library(tidyr)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(plotly)


library(tidyverse)

vax_state<-read.csv("vax_state.csv")

vax_state_1<-pivot_longer(vax_state,cols ='pfizer1':'pending3',names_to = "vaccine",values_to ="number")


vax_state_2<-vax_state_1%>%
  select(date,state,vaccine,number)

#unique(vax_state_2$vaccine)

vaccine_brand<-data.frame(
  vaccine=c("pfizer1","pfizer2","pfizer3",
            "sinovac1","sinovac2","sinovac3",
            "astra1","astra2","astra3",
            "sinopharm1","sinopharm2","sinopharm3",
            "cansino","cansino3","pending1","pending2","pending3"),
  brand=c("pfizer","pfizer","pfizer",
          "sinovac","sinovac","sinovac",
          "astra","astra","astra",
          "sinopharm","sinopharm","sinopharm",
          "cansino","cansino","pending","pending","pending")
)

vax_state_3<-inner_join(vax_state_2,vaccine_brand)


vax_state_4<-vax_state_3%>%
  group_by(state,brand)%>%
  summarise(vaccine_num=sum(number))



ui <- dashboardPage(
  dashboardHeader(title = "brand of vaccine"),
  dashboardSidebar(
    
    
    pickerInput("state",h4("state"),
                c(unique(vax_state_3$state)),options = list(`actions-box` = TRUE),multiple = T)
    
  ),
  
  dashboardBody(
    
    tabBox(
      title = "Which brand of vaccine is popular ", height = "1440px", width = 24,  
      
      tabPanel("Which brand of vaccine is popular", 
               
               plotOutput("Plot1"),
               
               
      ),
      
      
    ),
    
  )
)



server <- function(input, output) {
  
  
  
  
  data1=reactive(
    {
      vax_state_4%>%
        filter(
          state %in% input$state
          
        )
      
    }
  )
  
  
  output$Plot1 <- renderPlot({
    
    ggplot(data=data1(),aes(x=reorder(brand,-vaccine_num),y=vaccine_num))+
      geom_bar(stat = 'identity',fill='blue')+
      labs(xlab="brand",ylab="vaccine number",title = "the brand of vaccine number ")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  
  
}
shinyApp(ui, server)

