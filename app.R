library(tidyr)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(plotly)

library(sp)
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

cases_malaysia<-read.csv("cases_malaysia.csv")
cases_malaysia_1<-subset(cases_malaysia,select = c(date,cases_new))

deaths_malaysia<-read.csv("deaths_malaysia.csv")
deaths_malaysia_1<-subset(deaths_malaysia,select = c(date,deaths_new))
cases_deaths<-left_join(cases_malaysia_1,deaths_malaysia_1)

cases_deaths$deaths_new<-ifelse(is.na(cases_deaths$deaths_new)==TRUE,0,cases_deaths$deaths_new)
cases_deaths$date<-as.Date(cases_deaths$date)


deaths_state<-read.csv("deaths_state.csv")

deaths_state$date<-as.Date(deaths_state$date)

deaths_state_1<-deaths_state%>%
  group_by(state)%>%
  summarise(deaths_new_total=sum(deaths_new))

deaths_state_2<-rename(deaths_state_1,"NAME_1"="state")

Malaysia<-readRDS("dt/gadm36_MYS_1_sf.rds")

Malaysia1<- rmapshaper::ms_simplify(Malaysia) 
deaths_state_Malaysia1<-left_join(Malaysia1,deaths_state_2)

vax_state_5<-vax_state_4%>%
  group_by(state)%>%
  summarise(vaccine_total=sum(vaccine_num))


population<-read.csv("population.csv")

population_1<-subset(population,select = c(state,pop))

vax_state_5_1<-inner_join(vax_state_5,population_1)

vax_state_5_2<-vax_state_5_1%>%
  mutate(vaccine_percent=vaccine_total/pop/3)

vax_state_5_3<-rename(vax_state_5_2,"NAME_1"="state")

vaccine_state_Malaysia2<-left_join(Malaysia1,vax_state_5_3)

population_2<-pivot_longer(population,cols ='pop_18':'pop_5',names_to = "pop_age",values_to ="pop_num")


cases_deaths_1<-cases_deaths%>%
  mutate(death_rates=deaths_new/cases_new )

cases_deaths_1$death_rates<-ifelse(cases_deaths_1$death_rates=='NaN',0,cases_deaths_1$death_rates)

cases_state<-read.csv("cases_state.csv")

cases_state$date<-as.Date(cases_state$date)

cases_state_1<-cases_state%>%
  group_by(state)%>%
  summarise(cases_new_total=sum(cases_new))

cases_state_2<-rename(cases_state_1,"NAME_1"="state")


cases_state_Malaysia1<-left_join(Malaysia1,cases_state_2)



#new  q4

icu<-read.csv("icu.csv")
icu_1<-icu%>%
  group_by(state)%>%
  summarise(beds_icu_total_t=sum(beds_icu_total),
            beds_icu_covid_t=sum(beds_icu_covid))

icu_2<-icu_1%>%
  mutate(icu_covid_rate=beds_icu_covid_t/beds_icu_total_t)


vax_state_icu<-inner_join(vax_state_5_2,icu_2)

#q3
deaths_state_cases<-inner_join(deaths_state_1,cases_state_1)

deaths_state_cases_1<-deaths_state_cases%>%
  mutate(mortality_rate=deaths_new_total/cases_new_total)

deaths_state_cases_vax<-inner_join(deaths_state_cases_1,vax_state_5_2)


#q5


cases_state_aa<-subset(cases_state,select = c(date,state,cases_new))

population_3<-subset(population,select = c(state,pop))

cases_state_population<-left_join(cases_state_aa,population_3)

cases_state_population$type<-ifelse(cases_state_population$date<as.Date('2021-02-24'),"bedore","after")

cases_state_population_1<-cases_state_population%>%
  group_by(state,type)%>%
  summarise(max_cases_new=max(cases_new),pop=mean(pop))

cases_state_population_2<-cases_state_population_1%>%
  mutate(infection_rate=max_cases_new/pop)


#q2
population_4<-population%>%
  mutate(old_age_rate=pop_60/pop)



old_age_mortality_rate<-inner_join(population_4,deaths_state_cases_1)


ui <- dashboardPage(
  dashboardHeader(title = "Keep CoviDown "),
  dashboardSidebar(
    
    dateRangeInput(inputId="dates", 
                   label="Pick date range",
                   start  = "2020-01-01",
                   end    = "2022-06-30",
                   min    = "2020-01-01",
                   max    = "2022-06-30",
                   format = "mm/dd/yy"),
    
    
    
    pickerInput("state",h4("state"),
                c(unique(vax_state_3$state)),options = list(`actions-box` = TRUE),multiple = T)
    
  ),
  
  dashboardBody(
    
    tabBox(
      title = " ", height = "1920px", width = 24,  
      
      tabPanel("new cases and new deaths", 
               
               plotOutput("Plot"),
               plotOutput("Plot1"),
               
      ),
      
      tabPanel(" brand of vaccine", 
               
               
               
               plotOutput("Plot5"),
               plotOutput("Plot2")
               
               
               
      ),
      tabPanel("new deaths total", 
               
               plotOutput("Plot3_1"),
               plotOutput("Plot3"),
               plotOutput("Plot4"),
               plotOutput("Plot6"),
               plotOutput("Plot7")
               
               
      ),
      
      tabPanel("total analysis", 
               
              
               plotOutput("Plotb"),
               plotOutput("Plotc"),
               plotOutput("Plotd"),
               plotOutput("Plote")
               
               
      ),
      
      tabPanel("total analysis1", 
               
               
               plotOutput("Plota1"),
               plotOutput("Plota2"),
               plotOutput("Plota3"),
               plotOutput("Plota4")
               
               
      ),
    ),
    
  )
)



server <- function(input, output) {
  
  
  data=reactive(
    {
      cases_deaths%>%
        filter(
          
          date >= min(ymd(input$dates)),
          date <= max(ymd(input$dates))
        )
    }
  )
  
  data1=reactive(
    {
      vax_state_4%>%
        filter(
          state %in% input$state
          
        )
      
    }
  )
  
  data1_2=reactive(
    {
      vax_state_4
      
    }
  )
  
  
  
  data2=reactive(
    {
      deaths_state_Malaysia1
      
    }
  )
  data3=reactive(
    {
      cases_state_Malaysia1
      
    }
  )
  
  
  data4=reactive(
    {
      vaccine_state_Malaysia2
      
    }
  )
  
  data5=reactive(
    {
      population_2
      
    }
  )
  
  data6=reactive(
    {
      cases_deaths_1
      
    }
  )
  
  
  datab=reactive(
    {
      old_age_mortality_rate
      
    }
  )
  
  
  datac=reactive(
    {
      deaths_state_cases_vax
      
    }
  )
  
  
  datad=reactive(
    {
      vax_state_icu
      
    }
  )
  datae=reactive(
    {
      cases_state_population_2
      
    }
  )
  
  dataa1=reactive(
    {
      cases_state_1
      
    }
  )
  
  dataa2=reactive(
    {
      vax_state_5_2
      
    }
  )
  
  dataa3=reactive(
    {
      deaths_state_cases_1
      
    }
  )
  
  
  
  
  output$Plot <- renderPlot({
    
    ggplot(data=data(),aes(x=date,y=cases_new))+
      geom_line(color='darkorchid3')+
      labs(xlab="date",ylab="cases_new number",title = "the covid19 cases new of malaysia ")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  output$Plot1 <- renderPlot({
    
    ggplot(data=data(),aes(x=date,y=deaths_new))+
      geom_line(color='goldenrod1')+
      labs(x="date",y="deaths_new number",title = "the covid19 deaths new of malaysia ")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$Plot2 <- renderPlot({
    
    ggplot(data=data1(),aes(x=reorder(brand,-vaccine_num),y=vaccine_num))+
      geom_bar(stat = 'identity',fill='dodgerblue4')+
      labs(x="brand",y="vaccine number",title = "the brand of vaccine number ")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  output$Plot3_1 <- renderPlot({
    
    ggplot(data=data3()) + 
      geom_sf(aes(fill = cases_new_total), colour = "dodgerblue4") +
      coord_sf() + 
      scale_fill_continuous(type = "viridis") +
      labs(title = "cases new total of malaysial")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  
  output$Plot3 <- renderPlot({
    
   
    ggplot(data=data2()) + 
      geom_sf(aes(fill = deaths_new_total), colour = "dodgerblue4") +
      coord_sf() + 
      scale_fill_continuous(type = "viridis") +
      labs(title = "new deaths  total of malaysial")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$Plot4 <- renderPlot({
    
  
    ggplot(data=data4()) + 
      geom_sf(aes(fill = vaccine_percent), colour = "dodgerblue4") +
      coord_sf() + 
      scale_fill_continuous(type = "viridis") +
      labs(title = "vaccination uptake")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$Plot5 <- renderPlot({
    
    
    
    ggplot(data=data1_2(),aes(x=state,y=vaccine_num,fill=brand))+
      geom_bar(stat = 'identity',position="fill")+coord_flip()+
      labs(title = "the type of vaccine used")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    

    
  })
  
  
  output$Plot6 <- renderPlot({
    
    

    ggplot(data=data5(),aes(x=state,y=pop_num,fill=pop_age))+
      geom_bar(stat = 'identity',position="fill")+coord_flip()+
      labs(title = "the population age structure")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
    
  })
  
  output$Plot7 <- renderPlot({
    
    
    ggplot(data=data6(),aes(x=date,y=death_rates))+
      geom_line(color="blue")+geom_vline(xintercept = as.Date('2021-02-24'),color="red",size=1)+
      labs(title = "death rates of Malaysia")+
      theme(axis.text.x = element_text(vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
    
    
  })
  
  output$Plotb <- renderPlot({
    
    
    ggplot(data=datab(),
           aes(x=old_age_rate,y=mortality_rate))+
      geom_point(color="blue",size=1)+
      labs(title="old_age_rate & mortality_rate")
    
    
  })
  
  
  output$Plotc <- renderPlot({
    
    
    ggplot(data = datac(),
           aes(x=vaccine_percent,y=mortality_rate))+  
      geom_point(size=2,color="orange")+
      labs(title = "vaccine_percent vs mortality_rate ")
    
    
  })
  
  output$Plotd <- renderPlot({
    
    
    ggplot(data = datad(),aes(x=vaccine_percent,y=icu_covid_rate))+
      geom_point(size=2,color="blue")+labs(title = "vaccine_percent vs icu_covid_rate ")
    
    
    
  })
  
  output$Plote <- renderPlot({
    
    
    ggplot(data = datae(),aes(infection_rate))+
      geom_histogram(bins = 30,color="blue")+facet_wrap(~type)
    
    
  })
  
  output$Plota1 <- renderPlot({
    
    
    ggplot(data =dataa1(),aes(x=reorder(state,cases_new_total),y=cases_new_total))+  geom_bar(stat = 'identity',fill="dodgerblue4")+
      labs(title = "cases_new_total")+coord_flip()
    
    
  })
  
  
  output$Plota2 <- renderPlot({
    
    
    ggplot(data =dataa2(),aes(x=reorder(state,vaccine_percent),y=vaccine_percent))+  geom_bar(stat = 'identity',fill="dodgerblue1")+
      labs(title = "vaccine_percent")+coord_flip()
    
    
  })
  
  output$Plota3 <- renderPlot({
    
    
    ggplot(data =dataa3(),aes(x=reorder(state,mortality_rate),y=mortality_rate))+  geom_bar(stat = 'identity',fill="dodgerblue3")+
      labs(title = "mortality_rate")+coord_flip()
    
    
  })
  
  
  
  output$Plota4 <- renderPlot({
    
    
    ggplot(data =datad(),aes(x=reorder(state,icu_covid_rate),y=icu_covid_rate))+  geom_bar(stat = 'identity',fill="dodgerblue3")+
      labs(title = "icu_covid_rate")+coord_flip()
    
    
  })
  
  
  
}
shinyApp(ui, server)

