#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(DT)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
###########################TAB1 Analysis Of Confirm Cases and Recoveration Rate
  
  DataP<-read.csv("covid19-download.csv")
  St=30000000      #Susceptible
  It=2         #Infected
  Rt=200         #Recovered
  Vt=5000         #Vaccined
  Nt=St+It+Rt  #Population
  DF1<-data.frame(Province=DataP$prname,Date=DataP$date,Confirm=DataP$numtoday,Tests=DataP$numtestedtoday,Recover=DataP$numrecoveredtoday) #Covid tests and confirmed
  DF1$beta=DataP$numconf/DataP$numtested
  #infection rate. we use fomula: Positive/Negative+Positive
  DF1$gamma= DataP$numrecover/DataP$numconf
  
  DF1$R0=DF1$beta/DF1$gamma #R0
  DF1$RE=DF1$R0*St/Nt   #effective R
  DF1$lambda= DF1$beta*It/Nt
  
  relation<-lm(DF1$Confirm~DF1$Recover)
  a<-data.frame(x=6)
  result <-  predict(relation,a)
  
  Q=data.frame(do.call("rbind", tapply(DF1$RE, DF1$Province, quantile)))
  G=data.frame(do.call("rbind", tapply(DF1$gamma, DF1$Province, quantile)))
  output$G<-renderPrint(G)
  
  output$plot4 <- renderPlot({
    
    DF1 %>%  ggplot( aes(x=Date, y=Confirm, colour=Province))+
      geom_jitter()
  }) #end of Plot4
  
  output$plot6 <- renderPlot({

  plot(DF1$Recover,DF1$Confirm,col = "violet",main = "Confirm & Recover Regression",
    abline(lm(DF1$Confirm~DF1$Recover)),cex = 0.7,pch = 16,xlab = "Confirm",ylab = "Recover") 
    }) #end of plot 6
########################################Tab2 Effective R  
 output$Q<-renderPrint(Q)
  output$plot5 <- renderPlot({
    DF1 %>% ggplot( aes(x=Date, y=Recover, colour=Province))+
      geom_jitter()  
  }) #end of Plot5
  
    output$plot1 <- renderPlot({
    
    tibble1<-tibble(Date=DF1$Date,RE=DF1$RE,Province=DF1$Province)
    tibble1%>%
      
      ggplot(aes(x=RE, y=Province ))+
      geom_point(aes(color=Province),shape=15)+
      geom_errorbarh(aes(xmin=0,xmax=2),height=0.0, colour="blue")+
      geom_vline(xintercept=1,linetype="dashed")+
      scale_size_continuous(breaks=c(5000,10000,15000))+
      xlim(0,2)+
      labs(title = "Effective R",
           x = "Spreading Daily",
           y = "",
           color = "Province")
                                  })#end RE plot
  

########################### End of Tab 2    
###########################TAB2 Vaccination and Infection    
   
  
  
   observeEvent(input$goButton, {
    N = input$Pop # Number of People
    T = input$Weeks # Weeks
    P.Infected.People  = input$REF #probability of encountering a sick person
    P.Recovered.From.Infection = 0.7  #Probability of recovering from Covid-19         
    P.Vaccined=0.05 # probability of bieng choose for vaccination, Every one will be immuned 100 %
    
    Susceptible = rnorm(N, mean = 0.7, sd = 0.1)# Susceptible Number, with the mean of recovering 70% and Sd of 10%
    Cumulitive.Vaccined.People=25 # starting with a few vaccined poeple
    DFVaccined<-NULL # Creating data.frame by r bind for every iteration. 
    
    for(Weeks in 1:T){ 
        
        Infected.People = rpois(N,P.Infected.People)>P.Infected.People #randomly select Infection transmission by poisoon distribution
        Number.of.Infected.Poeple=length(Infected.People[Infected.People== TRUE]) #counting infected poeple
        Recovered.People = Infected.People<P.Recovered.From.Infection #recovered people according to the mean of Population
        Number.of.Recovered.Poeple=length(Recovered.People[Recovered.People== TRUE]) #number of Recovered 
        Vaccined.Poeple=runif(N)<P.Vaccined  #randomly choose of people for vaccination, since the prior group are vaccined
        Number.of.Vaccined.Poeple=length(Vaccined.Poeple[Vaccined.Poeple== TRUE]) #number of vaccination
        Cumulitive.Vaccined.People=Cumulitive.Vaccined.People+Number.of.Vaccined.Poeple #Summation of vaccined poeple
        People.Of.Last.Week = data.frame(Susceptible, Infected.People, Recovered.People,Vaccined.Poeple) # one week before data.frame
        New.Susceptible= People.Of.Last.Week%>% filter(Infected.People==FALSE | (Infected.People ==TRUE & Recovered.People ==TRUE)|Vaccined.Poeple==FALSE)%>% 
            dplyr::select(Susceptible) #new Susceptible Population Generated excluding vaccined
        
        
        Number.of.New.Susceptible=N-Cumulitive.Vaccined.People #length of new susceptible for plotting in every iteration
         if(Number.of.New.Susceptible>30){
             New.Number.of.Infected.Poeple=Number.of.New.Susceptible*0.1 #length of new infected for plotting in every iteration
             Susceptible = rnorm(N,mean = 0.7, sd = 0.1) # the new susceptible people should be random
             
             # creat a final table by using rbind
             rbind(DFVaccined,data.frame(Weeks,Cumulitive.Vaccined.People,Number.of.New.Susceptible,New.Number.of.Infected.Poeple))->DFVaccined
         }
         else{Number.of.New.Susceptible==0}            
    } #end Of For Loop
    
    output$Table2<- renderDataTable (DFVaccined)
    
    output$plot2 <- renderPlot({

      DFVaccined %>% ggplot( aes(x=Weeks,y=New.Number.of.Infected.Poeple))+
            geom_point(col="red")
    }) #end of Plot2
    output$plot3 <- renderPlot({ 
        
        DFVaccined %>%
         ggplot( aes(x=Weeks,y=Cumulitive.Vaccined.People))+
            geom_point(col="green")
        
    }) #end of plot#
   
    
    })#end of go bottom
######################## End Tab 3
})
