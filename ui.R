#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
    navbarPage("Covid Vaccination with 1 Dose",
               
               tabPanel( "Recover/Confirm Cases Analysis ", 
                         HTML("<h2>  Strategy 2</h2> 
                <p> At first we are going to understand the basic of the pandemic and why it spread so quickly?
                   what is the relation between the spreading rate and reCovery rate. we use all this arguman
                    later in Effective R and simuation. Here we have a table from Quiantile of recovery rate,
                    since some of data were inserted wrong we are going to use the quantile. Confirm Cases is the people who get the 
                              Covid-19 disease and they can transmit the virus to others.
                             </p>"),
            mainPanel(
                verbatimTextOutput("G"),
                 plotOutput("plot4"),
                plotOutput("plot6")
                     ) #end of main
                      ), #end of tab panel
            
#####################TAB2            
tabPanel( "Effective R Analysis", 
          HTML("<h2>  Strategy 2</h2> 
                <p>R0, pronounced “R naught,” is a mathematical term that indicates how contagious an infectious disease is.
It’s also referred to as the reproduction number.
As an infection is transmitted to new people, it reproduces itself. if R0 equalls to 1 it means the disease will survive but do not break out. 
if R0 is less than 1 the diseas will die out by itself.
Ultimately, if R0 is greater than 1, the disease will become epidemic to a city, country or it could be become 
pandemic as Covid-19 did. The R0 for COVID-19 is a median of 5.7, according to a study published online in
 Emerging Infectious Diseases. That’s about double an earlier R0 estimate of 2.2 to 2.
The 5.7 means that one person with COVID-19 can potentially transmit the coronavirus to 5 to 6 people.
rather than before. However, government take action very fast, and they lock down many cities, airports, borders, etc..
In Canada, By analysing the available Data, we could obtain the Effecting R.the formua for R0 is Number.Of.Confirmed case / Number.of.Total.Tests and effective R is: 
RE=R0*S/N which S is the susceptible population and N is the targeted population.
 It is the average number of secondary cases arising from an infected case at a given point in the epidemic. in the
 simulation we use the Effective R.
           Note that since the data were not well inserted at the beginning of pandemic we have alot of zeros
           that some of theme cause false haigher rate or Infinity in the table. So I choose to work with the Quantiles as 
               you can see in the table.
               In the last graph you can assess that which province did well in order to keep the sprad rate lower than 1. The Vertical line
               shows the critical Number 1 of Effective R.The goverment policy regard Lock Down and social distancing works!
               Also, I tried to simulate a Forest plot for the each province.</p>"),          
          mainPanel(
              verbatimTextOutput("Q"),
              plotOutput("plot5"),
              plotOutput("plot1")
              
          ) #end of main
), #end of tab panel

#####################TAB3                    
tabPanel( "Simulation",
          HTML("<h2>  Strategy 2</h2> 
                <h4>  1 Dose of vaccine</h4>
    <p>Previously, we saw that using one dose of vaccine work better than 2 dosages for each person. Since
               poeple who are working right now are limited due to social distancing, and they mainly are not 
               elders (most of them are retired or do not have this high risk jobs), Injection of one dosage in most of them cause immunity or ease of disease.
               On the other side, elders who are in high risk of death, do not have a high contact rate due social distance.
             So I believe, injecting one dose of vaccine in priority to health care system, group living people and elders
               is a right move to do. First, it reduce the spread among people who have higer contact rate. Who have to do their job 
               in social distance situation. Second, reduce the death rate among elders by one dose of vaccine, since it cause to a milder
               disease.
               Since the government of Canada vaccined more than 50% of first group and they population consider to others is not 
               very high, I am going to ignore these groups. i want to focous on the progress of vaccination weekly, and 
               see how it effects the Spread of Virus. We see that whith vaccination in every week, with the same Effective R from the begining
               we observe a decline in Infected Population.
               So we Define Best Strategy again.
               </p><h4>  BEST STRATEGY</h4><p> 1. Since many of Priority People According to the Canada website has been vaccined,
               we change our viewpoint and assess Ordinary people which are the major proportion of population. 2.Government strategy of
               lock down and social distancing was very effective and it should be held until the vaccination completed. as a result we have
               lower spreading rate. Less sick people, less death.3. we using one dose of vaccine for most of the population and we achieve herd immunity faster</p>"),
          sidebarLayout(
              sidebarPanel(
              sliderInput("Weeks","Time (weeks):",
                        min = 1, max = 30, value = 20),
              sliderInput("REF","Effective R:",
                        min = 0.08, max = 0.4, value = 0.1),
              sliderInput("Pop", "Population :",
                        min = 10000, max = 20000, value = 2000),
              actionButton("goButton", "Go!") ), #end of Sidebar Panel

        mainPanel(
            dataTableOutput("Table2") ,
            plotOutput("plot2"), ##tab3 plot2
            plotOutput("plot3")  ##tab3 plot 3
        ) #end of mainpanel
    ) #end of sidebarLayout 
), # End of  Tab3
############################### 
tabPanel( "About Me",
                       HTML("<h2>  Fatemeh Khoshsiar</h2>
    <p>I am a Master student in University of Ottawa. Before, I was the student in Mathematics
in Iran, but due my passion to statistics, specially BioStatistics, I start my master degree
in statistics. I was working in advertising Company, which my main duty was to define 
strategies  for Brand, so they would hear by their audience. The strategy depends on some metrices
First, who is their target audience? Second, what is their Brand personality? is he a explorer or an Archtitect?
Finally, where we should put our advertisement so our interested people can find us?</p> ")  #tabsetPanel ends
)#tab panel end 
)#end of navbar page