## app.R ##
library(htmltools)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(datasets)
library(DT)
library(leaflet)
library(lubridate)
library(dplyr)
library(lubridate)
library(shiny)
library(datasets)
library(DT)
library(lemon)
library(shinybusy)


set.seed(1)


bird_data <- read_csv("CleanedEbirdSample2020.csv")


counties <- bird_data %>%
  select(county) %>%
  arrange(county) %>%
  distinct() 

birds <- bird_data %>%
  select(common_name) %>%
  arrange(common_name) %>%
  distinct() 




#Loading Tab Data 
maindata<-read_csv("EbirdCleaned.csv")



#Subsetting data by species

Robin <- maindata %>%
  filter(common_name == "American Robin")

Robin <- Robin %>%
  sample_n(2000)


Cardinal <- maindata %>% filter(common_name == "Northern Cardinal")
Cardinal <- Cardinal %>% sample_n(2000)


BlueJay <- maindata %>% filter(common_name == "Blue Jay")
BlueJay <- BlueJay %>% sample_n(2000)

Chickadee <- maindata %>% filter(common_name == "Black-capped Chickadee")
Chickadee <- Chickadee %>% sample_n(2000)


Goldfinch <- maindata %>% filter(common_name == "American Goldfinch")
Goldfinch <- Goldfinch %>% sample_n(2000)





ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("fab fa-connectdevelop",lib="font-awesome")),
    menuItem("The American Robin", tabName = "robin", icon = icon("fal fa-crow",lib="font-awesome")),
    menuItem("The Northern Cardinal", tabName = "cardinal", icon = icon("fal fa-crow",lib="font-awesome")),
    menuItem("Blue Jay", tabName = "bluejay", icon = icon("fal fa-crow",lib="font-awesome")),
    menuItem("The American Gold Finch", tabName = "finch", icon = icon("fal fa-crow",lib="font-awesome")),
    menuItem("The Black-Capped Chickadee", tabName = "chick", icon = icon("fal fa-crow",lib="font-awesome")))),
  dashboardBody(tabItems(
    tabItem(tabName = "dashboard",
            
            fluidRow(align="center",
                                            img(src="americanrobin.jpg",height=144,width=192),
                                            img(src="Cardinal.jpg",height=144,width=192),
                                            img(src="bluejay.jpg",height=144,width=192),
                                            img(src="finch.jpg",height=144,width=192),
                                            img(src="chick.jpg",height=144,width=192)),
            fluidRow(align="center",h1("Michigan Bird Analysis"),
                         
                         
           box(width=15,style = "font-size:15px;",
                        "Welcome to this shiny app exploring the migratory patterns of 5
                        select Michigan birds over the course of a year."),
  
    
           box(width=15,style = "font-size:15px;","
           This app is intended to provide insight into where certain birds can be most likely observed throughout
           the state of Michigan over the course of the year. The data used to generate this website was collected from the Cornell
           lab of Ornithology's E-bird database.
           These birds are among Michigan's finest and most prevalent species (pictured above in this order): The American Robin, The Northern Cardinal,
           The Blue Jay, The Black-Capped Chickadee, and The American Goldfinch. This app is laid out in such a way that will allow you to explore these 5
           bird species, and plan a bird watching trip of your own!"), 
           h3("Directions:"),
            box(width=15,style = "font-size:15px;","Use the below list, selectors and accompanying table and visualization to learn more about which birds 
            you might see in a particular county throughout the year. Click on any of the tabs listed in the left for detailed maps of bird sightings.")),
           

    
    
    
    
    fluidRow(width = 7,align="center",
                                                                                                                                                                              
           verbatimTextOutput("value"),
           
           # App title ----
           h3("Distribution of Bird Sightings by County Throughout the Year",
                      htmlOutput("example")),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: List Values for Counties and allow multiple select ----
               selectInput(
                 inputId = "Counties",
                 label = "Please select As Many as 6 Counties",
                 choices = counties$county,
                 multiple = TRUE,
                 selectize = FALSE,
                 width = NULL,
                 size = 8
               ),
               
               # Input: List Values for Bird Selection and allow multiple select ----
               selectInput(
                 inputId = "Birds",
                 label = "Please Select Birds for Data",
                 choices = birds$common_name,
                 multiple = TRUE,
                 selectize = FALSE,
                 width = NULL,
                 size = 5
               ),
               
               actionButton("process_selection","Process Selection",style='width:260px'),
               br(),
               br(),
               actionButton("clear_choices", "Clear All Inputs", style='width:260px'),
               br()
               #  textOutput("plot_size")
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotOutput(outputId = "distPlot")),
                           tabPanel("Summary", DTOutput('table'),width = 1)
               )
             )
           
           
    )), fluidRow(align="center",box(width=15,style = "font-size:15px;","The data used to form this app was gathered from Cornell's
               Lab of Ornathology - which can be found at the following website: https://ebird.org/home"))),
    tabItem(tabName = "robin",h2("The American Robin"),
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(fluidRow(column(width=5,img(src="americanrobin.jpg",height="100%", width="100%"),
                                                   box(width=15,style = "font-size:10px;","(Turdus migratorius)
                                                   The American Robin is the orange-breasted, official state bird of Michigan.")),
                                                   column(width=7,h4("Directions:",align="center"),
                                                          box(width=16,style = "font-size:13px;","Use the slider below to interactively display 
                                                          sightings on the top map to the right. Also experiment with the \"Choose a Locality\" selector
                                                              to interactively display sighting frequency in the below bar graph and the accompanying map 
                                                              on the lower right. These bottom visualizations illustrate frequency by month at some of the 
                                                              top localities for bird watching. Happy Birding! :)"))),
                                                     
                                                     sliderInput("Robinmonth", 
                                                                 label = "Month Sighted:",
                                                                 min = 1, max = 12, value = c(1), 
                                                                 animate = animationOptions(interval = 700, loop = FALSE)
                                                     ), 
                                                     selectInput("Robinloc", 
                                                                 label = "Choose a locality",
                                                                 choices = c("Calvin University Ecosystem Preserve", "Fenner Nature Center",
                                                                             "Lake St. Clair Metropark (Metro Beach Metropark)", "Nichols Arboretum"),
                                                                 selected = "Calvin University Ecosystem Preserve"),
                                                     
                                                     plotOutput("Robinhist")
                                                   ),
                                                   
                                                   mainPanel(leafletOutput("Robinmonthmap"),
                                                             leafletOutput("Robinlocmap"),h4("Brief Analysis:"),box(width=16,style = "font-size:13px;",
                                                             "Based on the random sampling of the data, the American Robin was observed most often in the 
                                                             Oakland, Washtenaw, Kent, Wayne, and Macomb counties respectively.  Oakland county had the most 
                                                             observations in October, totaling 2,500 observations.  For Washtenaw county, the greatest number 
                                                             of observations in a single month occurred in April with 1,218 observations.  The most observations 
                                                             for Kent county totaled 1,761 observations and occurred in April.  Wayne county shows its most observations 
                                                             of 1,408 in May.  Macomb county recorded their most observations of 1,128 in October.  The mean number of observations 
                                                             of the American Robin in Oakland county is approximately 833, where Washtenaw county had an average number of observations 
                                                             of 669.  For Kent county, the average number of observations for the year was approximately 596.  For Wayne and Macomb 
                                                             counties, the average number of observations was approximately 460 and 378 respectively.
"))))
    ,
    
    
    
    
    
    tabItem(tabName = "cardinal", 
      h2("The Northern Cardinal"),
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(fluidRow(column(width=5,img(src="Cardinal.jpg",height="100%", width="100%"),
                                                   box(width=15,style = "font-size:10px;","A male Northern Cardinal (Cardinalis cardinalis).
                                                   These beautiful red birds are a Midwesetern gem.
                                                   ")),
                                                   column(width=7,h4("Directions:",align="center"),
                                                          box(width=16,style = "font-size:13px;","Use the slider below to interactively display sightings on the top map to 
                                                          the right. Also experiment with the \"Choose a Locality\" selector
                                                              to interatively display sighting frequency in the below bar graph and the accompanying map on the lower right. 
                                                              These bottom visualizations illustrate frequncy by month at some of the top localities for bird watching. Happy Birding! :)"))),
          
          sliderInput("Cardinalmonth", 
                      label = "Month Sighted:",
                      min = 1, max = 12, value = c(1), 
                      animate = animationOptions(interval = 700, loop = FALSE)
          ), 
          selectInput("Cardinalloc", 
                      label = "Choose a locality",
                      choices = c("Calvin University Ecosystem Preserve", "Fenner Nature Center",
                                  "Lake St. Clair Metropark (Metro Beach Metropark)", "Nichols Arboretum"),
                      selected = "Calvin University Ecosystem Preserve"),
          
          plotOutput("Cardinalhist")
        ),
        
        mainPanel(leafletOutput("Cardinalmonthmap"),
                  leafletOutput("Cardinallocmap"),h4("Brief Analysis:"),box(width=16,style = "font-size:13px;","Based on the random sampling of the data, the Northern Cardinal was 
                  observed most often in the Washtenaw, Kent, Oakland, Bay, and Wayne counties respectively.  Washtenaw county recorded its most observations of the Northern Cardinal 
                  in May, with a total number of observations of 626.  For Kent county, the largest number of observations was in April, with a total of 613 observations.  Oakland had 
                  its highest number of observations, 323 were documented to have occurred in May.  Bay county recorded its most observations, 319 in December and Wayne county had the 
                  most observations of the Northern Cardinal in May.  The average number of observations for the Northern Cardinal for the full year was approximately 306 in Washtenaw 
                  county, 296 in Kent county, 199 in Oakland county, 191 in Bay county, and 183 observations in Wayne county.
"),
        )
      
    )
    ),
    
    
    
    
    
    tabItem(tabName = "bluejay",h2("The Blue Jay"),
            
            sidebarLayout(
              sidebarPanel(fluidRow(column(width=5,img(src="bluejay.jpg",height="100%", width="100%"),box(width=15,style = "font-size:10px;","(Cyanocitta cristata) The oldest known wild, 
              banded Blue Jay was at least 26 years, 11 months old. Keep your eyes peeled for these gorgeous blue birds.
                                                   ")),
                                    column(width=7,h4("Directions:",align="center"),
                                           box(width=16,style = "font-size:13px;","Use the slider below to interactively display sightings on the top map to the right. 
                                           Also experiment with the \"Choose a Locality\" selector
                                                              to interactively display sighting frequency in the below bar graph and the accompanying map on the lower right. 
                                               These bottom visualizations illustrate frequency by month at some of the top localities for bird watching. Happy Birding! :)"))),
        sliderInput("BlueJaymonth", 
                    label = "Month Sighted:",
                    min = 1, max = 12, value = c(1), 
                    animate = animationOptions(interval = 700, loop = FALSE)
        ), 
        selectInput("BlueJayloc", 
                    label = "Choose a locality",
                    choices = c("Calvin University Ecosystem Preserve", "Fenner Nature Center",
                                "Lake St. Clair Metropark (Metro Beach Metropark)", "Nichols Arboretum"),
                    selected = "Calvin University Ecosystem Preserve"),
        
        plotOutput("BlueJayhist")
      ),
      
      mainPanel(leafletOutput("BlueJaymonthmap"),
                leafletOutput("BlueJaylocmap"),h4("Brief Analysis:"),box(width=16,style = "font-size:13px;","Based on the random sampling of the data, the Blue Jay was observed 
                                                                         most often in the Wayne, Washtenaw, Kent, Oakland, and Iosco counties respectively.  For Wayne county, the most 
                                                                         observations recorded was 76,671 in September.  Washtenaw and Kent county also had its highest number of sightings 
                                                                         in September, recording 914 and 673 observations, respectively.  Oakland and Iosco county both recorded their highest 
                                                                         number of observations in May of 542 and 1,758, respectively.  The average number of observations for the Blue Jay Based 
                                                                         on the random sampling of the data for Wayne county was approximately 6,560, for Washtenaw county, 297, for Iosco 262, 
                                                                         for Kent, 258, and for Oakland, 231.")
      )
    )
            ),
    tabItem(tabName = "finch",h2("The American Goldfinch"), 
            
            sidebarLayout(
              sidebarPanel(fluidRow(column(width=5,img(src="finch.jpg",height="100%", width="100%"),box(width=15,style = "font-size:10px;","(Spinus tristis) These small yellow birds are sure 
              to catch your eye. Their vivid yellow color is a sign of reprieve from winter's harshness, welcoming the warm embrace of spring.
                                                   ")),
                                    column(width=7,h4("Directions:",align="center"),
                                           box(width=16,style = "font-size:13px;","Use the slider below to interactively display sightings on the top map to the right. Also experiment with the 
                                           \"Choose a Locality\" selector
                                                              to interactively display sighting frequency in the below bar graph and the accompanying map on the lower right. These bottom 
                                               visualizations illustrate frequency by month at some of the top localities for bird watching. Happy Birding! :)"))),
        sliderInput("Goldfinchmonth", 
                    label = "Month Sighted:",
                    min = 1, max = 12, value = c(1), 
                    animate = animationOptions(interval = 700, loop = FALSE)
        ), 
        selectInput("Goldfinchloc", 
                    label = "Choose a locality",
                    choices = c("Calvin University Ecosystem Preserve", "Fenner Nature Center",
                                "Lake St. Clair Metropark (Metro Beach Metropark)", "Nichols Arboretum"),
                    selected = "Calvin University Ecosystem Preserve"),
        
        plotOutput("Goldfinchhist")
      ),
      
      mainPanel(leafletOutput("Goldfinchmonthmap"),
                leafletOutput("Goldfinchlocmap"),h4("Brief Analysis:"),box(width=16,style = "font-size:13px;","Based on the random sampling of the data, the American 
                                                                           Goldfinch was observed most often in Kent, Washtenaw, Wayne, Oakland and Ingham counties respectively.  
                                                                           In Kent county, the most observations occurred in April with 885 observations.  In September, Washtenaw county saw 
                                                                           its most observations of 709.  In Wayne county, the most observations occurred in January, having 690 observations.  
                                                                           Oakland county had its most observations in May, with 459 observations.   Ingham county had the most recorded observations 
                                                                           of the American Goldfinch in October, with 663 recorded observations.  The mean number of observations for Kent county was 
                                                                           approximately 509 observations, for Washtenaw county, 438, for Wayne county, 308, for Oakland county, 301, and for Ingham county, 
                                                                           the mean number of observations of the American Goldfinch was 250."))
      )
    ),
    tabItem(tabName = "chick",h2("The Black-capped Chicakdee,"),
            
            sidebarLayout(
              sidebarPanel(fluidRow(column(width=5,img(src="chick.jpg",height="100%", width="100%"),box(width=15,style = "font-size:10px;","(Poecile atricapillus) Black-capped chicakdees replace dead 
                                                                                                        neurons with new ones in the autumn to adapt to changes in their environment and social flocks.")),
                                    column(width=7,h4("Directions:",align="center"),
                                           box(width=16,style = "font-size:13px;",
                                           "Use the slider below to interactively display sightings on the top map to the right. Also experiment with the \"Choose 
                                           a Locality\" selector to interactively display sighting frequency in the below bar graph and the accompanying map on the 
                                           lower right. These bottom visualizations illustrate frequency by month at some of the top localities for bird watching. Happy Birding! :)"))),
        
        sliderInput("Chickadeemonth", 
                    label = "Month Sighted:",
                    min = 1, max = 12, value = c(1), 
                    animate = animationOptions(interval = 700, loop = FALSE)
        ), 
        selectInput("Chickadeeloc", 
                    label = "Choose a locality",
                    choices = c("Calvin University Ecosystem Preserve", "Fenner Nature Center",
                                "Lake St. Clair Metropark (Metro Beach Metropark)", "Nichols Arboretum"),
                    selected = "Calvin University Ecosystem Preserve"),
        
        plotOutput("Chickadeehist")
      ),
      
      mainPanel(leafletOutput("Chickadeemonthmap"),
                leafletOutput("Chickadeelocmap"),h4("Brief Analysis:"),box(width=16,style = "font-size:13px;","Based on the random sampling of the data, the Black-capped Chickadee 
                                                                           was observed most often in Kent, Washtenaw, Oakland, Ingham, and Macomb counties respectively.  
                                                                           Kent county has the most recorded sightings of the Black-capped Chickadee in February, with 630 observations.  
                                                                           September has the most recorded observations for Washtenaw county, totaling 575.  Oakland county’s highest number of 
                                                                           observations, 482 was recorded for November.  Ingham county recorded 493 observations, its highest number of observations 
                                                                           in May.  Macomb county had its most observation in October with 323.  The average number of observations of the Black-capped 
                                                                           Chickadee Based on the random sampling of the data was approximately 381 observations, Washtenaw 338, Oakland 267, and Ingham 
                                                                           and Macomb counties had an average observation count of 191."))
      )
    )
    )
))







































server <- function(input, output,session) {     
  
  
  
FilteredRobinmonth <- reactive({
  Robin = Robin %>% filter(Month == input$Robinmonth)
})

FilteredRobinloc <- reactive({
  Robin = Robin %>% 
    mutate(Month=month(Month,label=TRUE))%>%
    group_by(locality, Month, common_name) %>% 
    filter(locality == input$Robinloc) %>%
    summarise(Lat = latitude, Long = longitude, AveCount = mean(observation_count))
  return(Robin)
})

Robinpal <- colorNumeric(palette = c("orange","black"), domain = Robin$observation_count)

output$Robinmonthmap <- renderLeaflet({
  leaflet(FilteredRobinmonth()) %>% 
    addTiles() %>%
    setView(lng = -85.3, lat = 44, zoom = 6) %>% 
    addCircleMarkers(lat = ~latitude,lng =  ~longitude, color = Robinpal(Robin$observation_count), opacity = 0.5) %>%
    addLegend("bottomright", pal = Robinpal, values = Robin$observation_count,
              title = "Observation Count",
              opacity = 1
    ) })

output$Robinlocmap <- renderLeaflet({leaflet(FilteredRobinloc()) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    setView(lng = mean(FilteredRobinloc()$Long), lat = mean(FilteredRobinloc()$Lat), zoom = 11) %>%
    addMarkers(lng = mean(FilteredRobinloc()$Long), lat = mean(FilteredRobinloc()$Lat))
})

output$Robinhist <- renderPlot({
  ggplot(FilteredRobinloc(), aes(x= Month, y = AveCount))+
    geom_bar(stat = "identity", fill = "orange")+
    ylab("Average Observation Count")+
    ggtitle("Average Observation Count Per Month ")+
    labs(subtitle = "by locality")
})





FilteredCardinalmonth <- reactive({
  Cardinal = Cardinal %>% filter(Month == input$Cardinalmonth)
})

FilteredCardinalloc <- reactive({
  Cardinal = Cardinal %>% 
    mutate(Month=month(Month,label=TRUE))%>%
    group_by(locality, Month, common_name) %>% 
    filter(locality == input$Cardinalloc) %>%
    summarise(Lat = latitude, Long = longitude, AveCount = mean(observation_count))
  return(Cardinal)
})

Cardpal <- colorNumeric(palette = c("Red","black"), domain = Cardinal$observation_count)

output$Cardinalmonthmap <- renderLeaflet({
  leaflet(FilteredCardinalmonth()) %>% 
    addTiles() %>%
    setView(lng = -85.3, lat = 44, zoom = 6) %>% 
    addCircleMarkers(lat = ~latitude,lng =  ~longitude, color = Cardpal(Cardinal$observation_count), opacity = 0.5) %>%
    addLegend("bottomright", pal = Cardpal, values = Cardinal$observation_count,
              title = "Observation Count",
              opacity = 1
    ) })

output$Cardinallocmap <- renderLeaflet({leaflet(FilteredCardinalloc()) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    setView(lng = mean(FilteredCardinalloc()$Long), lat = mean(FilteredCardinalloc()$Lat), zoom = 11) %>%
    addMarkers(lng = mean(FilteredCardinalloc()$Long), lat = mean(FilteredCardinalloc()$Lat))
})

output$Cardinalhist <- renderPlot({
  ggplot(FilteredCardinalloc(), aes(x= Month, y = AveCount))+
    geom_bar(stat = "identity", fill = "red")+
    ylab("Average Observation Count")+
    ggtitle("Average Observation Count Per Month ")+
    labs(subtitle = "by locality")
})



FilteredBlueJaymonth <- reactive({
  BlueJay = BlueJay %>% filter(Month == input$BlueJaymonth)
})

FilteredBlueJayloc <- reactive({
  BlueJay = BlueJay %>% 
    mutate(Month=month(Month,label=TRUE))%>%
    group_by(locality, Month, common_name) %>% 
    filter(locality == input$BlueJayloc) %>%
    summarise(Lat = latitude, Long = longitude, AveCount = mean(observation_count))
  return(BlueJay)
})

BlueJaypal <- colorNumeric(palette = c("Blue","black"), domain = BlueJay$observation_count)

output$BlueJaymonthmap <- renderLeaflet({
  leaflet(FilteredBlueJaymonth()) %>% 
    addTiles() %>%
    setView(lng = -85.3, lat = 44, zoom = 6) %>% 
    addCircleMarkers(lat = ~latitude,lng =  ~longitude, color = BlueJaypal(BlueJay$observation_count), opacity = 0.5) %>%
    addLegend("bottomright", pal = BlueJaypal, values = BlueJay$observation_count,
              title = "Observation Count",
              opacity = 1
    ) })

output$BlueJaylocmap <- renderLeaflet({leaflet(FilteredBlueJayloc()) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    setView(lng = mean(FilteredBlueJayloc()$Long), lat = mean(FilteredBlueJayloc()$Lat), zoom = 11) %>%
    addMarkers(lng = mean(FilteredBlueJayloc()$Long), lat = mean(FilteredBlueJayloc()$Lat))
})

output$BlueJayhist <- renderPlot({
  ggplot(FilteredBlueJayloc(), aes(x= Month, y = AveCount))+
    geom_bar(stat = "identity", fill = "Blue")+
    ylab("Average Observation Count")+
    ggtitle("Average Observation Count Per Month ")+
    labs(subtitle = "by locality")
})


FilteredGoldfinchmonth <- reactive({
  Goldfinch = Goldfinch %>% filter(Month == input$Goldfinchmonth)
})

FilteredGoldfinchloc <- reactive({
  Goldfinch = Goldfinch %>% 
    mutate(Month=month(Month,label=TRUE))%>%
    group_by(locality, Month, common_name) %>% 
    filter(locality == input$Goldfinchloc) %>%
    summarise(Lat = latitude, Long = longitude, AveCount = mean(observation_count))
  return(Goldfinch)
})

fpal <- colorNumeric(palette = c("gold","black"), domain = Goldfinch$observation_count)

output$Goldfinchmonthmap <- renderLeaflet({
  leaflet(FilteredGoldfinchmonth()) %>% 
    addTiles() %>%
    setView(lng = -85.3, lat = 44, zoom = 6) %>% 
    addCircleMarkers(lat = ~latitude,lng =  ~longitude, color = fpal(Goldfinch$observation_count), opacity = 0.5) %>%
    addLegend("bottomright", pal = fpal, values = Goldfinch$observation_count,
              title = "Observation Count",
              opacity = 1
    ) })

output$Goldfinchlocmap <- renderLeaflet({leaflet(FilteredGoldfinchloc()) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    setView(lng = mean(FilteredGoldfinchloc()$Long), lat = mean(FilteredGoldfinchloc()$Lat), zoom = 11) %>%
    addMarkers(lng = mean(FilteredGoldfinchloc()$Long), lat = mean(FilteredGoldfinchloc()$Lat))
})

output$Goldfinchhist <- renderPlot({
  ggplot(FilteredGoldfinchloc(), aes(x= Month, y = AveCount))+
    geom_bar(stat = "identity", fill = "Gold")+
    ylab("Average Observation Count")+
    ggtitle("Average Observation Count Per Month ")+
    labs(subtitle = "by locality")
})





FilteredChickadeemonth <- reactive({
  Chickadee = Chickadee %>% filter(Month == input$Chickadeemonth)
})

FilteredChickadeeloc <- reactive({
  Chickadee = Chickadee %>% 
    mutate(Month=month(Month,label=TRUE))%>%
    group_by(locality, Month, common_name) %>% 
    filter(locality == input$Chickadeeloc) %>%
    summarise(Lat = latitude, Long = longitude, AveCount = mean(observation_count))
  return(Chickadee)
})

Chickadeepal <- colorNumeric(palette = c("grey","black"), domain = Chickadee$observation_count)

output$Chickadeemonthmap <- renderLeaflet({
  leaflet(FilteredChickadeemonth()) %>% 
    addTiles() %>%
    setView(lng = -85.3, lat = 44, zoom = 6) %>% 
    addCircleMarkers(lat = ~latitude,lng =  ~longitude, color = Chickadeepal(Chickadee$observation_count), opacity = 0.5) %>%
    addLegend("bottomright", pal = Chickadeepal, values = Chickadee$observation_count,
              title = "Observation Count",
              opacity = 1
    ) })

output$Chickadeelocmap <- renderLeaflet({leaflet(FilteredChickadeeloc()) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    setView(lng = mean(FilteredChickadeeloc()$Long), lat = mean(FilteredChickadeeloc()$Lat), zoom = 11) %>%
    addMarkers(lng = mean(FilteredChickadeeloc()$Long), lat = mean(FilteredChickadeeloc()$Lat))
})

output$Chickadeehist <- renderPlot({
  ggplot(FilteredChickadeeloc(), aes(x= Month, y = AveCount))+
    geom_bar(stat = "identity", fill = "grey39")+
    ylab("Average Observation Count")+
    ggtitle("Average Observation Count Per Month ")+
    labs(subtitle = "by locality")
})



observe({
  if(input$process_selection == 0){
    
    return()
  }
  
  isolate({
    
    if(length(input$Counties) > 6) {
      
      show_modal_gif(src="https://media.giphy.com/media/5ftsmLIqktHQA/giphy.gif", 
                     text = "Choose less counties", height="250px", width="500px", modal = "m")
      Sys.sleep(3)
      remove_modal_gif()
    }
    
    if(length(input$Counties) == 0) {
      
      show_modal_gif(src="https://media.giphy.com/media/NFZOOnYSkYZpkajwsg/giphy.gif", 
                     text = "Please choose a county", height="250px", width="500px", modal = "m")
      Sys.sleep(3)
      remove_modal_gif()
      }
    
    if(length(input$Birds) == 0) {
      
      show_modal_gif(src="https://media.giphy.com/media/l4EpjqXhHlmZdJUys/giphy.gif", 
                     text = "Please select at least one bird.", height="250px", width="500px", modal = "m")
      Sys.sleep(3)
      remove_modal_gif()
    }
    
    req(input$Birds, input$Counties)
    
    colors <- c("blue4","deepskyblue4","darkseagreen3","darkolivegreen4", "chartreuse4")
    nrow_var <- ceiling(length(input$Counties)/2)
    n_plotter <- length(input$Counties)
    Counties <- c(input$Counties)
    Birds <- c(input$Birds)
    count_count <- as_vector(length(input$Counties)) 
    
    bird_data_mod <- bird_data %>%
      mutate(obs_date = ymd(observation_date), 
             month = month(obs_date, 
                           label = TRUE, 
                           abbr = TRUE )) %>%
      select(common_name, month, observation_count, county)%>%
      group_by(common_name, month, county) %>%
      summarise(obs_count = sum(observation_count)) %>%
      filter(common_name %in% input$Birds & county %in% input$Counties)
    
    if(n_plotter >= 3) {
      ncol_var <-  3
    } else if(n_plotter == 2) {
      ncol_var <-  2
    }else{
      ncol_var <- 1
    }
    
    if(n_plotter > 3){
      plotter <-  445
    }else {
      plotter <- 445
    }
    
    if(n_plotter == 1) {
      plotter_w <- 800
    } else if(n_plotter == 2){
      plotter_w <- 800
    } else {
      plotter_w <- 800
    }
    
    
    # output$plot_size <- renderText({plotter})
    
    output$distPlot <- renderPlot({
      if(length(Counties) > 6 | length(Counties) == 0 ) {
        
      }else{
        
        ggplot(subset(bird_data_mod, county %in% Counties),mapping = aes(x = month, y = obs_count, fill = common_name)) + 
          geom_bar(stat = "identity") +
          theme(axis.title=element_text(size=14,face="bold"),
                legend.title=element_text(size=12,face = "bold"),
                legend.text=element_text(size=11),
                panel.background = element_blank())+
          facet_wrap(.~county, scales = 'fixed', ncol = ncol_var) +
          scale_fill_manual(values=colors) +
          theme(axis.text.x = element_text(angle = 90, size = 11, color = "black"),
                axis.text.y = element_text(size = 11, color = "black"),
                axis.line = element_line(colour = "black"))+
          labs(title = "Count of Bird Sightings, by Month and County",
               subtitle = paste(Birds, collapse = ", "),
               x = "Month",
               y = "Sightings Count",
               fill = "Bird Type") 
        
        #nrow = nrow_var     
      }
    }, height = plotter, width = plotter_w)
    
    output$table <- renderDT(bird_data_mod,
                             filter = "top",
                             options = list(
                               pageLength = 10
                             ))
    
  })
  
})    

observe({
  if(input$clear_choices == 0){
    
    return()
  }
  
  isolate({
    updateSelectInput(session, "Counties", selected = "")
    updateSelectInput(session, "Birds", selected = "")
    output$distPlot <- NULL
    output$table <- NULL
  })
})  







}

shinyApp(ui, server)