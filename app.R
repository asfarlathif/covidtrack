
#LOADING DEPENDENCIES

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(DT)
library(leaflet)
library(plotly)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)

options(shiny.autoreload = TRUE)

#IMPORTING COVID19 DATA FROM DATABASE
covid_data <-
    read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#EARTH MAP DATA
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points <- sf::st_centroid(world)

#DATA CLEANING AND TIDYING

covid_data$date <- as.Date(covid_data$date) 

#adding admin (location) column from world data to covid data
covid_data <- rename(covid_data, adm0_a3 = iso_code) %>%
    left_join(select(world, adm0_a3, admin), by = "adm0_a3") %>%
    select(-geometry)

#imputing missing admin locations
covid_data[covid_data$adm0_a3 == "OWID_KOS", 51] <- "Kosovo"
covid_data[covid_data$adm0_a3 == "OWID_WRL", 51] <- "World"

#finding the latest total number of cases for each locations
case <- select(covid_data, adm0_a3, admin,total_cases) %>%
    group_by(adm0_a3) %>%
    nest(total_cases = total_cases) %>%
    transmute(admin, total_cases = map_dbl(total_cases, max, na.rm = T)) %>%
    filter(total_cases > 0)

#finding the latest total number of deaths for each locations
death <- select(covid_data,admin,total_deaths) %>%
    group_by(admin) %>%
    nest(total_deaths = total_deaths) %>%
    transmute(total_deaths = map_dbl(total_deaths, max, na.rm = T)) %>%
    filter(total_deaths > 0)

#merging case and death data
data1 <- left_join(case, death, by = "admin")

#merging world data from case and death data
world <- left_join(world, select(data1, -admin), by = "adm0_a3")

world_points <-
    cbind(world, st_coordinates(sf::st_centroid(world$geometry)))

#ADJUSTING THE COORDINATES FOR FRANCE
world_points$X[world_points$admin == "France"] = 3
world_points$Y[world_points$admin == "France"] = 46


# DEFINING UI
ui <- fluidPage(
    
    #UI theme -- "UNITED"
    theme = shinytheme("cyborg"),
    
    # Application title
    titlePanel("COVID19 Tracker"),
    
    # Sidebar layout
    sidebarLayout(
        
        #SIdebar panel
        sidebarPanel(
            
            #Dropdown menu to choose country
            selectInput(
                "country",
                label = "Country",
                choices = c("", unique(world$admin))
            ),
            
            #Data about total cases, deaths and new cases
            strong(verbatimTextOutput("stats")),
            
            br(), br(),
            
            #DATA SOURCE
            "This app uses data from ",
            em(a("Our World in Data", href = "https://ourworldindata.org/")), 
            ". To visit their GitHub repository,", 
            "Click ", a("here", href ="https://github.com/owid/covid-19-data/tree/master/public/data"),
            
            br(), br(),
            
            "This app is created by Asfar Lathif",
            "View app code", a("here", href =)
        ),
        
        
        
        # Main Panel
        mainPanel(
            # country Name
            h2(strong(verbatimTextOutput("name"))),
            
            #Tab Panels
            tabsetPanel
            (
                #Tab 1: Leaflet Map and Case and Death Table (Country-wise) 
                tabPanel("Map",leafletOutput("Map"),
                         br(), br(), 
                         DTOutput("table")),
                
                #Tab 2: Number of Cases - bar graph and table
                tabPanel("Daily Cases", plotlyOutput("CaseCurve"),
                         br(),
                         em("Zoom in on the plot to filter the data table below. 
                            Double click to set it back to default"),
                         br(), br(), 
                         downloadButton("download1", "Download results"),
                         br(), br(),
                         DTOutput("table1")),
                
                #Tab 3: Number of Deaths - bar graph and table
                tabPanel("Deaths",plotlyOutput("DeathCurve"), 
                         br(),
                         em("Zoom in on the plot to filter the data table below. 
                            Double click to set it back to default"),
                         br(), br(), 
                         downloadButton("download2", "Download results"),
                         br(), br(),
                         DTOutput("table2"))
            ),
        )
    ))


# DEFINING SERVER
server <- function(input, output) {
    
    #Filetring covid data based on input country
    covid_data1 <- reactive({
        
        if (input$country != "")
            covid_data1 <- filter(covid_data, admin == input$country)
        else
            # "World" is the default
            covid_data1 <- filter(covid_data, admin == "World")
        
    })
    
    ##Country Name
    output$name <- renderText({
        paste0("Country:", "   ", input$country)
        
    })
    
    ##Data about total cases, deaths and new cases  - on the sidebar panel
    output$stats <- renderText({
        
        name <- ifelse(input$country != "", input$country, "World")
        
        data1 <- filter(data1, admin == name)
        
        #filtering the latest number of total cases and deaths
        newcase <- (covid_data1() %>% arrange(desc(date)) %>% slice_head())
        
        paste(paste0("Total cases:", "   ", data1$total_cases),
              paste0("Total Deaths:", "   ", data1$total_deaths),
              paste0("New cases:", "   ", newcase$new_cases),
              sep = "\n")
    })
    
    output$Map <- renderLeaflet({
        
        #filtering coordinates based on input country
        if (input$country != "") {
            world_points <- filter(world_points, admin == input$country)
            lon <- world_points$X
            lat <- world_points$Y
            zoom <- 4
        }
        else{
            lon <- 0
            lat <- 30
            zoom <- 1
        }
        
        #Total cases in the World
        world_total <- (data1 %>% filter(adm0_a3 == "OWID_WRL"))$total_cases
        
        #Interactive Leaflet map
        leaflet() %>%
            
            setView(lng = lon,
                    lat = lat,
                    zoom = zoom) %>% #Setting zooming based on coordinates
            
            addTiles() %>% #country layer
            
            #Adding Cirlce marks that are proportional to the number of cases in each country
            addCircleMarkers(
                world_points$X,
                world_points$Y,
                color = "red",
                radius = world_points$total_cases * 100 / world_total,
                
                #Appears on clicking the marker
                popup = paste0(world_points$admin, br(),
                               "Total Cases: ", world_points$total_cases, br(),
                               "Total Deaths: ", world_points$total_deaths),
                
                #Appears on hovering over the marker
                label = world_points$admin
            )
    })
    
    #Countrywise Total cases and deaths table (Tab 1)
    output$table <- renderDT({
        
        t <- data1 [,-1] %>% 
            arrange(desc(total_cases),.by_group = FALSE) %>% 
            rename(Country = admin, 
                   `Total Cases` = total_cases, 
                   `Total Deaths` = total_deaths)
        
        if(input$country != "")
            t <- t %>% filter(Country == input$country)
        
        datatable(t, 
                  style = "bootstrap", #Theme
                  options = list(sDom  = '<"top">lt<"bottom">ip')) #Options to remove search bar
        
    })
    
    #Rendering Cases Graph
    output$CaseCurve <- renderPlotly({
        
        #plotly interactive graph
        c <- plot_ly(data = covid_data1(), x = ~date, y = ~ new_cases, 
                     type = "bar", mode = "markers", source = "c") %>%  
            
            layout(xaxis = list(title = ""),
                   yaxis = list(title = "Daily Cases"),
                   fixedrange = FALSE, 
                   autosize = TRUE) %>% 
            
            #event register to filter data based on zooming
            event_register("plotly_relayout")
    })
    
    #Rendering Deaths Graph
    output$DeathCurve <- renderPlotly({
        
        #plotly interactive graph
        d <-  plot_ly(data = covid_data1(), x = ~date, y = ~ new_deaths, 
                      type = "bar", mode = "markers", source = "d") %>% 
            
            layout(xaxis = list(title = ""),
                   yaxis = list(title = "Daily Deaths"),
                   fixedrange = FALSE, 
                   autosize = TRUE) %>% 
            
            #event register to filter data based on zooming
            event_register("plotly_relayout")
    })
    
    #Extracting zooming details from the cases graph - dates from x axis
    xaxis1 <- reactive({
        
        c1 <- event_data("plotly_relayout", "c")
        
        c <- c(c1$`xaxis.range[0]`, c1$`xaxis.range[1]`)
        
        if(all(is.null(c)))
        {
            #No zooming ~ no filtering - all dates from (31-12-2019 to Present day)
            a <- min(covid_data$date)
            b <- max(covid_data$date)
            return(c(a,b))
        }
        
        else{
            a <- as.Date(c[1], origin="1970-01-01")+1
            b <- as.Date(c[2], origin="1970-01-01")
            return(c(a,b))
        }
    })
    
    #Extracting zooming details from the deaths graph - dates from x axis
    xaxis2 <- reactive({
        
        d1 <- event_data("plotly_relayout", "d")
        
        d <- c(d1$`xaxis.range[0]`, d1$`xaxis.range[1]`)
        
        if(all(is.null(d)))
        {
            #No zooming ~ no filtering - all dates from (31-12-2019 to Present day)
            a <- min(covid_data$date)
            b <- max(covid_data$date)
            return(c(a,b))
        }
        
        else{
            a <- as.Date(d[1], origin="1970-01-01")+1
            b <- as.Date(d[2], origin="1970-01-01")
            return(c(a,b))
        }
    })
    
    #Datatable for Cases and Deaths
    tablemain <- reactive({
        
        #Cases table
        t1 <- covid_data1() %>% 
            
            select(admin, date, total_cases, new_cases) %>%
            
            rename(Country = admin, Date = date,
                   `Total Cases` = total_cases, 
                   `New Cases` = new_cases) %>%
            
            #filtering based on zooming
            filter(Date %in% c(xaxis1()[1]:xaxis1()[2]))
        
        #Deaths table
        t2 <- covid_data1() %>% 
            
            select(admin, date, total_deaths, new_deaths) %>%
            
            rename(Country = admin, Date = date,
                   `Total Deaths` = total_deaths, 
                   `New Deaths` = new_deaths) %>% 
            
            #filtering based on zooming
            filter(Date %in% c(xaxis2()[1]:xaxis2()[2]))
        
        return(list(t1, t2))
        
    })
    
    #Rendering Cases Datatable
    output$table1 <- renderDT({

        datatable(tablemain()[[1]],
                  style = "bootstrap", #Theme
                  options = list(sDom  = '<"top">lt<"bottom">ip')) #Options to remove search bar
        
    })
    
    #Rendering Deaths Datatable
    output$table2 <- renderDT({

        datatable(tablemain()[[2]],
                  style = "bootstrap", #Theme
                  options = list(sDom  = '<"top">lt<"bottom">ip')) #Options to remove search bar
        
    })
    
    #Download handler for Cases Table
    output$download1 <- downloadHandler(
        filename = function() {
            "COVID19_Cases.csv"
        },
        content = function(con) {
            write.csv(tablemain()[[1]], con)
        }
    )
    
    #Download handler for Deaths Table
    output$download2 <- downloadHandler(
        filename = function() {
            "COVID19_Deaths.csv"
        },
        content = function(con) {
            write.csv(tablemain()[[2]], con)
        }
    )
}
# Run the application
shinyApp(ui = ui, server = server)