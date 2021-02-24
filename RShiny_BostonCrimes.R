#-------------------------------------------------------------------------------------------------
# MSIS 2506- Fall 2019- Project 4 (Final Project)
# Authors : Lahari Kuchibhotla
#           Suchita Negi
#           Stuti Sanghavi
#           Tanmayi Varanasi
# Project : Exploratory Data Analysis of Crime data in Boston City.
# Dataset : https://www.kaggle.com/AnalyzeBoston/crimes-in-boston
#----------------------------------------------------------------------------------------------------

## Installing required packages for project in R

if (!require(shiny))
  install.packages("shiny")               #install R shinny app if not already installed in R .
if (!require(tidyverse))                  #install tidyverse which includes dplyr ,lubridate and ggplot package within itself.
  install.packages("tidyverse")           #This helps manipulate dataset, work with dates and plot graphs.
if (!require(leaflet))
  install.packages("leaflet")             #install leaflet package to plot maps (GIS Data)
if (!require(europepmc))
  install.packages("europepmc")           #install europepmc to get the hits on dataset query.
if (!require(ggthemes))
  install.packages("ggthemes")            #install ggthemes package to set different themes for the shinyapp layout.
if (!require(shinyWidgets))
  install.packages("shinyWidgets")        #install shinyWidgets package for custom input widgets like sliderws, radio buttons etc.
if (!require(shinythemes))
  install.packages("shinythemes")
if (!require(DT))
  install.packages("DT")                  #install DT and datatable package for R interface of data tables in R shiny .
if (!require(data.table))
  install.packages("data.table")
if (!require(forecast))
  install.packages("forecast")            # install forecast package for time series analysis.

## Loading Libraries required for the data analysis.
library(shiny)
library(tidyverse)
library(leaflet)
library(europepmc)
library(ggthemes)
library(shinyWidgets)
library(data.table)
library(DT)
library(shinythemes)
library(forecast)

#----------------------------------------  CRIME IN BOSTON DATASET -----------------------------------------------------------------------
#Reading the csv data set into R
crime <- read.csv(file = file.path("crimes-in-boston", "crime.csv"))

#----------------------------------------  Cleaning the data  ---------------------------------------------------------------------------
#Omitting the NAs in the dataset.
crime <- na.omit(crime)

#Removing the latitude and longitude that do not correspond to the city of Boston
crime <- crime[!(crime$Lat == -1 & crime$Long == -1), ]

#----------------------------------------  sub-datasets  ---------------------------------------------------------------------

#Finding min and max months for each year (for slider input)

#Count of each month for every year and sorting the data
month_count_per_year <- count(crime, MONTH, YEAR)
month_count_per_year <-
  month_count_per_year[with(month_count_per_year, order(YEAR,-n, MONTH)),]
sorted_months <-
  month_count_per_year[with(month_count_per_year, order(YEAR,-n, MONTH)),]

#Filtering data to get start and end months for year 2015
months_2015 <- filter(sorted_months, YEAR == "2015")
#Extracting minimum and maximum months in the year 2015
min_month_2015 <- min(months_2015[, 1])
max_month_2015 <- max(months_2015[, 1])

#Filtering data to get start and end months for year 2016
months_2016 <- filter(sorted_months, YEAR == "2016")
min_month_2016 <- min(months_2016[, 1])
max_month_2016 <- max(months_2016[, 1])

#Filtering data to get start and end months for year 2017
months_2017 <- filter(sorted_months, YEAR == "2017")
min_month_2017 <- min(months_2017[, 1])
max_month_2017 <- max(months_2017[, 1])

#Filtering data to get start and end months for year 2018
months_2018 <- filter(sorted_months, YEAR == "2018")
min_month_2018 <- min(months_2018[, 1])
max_month_2018 <- max(months_2018[, 1])

# ------------------------------- sub-datasets for Tab 1:Crime Locations and Tab 2: Trends ------------------------------------------------

#Data for top 5 crimes each year
crimes_each_year <- count(crime, OFFENSE_CODE_GROUP, YEAR)
crimes_each_year <- as.data.frame(crimes_each_year)

#Sorting crimes by grouping it by year, number of crimes in decsending order
crime_sorted <-
  crimes_each_year[with(crimes_each_year, order(YEAR, -n, OFFENSE_CODE_GROUP)), ]
top5_crimes_per_year <- c()
hits_year <- unique(crime_sorted$YEAR)

#Loop for selecting the top 5 crimes each year
for (x in hits_year)
{
  hits_crimes_year <- subset(crime_sorted, crime_sorted$YEAR == x)
  top5 <- head(hits_crimes_year, 5)
  top5_crimes_per_year <- rbind(top5_crimes_per_year, top5)
}

#Crimes per district
district <- table(crime$DISTRICT)
district <- na.omit(district)

#Ordering the districts in descending order
ordered_by_district <- sort(district, decreasing = TRUE)
ordered_by_district <- as.data.frame(ordered_by_district)

#Renaming the columns
colnames(ordered_by_district) <- c("District", "Offenses")

#Number of crimes for Top 5 offense group
top_5_crimes <- crime %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarise(num_crime = n()) %>%
  arrange(desc(num_crime)) %>%
  head(5)

#Top 5 streets with number of crimes
top_5_street <- crime %>%
  group_by(STREET) %>%
  summarise(num_crime = n()) %>%
  arrange(desc(num_crime)) %>%
  head(5)

#Overall crime trends over the years
trend_by_year <-
  epmc_hits_trend('crime', period = 2015:2018, synonym = FALSE)


#-------------------------------- sub-datasets for Tab 3 : Monthly UCR Data ------------------------------------------------

#Number of monthly crimes based on UCR_PART
ucr_month <- crime %>%
  filter(!is.na(UCR_PART)) %>%
  filter(!(UCR_PART == "")) %>%
  group_by(MONTH, UCR_PART) %>%
  summarise(crime_count = n())


#------------------------------- sub-datasets for Tab 4 : Yearly UCR Data -----------------------------------------------------------------

#Number of yearly crimes based on UCR_PART
ucr_year <- crime %>%
  filter(!is.na(UCR_PART)) %>%
  filter(!(UCR_PART == "")) %>%
  group_by(YEAR, UCR_PART) %>%
  summarise(crime_count = n())

#------------------------------- sub-datasets for Tab 5 : Forecast ---------------------------------------------------------

#Time series data
ts_crimes = ts(ucr_year$crime_count,
               start = 2015,
               frequency = 3)


#Seasonal Time series
time_series_decompose <- decompose(ts_crimes)

#--------------------------------------------   UI    ---------------------------------------------------------------------------------


#Launching the shiny app
ui <- fluidPage(
  #Define theme for the page.
  theme = shinytheme("superhero"),
  #------------------------------ Application title ------------------------------------
  titlePanel(h1(
    id = "title", "Exploratory Analysis of Boston Crime Data"
  )),
  
  tags$style(
    HTML("#title{color: orange;font-size: 40px;font-style: bold;}")
  ),
  
  titlePanel(
    h2(
      id = "Authors",
      "Authors: Lahari Kuchibhotla, Suchita Negi, Stuti Sanghavi, Tanmayi Varanasi"
    )
  ),
  tags$style(
    HTML("#Authors{color: white;font-size: 20px;font-style: italic;}")
  ),
  
  #----------------------------- Sidebar with a slider input for number of bins --------------------------------------
  sidebarLayout(
    sidebarPanel(
      helpText("Let's view the crimes on a map."),
      #allows the side panel to move as your scroll
      style = "position:fixed;width:inherit;",
      
      selectInput(
        "crime_by_hour",
        label = "Choose one of the Top Offenses",
        choices = list(
          "Larceny",
          "Investigate Person",
          "Medical Assistance",
          "Motor Vehicle Accident Response",
          "Other"
        ),
        selected = "Larceny",
        #default value selected
        multiple = TRUE
      )
      ,
      fluidRow(column(
        3,
        radioButtons(
          "year",
          h3("Choose Year"),
          choices = list(
            "2015" = 2015,
            "2016" = 2016,
            "2017" = 2017,
            "2018" = 2018
          ),
          selected = 2015          #default value selected
        )
        
      )),
      sliderInput(
        inputId = "month",
        label = "Choose Month:",
        value = c(1, 12),
        min = 1,
        max = 12
      ),
      sliderTextInput(
        "day",
        "Select a Day of the Week",
        choices = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        ),
        selected = c("Monday"),
        #default value selected
        animate = FALSE,
        grid = TRUE,
        hide_min_max = FALSE,
        from_fixed = FALSE,
        to_fixed = FALSE,
        from_min = NULL,
        from_max = NULL,
        to_min = NULL,
        to_max = NULL,
        force_edges = FALSE,
        width = 400,
        pre = NULL,
        post = NULL,
        dragRange = TRUE
      )
      
    ),
    
    
    #----------------------------------  Main Panel  --------------------------------------------------
    mainPanel(
      h2("Let's Explore Crime Data in Boston"),
      textOutput("selected_var"),
      #Creating different Tabs for the Analysis
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Crime Locations",
          br(),
          plotOutput("top5crimes"),
          br(),
          br(),
          textOutput("dynamic1"),
          leafletOutput("mymap"),
          br(),
          br(),
          plotOutput("district")
        ),
        tabPanel(
          "Trends",
          br(),
          textOutput("dynamic2"),
          plotOutput("monthly_trend"),
          br(),
          br(),
          textOutput("dynamic3"),
          plotOutput("crimes_by_hour"),
          br(),
          br(),
          plotOutput("overall_trend")
        ),
        tabPanel(
          "Monthly UCR Data",
          br(),
          textOutput("ucr_text"),
          textOutput("ucr_part1"),
          textOutput("ucr_part2"),
          textOutput("ucr_part3"),
          br(),
          br(),
          plotOutput("ucr_monthly"),
          br(),
          br(),
          DT::dataTableOutput("table_month")
        ),
        tabPanel(
          "Yearly UCR Data",
          br(),
          textOutput("ucr_text_yr"),
          textOutput("ucr_part1_yr"),
          textOutput("ucr_part2_yr"),
          textOutput("ucr_part3_yr"),
          br(),
          br(),
          plotOutput("ucr_yearly"),
          br(),
          br(),
          DT::dataTableOutput("table_year")
          
        ),
        tabPanel(
          "Forecast",
          br(),
          plotOutput("ts_year"),
          br(),
          br(),
          plotOutput("tsdecom")
        )
      ),
      #adding html style to the data table.
      tags$style(
        HTML(
          "
          .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          color: #ffffff;
          }
          
          .dataTables_wrapper .dataTables_paginate .paginate_button{color:white;box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:white !important;border:1px solid transparent;border-radius:2px}
          
          #To change text and background color of the `Select` box #
          .dataTables_length select {
          color: #000000;
          background-color: white
          }
          
          #To change text and background color of the `Search` box #
          .dataTables_filter input {
          color: #ffffff;
          background-color: #000000
          }
          
          thead {
          color: #ffffff;
          }
          
          tbody {
          color: black;
          }
          
          "
        )
        ),
      #Supress Warnings for the shiny output
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      )
      
        )
        )
        )


#-------------------------------------------------- Server-------------------------------------------------------------

server <- function(input, output, session) {
  #------------------------------------------ Tab 1 : Crime Locations -----------------------------------------------------------
  
  # Text output for showing dynamic graphs
  output$dynamic1 <- renderText({
    paste("Following graph changes with all the filters on the side panel:")
  })
  
  # Text output for showing dynamic graphs
  output$dynamic2 <- renderText({
    paste("Following graph changes with year and month filters on the side panel:")
  })

  # Text output for showing dynamic graphs
  output$dynamic3 <- renderText({
    paste("Following graph changes with all the filters on the side panel:")
  })
  
  
  #Plot:top 5 crimes for each year
  output$top5crimes <- renderPlot({
    ggplot(data = top5_crimes_per_year,
           aes(x = YEAR, y = n, fill = OFFENSE_CODE_GROUP)) +
      geom_col(position = position_dodge()) +
      labs(title = "Top 5 Crimes Commited each Year",
           y = "Number of Crimes Committed",
           x = "Year") + theme_bw(base_size = 10) +
      scale_fill_manual(
        "Offense",
        values = c(
          "Drug Violation" = "red",
          "Investigate Person" = "orange",
          "Larceny" = "darkgreen",
          "Medical Assistance" = "yellow",
          "Motor Vehicle Accident Response" = "blue",
          "Other" = "black",
          "Simple Assault" = "grey",
          "Vandalism" = "magenta"
        )
      )
  })
  
  
  #Plot: total number of crimes per district in descending order
  districtplot <- reactive({
    offense <- subset(ordered_by_district, Offenses == input$offense)
    offense_table <- with(off, table(District, Offenses))
    offense_district <- data.frame(offense_table)
    colnames(offense_district) <- c("District", "Offenses")
  })
  
  #Plot: crimes for district
  output$district <- renderPlot({
    ggplot(data = ordered_by_district,
           main = "Deaths for each District",
           aes(x = District, y = Offenses)) +
      geom_col(position = position_dodge()) +
      labs(title = "Crimes Commited per District",
           y = "Number of Crimes Committed",
           x = "District") + theme_bw(base_size = 10)
  })
  
  #if the user selects multiple offenses to view them the circle markers will be different colors
  pal <- colorFactor(
    palette = c('red', 'green', 'navy', 'purple', 'black'),
    domain = top_5_crimes$OFFENSE_CODE_GROUP
  )
  
  #Geospatial map of the crimes
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(color = ~ pal(OFFENSE_CODE_GROUP),
                       data = crimebyhour())
  })
  
  #------------------------------------- Tab 2 : Trends ---------------------------------------------------------
  
  #Storing the minimum and maximum values of the month the user selects on the slider for each year
  observe({
    if (input$year == "2015") {
      updateSliderInput(
        session,
        "month",
        min = min_month_2015,
        max = max_month_2015,
        value = c(min_month_2015, max_month_2015)
      )
    } else if (input$year == "2018") {
      updateSliderInput(
        session,
        "month",
        min = min_month_2018,
        max = max_month_2018,
        value = c(min_month_2018, max_month_2018)
      )
    } else{
      updateSliderInput(
        session,
        "month",
        min = 1,
        max = 12,
        value = c(1, 12)
      )
    }
  })
  
  #Storing data for each year into separate tables for plotting graphs.
  filteredData <- reactive({
    if (input$year == "2015") {
      subset_year <-
        subset(crime,
               YEAR == input$year &
                 MONTH > min_month_2015 - 1 &
                 MONTH < max_month_2015 + 1)
      table_year <- with(subset_year, table(YEAR, MONTH))
      axisRange <-
        table_year[, which(colnames(table_year) == input$month[1]):which(colnames(table_year) == input$month[2])]
    } else if (input$year == "2018") {
      subset_year <-
        subset(crime,
               YEAR == input$year &
                 MONTH > min_month_2018 - 1 &
                 MONTH < max_month_2018 + 1)
      table_year <- with(subset_year, table(YEAR, MONTH))
      axisRange <-
        table_year[, which(colnames(table_year) == input$month[1]):which(colnames(table_year) == input$month[2])]
    } else{
      subset_year <-
        subset(crime, YEAR == input$year & MONTH > 0 & MONTH < 13)
      table_year <- with(subset_year, table(YEAR, MONTH))
      axisRange <-
        table_year[, which(colnames(table_year) == input$month[1]):which(colnames(table_year) == input$month[2])]
    }
  })
  
  #Plot: The number of death per month for each year
  output$monthly_trend <- renderPlot({
    barplot(
      filteredData(),
      beside = TRUE,
      main = "Number of Deaths Per Month",
      xlab = "Month",
      ylab = "Number of Deaths",
      legend = FALSE,
      col = terrain.colors(12)
    )
  })
  
  
  # breakdown of the crimes per hour
  crimebyhour <- reactive({
    hourly <-
      subset(
        crime,
        OFFENSE_CODE_GROUP == input$crime_by_hour &
          YEAR == input$year &
          MONTH == input$month & DAY_OF_WEEK == input$day
      )
  })
  
  
  # Plot: Data for number of top 5 crimes each hour.
  output$crimes_by_hour <- renderPlot({
    crimebyhour() %>%
      inner_join(top_5_crimes, by = "OFFENSE_CODE_GROUP") %>%
      group_by (OFFENSE_CODE_GROUP, HOUR) %>%
      summarise(count = n()) %>%
      ggplot (aes(x = HOUR, y = count, fill = OFFENSE_CODE_GROUP)) +
      geom_bar(stat = "identity") +
      ggtitle ("Number of crimes per hour by top 5 crimes") +
      theme_bw()
  })
  
  
  #Plot: Trend for all crimes for 2015-2018
  output$overall_trend <- renderPlot({
    ggplot(trend_by_year, aes(year, all_hits)) + geom_point() + geom_line(color='red',size=2) +
      xlab("Year") +
      ylab("Total Number of Crimes") + ggtitle("Total Number of Crimes each Year")
  })
  
  #--------------------------------------------- Tab 3 : Monthly UCR Data -----------------------------------------------------------------------------
  # Text output displaying UCR categories
  output$ucr_text <- renderText({
    paste("Crimes in the United States are categorized by the Uniform Crime Reporting (UCR) index.")
  })
  output$ucr_part1 <- renderText({
    paste("Part 1: Crimes are serious offenses such as aggravated assult, homicide, etc.")
  })
  output$ucr_part2 <- renderText({
    paste("Part 2: Crimes are less severe such as embezzelment or fraud.")
  })
  output$ucr_part3 <- renderText({
    paste(
      "Part 3: Crimes are the least serious offenses such as vandalism, property investigation, etc."
    )
  })
  
  # Plot: Data for monthly crime based on UCR parts
  output$ucr_monthly <- renderPlot({
    ggplot(ucr_month,
           aes(
             x = MONTH,
             y = crime_count ,
             fill = UCR_PART ,
             colour = UCR_PART
           )) + geom_point(size = 2) + geom_line(size = 1) +
      xlab("Month") +
      ylab("Total Number of Crimes") + ggtitle("Crimes based on UCR Part")
  })
  
  #Generate Table with monthly UCR crime information
  output$table_month <- DT::renderDataTable({
    #DT::datatable(ucr_month2[, input$show_vars, drop = FALSE])
    DT::datatable(ucr_month, options = list(orderClasses = TRUE)) %>%
      formatStyle(
        columns = "MONTH",
        target = "cell",
        color = "orange",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "UCR_PART",
        target = "cell",
        color = "teal",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "crime_count",
        target = "cell",
        color = "Red",
        fontWeight = 'bold'
      )
  })
  
  
  #---------------------------------  Tab 4 : Yearly UCR Data  ---------------------------------------------------------------------------------------
  
  # Text output displaying UCR categories
  output$ucr_text_yr <- renderText({
    paste("Crimes in the United States are categorized by the Uniform Crime Reporting (UCR) index.")
  })
  output$ucr_part1_yr <- renderText({
    paste("Part 1: Crimes are serious offenses such as aggravated assult, homicide, etc.")
  })
  output$ucr_part2_yr <- renderText({
    paste("Part 2: Crimes are less severe such as embezzelment or fraud.")
  })
  output$ucr_part3_yr <- renderText({
    paste(
      "Part 3: Crimes are the least serious offenses such as vandalism, property investigation, etc."
    )
  })
  
  # Plot: Data for yearly crime based on UCR parts
  output$ucr_yearly <- renderPlot({
    ggplot(data = ucr_year,
           main = "Crimes based on UCR_Part",
           aes(x = YEAR, y = crime_count, fill = UCR_PART)) +
      geom_col(position = position_dodge()) +
      labs(title = "Boston Crimes Based on UCR PART",
           y = "Number of Crimes Committed",
           x = "Year") + theme_bw(base_size = 10)
    
  })
  # Generate Table with Yearly UCR crime information.
  output$table_year <- DT::renderDataTable({
    DT::datatable(ucr_year, options = list(orderClasses = TRUE)) %>%
      formatStyle(
        columns = "YEAR",
        target = "cell",
        color = "orange",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "UCR_PART",
        target = "cell",
        color = "teal",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "crime_count",
        target = "cell",
        color = "Red",
        fontWeight = 'bold'
      )
  })
  
  #----------------------------------------  Tab 5 : Forecast  -----------------------------------------------------------------------------------
  
  #Plot: Time Series Analysis
  output$ts_year <- renderPlot({
    plot(ts_crimes)
    title("Time Series Analysis for Crimes in Boston")
  })
  
  #Plot: Decomposing Time series
  output$tsdecom <- renderPlot({
    plot(time_series_decompose)
  })
}


################################################   Run on Server  ###############################################################

# Run the R Shinny Application
shinyApp(ui = ui, server = server)
