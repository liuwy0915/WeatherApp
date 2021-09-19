library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinythemes)
library(shinybusy)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(httr)
library(stats)
library(sf)
library(janitor)
library(tibble)
library(USAboundaries)
library(lubridate)
library(forecast)

source("R/visualization.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    
    useShinyalert(),
    useShinyjs(),
                
    titlePanel(h1("Weather Explore")),
    
    navbarPage(
        
        title = strong("MENU"),
        
        navbarMenu(
            
            strong("Guidline"),
            
            tabPanel(
                "About this App",
                div(
                    align = "left",
                    h2("About this App"),
                    h3("Who am I?"),
                    p("I am an app helping you on weather data visualization.
                    You can use me to get visualization on both ",
                      strong("geographical"), " and ", strong("time"),
                      "dimension. My weather data comes from ",
                      strong("OpenWeather API"), ", and I am built based on ",
                      strong("ShinyApp"),
                      ". You can see the details in References.",
                      style = "font-si30pt"),
                    hr(),
                    h3("How to use me?"),
                    h4("Geographical Dimension"),
                    p("If you want to view weather information in geographical
                      dimension, please click ",
                      span(strong("View on Map"), style = "color:blue"),
                      " button in the menu."),
                    p("In this page, you can select any state in the US, and
                      get map visualizations of weather information in the
                      future 48-hour for all observable cities in this state.
                      Firstly, you need to ", strong("select a state"),
                      "in the state box, and type-in your ",
                      strong("API Key"), "(or you can either use our default
                      key). For example, let’s select ", em("Kansas"),
                      "and use the default key by ticking the box:"),
                    br(),
                    img(src = "img1.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    p("Then we can click the ",
                      span(strong("Fetch Data"), style = "color:blue"),
                      " button to get 48-hour weather data in ", em("Kansas"),
                      ". This may take a few minutes since the ",
                      strong("OpenWeather API"), " has a limit of calling per
                    minute. After that, a series of map about ",
                      strong("temperature, humidity, visibility, and clouds level"),
                      "in ", em("Kansas"), " at current hour will be displayed.
                    Weather of each city will also be displayed on the maps.
                    For example, the temperature map:"),
                    br(),
                    img(src = "img2.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    p("Note that the color of temperature is colored by
                      counties. The temperature of each country is represented
                      by the average temperature of all observed cities in
                      this county. (Similar for humid, visibility, and clouds
                      level.) Now if we do not want to see current hour, but
                      instead 38 hours later, we can just ",
                      strong("scroll the time bar"), " to 38:"),
                    br(),
                    img(src = "img3.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    p("After waiting for a few seconds, the plots will be
                      automatically changed to show forecasted weathers
                      38 hours later. For example, the temperature map:"),
                    br(),
                    img(src = "img4.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    h4("Time Dimension"),
                    p("If you want to view weather information in time
                      dimension, please click ",
                      span(strong("View by Time"), style = "color:blue"),
                      " button in the menu."),
                    p("In this page, you can select at most 3 cities in the US,
                      and obtain their comparison visualizations of weather
                      information in the past 5-day and the future 48-hour.
                      Firstly, you can ", strong("select the state"),
                      ", then all available cities in this state will be
                      automatically included in the ",
                      strong("city selection box"),
                      ". Pick the city you want: "),
                    br(),
                    img(src = "img4.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    br(),
                    p("Note that you must at least select ",
                      strong("state1/city1"), " and your selected cities must
                      be different. Otherwise, the app will not process your
                      request. However, you can leave the second and/or the
                      third city blank."),
                    p("Click the ",
                      span(strong("Fetch Data"), style = "color:blue"),
                      " button and wait for a few seconds, several
                      visualizations, including ",
                      strong("weather grid map, temperature time series plot,
                      temperature circle heatmap, humidity time seris plot"),
                      ", will be provided on the ",
                      span(strong("Historical Weather Records"),
                           style = "color:blue"),
                      " page. For example, if we compare weather records in ",
                      em("Austin, Texas"), " with ", em("Juneau, Alaska"),
                      ", we will get the following plots:"),
                    br(),
                    img(src = "img5.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    img(src = "img6.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    img(src = "img7.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    img(src = "img8.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    p("At the same time, we applied the ", strong("SARIMA"),
                      " model to predict temperatures in each city in
                      48~72 hours. The SARIMA forecst plots are displayed on ",
                      span(strong("SARIMA Temperature Forecast"),
                           style = "color:blue"),
                      " page. For the same example of ", em("Austin"), " and ",
                      em("Juneau"), ", the corresponding SARIMA plots are:"),
                    br(),
                    img(src = "img9.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;"),
                    br(),
                    img(src = "img10.png",
                        style = "display: block;
                        margin-left: auto;
                        margin-right: auto;")
                )
            ),
            tabPanel(
                "References",
                h2("References"),
                br(),
                br(),
                htmlOutput("references")
            ),
            tabPanel(
                "Developer",
                h2("Developer"),
                br(),
                br(),
                htmlOutput("developer")
            )
        )
        
        ,
        
        tabPanel(
            title = strong("View on Map: 48 Hours Weather Forecast"),
            
            sidebarLayout(
                div(
                    id = "sidebar1",
                    
                    sidebarPanel(
                        width = 3,
                        
                        helpText(h3("Step1: Fetch Data")),
                        
                        selectInput(
                            inputId = "state",
                            label = h4("State"),
                            choices = list("Please select a state" = "null",
                                           "Alabama",
                                           "Alaska",
                                           "Arizona",
                                           "Arkansas",
                                           "California",
                                           "Colorado",
                                           "Connecticut",
                                           "Delaware",
                                           "Florida",
                                           "Georgia",
                                           "Hawaii",
                                           "Idaho",
                                           "Illinois",
                                           "Indiana",
                                           "Iowa",
                                           "Kansas",
                                           "Kentucky",
                                           "Louisiana",
                                           "Maine",
                                           "Maryland",
                                           "Massachusetts",
                                           "Michigan",
                                           "Minnesota",
                                           "Mississippi",
                                           "Missouri",
                                           "Montana",
                                           "Nebraska",
                                           "Nevada",
                                           "New Hampshire",
                                           "New Jersey",
                                           "New Mexico",
                                           "New York",
                                           "North Carolina",
                                           "North Dakota",
                                           "Ohio",
                                           "Oklahoma",
                                           "Oregon",
                                           "Pennsylvania",
                                           "Rhode Island",
                                           "South Carolina",
                                           "South Dakota",
                                           "Tennessee",
                                           "Texas",
                                           "Utah",
                                           "Vermont",
                                           "Virginia",
                                           "Washington",
                                           "West Virginia",
                                           "Wisconsin",
                                           "Wyoming"),
                            selected = "Please select a state"
                        ),
                        
                        textInput(
                            inputId = "api_key1",
                            label = h4("API Key"),
                            placeholder = "************"
                        ),
                        
                        checkboxInput(
                            inputId = "api_check1",
                            label = paste0(
                                "If you don't have an API key.", 
                                "Click here to use the default API key"
                            ),
                            value = FALSE
                        ),
                        
                        HTML(paste0(
                            "Create a free account and receive your own API key at ",
                            "<a href='https://openweathermap.org/price'>",
                            "https://openweathermap.org/ </a>"
                        )),
                        
                        br(),
                        br(),
                        
                        div(align = "right",
                            actionButton(
                                inputId = "fetch",
                                label = strong("Fetch Data")
                            )
                        ),
                        
                        hr(),
                        
                        helpText(h3("Step2: Create Plots")),
                        sliderInput(
                            inputId = "time",
                            label = "Time from Now (in hour)",
                            min = 1,
                            max = 48,
                            value = 1,
                            ticks = FALSE
                        ),
                        
                        helpText(paste0("You can adjust the time to look at the ",
                                        "weather forecast in 48 hours."))
                        
                    )
                ),
                
                mainPanel(
                    htmlOutput("title"),
                    div(
                        plotOutput(outputId = "plot1") %>% withSpinner()
                    ),
                    hr(),
                    div(
                        plotOutput(outputId = "plot2") %>% withSpinner()
                    ),
                    hr(),
                    div(
                        plotOutput(outputId = "plot3") %>% withSpinner()
                    ),
                    hr(),
                    div(
                        plotOutput(outputId = "plot4") %>% withSpinner()
                    )
                )
            )
            
        ),
        
        tabPanel(
            title = strong("View by Time: Historical Weather Record"),
            
            sidebarLayout(
                div(
                    id = "sidebar2",
                    
                    sidebarPanel(
                        
                        width = 3,
                        
                        helpText(h3("Choose one city")),
                        
                        selectInput(
                            inputId = "state2_1",
                            label = h4("State1"),
                            choices = list("Please select a state" = "null",
                                           "Alabama",
                                           "Alaska",
                                           "Arizona",
                                           "Arkansas",
                                           "California",
                                           "Colorado",
                                           "Connecticut",
                                           "Delaware",
                                           "Florida",
                                           "Georgia",
                                           "Hawaii",
                                           "Idaho",
                                           "Illinois",
                                           "Indiana",
                                           "Iowa",
                                           "Kansas",
                                           "Kentucky",
                                           "Louisiana",
                                           "Maine",
                                           "Maryland",
                                           "Massachusetts",
                                           "Michigan",
                                           "Minnesota",
                                           "Mississippi",
                                           "Missouri",
                                           "Montana",
                                           "Nebraska",
                                           "Nevada",
                                           "New Hampshire",
                                           "New Jersey",
                                           "New Mexico",
                                           "New York",
                                           "North Carolina",
                                           "North Dakota",
                                           "Ohio",
                                           "Oklahoma",
                                           "Oregon",
                                           "Pennsylvania",
                                           "Rhode Island",
                                           "South Carolina",
                                           "South Dakota",
                                           "Tennessee",
                                           "Texas",
                                           "Utah",
                                           "Vermont",
                                           "Virginia",
                                           "Washington",
                                           "West Virginia",
                                           "Wisconsin",
                                           "Wyoming"),
                            selected = "Please select a state"
                        ),
                        
                        uiOutput("city1"),
                        
                        hr(),
                        
                        helpText(h3("You may choose two other cities")),
                        
                        selectInput(
                            inputId = "state2_2",
                            label = h4("State2"),
                            choices = list("Please select a state" = "null",
                                           "Alabama",
                                           "Alaska",
                                           "Arizona",
                                           "Arkansas",
                                           "California",
                                           "Colorado",
                                           "Connecticut",
                                           "Delaware",
                                           "Florida",
                                           "Georgia",
                                           "Hawaii",
                                           "Idaho",
                                           "Illinois",
                                           "Indiana",
                                           "Iowa",
                                           "Kansas",
                                           "Kentucky",
                                           "Louisiana",
                                           "Maine",
                                           "Maryland",
                                           "Massachusetts",
                                           "Michigan",
                                           "Minnesota",
                                           "Mississippi",
                                           "Missouri",
                                           "Montana",
                                           "Nebraska",
                                           "Nevada",
                                           "New Hampshire",
                                           "New Jersey",
                                           "New Mexico",
                                           "New York",
                                           "North Carolina",
                                           "North Dakota",
                                           "Ohio",
                                           "Oklahoma",
                                           "Oregon",
                                           "Pennsylvania",
                                           "Rhode Island",
                                           "South Carolina",
                                           "South Dakota",
                                           "Tennessee",
                                           "Texas",
                                           "Utah",
                                           "Vermont",
                                           "Virginia",
                                           "Washington",
                                           "West Virginia",
                                           "Wisconsin",
                                           "Wyoming"),
                            selected = "Please select a state"
                        ),
                        
                        uiOutput("city2"),
                        
                        br(),
                        
                        selectInput(
                            inputId = "state2_3",
                            label = h4("State3"),
                            choices = list("Please select a state" = "null",
                                           "Alabama",
                                           "Alaska",
                                           "Arizona",
                                           "Arkansas",
                                           "California",
                                           "Colorado",
                                           "Connecticut",
                                           "Delaware",
                                           "Florida",
                                           "Georgia",
                                           "Hawaii",
                                           "Idaho",
                                           "Illinois",
                                           "Indiana",
                                           "Iowa",
                                           "Kansas",
                                           "Kentucky",
                                           "Louisiana",
                                           "Maine",
                                           "Maryland",
                                           "Massachusetts",
                                           "Michigan",
                                           "Minnesota",
                                           "Mississippi",
                                           "Missouri",
                                           "Montana",
                                           "Nebraska",
                                           "Nevada",
                                           "New Hampshire",
                                           "New Jersey",
                                           "New Mexico",
                                           "New York",
                                           "North Carolina",
                                           "North Dakota",
                                           "Ohio",
                                           "Oklahoma",
                                           "Oregon",
                                           "Pennsylvania",
                                           "Rhode Island",
                                           "South Carolina",
                                           "South Dakota",
                                           "Tennessee",
                                           "Texas",
                                           "Utah",
                                           "Vermont",
                                           "Virginia",
                                           "Washington",
                                           "West Virginia",
                                           "Wisconsin",
                                           "Wyoming"),
                            selected = "Please select a state"
                        ),
                        
                        uiOutput("city3"),
                        
                        textInput(
                            inputId = "api_key2",
                            label = h4("API Key"),
                            placeholder = "************"
                        ),
                        
                        checkboxInput(
                            inputId = "api_check2",
                            label = paste0(
                                "If you don't have an API key.", 
                                "Click here to use the default API key"
                            ),
                            value = FALSE
                        ),
                        
                        HTML(paste0(
                            "Create a free account and receive your own API key at ",
                            "<a href='https://openweathermap.org/price'>",
                            "https://openweathermap.org/ </a>"
                        )),
                        
                        br(),
                        br(),
                        
                        div(align = "right",
                            actionButton(
                                inputId = "fetch2",
                                label = strong("Fetch Data")
                            )
                        )
                    )
                ),
                
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            title = strong("Historical Weather Records"),
                            htmlOutput("title2"),
                            div(
                                plotOutput(outputId = "plot2_1") %>%
                                    withSpinner()
                            ),
                            hr(),
                            div(
                                plotOutput(outputId = "plot2_2") %>%
                                    withSpinner()
                            ),
                            hr(),
                            div(
                                plotOutput(outputId = "plot2_3") %>%
                                    withSpinner()
                            ),
                            hr(),
                            div(
                                plotOutput(outputId = "plot2_4") %>%
                                    withSpinner()
                            )
                        ),
                        tabPanel(
                            title = strong("SARIMA Temperature Forecast"),
                            htmlOutput("title3"),
                            div(
                                plotOutput(outputId = "plot3_1") %>%
                                    withSpinner()
                            ),
                            hr(),
                            div(
                                plotOutput(outputId = "plot3_2") %>%
                                    withSpinner()
                            ),
                            hr(),
                            div(
                                plotOutput(outputId = "plot3_3") %>%
                                    withSpinner()
                            )
                        )
                    )
                    
                )
            )
        )
        
    )
                
             
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    shinyalert(
        text = HTML("WELCOME TO WEATHER EXPLORE!"),
        type = "success",
        showConfirmButton = TRUE,
        confirmButtonText = "Start"
    )
    
    
    output$references <- renderPrint(
        HTML(
            "
            OpenWeather API: <a href='https://openweathermap.org/price'> 
            https://openweathermap.org/ </a>
            <br><br>
            About the API: <a href='https://openweathermap.org/appid'>
            https://openweathermap.org/appid </a>
            "
        )
    )
    
    output$developer <- renderPrint(
        HTML(
            "
            Weihang Wu: <a href='https://github.com/WeihangWu'> 
            https://github.com/WeihangWu </a>
            <br><br>
            Weiyi Liu: <a href='https://github.com/liuwy0915'>
            https://github.com/liuwy0915 </a>
            "
        )
    )
    
    observeEvent(input$fetch, {
        if (input$state == "null") {
            shinyalert(
                title = "Error!",
                text = HTML("Please select a state!"),
                type = "error"
            )
        } else {
            test_url = paste0(
                "https://api.openweathermap.org/data/2.5/onecall?exclude=alerts",
                "&appid=", ifelse(input$api_check1 == TRUE,
                                  "b351a493243bfe5ad524b81507325daf",
                                  input$api_key1),
                "&lat=", 35.9940, "&lon=", -78.8986)
            if (GET(test_url)$status_code != "200") {
                shinyalert(
                    title = "Error!",
                    text = HTML("Your API key is not valid.
                                Please check your input or 
                                use the default API key."),
                    type = "error"
                )
            } else{
                shinyalert(
                    title = "Fetching Data...",
                    text = HTML(
                        "It may take a few minutes to fetch the data.
                        Thanks for your patience!"),
                    type = "info",
                    showConfirmButton = TRUE,
                    confirmButtonText = "Click here to start fetching",
                    showCancelButton = TRUE,
                    cancelButtonText = "Cancel",
                    inputId = "alert1"
                )
            }
        }
    })
    
    forecast <- eventReactive(input$alert1, {
        req(input$alert1)
        plt_data(
            state = input$state,
            apiKey = ifelse(input$api_check1 == TRUE,
                            "b351a493243bfe5ad524b81507325daf",
                            input$api_key1))
    })
    
    forecast_state <- eventReactive(input$alert1, {
        req(input$alert1)
        input$state
    })
    
    output$title <- renderPrint({
        shiny::validate(need(!is.null(forecast()), message = FALSE))
        h3(paste0("Weather Forecast in ", forecast_state(), " at ",
                  as.character(round(now(tzone = "UTC"), units = "hours")
                               + hours(input$time-1)),
                  " UTC"))
    })
    
    output$plot1 <- renderPlot({
        shiny::validate(need(!is.null(forecast()), message = FALSE))
        map_plot(data = forecast(),
                 time = input$time-1,
                 state = forecast_state())$temp
    })
    
    output$plot2 <- renderPlot({
        shiny::validate(need(!is.null(forecast()), message = FALSE))
        map_plot(data = forecast(),
                 time = input$time-1,
                 state = forecast_state())$humid
    })
    
    output$plot3 <- renderPlot({
        shiny::validate(need(!is.null(forecast()), message = FALSE))
        map_plot(data = forecast(),
                 time = input$time-1,
                 state = forecast_state())$vis
    })
    
    output$plot4 <- renderPlot({
        shiny::validate(need(!is.null(forecast()), message = FALSE))
        map_plot(data = forecast(),
                 time = input$time-1,
                 state = forecast_state())$cloud
    })
    
    
    output$city1 <- renderUI({
        if (input$state2_1 == "null") {
            selectInput("city_1", h4("City1"), 
                        list("Please select a state" = "null"))
        } else {
            city1 <- append(list("Not specified" = "null"), 
                            as.list(get_locations(input$state2_1)$name))
            selectInput(inputId = "city_1", h4("City1"), city1)
        }
    })
    
    output$city2 <- renderUI({
        if (input$state2_2 == "null") {
            selectInput("city_2", h4("City2"), 
                        list("Please select a state" = "null"))
        } else {
            city2 <- append(list("Not specified" = "null"), 
                            as.list(get_locations(input$state2_2)$name))
            selectInput("city_2", h4("City2"), city2)
        }
    })
    
    output$city3 <- renderUI({
        if (input$state2_3 == "null") {
            selectInput("city_3", h4("City3"), 
                        list("Please select a state" = "null"))
        } else {
            city3 <- append(list("Not specified" = "null"), 
                            as.list(get_locations(input$state2_3)$name))
            selectInput("city_3", h4("City3"), city3)
        }
    })
    
    observeEvent(input$fetch2, {
        if (input$city_1 == "null") {
            shinyalert(
                title = "Error!",
                text = HTML("Please specify city1!"),
                type = "error"
            )
        } else if(input$city_1 == input$city_2 | 
                  input$city_1 == input$city_3 |
                  ((input$city_2 == input$city_3) & (input$city_2 != "null"))) {
            shinyalert(
                title = "Error!",
                text = HTML("Cities must be different!"),
                type = "error"
            )
        } else {
            test_url = paste0(
                "https://api.openweathermap.org/data/2.5/onecall/timemachine?",
                "&appid=", ifelse(input$api_check2 == TRUE,
                                  "b351a493243bfe5ad524b81507325daf",
                                  input$api_key2),
                "&lat=", 35.9940, "&lon=", -78.8986,
                "&dt=", 1619508200)
            if (GET(test_url)$status_code != "200") {
                shinyalert(
                    title = "Error!",
                    text = HTML("Your API key is not valid.
                                Please check your input or 
                                use the default API key."),
                    type = "error"
                )
            } else{
                shinyalert(
                    title = "Fetching Data...",
                    text = HTML(
                        "It may take several seconds to fetch the data.
                        Thanks for your patience!"),
                    type = "info",
                    showConfirmButton = TRUE,
                    confirmButtonText = "Click here to start fetching",
                    showCancelButton = TRUE,
                    cancelButtonText = "Cancel",
                    inputId = "alert2"
                )
            }
        }
    })
    
    location1 <- eventReactive(input$alert2, {
        req(input$alert2)
        get_location_one(input$state2_1, input$city_1)
    })
    
    location2 <- eventReactive(input$alert2, {
        req(input$alert2)
        if (input$city_2 == "null") {
            ""
        } else {
            get_location_one(input$state2_2, input$city_2)
        }
    })
    
    location3 <- eventReactive(input$alert2, {
        req(input$alert2)
        if (input$city_3 == "null") {
            ""
        } else {
            get_location_one(input$state2_3, input$city_3)
        }
    })
    
    
    historical <- eventReactive(input$alert2, {
        req(input$alert2)
        get_3loc_data(
            loc1 = location1(),
            loc2 = location2(),
            loc3 = location3(),
            apiKey = ifelse(input$api_check2 == TRUE,
                            "b351a493243bfe5ad524b81507325daf",
                            input$api_key2))
    })
    
    historical_city <- eventReactive(input$alert2, {
        req(input$alert2)
        list(
            l1 = paste0(input$city_1, ", ", input$state2_1),
            l2 = ifelse(input$city_2 == "null", "",
                        paste0(" | ", input$city_2, ", ", input$state2_2)),
            l3 = ifelse(input$city_3 == "null", "",
                        paste0(" | ", input$city_3, ", ", input$state2_3))
        )
    })
    
    historical_plot <- eventReactive(input$alert2, {
        req(input$alert2)
        compare_plot(historical())
    })
    
    output$title2 <- renderPrint({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        h3(paste0("Historical Weather Records in ", historical_city()$l1,
                  historical_city()$l2, historical_city()$l3))
    })
    
    output$plot2_1 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        historical_plot()$weath_grid
    })
    
    output$plot2_2 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        historical_plot()$temp_ts
    })
    
    output$plot2_3 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        historical_plot()$temp_circ
    })
    
    output$plot2_4 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        historical_plot()$humid_ts
    })
    
    output$title3 <- renderPrint({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        h3(paste0("48~72-Hour Temperature Forecast in", historical_city()$l1,
                  historical_city()$l2, historical_city()$l3))
    })
    
    output$plot3_1 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE))
        sarima_plot(historical())[[1]]
    })
    
    output$plot3_2 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE),
                        need(historical_city()$l2 != "", message = FALSE))
        sarima_plot(historical())[[2]]
    })
    
    output$plot3_3 <- renderPlot({
        shiny::validate(need(!is.null(historical()), message = FALSE),
                        need(historical_city()$l3 != "", message = FALSE))
        sarima_plot(historical())[[3]]
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
