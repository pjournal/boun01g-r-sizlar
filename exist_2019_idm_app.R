library(shiny)
library(tidyverse)
library(readr)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

idm_data <- read.csv("https://github.com/pjournal/boun01g-r-sizlar/blob/gh-pages/idm_data.csv?raw=true")



ui <- fluidPage(

    
    titlePanel("EXIST 2019 IDM Overview"),
    theme = shinythemes::shinytheme("cyborg"),
     
    sidebarLayout(
        sidebarPanel(
            selectInput("time",
                         "Please select how your data will be evaluated:",
                         c("Between Any Two Dates", "Monthly", "Daily", "Hourly"),
                            selected = "Hourly"),
            conditionalPanel(
                condition = "input.time" == "Between Any Two Dates",
                dateRangeInput("date",
                               "Please select the date range:",
                               start = min(idm_data$Date.Time),
                               end = max(idm_data$Date.Time))
                
            ),
            
            conditionalPanel(
                condition = "input.time" == "Monthly",
                sliderTextInput(
                    inputId = "month",
                    label = "Please select the month:", 
                    choices = as.vector(unique(idm_data$Month)),
                    selected = as.vector(unique(idm_data$Month))[c(4, 8)]
                )
                ),
                
                conditionalPanel(
                    condition = "input.time" == "Daily",
                    sliderTextInput(
                        inputId = "day",
                        label = "Please select the day:", 
                        choices = as.vector(unique(idm_data$Day.Week)),
                        selected = as.vector(unique(idm_data$Day.Week))[c(3,5)]
                    )
                    ),
            
            conditionalPanel(
                condition = "input.time" == "Hourly",
                sliderInput(
                    inputId = "hour",
                    label = "Please select the hour:", 
                    min = 0,
                    max = 23,
                    value = c(0, 3)
                )
            )),
            
            
          
      


        
        mainPanel(
            tabsetPanel(
                tabPanel("Minimum Bid/Offer/Matching Prices", plotOutput("min_bid_offer_match")),
                tabPanel("Weighted Average Price and Clearing Quantity", plotOutput("wap_clearing")),
                tabPanel("Bid Quantity vs. Offer Quantity", plotOutput("bid_offer_quan")),
                tabPanel("Maximum Bid/Offer/Matching Prices"), plotOutput("max_bid_offer_match")),
                tabPanel("Trading Volume", plotOutput("trade_value"))
           )
        
    )
)


server <- function(input, output) {

    output$wap_clearing <- renderPlot({
        
        if ("input.time" == "Between Any Two Dates") {
            idm_data %>%
                filter(Date.Time >= input$dates[1],
                       Date.Time <= input$dates[2]) %>%
                group_by(Date.Time) %>% 
                summarise(Clearing.Quantity..MWh.= sum(as.numeric(Clearing.Quantity..MWh.)),WAP..TL.MWh.=sum(as.numeric(WAP..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Clearing.Quantity..MWh., WAP..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Date", y = "# TL/MWh")
        } else if("input.time" == "Monthly"){
            idm_data %>%
                filter(Month >= input$month[1],
                       Month <= input$month[2]) %>%
                group_by(Month) %>% 
                summarise(Clearing.Quantity..MWh.= sum(as.numeric(Clearing.Quantity..MWh.)),WAP..TL.MWh.=sum(as.numeric(WAP..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Clearing.Quantity..MWh., WAP..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Month", y = "# TL/MWh")
        } else if("input.time" == "Daily"){
            idm_data %>%
                filter(Day.Week >= input$day[1],
                       Day.Week <= input$day[2]) %>%
                group_by(Day.Week) %>% 
                summarise(Clearing.Quantity..MWh.= sum(as.numeric(Clearing.Quantity..MWh.)),WAP..TL.MWh.=sum(as.numeric(WAP..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Clearing.Quantity..MWh., WAP..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Day", y = "# TL/MWh")
        } else{
            idm_data %>%
                filter(Hour >= input$hour[1],
                       Hour <= input$hour[2]) %>%
                group_by(Hour) %>% 
                summarise(Clearing.Quantity..MWh.= sum(as.numeric(Clearing.Quantity..MWh.)),WAP..TL.MWh.=sum(as.numeric(WAP..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Clearing.Quantity..MWh., WAP..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Hour", y = "# TL/MWh")
            }})
    
    output$bid_offer_quan <- renderPlot({
        
        if ("input.time" == "Between Any Two Dates") {
            idm_data %>%
                filter(Date.Time >= input$dates[1],
                       Date.Time <= input$dates[2]) %>%
                group_by(Date.Time) %>% 
                summarise(Bid.Quantity..MWh.= sum(as.numeric(Bid.Quantity..MWh.)), Offer.Quantity..MWh. = sum(as.numeric(Offer.Quantity..MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Bid.Quantity..MWh., Offer.Quantity..MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Date", y = "# TL/MWh")
        } else if("input.time" == "Monthly"){
            idm_data %>%
                filter(Month >= input$month[1],
                       Month <= input$month[2]) %>%
                group_by(Month) %>% 
                summarise(Bid.Quantity..MWh.= sum(as.numeric(Bid.Quantity..MWh.)), Offer.Quantity..MWh. = sum(as.numeric(Offer.Quantity..MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Bid.Quantity..MWh., Offer.Quantity..MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Month", y = "# TL/MWh")
        } else if("input.time" == "Daily"){
            idm_data %>%
                filter(Day.Week >= input$day[1],
                       Day.Week <= input$day[2]) %>%
                group_by(Day.Week) %>% 
                summarise(Bid.Quantity..MWh.= sum(as.numeric(Bid.Quantity..MWh.)), Offer.Quantity..MWh. = sum(as.numeric(Offer.Quantity..MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Bid.Quantity..MWh., Offer.Quantity..MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Day", y = "# TL/MWh")
        } else{
            idm_data %>%
                filter(Hour >= input$hour[1],
                       Hour <= input$hour[2]) %>%
                group_by(Hour) %>% 
                summarise(Bid.Quantity..MWh.= sum(as.numeric(Bid.Quantity..MWh.)), Offer.Quantity..MWh. = sum(as.numeric(Offer.Quantity..MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Bid.Quantity..MWh., Offer.Quantity..MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Hour", y = "# TL/MWh")
        }})
    
    output$min_bid_offer_match <- renderPlot({
        
        if ("input.time" == "Between Any Two Dates") {
            idm_data %>%
                filter(Date.Time >= as.POSIXct(input$dates[1], format = "%Y-%m-%d"),
                       Date.Time <= as.POSIXct(input$dates[2], format = "%Y-%m-%d")) %>%
                group_by(Date.Time) %>% 
                summarise(Min.Bid.Price..TL.MWh.= sum(as.numeric(Min.Bid.Price..TL.MWh.)), Min..Offer.Price..TL.MWh. = sum(as.numeric(Min..Offer.Price..TL.MWh.)), Min..Matching.Price..TL.MWh. = sum(as.numeric(Min..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Min.Bid.Price..TL.MWh., Min..Offer.Price..TL.MWh., Min..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Date", y = "# TL/MWh")
        } else if("input.time" == "Monthly"){
            idm_data %>%
                filter(Month >= input$month[1],
                       Month <= input$month[2]) %>%
                group_by(Month) %>% 
                summarise(Min.Bid.Price..TL.MWh.= sum(as.numeric(Min.Bid.Price..TL.MWh.)), Min..Offer.Price..TL.MWh. = sum(as.numeric(Min..Offer.Price..TL.MWh.)), Min..Matching.Price..TL.MWh. = sum(as.numeric(Min..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Min.Bid.Price..TL.MWh., Min..Offer.Price..TL.MWh., Min..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Month", y = "# TL/MWh")
        } else if("input.time" == "Daily"){
            idm_data %>%
                filter(Day.Week >= input$day[1],
                       Day.Week <= input$day[2]) %>%
                group_by(Day.Week) %>% 
                summarise(Min.Bid.Price..TL.MWh.= sum(as.numeric(Min.Bid.Price..TL.MWh.)), Min..Offer.Price..TL.MWh. = sum(as.numeric(Min..Offer.Price..TL.MWh.)), Min..Matching.Price..TL.MWh. = sum(as.numeric(Min..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Min.Bid.Price..TL.MWh., Min..Offer.Price..TL.MWh., Min..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Day", y = "# TL/MWh")
        } else{
            idm_data %>%
                filter(Hour >= input$hour[1],
                       Hour <= input$hour[2]) %>%
                group_by(Hour) %>% 
                summarise(Min.Bid.Price..TL.MWh.= sum(as.numeric(Min.Bid.Price..TL.MWh.)), Min..Offer.Price..TL.MWh. = sum(as.numeric(Min..Offer.Price..TL.MWh.)), Min..Matching.Price..TL.MWh. = sum(as.numeric(Min..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Min.Bid.Price..TL.MWh., Min..Offer.Price..TL.MWh., Min..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Hour", y = "# TL/MWh")
        }})
    
    output$max_bid_offer_match <- renderPlot({
        
        if ("input.time" == "Between Any Two Dates") {
            idm_data %>%
                filter(Date.Time >= as.POSIXct(input$date[1], format = "%Y-%m-%d"),
                       Date.Time <= as.POSIXct(input$date[2], format = "%Y-%m-%d")) %>%
                group_by(Date.Time) %>% 
                summarise(Max..Bid.Price..TL.MWh.= sum(as.numeric(Max..Bid.Price..TL.MWh.)), Max..Offer.Price..TL.MWh. = sum(as.numeric(Max..Offer.Price..TL.MWh.)), Max..Matching.Price..TL.MWh. = sum(as.numeric(Max..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Max..Bid.Price..TL.MWh., Max..Offer.Price..TL.MWh., Max..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Date", y = "# TL/MWh")
        } else if("input.time" == "Monthly"){
            idm_data %>%
                filter(Month >= input$month[1],
                       Month <= input$month[2]) %>%
                group_by(Month) %>% 
                summarise(Max..Bid.Price..TL.MWh.= sum(as.numeric(Max..Bid.Price..TL.MWh.)), Max..Offer.Price..TL.MWh. = sum(as.numeric(Max..Offer.Price..TL.MWh.)), Max..Matching.Price..TL.MWh. = sum(as.numeric(Max..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Max..Bid.Price..TL.MWh., Max..Offer.Price..TL.MWh., Max..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Month", y = "# TL/MWh")
        } else if("input.time" == "Daily"){
            idm_data %>%
                filter(Day.Week >= input$day[1],
                       Day.Week <= input$day[2]) %>%
                group_by(Day.Week) %>% 
                summarise(Max..Bid.Price..TL.MWh.= sum(as.numeric(Max..Bid.Price..TL.MWh.)), Max..Offer.Price..TL.MWh. = sum(as.numeric(Max..Offer.Price..TL.MWh.)), Max..Matching.Price..TL.MWh. = sum(as.numeric(Max..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Max..Bid.Price..TL.MWh., Max..Offer.Price..TL.MWh., Max..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Day", y = "# TL/MWh")
        } else{
            idm_data %>%
                filter(Hour >= input$hour[1],
                       Hour <= input$hour[2]) %>%
                group_by(Hour) %>% 
                summarise(Max..Bid.Price..TL.MWh.= sum(as.numeric(Max..Bid.Price..TL.MWh.)), Max..Offer.Price..TL.MWh. = sum(as.numeric(Max..Offer.Price..TL.MWh.)), Max..Matching.Price..TL.MWh. = sum(as.numeric(Max..Matching.Price..TL.MWh.))) %>%
                ungroup() %>%
                pivot_longer(cols=c(Max..Bid.Price..TL.MWh., Max..Offer.Price..TL.MWh., Max..Matching.Price..TL.MWh.), names_to="Types", values_to="Quantities") %>%
                ggplot(aes(x = Types, y = Quantities, fill = Types)) +
                geom_bar(stat = "identity") +
                labs(x = "Hour", y = "# TL/MWh")
        }})
    
    output$trade_value <- renderPlot({
        
        if ("input.time" == "Between Any Two Dates") {
            idm_data %>%
                filter(Date.Time >= input$dates[1],
                       Date.Time <= input$dates[2]) %>%
                group_by(Date.Time) %>% 
                summarise(Trade.Value..TL.= sum(Trade.Value..TL.)) %>%
                ggplot(aes(Month)) +
                geom_line() +
                geom_point() +
                labs(
                    title = "Trading Volume for 2019",
                    x = "Date",
                    y = "# TL/MWh"
                )
        } else if("input.time" == "Monthly"){
            idm_data %>%
                filter(Month >= input$month[1],
                       Month <= input$month[2]) %>%
                group_by(Month) %>% 
                summarise(Trade.Value..TL.= sum(Trade.Value..TL.)) %>%
                ggplot(aes(Month)) +
                geom_line() +
                geom_point() +
                labs(
                    title = "Trading Volume for 2019",
                    x = "Month",
                    y = "# TL/MWh"
                )
        } else if("input.time" == "Daily"){
            idm_data %>%
                filter(Day.Week >= input$day[1],
                       Day.Week <= input$day[2]) %>%
                group_by(Day.Week) %>% 
                summarise(Trade.Value..TL.= sum(Trade.Value..TL.)) %>%
                ggplot(aes(Month)) +
                geom_line() +
                geom_point() +
                labs(
                    title = "Trading Volume for 2019",
                    x = "Day",
                    y = "# TL/MWh"
                )
        } else{
            idm_data %>%
                filter(Hour >= input$hour[1],
                       Hour <= input$hour[2]) %>%
                group_by(Hour) %>% 
                summarise(Trade.Value..TL.= sum(Trade.Value..TL.)) %>%
                ggplot(aes(Month)) +
                geom_line() +
                geom_point() +
                labs(
                    title = "Trading Volume for 2019",
                    x = "Hour",
                    y = "# TL/MWh"
                )

           
        
        }})
    
}


shinyApp(ui = ui, server = server)
