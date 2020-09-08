library(shiny)
library(tidyverse)
library(readr)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


data <- read.csv("https://raw.githubusercontent.com/pjournal/boun01g-r-sizlar/gh-pages/AB_NYC_2019.csv", 
                 header = TRUE, 
                 check.names=TRUE)


ui <- fluidPage(
    
    
    titlePanel("Inclusive View of NYC - Airbnb 2019"),
    
    
    sidebarLayout(
        sidebarPanel(("You can define your options below:"),
            sliderInput("price",
                        "Please select your price range:",
                        min = min(data$price),
                        max = max(data$price),
                        value = c(100,500),
                        sep = "" ),
            selectInput("roomtype",
                        "Please select your room type:",
                        choices= c("All", data$room_type),
                        selected = c("Private room", "Entire home/apt", "Shared room"),
                        multiple = TRUE),
            
            selectInput("neig",
                        "Please select your neighbourhood:",
                        choices = data$neighbourhood_group,
                        selected = c("Manhattan", "Brooklyn"),
                        multiple = TRUE),
            
            sliderInput("number_of_reviews",
                        "Please select for minimum review number:",
                        min = min(data$number_of_reviews),
                        max = max(data$number_of_reviews),
                        value=0),
        ),
        
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Average Prices for Room Type", plotOutput("hist1")),
                tabPanel("Total Number of Rooms", plotOutput("hist2")),
                tabPanel("Availability for Room Types", plotOutput("box")),
                tabPanel("Reviews of Neighbourhood Groups",plotOutput("line")),
                tabPanel("Average Prices of Neighbourhood Groups", plotOutput("scatterplot"))
                
            )
        )
    )
)


server <- function(input, output) {
        
    output$hist1<- renderPlot({
        
        selected <- data %>%
            filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & number_of_reviews>input$number_of_reviews & neighbourhood_group==input$neig)
        
       
        plot_df <- selected %>% 
            group_by(room_type) %>%
            summarise(mean_price=mean(price), .groups = 'drop') 
            
        
        ggplot(plot_df, aes(x = room_type,y = mean_price, fill = (room_type)))+
            geom_bar(stat="identity",position="dodge") + 
            theme_minimal() + 
            labs(x="Room Types",y="Avg. Price ",title="Average Prices for Room Type",
                 fill="Room Types")
    })
    

 
        output$hist2 <- renderPlot({
        
        selected <- data %>% filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & number_of_reviews>input$number_of_reviews)
        
        ggplot(selected,aes(x=room_type,fill=neighbourhood_group)) +
            labs(x = "Room Type",y = "Number",title="Total Number of Rooms") +
            geom_bar(position = "dodge") +
            theme_minimal()
    })
        
        
        output$box <- renderPlot({
            
            plot_df <- data %>%
                filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype & number_of_reviews>input$number_of_reviews & neighbourhood_group==input$neig)
            
           
            
            ggplot(plot_df, aes(x=room_type, y=availability_365, fill=room_type)) +
                labs(x="Room Type",y="Availability",title="Availability for Room Types") + 
                geom_boxplot() +
                theme_minimal()
        })
    
        output$line <- renderPlot({
            
            selected <- data %>%
                filter(data$price>input$price[1] & data$price<input$price[2] & room_type==input$roomtype)
            
            
            plot_df <- selected %>% 
                group_by(neighbourhood_group) %>%
                summarise(mean_number_of_reviews=mean(number_of_reviews) , .groups = 'drop')
            
            ggplot(plot_df,aes(x=neighbourhood_group ,y= mean_number_of_reviews, group=1, fill=(neighbourhood_group))) +
                ggtitle("Reviews for Neighbourhood Groups")+ 
                geom_line() +  
                geom_point() +
                theme_minimal() +
                labs(x = "Neigbourhood Group",y = "Avg. Number of Reviews", fill="Neighbourhood Group")
        })
        
        output$scatterplot <- renderPlot({
            
            selected <- data %>%
                filter(room_type==input$roomtype & number_of_reviews>input$number_of_reviews)
            
            
            plot_df <- selected %>% 
                group_by(neighbourhood_group,room_type) %>% summarise(room_type,mean_price=mean(price))
            
            ggplot(plot_df, aes(x= room_type ,y=mean_price,color=neighbourhood_group) ) +
                geom_point()+ 
                labs(title="Average  Prices for Neighbourhood Groups",x = "Neigbourhood Group",y = "Avg. Price",color="Neigbourhood Group")+
                theme(plot.title = element_text(size=15)) +
                theme_minimal()
        })
        
}


shinyApp(ui = ui, server = server)

?dateRangeInput
