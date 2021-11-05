library(shiny)
library(randomForest)
library(ggplot2)
library(dplyr)
library(plotly)

# Load Model

load("price_randomforest.rda")


thefunny <- function(year, odometer, state, manufacturer, condition, cylinders, fuel, title_status, transmission, drive, type, paint_color){
  test <- as.list(rep(0,161))
  names(test) <- names(price.prf$forest$xlevels[1:161])
  test[1] <- year
  test[2] <- odometer
  test[3] <- list(factor(condition,levels = c("missing","salvage","fair","good","excellent","like new","new")))
  test[grep(manufacturer,names(test))] <- 1
  test[grep(cylinders,names(test))] <- 1
  test[grep(fuel,names(test))] <- 1
  test[grep(title_status,names(test))] <- 1
  test[grep(transmission,names(test))] <- 1
  test[grep(drive,names(test))] <- 1
  test[grep(type,names(test))] <- 1
  test[grep(paint_color,names(test))] <- 1
  
  return(as.numeric(predict(price.prf, test)))
}



# Define UI ----
ui <- fluidPage(
  titlePanel("Exploring Car Price Predictions"),
  
    tabsetPanel(
      tabPanel("Plot", 
              fluidRow(
                column(3,
                br(),
                br(),
                p("Here is the sample of my graph. As you can see the predictions are really off with this first model. However with more time, and sectioning off the model based on highly predivtive variables, we will increase the accuracy of the predictions."),
                br(),
                p("The graph to the right shows your data input as a red dot, which you can hover over and gather more information about. The other black dots are cars which match your vehicle's age, make, and type. Additionally, as this is a plotly output, you can pan around the graph as well.")),
                column(7,plotlyOutput("comparegraph")))
              ),
      tabPanel("Stats", 
               textOutput("predictedprice"),
               plotOutput("modelerror"))
    ),
  
  hr(),
  
  fluidRow(
    column(3,
      sliderInput("year",
                   h5("Year"),
                   min = 1935,
                   max = 2021,
                   value = 2010,
                   step = 1,
                  sep = ""),
      numericInput("odometer",
                   h5("Odometer"),
                   value = 30000)),
    column(3,
           selectInput("make",
                       label = "Make",
                       choices = list("chevrolet",
                                      "hyundai",
                                      "bmw",
                                      "ford",
                                      "toyota",
                                      "jeep",
                                      "ram",
                                      "honda",
                                      "dodge",
                                      "lexus",
                                      "mercedes_benz",  
                                      "cadillac",
                                      "gmc",
                                      "subaru",
                                      "mazda",          
                                      "mini",
                                      "nissan",
                                      "volkswagen",
                                      "kia",            
                                      "lincoln",
                                      "mitsubishi",
                                      "buick",
                                      "infiniti",       
                                      "audi",
                                      "rover",
                                      "acura",
                                      "volvo" ,         
                                      "chrysler",
                                      "pontiac",
                                      "tesla",
                                      "saturn",         
                                      "mercury",
                                      "fiat",
                                      "datsun",
                                      "porsche",        
                                      "jaguar",
                                      "ferrari",
                                      "alfa_romeo",
                                      "harley_davidson",
                                      "landrover",
                                      "aston_martin",
                                      "morgan",
                                      "hennessey" ),
                       selected = "honda"),
     selectInput("type",
                 label = "Body Type",
                 choices = list("sedan",
                                "SUV",
                                "pickup",
                                "coupe",
                                "van",
                                "truck",
                                "mini-van",
                                "wagon",
                                "convertible",
                                "hatchback",
                                "bus",
                                "offroad")),
      
      selectInput("cond",
                  label = "Condition",
                  choices = list("new",
                                 "excellent",
                                 "good",
                                 "fair",
                                 "salvage"))),
          column(3,
      selectInput("cyl",
                  label = "Cylinders",
                  choices = list("cylinders12",
                                 "cylinders10",
                                 "cylinders8",
                                 "cylinders6",
                                 "cylinders5",
                                 "cylinders4",
                                 "cylinders3")),
      selectInput("fuel",
                  label = "Fuel",
                  choices = list("gas",
                                 "diesel",
                                 "hybrid",
                                 "electric")),
      selectInput("title",
                  label = "Title",
                  choices = list("clean",
                                 "missing",
                                 "salvage",
                                 "rebuilt",
                                 "partsonly"))),
    column(3,
      selectInput("state",
                 label = "State",
                 choices = list("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID", "IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY", "OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")),

      selectInput("drive",
                  label = "Drive",
                  choices = list("rdw",
                                 "fdw",
                                 "fourwd")),
      selectInput("trans",
                  label = "Transmission",
                  choices = list("automatic",
                                 "manual")))

    )
  )

# Define server logic ----
server <- function(input, output) {
  
  cars <- read.csv("format_cars.csv")
  
  fetchprice <- reactive({
    return(thefunny(input$year, input$odometer, input$state, input$make, input$cond, input$cyl, input$fuel, input$title, input$trans, input$drive, input$type, "blue"))
  })
  
  fetchdata <- reactive({
    return(cars %>% 
      select(year, price, manufacturer, type, odometer) %>% 
      filter(year >= input$year-3, year <= input$year+3, manufacturer == input$make, type == input$type, odometer <= quantile(odometer, p = .99, na.rm = T))
    )
  })
  
  output$predictedprice <- renderText({
    paste("Price:", round(fetchprice(),digits = 2))
  })
  
  output$comparegraph <- renderPlotly({
    
    point <- as.data.frame(list(input$year,fetchprice(),input$make,input$type, input$odometer), col.names = c("year", "price","make", "type", "odometer"))
    
    ggplotly( ggplot(data = fetchdata(), mapping = aes(odometer, price)) +
      geom_point(alpha = .8) +
      geom_point(data = point, color = "red", size = 4)+
      scale_x_continuous(labels=scales::comma, n.breaks = 5)) %>% layout(dragmode = "pan") %>% config(displayModeBar = F)
    
    
  })
  
  output$modelerror <- renderPlot({
    plot(price.prf)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)