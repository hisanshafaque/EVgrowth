library(shiny)
library(leaflet)
library(DT)
library(rgdal)
library(dplyr)
library(sp)

#Load Modelled Data
evdata <- read.csv("ev_growth_data.csv")
demand <- read.csv("ev_demand_data.csv")

#Load GIS Data from Open-Data Toronto
wards <- readOGR(dsn = "/Users/hisanshafaque/Desktop/Random/R -Shiny_Apps/eV/ESRI", layer="tcl3_icitw")
wards$AREA_LONG_<-as.numeric(as.character(wards$SCODE_NAME))
wards<-spTransform(wards, CRS("+init=epsg:4326"))

#Bound Leaflet map
bounds <- bbox(wards)

#Design the UI page
ui <- fluidPage(
  
  titlePanel(h1("Modelling Electric Vehicle Growth in the City of Toronto", align = "center")),
  hr(),
  HTML("Created by <a href=\'https://hisanshafaque.com/\'>Hisan Shafaque</a>"),
  hr(),
  HTML("Note: This representation was based on data available for 2016 and therefore, the map shows 44 electoral wards."),
  hr(),
  HTML("Source of Data: <a href=\'https://www.toronto.ca/city-government/data-research-maps/open-data/\'>City of Toronto</a>, 
       <a href=\'https://www.oeb.ca/oeb/_Documents/RRR/2018_Yearbook_of_Electricity_Distributors.pdf/\'>Ontario Energy Board</a> and
       <a href=\'http://www.ieso.ca/en/Power-Data/Demand-Overview/Historical-Demand/\'>Independent Electricity System Operator</a>."),
  
  fluidRow(
    align = "center",
    column(width= 12,
           sliderInput("year", HTML("<h4><b>Select Year:<b></h4>"),
                       min = 2013, max = 2045,
                       value = 2020,
                       width = "200%"),
    ),  
  ),
  
  fluidRow(
    align = "center",
    column(width= 12,
           leafletOutput("TorontoMap", height=400, width = 800),
           hr(),
           textOutput("texttoronto"),
           hr(),
           HTML("<h4><b>Electricle Vehicle Data for Each Ward<b></h4>"),
           DT::dataTableOutput ("tableward")
    ),  
  ),
)


server <- function(input, output) {
  
  #Determine the model outputs based on year input
  getDataSet<- reactive ({
    dataSet <-evdata[evdata$year == input$year, ]
    joinedDataset <- wards
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by = "AREA_LONG_"))  
    joinedDataset
    })
  
  #Render Leaflet Map
  output$TorontoMap <- renderLeaflet({
    leaflet() %>%
       addTiles() %>%
       setView(mean(bounds[1, ]),
               mean(bounds[2, ]),
               zoom = 10
               )
  })
  
  #Render EV Demand text 
  output$texttoronto <-renderText ({
    demand <- demand[demand$year == input$year, ]
    paste(
      "The total number of electric vehicles in the City of Toronto in ",
      input$year,
      "is ",
      sum(as.numeric(demand$ward_evpop), na.rm = NULL),
      "requiring an additional ",
      sum(as.numeric(demand$EV_Load)*0.001, na.rm = NULL),
      "GWh load. This is assuming an average EV travels 15,200 km annualy and requires 17.4 KWh per 100 km."
      )
  })
  
  
  observe({
   
     theData <-getDataSet()
    
    pal <- colorNumeric("Greens", theData$ward_evpop, n = 20)
    
    #Ward Popup tiles for the map
    ward_popup <- paste0("<strong>Ward: </strong>", 
                         theData$AREA_LONG_,
                         "<br><strong>",
                         "Prediction: </strong>", 
                         formatC(theData$ward_evpop, format="d", big.mark=','),
                         "<br><strong>",
                         "Additional  EV load (MWh): </strong>", 
                         formatC(theData$EV_Load, format="d", big.mark=',')
                         
    )
    
    #Displaying the Toronto Map
    leafletProxy("TorontoMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$ward_evpop),    # colour palette mapped to data
                  fillOpacity = 0.7, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = ward_popup)
  })
  
  #Render the table with all details
  output$tableward <- DT::renderDataTable(DT::datatable(
      demand[demand$year == input$year, ],
      rownames = FALSE,
      class = 'cell-border stripe',
      options = list(paging = FALSE),
      colnames =c("Year", "Ward", "Electric Vehicle Population", "Additional EV Load [MWh]")
                    
                    )
  ) 
}

#Deploy Shiny App
shinyApp(ui, server)
