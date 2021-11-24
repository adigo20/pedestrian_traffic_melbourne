# import library block 
library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(ggplot2)
# import end######


# reading the CSV files
ped_sensor_location = read.csv("./Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")

ped_sensor_data = read.csv("./Pedestrian_Counting_System_2019 (Exercise 2).csv")

# adjusting the data set name for easy use.
names(ped_sensor_data)[6] = "sensor_name"

#merging both the datasets based on sensor name. Here all the sensor name without any entry in the ped_sensor_data will be deleted. 

ped_collective_data = merge(ped_sensor_data,ped_sensor_location,by.x = "sensor_name" ,by.y  ="sensor_name",all.x =FALSE)

#head(ped_collective_data)

#finding the hourly average count day wise for all sensors 
ped_data_summary = aggregate(Hourly_Counts~Day + Time + sensor_name + latitude + longitude , ped_collective_data ,FUN =mean)
ped_data_summary$Day = factor(ped_data_summary$Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#ordering the data as per days 
ped_data_summary[order(ped_data_summary$Day),]
#ped_data_summary

# setting up the UI 
ui = fluidPage(
  
  #calling the map plot
  headerPanel("Melbourne Pedestrian Traffic Visualisation"),
  leafletOutput("map_plot"),
  
  hr(),
  
  
  fluidRow(
    #calling the drop down and displaying the default location. 
    column(3,
           h4("Locations"),
           uiOutput("ped_sensor_location")
           
    ),
    #calling the line plot visualization.
    column(4,
           h4("Plot"),
           plotOutput("day_graph") 
    )
  )
)

#server function block.
server = function(input, output) {
  
  # finding the hourly count of pedestrian on bases of location of sensor name. 
  pedsensor_location = aggregate(Hourly_Counts~latitude+longitude+sensor_name,ped_data_summary,FUN = mean)
  
  output$ped_sensor_location = renderUI({
    #setting up the dropdown menu options. 
    selectInput("ped_sensor_location", "Select the Sensor Name",
                c('Spencer St-Collins St (South)',	'Chinatown-Lt Bourke St (South)',	'Flinders St-Spark La',	'Bourke St Bridge',	'Lonsdale St-Spring St (West)',	'Lygon St (West)',	'Chinatown-Swanston St (North)',	'Lygon St (East)',	'Queen St (West)',	'Bourke St - Spencer St (North)',	'Webb Bridge',	'Waterfront City',	'Collins Place (North)',	'QVM-Therry St (South)',	'QV Market-Elizabeth St (West)',	'Flinders St-Elizabeth St (East)',	'Spencer St-Collins St (North)',	'QV Market-Peel St',	'The Arts Centre',	'Tin Alley-Swanston St (West)',	'QVM-Queen St (East)',	'QVM-Franklin St (North)',	'Victoria Point',	'Monash Rd-Swanston St (West)',	'New Quay',	'Melbourne Central',	'Southern Cross Station',	'Lincoln-Swanston (West)',	'Lonsdale St (South)',	'Flinders Street Station Underpass',	'Bourke St-Russell St (West)',	'Town Hall (West)',	'Pelham St (South)',	'Lonsdale St - Elizabeth St (North)',	'Grattan St-Swanston St (West)',	'Princes Bridge',	'Faraday St-Lygon St (West)',	'Elizabeth St-Lonsdale St (South)',	'Bourke Street Mall (South)',	'Southbank',	'Bourke Street Mall (North)',	'Little Collins St-Swanston St (East)',	'Birrarung Marr',	'Elizabeth St-La Trobe St (East)',	'La Trobe St (North)',	'Flinders St-Spring St (West)',	'Sandridge Bridge',	'Flinders La-Swanston St (West)',	'Melbourne Convention Exhibition Centre',	'St Kilda Rd-Alexandra Gardens',	'Alfred Place',	'Collins Place (South)',	'State Library'),
                selected = 1)
    
  })
  
  #map plot. 
  output$map_plot = renderLeaflet({
    # we select the location from the data filtered above. The circle markers are set on the bases of latitude  and longitude. 
    leaflet(pedsensor_location) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       label = ~ sensor_name,
                       #the radius is set up on the bases of hourly count avg. we take square root and divide it by 2 so that the circles wont overllap each other.
                       radius = ~sqrt(Hourly_Counts)/2)
    
  })
  
  # setting up map click event. 
  data = reactiveValues(clickedMarker = NULL)
  
  observeEvent(input$map_plot_marker_click,{
    
    print(input$map_plot_marker_click)
    
    # here we get the location that the user clicks on the map. We get the input in form of longitude and latitude 
    data$clickedMarker = input$map_plot_marker_click
    
    #tim = data$clickedMarker
    
    #finding the name of sensor from the dataset by passing the latitude and longitude as input. 
    ped_test_data = pedsensor_location[(round(pedsensor_location$latitude,5) == round(data$clickedMarker$lat,5))&(round(pedsensor_location$longitude,4) == round(data$clickedMarker$lng,4)),]
    
    #print(ped_test_data)
    
    #fetching current location based on the the search.
    curr_loc = sapply(ped_test_data$sensor_name,as.character)
    
    #print(x)
    
    #updating the dropdown on the bases of current location . 
    
    output$ped_sensor_location = renderUI({
      
      selectInput("ped_sensor_location", "Select the Sensor Name",
                  c('Spencer St-Collins St (South)',	'Chinatown-Lt Bourke St (South)',	'Flinders St-Spark La',	'Bourke St Bridge',	'Lonsdale St-Spring St (West)',	'Lygon St (West)',	'Chinatown-Swanston St (North)',	'Lygon St (East)',	'Queen St (West)',	'Bourke St - Spencer St (North)',	'Webb Bridge',	'Waterfront City',	'Collins Place (North)',	'QVM-Therry St (South)',	'QV Market-Elizabeth St (West)',	'Flinders St-Elizabeth St (East)',	'Spencer St-Collins St (North)',	'QV Market-Peel St',	'The Arts Centre',	'Tin Alley-Swanston St (West)',	'QVM-Queen St (East)',	'QVM-Franklin St (North)',	'Victoria Point',	'Monash Rd-Swanston St (West)',	'New Quay',	'Melbourne Central',	'Southern Cross Station',	'Lincoln-Swanston (West)',	'Lonsdale St (South)',	'Flinders Street Station Underpass',	'Bourke St-Russell St (West)',	'Town Hall (West)',	'Pelham St (South)',	'Lonsdale St - Elizabeth St (North)',	'Grattan St-Swanston St (West)',	'Princes Bridge',	'Faraday St-Lygon St (West)',	'Elizabeth St-Lonsdale St (South)',	'Bourke Street Mall (South)',	'Southbank',	'Bourke Street Mall (North)',	'Little Collins St-Swanston St (East)',	'Birrarung Marr',	'Elizabeth St-La Trobe St (East)',	'La Trobe St (North)',	'Flinders St-Spring St (West)',	'Sandridge Bridge',	'Flinders La-Swanston St (West)',	'Melbourne Convention Exhibition Centre',	'St Kilda Rd-Alexandra Gardens',	'Alfred Place',	'Collins Place (South)',	'State Library'),
                  selected = curr_loc)
      
    })
    
    
  })
  
  #passing the sensor name for plot. 
  select_sensor <- reactive({
    ped_data_summary[(ped_data_summary$sensor_name == input$ped_sensor_location),]
  })
  
  #fetching the data for the selected sensor name and plotting the graph using ggplot. 
  output$day_graph = renderPlot({
    
    selected_loc = select_sensor()
    
    ggplot(selected_loc,aes(x = Time, y = Hourly_Counts))+ geom_line()+ ylab("Average Pedestrian Count") + xlab ("Time") + facet_wrap(~Day)
    
  })
  
  
}

shinyApp(ui = ui, server = server)