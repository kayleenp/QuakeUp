
#load libraries
library(shiny)
library(shinyMobile)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinyWidgets)
library(fixerapi)
library(ROpenWeatherMap)
library(revgeo)
library(httr)
library(gtools)
library(plyr)
library(readr)
library(caret)
library(xml2)
library(rvest)
library(jsonlite)
library(kableExtra)
library(formattable)
library(geosphere)
library(NISTunits)

#myscript <- system.file("extdata", "earthquake.R", package = "taskscheduleR")
#
#taskscheduler_create(taskname = "update_BMKG_12HRS", rscript = myscript,
#                    schedule = "HOURLY", starttime = "00:00", modifier = 12)

setwd("./10Data")
url <- 'https://www.bmkg.go.id/gempabumi/gempabumi-terkini.bmkg'

out_df <- url %>% read_html() %>% html_table() %>% .[[1]]
print(head(out_df))
write.csv(out_df, 'current_earthquake_data/current_earthquake_data.csv', row.names = FALSE)

data <- read.csv('current_earthquake_data/current_earthquake_data.csv')
Sys.setenv(FIXER_API_KEY='cf24b3b8a8588c80c7dfcac67bc35709')
mydata<-fixer_latest()
#EARTHQUAKE
data$depth_type <-  ifelse(data$Kedalaman <= 70, "shallow", 
                           ifelse(data$Kedalaman <= 300 | out_df$KEDALAMAN >70, "intermediate", 
                                  ifelse(data$Kedalaman > 300, "deep", "other")))




#CONVERT MONEY
GetExchangeRates <- function(from, to, dt=Sys.Date()) {
  require(quantmod)
  obj.names <- getFX(paste0(from, "/", to), from=dt, to=dt)
  result <- numeric(length(obj.names))
  names(result) <- obj.names
  for (obj.name in obj.names) {
    result[obj.name] <- as.numeric(get(obj.name))[1]
    # Clean up    
    rm(obj.name)
  }
  return(result)
}
r_birthplace_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addMarkers(lng=174.768, lat=-36.852,
             popup="The birthplace of R")
reverseGeoCodeCity <- function(dataframe, row) {
  
  
  latitude = dataframe[row, "LATITUDE"]
  longitude = dataframe[row, "LONGITUDE"]
  JSONURL = "https://api.bigdatacloud.net/data/reverse-geocode?latitude=" 
  JSONURL2 = "&longitude="
  JSONURL3 = "&localityLanguage=en&key=d853aadb6fe04084938dd4b64d5c0e2a"
  getCity = paste(JSONURL, latitude, JSONURL2, longitude, JSONURL3, sep ="", collapse="")
  
  # cityName <- jsonlite::fromJSON(getCity, flatten=TRUE)
  
  return(getCity)
  # return(cityName$locality)
}




shiny::shinyApp(
  
  
  
  ui = f7Page( init = f7Init(skin = "auto", theme = "light"),
               title = "Tab Layout",
               f7TabLayout(
                 navbar = f7Navbar(title = "QuakeUp"),
                 f7Tabs(
                   id = "tabdemo",
                   swipeable = FALSE,
                   animated = TRUE,
                   f7Tab(tabName = "Prediction", icon= f7Icon("waveform_circle", fill=TRUE), f7Align(h1("PREDICT EARTHQUAKE"), side="center"),
                         f7Card(f7Block(
                           f7Accordion(
                             inputId = "myaccordion1",
                             f7AccordionItem(
                               open=TRUE,
                               title = "SHOW MAP",
                               
                               #this will create a space for us to display our map
                               leafletOutput(outputId = "mymap", width="100%", height="450"),
                               #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                               
                               absolutePanel(bottom=50, left = 20, setBackgroundColor("white"),f7Toggle(
                                 inputId = "markers",
                                 label = "Depth",
                                 color = "blue"
                                 
                               ),
                               
                               f7Toggle(
                                 inputId = "heat",
                                 label = "Heatmap",
                                 color = "pink"
                               )),
                               
                               
                               
                             ), ),
                           open = TRUE
                         )
                         ,
                         
                         
                         
                         f7Card( 
                           title = "",
                           height="1000",
                           f7Row(f7Col(f7Button(inputId="slider_input_act", label="SLIDER INPUT")), f7Col(f7Button(inputId="number_input_act", label= "INPUT BY NUMBERS"))),
                           f7Col(uiOutput("inputControls")), f7Row(f7Block(height="100")),
                           
                           f7Row(f7Picker(
                             inputId = "prediction_model",
                             placeholder = NULL,
                             choices = c("SVM", "NAIVEBAYES", "MULTILOGREGRESSION"),
                             label = "Choose Prediction Model :"
                             
                           )), f7Block(hairlines=TRUE,  f7Button(inputId="predict", label="PREDICT", color="green", shadow=TRUE, size="large")), f7Block(title=""),
                           f7Card(title="", f7Block(uiOutput("table_title")),f7Block(DT::dataTableOutput("pred_result_table_ui"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;")))
                         
                         
                         
                         
                         
                         )),
                   f7Tab(tabName = "Current", icon= f7Icon("calendar_today", lib="md"),  f7Align(h1("CURRENT EARTHQUAKE"), side="center"), f7Card(title = "Current Earthquake", f7Block(f7Accordion(
                     inputId = "myaccordion1",
                     f7AccordionItem(
                       open=TRUE,
                       title = "SHOW MAP",  leafletOutput(outputId = "mymap_current", width="100%", height="400"), 
                       absolutePanel(bottom=50, left = 20, setBackgroundColor("white"),   f7Toggle(
                         inputId = "markers_current",
                         label = "Depth",
                         color = "blue"
                         
                       ),
                       
                       f7Toggle(
                         inputId = "heat_current",
                         label = "Heatmap",
                         color = "pink"
                       ),
                       
                       ))))), 
                     tags$script('
               $(document).ready(function () {
               navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
                function onError (err) {
                  Shiny.onInputChange("geolocation", false);
                }
                      
                function onSuccess (position) {
                  setTimeout(function () {
                    var coords = position.coords;
                    console.log(coords.latitude + ", " + coords.longitude);
  
                    Shiny.onInputChange("lat", coords.latitude);
                    Shiny.onInputChange("long", coords.longitude);
                  }, 1100)
                }
              });
                      '),  f7BlockTitle(title="YOUR LOCATION:"),
                     f7Row(width = 2,
                           verbatimTextOutput("lat"),
                           verbatimTextOutput("long")),  
                     f7Card( height="500",
                             f7Block( f7Button(inputId="current_eq_act", label="Activate Location"))), f7Card(title="Current Earthquake",DT::dataTableOutput("current_result_table_ui"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                   ), 
                   f7Tab(tabName = "Tab 3", "tab 3 text")
                 )
               )
  ),
  server = function(input, output, session) {
    output$selected <- renderText(input$tabdemo)
    output$lat <- renderPrint({
      input$lat
    })
    output$table_title <- renderUI({ HTML(paste(h4("Date Predicted:" )))})
    output$long <- renderPrint({
      input$long
    })
    
    output$geolocation <- renderPrint({
      input$geolocation
    })
    #  updateF7Tabs(session, id = "tabdemo", selected = "Tab 1")
    pal <- colorNumeric(
      palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
      domain = data$Magnitudo)
    
    #define the color of for the depth of the earquakes
    pal2 <- colorFactor(
      palette = c('blue', 'yellow', 'red'),
      domain = data$depth_type
    )
    
    output$mymap <- renderLeaflet({
      leaflet(data, width="1000", height="1000") %>% 
        setView(lng =  113.9213257, lat = -0.789275, zoom = 6)  %>% #setting the view over ~ center of Indonesia
        addTiles() %>%
        addFullscreenControl()
    })
    
    ##CURRENT MAP 
    
    output$mymap_current <- renderLeaflet({
      leaflet(data, width="1000", height="1000") %>% 
        setView(lng =  113.9213257, lat = -0.789275, zoom = 6)  %>% #setting the view over ~ center of Indonesia
        addTiles() %>%
        addFullscreenControl() %>%
        addCircles(data = data, lat = ~ Lintang, lng = ~ Bujur, weight = 1, radius = ~sqrt(Magnitudo)*25000, popup = paste0("Wilayah: ", data$Wilayah, "<br>", "Lintang: ", data$Lintang, "<br>", "Bujur: ", data$Bujur, "<br>", "Kedalaman: ", data$Kedalaman, "<br>", "Waktu Gempa: ", data$Waktu.Gempa, "<br>", "Magnitudo: ", data$Magnitudo), label = ~as.character(paste0("Magnitude: ", sep = " ", Magnitudo)), color = ~pal(Magnitudo), fillOpacity = 0.5)
    })
    
    observe({
      proxy <- leafletProxy("mymap_current", data = data)
      proxy %>% clearMarkers()
      if (input$heat_current) {
        proxy %>%  addHeatmap(lng=~Bujur, lat=~Lintang, intensity = ~Magnitudo, blur =  10, max = 0.05, radius = 15) 
      }
      else{
        proxy %>% clearHeatmap()
      }
      
      
    })
    
  
    observe( {
      
      if(!isTruthy(input$lat) && !isTruthy(input$long)){
        data$distance = "NULL"
        f7Toast(session, text="Turn on your GPS to activate location feature", position="center")
      }
      else{
      output$map_type<- renderUI({ f7BlockTitle(title="Current Earthquke", size="medium") })  
      EQLocation <- cbind(data$Bujur,  data$Lintang)
      YourLocation <- cbind(input$long, input$lat)
      data$distance <- (distHaversine(EQLocation, YourLocation)/1000)
      print(head(data))
      output$mymap_current <- renderLeaflet({
        leaflet(data, width="1000", height="1000") %>% 
          setView(lng =  input$long,  lat = input$lat,zoom = 4.5)  %>% #setting the view over ~ center of Indonesia
          addTiles() %>%
          addFullscreenControl() %>% addMarkers(lng=input$long, lat=input$lat)%>% 
          addCircles(data = data, lat = ~ Lintang, lng = ~ Bujur, weight = 1, radius = ~sqrt(Magnitudo)*25000, popup = paste0("Wilayah: ", data$Wilayah, "<br>", "Lintang: ", data$Lintang, "<br>", "Bujur: ", data$Bujur, "<br>", "Kedalaman: ", data$Kedalaman, "<br>", "Waktu Gempa: ", data$Waktu.Gempa, "<br>", "Magnitudo: ", data$Magnitudo), label = ~as.character(paste0("Magnitude: ", sep = " ", Magnitudo)), color = ~pal(Magnitudo), fillOpacity = 0.5)
      })
      }
      
      #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
      observe({
        proxy <- leafletProxy("mymap_current", data = data)
        proxy %>% clearMarkers()
        if (input$markers_current) {
          proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2, label = ~as.character(paste0("Magnitude: ", sep = " ", Magnitudo))) %>%
            addLegend("bottomright", pal = pal2, values = data$depth_type,
                      title = "Depth Type",
                      opacity = 1)}
        else {
          proxy %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        proxy <- leafletProxy("mymap_current", data = data)
        proxy %>% clearMarkers()
        if (input$heat_current) {
          proxy %>%  addHeatmap(lng=~Bujur, lat=~Lintang, intensity = ~Magnitudo, blur =  10, max = 0.05, radius = 15) 
        }
        else{
          proxy %>% clearHeatmap()
        }
        
        
      })
      output$current_result_table_ui =
        
        DT::renderDataTable({as.datatable(formattable(data,  list(
          Magnitudo = color_tile("transparent", "red"), height="100")
        ))})})
    
    output$inputControls <- renderUI({tagList(
      
      f7BlockTitle(title = "INPUT DAYS TO PREDICT", size = "medium"), chooseSliderSkin("Flat"),
      f7Slider(
        inputId = "days_input",
        label = "",
        max = 101,
        min = 1,
        value = 1,
        scale = TRUE, 
        
      )
    )
    })
    ########################### USER INPUT ###########################################
    ####################################################################################
    observeEvent(input$number_input_act, {
      
      output$inputControls <- renderUI({
        tagList(
          f7BlockTitle(title = "INPUT DAYS TO PREDICT", size = "medium"),
          f7Stepper(
            inputId = "days_input",
            label = "",
            min = 1,
            max = 100,
            value = 7,
            color = "BLACK",
            raised = TRUE,
            fill = TRUE,
            rounded = FALSE
          ),verbatimTextOutput(outputId = "days_output"))
        
      })
      
    })
    observeEvent(input$slider_input_act, {
      
      
      output$inputControls <- renderUI({tagList(
        
        f7BlockTitle(title = "INPUT DAYS TO PREDICT", size = "medium"), chooseSliderSkin("Flat"),
        f7Slider(
          inputId = "days_input",
          label = "",
          max = 101,
          min = 1,
          value = 1,
          scale = TRUE, 
          
        )
      )
      })
      
    })
    observeEvent(input$predict, {
      
      output$map_type<- renderUI({ f7BlockTitle(title="Earthquake Prediction", size="medium") })
      
      if(input$prediction_model == "SVM"){
        
        mydir = "2020eq-data/SVM/Hasil"
        
      }
      if(input$prediction_model == "NAIVEBAYES")
      {
        mydir = "2020eq-data/NaiveBayes/Hasil"
      }
      if(input$prediction_model == "MULTILOGREGRESSION")
      {
        mydir = "2020eq-data/MultiLogRegression/Hasil"
      }
      myfiles = list.files(path=mydir , pattern = "*.csv",full.names = TRUE)
      myfiles = mixedsort(sort(myfiles))
      days_input = (input$days_input) %% 367
      
      print("PREDICTION DATE :")
      
      dateToPredict = (as.Date(Sys.Date()) + days_input)
      print(dateToPredict)
      
      sumDate = (as.Date(dateToPredict) - as.Date("2020-01-01"))
      dateInFile = as.vector(sumDate)
      output$table_title <- renderUI({ HTML(paste(h4("Date Predicted :"), h2(dateToPredict) ))})
      print("NOMOR FILE YG BAKAL DIAMBIL :")
      print(dateInFile)
      
      print("FILE YANG DIAMBIL :")
      print(myfiles[dateInFile])
      datacsv = ldply(myfiles[dateInFile], read_csv)
      
      print(head(datacsv))
      
      
      output$date_predicted_info <- renderUI({ f7BlockTitle(title= dateToPredict, size="medium") })
      datacsv2 <- mutate(datacsv, CITY)
      
      for(row in 1:nrow(datacsv2)) {
        datacsv[row, "CITY"] = reverseGeoCodeCity(dataframe = datacsv2, row = row)
      }
      
      write.table(datacsv, file = myfiles[dateInFile],sep=",", row.names=FALSE,col.names=TRUE)
      print(head(datacsv))
      
      datacsv$depth_type <-  ifelse(datacsv$KEDALAMAN <= 70, "shallow", 
                                    ifelse(datacsv$KEDALAMAN <= 300 | datacsv$KEDALAMAN >70, "intermediate", 
                                           ifelse(datacsv$KEDALAMAN > 300, "deep", "other")))
      
      
      pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = datacsv$MAG)
      
      #define the color of for the depth of the earquakes
      pal2 <- colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = datacsv$depth_type
      )
      
      #create the map
      
      observe({
        
        proxy <- leafletProxy("mymap", data = datacsv)
        proxy %>% clearHeatmap()
        proxy %>% clearMarkers()
        if (input$heat) {
          proxy %>%  addHeatmap(lng=~LONGITUDE, lat=~LATITUDE, intensity = ~MAG, blur =  10, max = 0.05, radius = 15) 
        }
        else{
          proxy %>% clearHeatmap()
        }
        
        
      })
      output$mymap <- renderLeaflet({
        leaflet(datacsv, width="1000", height="1000") %>% 
          setView(lng =  113.9213257, lat = -0.789275, zoom = 4.5)  %>% #setting the view over ~ center of Indonesia
          addTiles() %>% 
          addFullscreenControl() %>% 
          addCircles(data = datacsv, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~sqrt(MAG)*25000, popup = ~as.character(MAG), label = ~as.character(paste0("Magnitude: ", sep = " ", datacsv$CITY)), color = ~pal(MAG), fillOpacity = 0.5)
      })
      
      #create the text output
      output$days_output <- renderText( {input$days_input} )
      
      #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
      observe({
        proxy <- leafletProxy("mymap", data = datacsv)
        proxy %>% clearMarkers()
        if (input$markers) {
          proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,      label = ~as.character(paste0("Magnitude: ", sep = " ", MAG))) %>%
            addLegend("bottomright", pal = pal2, values = datacsv$depth_type,
                      title = "Depth Type",
                      opacity = 1)}
        else {
          proxy %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        if(!isTruthy(input$lat) && !isTruthy(input$long)){
          f7Toast(session, text="Turn on your GPS to activate location feature", position="center")
          datacsv$distance = "NULL"
          print(head(datacsv))
          output$pred_result_table_ui =
            
            DT::renderDataTable({as.datatable(formattable(datacsv,  list(
              MAG = color_tile("transparent", "red"))
            ))})
        }
        else{
          output$map_type<- renderUI({ f7BlockTitle(title="Current Earthquke", size="medium") })  
          EQLocation <- cbind(data$Bujur,  data$Lintang)
          YourLocation <- cbind(input$long, input$lat)
          datacsv$distance <- (distHaversine(EQLocation, YourLocation)/1000)
          print(head(datacsv))
          output$mymap <- renderLeaflet({
            leaflet(data, width="1000", height="1000") %>% 
              setView(lng =  113.9213257, lat = -0.789275,zoom = 4.5)  %>% #setting the view over ~ center of Indonesia
              addTiles() %>%
              addFullscreenControl() %>% addMarkers(lng=input$long, lat=input$lat)%>% 
              addCircles(data = datacsv, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~sqrt(MAG)*25000, popup = ~as.character(MAG), label = ~as.character(paste0("Magnitude: ", sep = " ", datacsv$CITY)), color = ~pal(MAG), fillOpacity = 0.5)
          })
          output$pred_result_table_ui =
            
            DT::renderDataTable({as.datatable(formattable(datacsv,  list(
              MAG = color_tile("transparent", "red"))
            ))})
        }
     
      })
   
      
      
    }) 
  }
)

