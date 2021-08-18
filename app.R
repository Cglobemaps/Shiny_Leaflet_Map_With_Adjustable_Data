# The result can be found on https://cglobe.shinyapps.io/shiny_leaflet_map_with_adjustable_data/


library(shiny)
library(shinydashboard)
library(shinyjs)
library(sodium)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(ggmap)
library(htmlwidgets)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(GISTools)
library(raster)


Border_NIR <- readOGR("geojson/Border_NIR.geojson")
Military_Zones <- readOGR("geojson/Military_Zones.geojson")
activitieslist <- as.list(as.character(unique(Military_Zones$milact)))
activitieslist <- c(list("No Filter"),activitieslist)
twenty_km_offshore <- readOGR("geojson/20_km_offshore.geojson")
Testfile <- readOGR("geojson/Testfile.geojson")

MPA_NIR <- readOGR("geojson/MPA_NIR.geojson")
Natura2000_NIR <- readOGR("geojson/Natura2000_NIR.geojson")

wind_speed <- raster("raster/Wind_Speel_NIR.tif")
palwin <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(wind_speed),
                       na.color = "transparent")

Bathymetry <- raster("raster/Bathymetry_NIR.tif")
palbat <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Bathymetry),
                       na.color = "transparent")

Shipping_Routes_Density_Cargo <- raster("raster/Shipping_Routes_Density_Cargo.tif")
palcar <- colorNumeric(c("Green", "Orange", "Red"), values(Shipping_Routes_Density_Cargo),
                       na.color = "transparent")

Shipping_Routes_Density_Fishing <- raster("raster/Shipping_Routes_Density_Fishing.tif")
palfis <- colorNumeric(c("Green", "Orange", "Red"), values(Shipping_Routes_Density_Fishing),
                       na.color = "transparent")

Shipping_Density_Global <- raster("raster/Ship_Density_All.tif")
palship <- colorNumeric(c("Green","Orange","Red"), values(Shipping_Density_Global),
                        na.color = "transparent")

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui <- fluidPage(
    shinyjs::useShinyjs(),
    div(
        textOutput("text"),leafletOutput("mymap"),
        #p(),
        #actionButton("recalc", "New points"),
        br(),
        #verbatimTextOutput("value"),
        downloadButton('downloadData', 'Download KML'),
        verbatimTextOutput("out"),
        titlePanel("Alter Layer Filters Here:"),
        sidebarLayout(
            
            # Sidebar panel for inputs 
            sidebarPanel(
                fileInput("KMLfile", "Import KML file",
                          multiple = TRUE,
                          accept = c("text/kml",".kml")),
                numericInput(
                    inputId = "Batmin",
                    label = "Set Minimum Value for Bathymetry (in meters)",
                    value = -250,
                    width = NULL
                ),
                numericInput(
                    inputId = "Batmax",
                    label = "Set Maximum Value for Bathymetry (in meters)",
                    value = -20,
                    width = NULL
                ),
                numericInput(
                    inputId = "Windmin",
                    label = "Set Minimum Value for Wind Speed (in m/s)",
                    value = 9,
                    width = NULL
                ),
                numericInput(
                    inputId = "Carmin",
                    label = "Set minimum Value for Cargo vessels per month per km2",
                    value = minValue(Shipping_Routes_Density_Cargo),
                    width = NULL
                ),
                numericInput(
                    inputId = "WBmin",
                    label = "Set minimum value for Shipping Vessel data from World Bank (per 500m)",
                    value = 0,
                    width = NULL
                ),
                numericInput(
                    inputId = "Fismin",
                    label = "Set minimum Value for Fishing vessels per month per km2",
                    value = minValue(Shipping_Routes_Density_Fishing),
                    width = NULL
                ),
                selectInput(inputId = "Milact",
                            label = "Filter for Military Activities in Military Zones",
                            multiple = TRUE,
                            selected = "No Filter",
                            choices = list("activity" = activitieslist))
            ),
            mainPanel(
                # Hide errors
                tags$style(type = "text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
            ))
        
    )
)



server<- function(input, output, session) {
    
    
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        filter_bat <- Bathymetry %in% as.numeric(input$Batmin):as.numeric(input$Batmax)
        suitable_bat <- mask(Bathymetry, filter_bat, maskvalue = 0)
        wind_speed_filt <- wind_speed > as.numeric(input$Windmin)
        filter_wind <- mask(wind_speed, wind_speed_filt, maskvalue = 0)
        suitable_wind <- filter_wind
        Shipping_Routes_Density_Cargo_filt <- Shipping_Routes_Density_Cargo > as.numeric(input$Carmin)
        Shipping_Routes_Density_Cargo <- mask(Shipping_Routes_Density_Cargo, Shipping_Routes_Density_Cargo_filt, maskvalue = 0)
        Shipping_Routes_Density_Fishing_filt <- Shipping_Routes_Density_Fishing > as.numeric(input$Fismin)
        Shipping_Routes_Density_Fishing <- mask(Shipping_Routes_Density_Fishing, Shipping_Routes_Density_Fishing_filt, maskvalue = 0)
        Shipping_Density_Global_filt <- Shipping_Density_Global > as.numeric(input$WBmin)
        Shipping_Density_Global <- mask(Shipping_Density_Global, Shipping_Density_Global_filt, maskvalue = 0)
        if (is.null(input$KMLfile)) {
            kmlinputfile <- Testfile
        } else {
            kmlinputfile <- readOGR(input$KMLfile$datapath)
        }
        if (input$Milact=="No Filter") {
            print("do nothing")
        } else {
            Military_Zones <- Military_Zones[Military_Zones$milact==input$Milact,]
        }
        leaflet("mymap") %>%
            addTiles(group = "OSM (default)") %>%
            addProviderTiles("CartoDB.Positron", group = "BlackWhite") %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addProviderTiles(providers$OpenSeaMap, group = "OpenSeamap") %>%
            addDrawToolbar(targetGroup = "drawnPoly", 
                           rectangleOptions = F, 
                           polylineOptions = F, 
                           markerOptions = F, 
                           circleOptions=F,
                           circleMarkerOptions = F,
                           polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE))) %>%
            
            addStyleEditor() %>%
            # LAYERS
            addPolygons(data=kmlinputfile, weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 1.0, color = "Orange",
                        fillColor = "Orange", group = "KML File", 
                        popup = paste("This is the added KML file")) %>%
            addPolylines(data=Border_NIR, color = "blue", weight=1.0,
                         popup = ("Border EEZ Northern Ireland")) %>%
            addPolygons(data=Military_Zones, weight = 1.0, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "Red",
                        fillColor = "Red", group = "Military Zones", 
                        popup = paste("Military Zone:","<br>",
                                      "used for:",Military_Zones$milact)) %>%
            addPolygons(data=twenty_km_offshore, weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "Red",
                        fillColor = "Red", group = "20KM Offshore", 
                        popup = paste("20KM Offshore")) %>%
            addPolygons(data=MPA_NIR, weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "Red",
                        fillColor = "Red", group = "MPA", 
                        popup = paste("Protected Area:","<br>",
                                      MPA_NIR$siteName)) %>%
            addPolygons(data=Natura2000_NIR, weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "Green",
                        fillColor = "Green", group = "MPA", 
                        popup = paste("Natura 2000 Area:","<br>",
                                      Natura2000_NIR$SITENAME)) %>%
            addRasterImage(suitable_wind, colors = 'Red', opacity = 0.5, group = "Suitable Wind Speed") %>%
            addRasterImage(suitable_bat, colors = 'Green', opacity = 0.5, group = "Suitable Bathymetry") %>%
            addPolygons(data=twenty_km_offshore, weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "Red",
                        fillColor = "Red", group = "20KM Offshore", 
                        popup = paste("20KM Offshore")) %>%
            addRasterImage(wind_speed, colors = palwin, opacity = 0.8, group = "Wind Speed") %>%
            addLegend(pal = palwin, values = values(wind_speed),
                      title = "Wind Speed", group = "Wind Speed") %>%
            addRasterImage(Bathymetry, colors = palbat, opacity = 0.8, group = "Bathymetry") %>%
            addLegend("bottomleft",pal = palbat, values = values(Bathymetry),
                      title = "Bathymetry", group = "Bathymetry") %>%
            addRasterImage(Shipping_Routes_Density_Cargo, colors = palcar, opacity = 0.8, group = "Shipping Routes Cargo") %>%
            addLegend(pal = palcar, values = values(Shipping_Routes_Density_Cargo),
                      title = "Cargo: Hours per km2 per Month", group = "Shipping Routes Cargo") %>%
            addRasterImage(Shipping_Routes_Density_Fishing, colors = palfis, opacity = 0.8, group = "Shipping Routes Fishing") %>%
            addLegend(pal = palfis, values = values(Shipping_Routes_Density_Fishing),
                      title = "Fishing: Hours per km2 per Month", group = "Shipping Routes Fishing") %>%
            addRasterImage(Shipping_Density_Global, colors = palship, opacity = 0.8, group = "Shipping Density WorldBank") %>%
            addLegend(pal = palship, values = values(Shipping_Density_Global),
                      title = "World Bank Vessels per 500m from 2015 till 2020", group = "Shipping Density WorldBank") %>%
            addMeasure(primaryLengthUnit = "meters",
                       secondaryLengthUnit = NULL,) %>%
            onRender(
                "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                      }"
            ) %>%
            addLayersControl(
                baseGroups = c("OSM (default)", "BlackWhite", "Toner", "Toner Lite"),
                overlayGroups = c("OpenSeamap","KML File", "Military Zones", "20KM Offshore", "MPA","Suitable Wind Speed","Suitable Bathymetry","Wind Speed" ,"Bathymetry","Shipping Routes Cargo","Shipping Routes Fishing","Shipping Density WorldBank"),
                options = layersControlOptions(collapsed = TRUE, overlayGroups = character(0))
            )%>%
            hideGroup(c("Distance from Power Stations", "Distance from Belfast Port", "Military Zones", "20KM Offshore", "MPA", "Wind Speed" ,"Bathymetry","Shipping Routes Cargo","Shipping Routes Fishing","Shipping Density WorldBank"))
        
    })
    
    output$out <- renderText({
        if(is.null(input$hover_coordinates)) {
            "Mouse outside of map"
        } else {
            paste0("Lat: ", input$hover_coordinates[1], 
                   "\nLng: ", input$hover_coordinates[2])
        }
    })
    
    
    latlongs<-reactiveValues()   #temporary to hold coords
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
    
    #########
    #empty reactive spdf
    value<-reactiveValues()
    SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
    
    #fix the polygon to start another
    
    observeEvent(input$mymap_draw_new_feature, {
        
        coor<-unlist(input$mymap_draw_new_feature$geometry$coordinates)
        
        Longitude<-coor[seq(1,length(coor), 2)] 
        
        Latitude<-coor[seq(2,length(coor), 2)]
        
        isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
        
        poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
        polys<-Polygons(list(poly),    ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
        spPolys<-SpatialPolygons(list(polys))
        
        
        #
        value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                        data=data.frame(notes=NA, row.names=
                                                                                            row.names(spPolys))))
        
        
        latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
        
    })
    
   
    
    
    #write the polys to .shp
    output$downloadData<-downloadHandler(
        
        filename = 'kmlExport.zip',
        content = function(file) {
            if (length(Sys.glob("kmlExport.*"))>0){
                file.remove(Sys.glob("kmlExport.*"))
            }
            
            proj4string(value$drawnPoly)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
            writeOGR(value$drawnPoly, dsn="kmlExport.kml", layer="kmlExport", driver="KML")
            zip(zipfile='kmlExport.zip', files=Sys.glob("kmlExport.*"))
            file.copy("kmlExport.zip", file)
            if (length(Sys.glob("kmlExport.*"))>0){
                file.remove(Sys.glob("kmlExport.*"))
            }
        }
    )
    
}


shinyApp(ui=ui,server=server)
