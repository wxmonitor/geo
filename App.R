library(tidyverse)
library(lubridate)
library(ggtext)
library(shiny)
library(leaflet)
library(jsonlite)
library(scales)
library(data.table)

#### Both map apps in tabs #####

# Wind rose function to convert wind direction degrees to compass points
wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

# Force local time zone
Sys.setenv(TZ="America/Los_Angeles")

ui <- tabsetPanel(
  tabPanel("Forecast",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("48 Hour Forecast", align = "center"),
             leafletOutput("map", width = "100%", height = "400px"),
             h4(textOutput("time.current"), align = "center"),
             h4(textOutput("weather.label"), align = "center"),
             fluidRow(column(12, align = "center",
                             plotOutput(outputId = "rose", width = "50%", height = "200px"))),
             plotOutput(outputId = "weather.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "dir.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "fog.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "rain.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "bar.plot", width = "100%", height = "400px")
           )
  ),
  tabPanel("Report Data",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("APRS Station Reports", align = "center"),
             leafletOutput("map2", width = "100%", height = "400px"),
             h4(textOutput("time.current2"), align = "center"),
             h4(textOutput("time.label2"), align = "center"),
             h4(textOutput("site.label2"), align = "center"),
             h4(textOutput("weather.label2"), align = "center"),
             fluidRow(column(12, align = "center",
                             plotOutput(outputId = "rose2", width = "50%", height = "200px"))),
             plotOutput(outputId = "weather.plot2", width = "100%", height = "400px"),
             plotOutput(outputId = "dir.plot2", width = "100%", height = "400px"),
             plotOutput(outputId = "bar.plot2", width = "100%", height = "400px"),
             sliderInput("days", "Number of days:",
                         min = 1, max = 14, value = 2, width = "100%")
           )
  )
)


server <- function(input, output, session) {
  
  # Initialize leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 48.04469360218284, lng = -122.84101928712471 , zoom=10) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  })
  
  # Container for reactive values
  plot_rct <- reactiveValues()
  
  # Input location data from click event
  click_data <- observeEvent(
    input$map_click,
    {
      click <- input$map_click
      clat <- click$lat
      clng <- click$lng
      
      # Format coords for hover display
      lat.disp <- case_when(clat > 0 ~ paste(clat %>% round(2), "N"),
                            clat < 0 ~ paste(-clat %>% round(2), "S"))
      lng.disp <- case_when(clng < 0 ~ paste(-clng %>% round(2), "W"),
                            clng > 0 ~ paste(clng %>% round(2), "E"))
      
      # Use proxy to redraw map with location marker
      leafletProxy('map') %>% 
        clearMarkers() %>%
        addMarkers(lng=clng, lat=clat, label = paste(lat.disp, lng.disp))
      
      #  Call API and decode JSON
      url <- paste0("https://api.openweathermap.org/data/2.5/onecall?lat=",
                    clat, "&lon=", clng, "&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9")
      weather.page <- fromJSON(url, flatten = TRUE)
      
      # Strip and format hourly data
      hourly.forecast <- data.frame(weather.page$hourly) %>%
        mutate(dt = as.POSIXct(dt, origin="1970-01-01")) %>%
        mutate_at(vars(wind_speed, wind_gust), ~ . * 0.868976)
      
      # Strip and format current data
      current <- data.frame(weather.page$current) %>%
        mutate_at(vars(dt, sunrise, sunset), ~ as.POSIXct(., origin="1970-01-01")) %>%
        mutate(wind_speed = wind_speed * 0.868976) %>%
        mutate(wind_deg = as.integer(wind_deg)) %>%
        mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                                   wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                                   TRUE ~ wind_deg))
      
      # Load reactive container with current data
      plot_rct$current <- current
      
      # Create night-time shade limits
      shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                          dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                          top = Inf,
                          bottom = -Inf)
      
      shade <- shade %>% 
        mutate_at(vars(dusk, dawn),
                  ~ replace(., which(. > tail(hourly.forecast$dt, 1)), tail(hourly.forecast$dt, 1))) %>%
        mutate_at(vars(dusk, dawn),
                  ~ replace(., which(. < head(hourly.forecast$dt, 1)), head(hourly.forecast$dt, 1)))
      
      # Wind rose polar plot
      plot_rct$rose <- ggplot(current, aes(x = mod_deg)) +
        coord_polar(theta = "x", start = -pi/45, direction = 1) +
        geom_bar(width = 7, color = "gray10", fill = "red") +
        scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                           labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                      'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.title = element_blank())
      
      # Wind direction plot 
      plot_rct$dir.plot <- ggplot() +
        geom_rect(data = shade, 
                  aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                  fill = 'light grey', alpha = 0.5) +
        geom_point(data = hourly.forecast, aes(x = dt, y = wind.rose(wind_deg)), size = 1) +
        theme_bw() +
        labs(title = "**Wind Direction**") +
        theme(plot.title = element_markdown()) +
        ylab("") +
        xlab("") +
        scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
        scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
      
      # Barometric pressure plot
      plot_rct$bar.plot <- ggplot() + 
        geom_rect(data = shade, 
                  aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                  fill = 'light grey', alpha = 0.5) +
        geom_line(data = hourly.forecast, aes(x = dt, y = pressure), size = 1) +
        geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
        theme_bw() +
        labs(title = "**Barometric Pressure**") +
        theme(plot.title = element_markdown()) +
        ylab("Millibars") +
        xlab("") +
        scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
      
      
      # Wind speed and gust plot
      plot_rct$weather.plot <- ggplot() +
        geom_rect(data = shade, 
                  aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                  fill = 'light grey', alpha = 0.5) +
        geom_line(data = hourly.forecast, aes(x = dt, y = wind_speed), size = 1) +
        geom_line(data = hourly.forecast, aes(x = dt, y = wind_gust), color = "#FF0000") +
        theme_bw() +
        labs(
          title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
        theme(plot.title = element_markdown()) +
        ylab("Knots") +
        xlab("") + 
        scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
      
      # Temperature and dew point plot
      plot_rct$fog.plot <- ggplot() + 
        geom_rect(data = shade, 
                  aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                  fill = 'light grey', alpha = 0.5) +
        geom_line(data = hourly.forecast, aes(x = dt, y = temp), size = 1) +
        geom_line(data = hourly.forecast, aes(x = dt, y = dew_point), color = "gray", size = 1) +
        theme_bw() +
        labs(title = "**Temperature** and <span style='color:#B0B0B0;'>**Dew Point**</span></span>") +
        theme(plot.title = element_markdown()) +
        ylab("°F") +
        xlab("") +
        scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
      
      # Rain plot
      if ("rain.1h" %in% colnames(hourly.forecast)) {
        
        plot_rct$rain.plot <- ggplot() +
          geom_rect(data = shade, 
                    aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                    fill = 'light grey', alpha = 0.5) +
          geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
          geom_col(data = hourly.forecast, aes(x = dt, y = rain.1h/5), color = "darkgrey", fill = "#28d0eb") +
          geom_text(data = hourly.forecast, aes(x = dt, y = rain.1h/5, label = rain.1h), size = 2, vjust = -0.5) +
          theme_bw() +
          labs(
            title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
          theme(plot.title = element_markdown()) +
          ylab("Percent") + 
          xlab("") +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          coord_cartesian(ylim = c(0,1)) +
          scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
        
      } else (
        plot_rct$rain.plot <- ggplot() +
          geom_rect(data = shade, 
                    aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
                    fill = 'light grey', alpha = 0.5) +
          geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
          theme_bw() +
          labs(
            title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
          theme(plot.title = element_markdown()) +
          ylab("Percent") + 
          xlab("") +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          coord_cartesian(ylim = c(0,1)) +
          scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
      )
      
    })
  
  # Current wind output
  output$weather.label <- renderText({
    req(plot_rct$current)
    paste0(wind.rose(plot_rct$current$wind_deg), " ",
           round(plot_rct$current$wind_speed, 0), " knots ",
           "(", plot_rct$current$wind_deg, "°)")
    
  }) 
  
  # Wind plot output
  output$weather.plot <- renderPlot({
    plot_rct$weather.plot
  }) 
  
  
  # Current time output
  output$time.current <- renderText({
    paste("",format(Sys.time(), "%a %m-%d %H:%M"))
  })
  
  
  # Wind direction plot output
  output$dir.plot <- renderPlot({
    plot_rct$dir.plot
  })
  
  
  # Rain plot output
  output$rain.plot <- renderPlot({
    plot_rct$rain.plot
  })
  
  
  # Temperature plot output
  output$fog.plot <- renderPlot({
    plot_rct$fog.plot
  })
  
  
  # Barometer plot output
  output$bar.plot <- renderPlot({
    plot_rct$bar.plot
  }) 
  
  
  # Wind rose output
  output$rose <- renderPlot({
    plot_rct$rose
  }) 
  
  ### APRS TAB ####
  
  aprs.sites <- read.csv("aprs.sites.csv")
  
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 48.04469360218284, lng = -122.84101928712471 , zoom=10) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(aprs.sites$lon, aprs.sites$lat,
                       layerId = aprs.sites$site,
                       popup = aprs.sites$site, 
                       color = "#575757",
                       radius = 8,
                       weight = 3)
  })
  
  plot_rct <- reactiveValues()
  
  observeEvent(
    input$map2_marker_click, 
    { 
      click <- input$map2_marker_click
      site <- click$id
      plot_rct$site <- site
      clat <- click$lat
      clng <- click$lng
      
      leafletProxy('map2') %>%
        addPopups(lng = clng, lat = clat, site)
      
      if (site == "KNOW"){
        
        # Ediz Hook 
        
        # Call API and decode JSON
        url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=1440&vars=wind_speed,wind_gust,wind_direction,sea_level_pressure&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
        total.page <- fromJSON(url, flatten = TRUE)
        
        # Strip and format data
        weather.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_speed_set_1) %>%
          setNames(c("Time", "Wind Speed")) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))
        
        dir.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_direction_set_1) %>%
          setNames(c("Time", "Wind Direction")) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z")) %>%
          mutate(`Mod Direction` = case_when(`Wind Direction` > 352 && `Wind Direction` < 356 ~ 352,
                                             `Wind Direction` >= 356 && `Wind Direction` <= 360 ~ 0,
                                             TRUE ~ `Wind Direction`))
        
        pressure.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.sea_level_pressure_set_1d) %>%
          setNames(c("Time", "Pressure (mb)")) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))
        
        
        # Wind direction plot
        plot_rct$dir.plot2 <- ggplot() + 
          geom_point(data = dir.table, aes(x = Time, y = wind.rose(`Wind Direction`)), size = 1) +
          theme_bw() +
          labs(title = "**Wind Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(dir.table$Time), max(dir.table$Time)), expand = c(0, 0))
        
        # Barometer plot
        plot_rct$bar.plot2 <- ggplot() + 
          geom_line(data = pressure.table, aes(x = Time, y = `Pressure (mb)`), size = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(pressure.table$Time), max(pressure.table$Time)), expand = c(0, 0))
        
        # Wind rose
        plot_rct$rose2 <- ggplot(tail(dir.table, 1), aes(x = `Mod Direction`)) +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(width = 7, color = "gray10", fill = "red") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        # Catch errors due to missing gust data
        
        a <- try({
          gust.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_gust_set_1) %>%
            setNames(c("Time", "Wind Speed")) %>%
            mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))
          
          gust.table$Time <- gust.table$Time
          
          # Wind + gust plot
          plot_rct$weather.plot2 <- ggplot() + 
            geom_line(data = weather.table, aes(x = Time, y = `Wind Speed`), color = "black", size = 1) +
            geom_point(data = gust.table, aes(x = Time, y = `Wind Speed`), color = "#FF0000") +
            theme_bw() +
            labs(
              title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
            theme(plot.title = element_markdown()) +
            scale_y_continuous(breaks = seq(0, max(na.omit(gust.table$`Wind Speed`)),5)) +
            scale_x_datetime(limits = c(min(weather.table$Time), max(weather.table$Time)), expand = c(0, 0)) +
            ylab("Knots") +
            xlab("")
        })
        
        if (class(a) == "try-error") {
          
          # Wind only plot
          plot_rct$weather.plot2 <- ggplot() +
            geom_line(data = weather.table, aes(x = Time, y = `Wind Speed`), size = 1) +
            theme_bw() +
            labs(
              title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
            theme(plot.title = element_markdown()) +
            scale_y_continuous(breaks = seq(0, max(weather.table$`Wind Speed`),5)) +
            scale_x_datetime(limits = c(min(weather.table$Time), max(weather.table$Time)), expand = c(0, 0)) +
            ylab("Knots") +
            xlab("")
        }
        
        # Ediz Hook last reading output
        output$time.label2 <- renderText({
          paste("Last reading:", format(tail(weather.table$Time, 1), "%m-%d %H:%M"))
        }) 
        
        # Ediz Hook current weather label output
        output$weather.label2 <- renderText({
          paste0(wind.rose(last(na.omit(dir.table$`Wind Direction`))), " ",
                 last(na.omit(weather.table$`Wind Speed`)), " knots ",
                 "(", last(na.omit(dir.table$`Wind Direction`)), "°)")
          
        }) 
        
      } else if (site == "PTWW1") {
        
        # Call API 
        weather <- fread("https://www.ndbc.noaa.gov/data/realtime2/PTWW1.txt", 
                         skip = 2,
                         na.strings = "MM",
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 3), 
                                        rep("factor", 4), rep("numeric", 3), rep("factor", 4)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", 
                                       "Wind.Speed", "Gust", NA, NA, NA, NA, "Pressure", 
                                       "Air.Temp", "Water.Temp", NA, NA, NA, NA))
        
        # Subset past 24 hours of observations, clean and format data
        weather <- weather[1:240,] %>%
          select(c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", "Wind.Speed",
                   "Gust", "Pressure", "Air.Temp", "Water.Temp")) %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate(Wind.Speed = Wind.Speed * 1.94384) %>%
          mutate(Gust = Gust *1.94384) %>%
          mutate(Mod.Dir = case_when(Wind.Dir > 350 ~  0,
                                     TRUE ~ Wind.Dir))
        
        # Wind direction plot
        plot_rct$dir.plot2 <- ggplot() + 
          geom_point(data = weather, aes(x = Time, y = wind.rose(Wind.Dir)), size = 1) +
          theme_bw() +
          labs(title = "**Wind Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        # Barometer plot
        plot_rct$bar.plot2 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Pressure), size = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        # Wind rose
        plot_rct$rose2 <- ggplot(weather, aes(x = first(na.omit(Mod.Dir)))) +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(width = 7, color = "gray10", fill = "red") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        # Handle missing gust erros
        if (any(is.na(weather$Gust)) == TRUE){
          weather <- weather %>%
            drop_na(Gust)
        }
        
        # Wind plot
        plot_rct$weather.plot2 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wind.Speed), size = 1) +
          geom_line(data = weather, aes(x = Time, y = Gust), color = "#FF0000") +
          theme_bw() +
          labs(
            title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, max(weather$Gust), 5)) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Knots") +
          xlab("")
        
        # PT last reading output
        output$time.label2 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # PT current weather label output
        output$weather.label2 <- renderText({
          paste0(wind.rose(first(na.omit(weather$Wind.Dir))), " ",
                 round(first(na.omit(weather$Wind.Speed)), 0), " knots ",
                 "(", first(na.omit(weather$Wind.Dir)), "°)")
          
        })
        
      } else {
        
        days <- input$days
        url <- paste0("https://weather.gladstonefamily.net/cgi-bin/wxobservations.pl?site=",site,"&days=",days)
        url.data <- RCurl::getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
        aprs <- read.csv(textConnection(url.data),
                         col.names = c("dt", "pressure", "temp", "dew_point", "rel_humidity", 
                                       "wind_speed", "wind_deg", "ABaro", "ATemp", "ADew", 
                                       "ARel", "AWind", "ADir")) %>%
          mutate(dt = as.POSIXct(dt, format = "%Y-%m-%d %H:%M")) %>%
          mutate(dt = case_when(
            dst(dt[1]) == TRUE ~ dt - 25200,
            dst(dt[1]) == FALSE ~ dt - 28800)) %>%
          mutate(wind_speed = wind_speed * 0.868976) %>%
          mutate(wind_deg = as.integer(wind_deg)) %>%
          mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                                     wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                                     TRUE ~ wind_deg)) %>%
          arrange(desc(row_number()))
        
        plot_rct$aprs <- aprs
        
        # Wind rose plot
        plot_rct$rose2 <- ggplot(aprs[1,], aes(x = mod_deg)) +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(width = 7, color = "gray10", fill = "red") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        # Wind direction plot
        plot_rct$dir.plot2 <- ggplot() +
          geom_point(data = aprs, aes(x = dt, y = wind.rose(wind_deg)), size = 1) +
          theme_bw() +
          labs(title = "**Wind Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(aprs$dt), max(aprs$dt)), expand = c(0, 0))
        
        # Barometric pressure plot
        plot_rct$bar.plot2 <- ggplot() + 
          geom_line(data = aprs, aes(x = dt, y = pressure), size = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(aprs$dt), max(aprs$dt)), expand = c(0, 0))
        
        # Wind speed plot
        plot_rct$weather.plot2 <- ggplot() +
          geom_line(data = aprs, aes(x = dt, y = wind_speed), size = 1) +
          theme_bw() +
          labs(
            title = "**Wind Speed**") +
          theme(plot.title = element_markdown()) +
          ylab("Knots") +
          xlab("") + 
          scale_x_datetime(limits = c(min(aprs$dt), max(aprs$dt)), expand = c(0, 0))
        
        # Current weather label output
        output$weather.label2 <- renderText({
          req(input$map2_marker_click)
          paste0(wind.rose(first(na.omit(plot_rct$aprs$wind_deg))), " ",
                 round(first(na.omit(plot_rct$aprs$wind_speed)), 0), " knots ",
                 "(", first(na.omit(plot_rct$aprs$wind_deg)), "°)")
          
        }) 
        
        # Last reading output
        output$time.label2 <- renderText({
          req(input$map2_marker_click)
          paste("Last reading:", format(first(na.omit(plot_rct$aprs$dt)), "%m-%d %H:%M"))
        }) 
        
      }
      
    })
  
  # Site label output
  output$site.label2 <- renderText({
    req(input$map2_marker_click)
    paste0("Station: ", plot_rct$site)
  })
  
  # Wind plot output
  output$weather.plot2 <- renderPlot({
    plot_rct$weather.plot2
  }) 
  
  # Current time label output
  output$time.current2 <- renderText({
    paste("",format(Sys.time(), "%a %m-%d %H:%M"))
  })
  
  # Wind direction plot output
  output$dir.plot2 <- renderPlot({
    plot_rct$dir.plot2
  })
  
  # Barometer plot output
  output$bar.plot2 <- renderPlot({
    plot_rct$bar.plot2
  }) 
  
  # Wind rose output
  output$rose2 <- renderPlot({
    plot_rct$rose2
  }) 
  
}

# Execute app
shinyApp(ui, server)






