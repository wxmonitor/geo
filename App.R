library(tidyverse)
library(lubridate)
library(ggtext)
library(shiny)
library(leaflet)
library(jsonlite)
library(scales)
library(data.table)
library(rvest)
library(janitor)

#### Edited 5/19/25 to improve MESOWEST call and add error handling ####
#### Edited 5/14/25 to fix deprecation of MESOWEST system
#### Edited 5/13/25 to reflect new backyard buoy stations, remove tidal tab, and fix pipe error in wave syntax ####
#### Edited 8/1/23 to add tidal stations
#### Edited 7/15/23 to fix mutate() bug for SOFAR wind data ####
#### Edited 5/1/23 to change name ####
#### Edited 12/18/22 to add Cape Elizabeth ####
#### Edited 11/20/22 to reflect new Oregon NWS forecast zones ####
#### Edited 9/28/22 to add Cape Flattery and New Dungeness ####
#### Edited 5/29/22 to add coastal forecast & discussion ####

## Functions ##

# Wind rose function to convert wind direction degrees to compass points
wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

# Wind degree function to convert compass points to wind direction
wind.deg <- function(x) {
  degree <- seq(from = 0, by = 22.5, length.out = 16)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')
  degree[match(x, card1)]
}

marcast <- function(zone) {
  if (zone == "afd") {
    link <- "https://tgftp.nws.noaa.gov/data/raw/fx/fxus66.ksew.afd.sew.txt"
    data <- readLines(link)
    return(paste0(data[grep("Area", data):length(data)], sep = "<br/>"))
  } else {
    link <- paste0("https://tgftp.nws.noaa.gov/data/forecasts/marine/coastal/pz/", zone, ".txt")
    data <- readLines(link)
    return(paste0(data[grep("PZZ", data):length(data)], sep = "<br/>"))
  }
}

# Force local time zone
Sys.setenv(TZ="America/Los_Angeles")

# UI layout
ui <- tabsetPanel(
  tabPanel("Station Reports",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("Station Reports", align = "center"),
             leafletOutput("map2", width = "100%", height = "400px"),
             h4(textOutput("time.current2"), align = "center"),
             h4(textOutput("time.label2"), align = "center"),
             h4(textOutput("site.label2"), align = "center"),
             h4(textOutput("weather.label2"), align = "center"),
             fluidRow(column(12, align = "center",
                             plotOutput(outputId = "rose2", width = "50%", height = "200px"))),
             plotOutput(outputId = "weather.plot2", width = "100%", height = "400px"),
             plotOutput(outputId = "dir.plot2", width = "100%", height = "400px"),
             plotOutput(outputId = "bar.plot2", width = "100%", height = "400px")
           )
  ),
  tabPanel("Wave Reports",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("Wave Reports", align = "center"),
             leafletOutput("map3", width = "100%", height = "400px"),
             h4(textOutput("time.current3"), align = "center"),
             h4(textOutput("time.label3"), align = "center"),
             h4(textOutput("site.label3"), align = "center"),
             h4(htmlOutput("weather.label3"), align = "center"),
             fluidRow(column(12, align = "center",
                             plotOutput(outputId = "rose3", width = "50%", height = "200px"))),
             plotOutput(outputId = "wave.plot3", width = "100%", height = "400px"),
             plotOutput(outputId = "wave.dir.plot3", width = "100%", height = "400px"),
             plotOutput(outputId = "swell.plot3", width = "100%", height = "400px"),
             plotOutput(outputId = "wind.wave.plot3", width = "100%", height = "400px")
           )
  ),
  tabPanel("Ship Reports",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("Ship Reports", align = "center"),
             h4(textOutput("ship.recency4"), align = "center"),
             leafletOutput("map4", width = "100%", height = "400px")
           )
  ),
  tabPanel("Open Forecast",
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
  tabPanel("NWS Forecast",
           fluidPage(
             h3("WX Monitor", align = "center"),
             h3("NWS Marine Forecast", align = "center"),
             selectInput("zone", "Zone:",
                         c("Synopsis" = "pzz100",
                           "Discussion" = "afd",
                           "Cape Flattery to James Island to 10 nm" = "pzz150", 
                           "James Island to Point Grenville to 10 nm" = "pzz153",
                           "Point Grenville to Cape Shoalwater to 10 nm" = "pzz156",
                           "Cape Flattery to James Island 10-60 nm" = "pzz170", 
                           "James Island to Point Grenville to 10-60 nm" = "pzz173",
                           "Point Grenville to Cape Shoalwater to 10-60 nm" = "pzz176",
                           "West Strait" = "pzz130",
                           "Central Strait" = "pzz131",
                           "East Strait" = "pzz132",
                           "Puget Sound & Hood Canal" = "pzz135",
                           "Admiralty Inlet" = "pzz134", 
                           "Northern Inland Waters" = "pzz133",
                           "Grays Harbor Bar" = "pzz110",
                           "Southern WA/Northern OR Synopsis" = "pzz200",
                           "Southern OR Synopsis" = "pzz300",
                           "Columbia River Bar" = "pzz210",
                           "Cape Shoalwater to Cape Falcon to 10 nm" = "pzz251",
                           "Cape Falcon to Cape Foulweather to 10 nm" = "pzz252",
                           "Cape Foulweather to Florence to 10 nm" = "pzz253",
                           "Florence to Cape Blanco to 10 nm" = "pzz350",
                           "Cape Shoalwater to Cape Falcon 10-60 nm" = "pzz271",
                           "Cape Falcon to Cape Foulweather 10-60 nm" = "pzz272",
                           "Cape Foulweather to Florence 10-60 nm" = "pzz273",
                           "Florence to Cape Blanco 10-60 nm" = "pzz370"), selected = NULL),
             fluidRow(htmlOutput("zone.data",
                                 style = "width:100%;
                                 align:center;
                                 text-align:justify;
                                 padding:10px;
                                 font-size:10px"))
           )
  )
  
)


server <- function(input, output, session) {
  
  ########################
  ### Lubber forecast ####
  ########################
  
  # Initialize leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 47.77920969878382, lng = -123.61182070598635 , zoom=8) %>%
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
      url <- paste0("https://api.openweathermap.org/data/3.0/onecall?lat=",
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
        geom_line(data = hourly.forecast, aes(x = dt, y = pressure), linewidth = 1) +
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
        geom_line(data = hourly.forecast, aes(x = dt, y = wind_speed), linewidth = 1) +
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
        geom_line(data = hourly.forecast, aes(x = dt, y = temp), linewidth = 1) +
        geom_line(data = hourly.forecast, aes(x = dt, y = dew_point), color = "gray", linewidth = 1) +
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
          geom_line(data = hourly.forecast, aes(x = dt, y = pop), linewidth = 1) +
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
          geom_line(data = hourly.forecast, aes(x = dt, y = pop), linewidth = 1) +
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
  
  
  ########################
  ### Station reports ####
  ########################
  
  aprs.sites <- read.csv("aprs.sites.csv",
                         colClasses = c("character", "character", "numeric", "numeric",
                                        "character", "character", "integer", "character",
                                        "logical", "character", "integer", "character", "logical"))
  wind.sites <- aprs.sites %>%
    filter(wind == "TRUE")
  
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 47.77920969878382, lng = -123.61182070598635 , zoom=8) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(wind.sites$lon, wind.sites$lat,
                       layerId = wind.sites$site,
                       popup = wind.sites$name, 
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
      type <- wind.sites$type[wind.sites$site == site]
      name <- wind.sites$name[wind.sites$site == site]
      plot_rct$site <- paste(name, site)
      wind.link <- wind.sites$wind.link[wind.sites$site == site]
      wind.n <- wind.sites$wind.n[wind.sites$site == site]
      clat <- click$lat
      clng <- click$lng
      
      leafletProxy('map2') 
      
      tryCatch({
      
      if (type == "MESO"){
        
        # Ediz Hook CG station
        url <- "https://mesowest.utah.edu/cgi-bin/droman/meso_table_mesodyn.cgi?stn=KNOW&unit=0&time=LOCAL&year1=&month1=&day1=0&hour1=00&hours=24&past=0&order=1"
        
        # Scrape most recent reading
        last_reading <- url %>%
          read_html() %>%
          html_elements('font[color="#730000"]') %>%
          html_text() %>%
          .[2] %>%
          str_replace("Most Recent Weather Conditions at: ", "") %>%
          str_replace(" PDT", "") %>%
          as.POSIXct(format = "%m/%d/%Y %H:%M")
        
        # Scrape and clean table data 
        weather.page <- url %>%
          read_html() %>%
          html_nodes("table") %>%
          html_table(fill = T) %>%
          .[which(sapply(., FUN=function(X) "Time(PDT)" %in% colnames(X)))] %>%
          .[[1]] %>%
          clean_names() %>%
          select(any_of(c("Time" = "time_pdt",
                          "Wind.Speed" = "wind_speed_mph",
                          "Wind.Gust" = "wind_gust_mph",
                          "Wind.Direction" = "wind_direction",
                          "Pressure" = "pressure_in"))) %>%
          mutate(Time = as.POSIXct(Time, format = "%H:%M")) %>%
          mutate(across(matches(c("Wind.Speed", "Wind.Gust")), ~ . * 0.868976)) %>%
          mutate(Pressure = Pressure * 33.8639) %>%
          mutate(Diff = Time - lead(Time))
    
        # Correct for times spanning midnight
        date_change <- which(weather.page$Diff != 20)
        
        for (i in seq_along(weather.page$Time)) {
          if (length(date_change) > 0) {
            if (i > date_change) {
            weather.page$Time[i] <-  weather.page$Time[i] - 86400
          }
          }
        }
        
        
        # Wind direction plot
        plot_rct$dir.plot2 <- ggplot() + 
          geom_point(data = weather.page, aes(x = Time, y = Wind.Direction), size = 1) +
          theme_bw() +
          labs(title = "**Wind Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather.page$Time), max(weather.page$Time)), expand = c(0, 0))
        
        # Barometer plot
        plot_rct$bar.plot2 <- ggplot() + 
          geom_line(data = weather.page, aes(x = Time, y = Pressure), linewidth = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(weather.page$Time), max(weather.page$Time)), expand = c(0, 0))
        
        # Wind rose
        plot_rct$rose2 <- ggplot(weather.page, aes(x = wind.deg(first(na.omit(Wind.Direction))))) +
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
        if (all(is.na(weather.page$Wind.Gust)) == FALSE) {
          
          # Wind + gust plot
          plot_rct$weather.plot2 <- ggplot() + 
            geom_line(data = weather.page, aes(x = Time, y = Wind.Speed), color = "black", linewidth = 1) +
            geom_point(data = weather.page, aes(x = Time, y = Wind.Gust), color = "#FF0000") +
            theme_bw() +
            labs(
              title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
            theme(plot.title = element_markdown()) +
            scale_y_continuous(breaks = seq(0, max(na.omit(weather.page$Wind.Speed)),5)) +
            scale_x_datetime(limits = c(min(weather.page$Time), max(weather.page$Time)), expand = c(0, 0)) +
            ylab("Knots") +
            xlab("")
        }
        
        else {
          
          # Wind only plot
          plot_rct$weather.plot2 <- ggplot() +
            geom_line(data = weather.page, aes(x = Time, y = Wind.Speed), linewidth = 1) +
            theme_bw() +
            labs(
              title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
            theme(plot.title = element_markdown()) +
            scale_y_continuous(breaks = seq(0, max(weather.page$Wind.Speed),5)) +
            scale_x_datetime(limits = c(min(weather.page$Time), max(weather.page$Time)), expand = c(0, 0)) +
            ylab("Knots") +
            xlab("")
        }
        
        # Last reading output
        output$time.label2 <- renderText({
          paste("Last Reading:", format(last_reading, "%m-%d %H:%M"))
        }) 
        
        # Current weather label output
        output$weather.label2 <- renderText({
          paste0(first(na.omit(weather.page$Wind.Direction)), " ",
                 first(na.omit(weather.page$Wind.Speed)) %>% round(0), " knots ",
                 "(", wind.deg(first(na.omit(weather.page$Wind.Direction))), "°)")
          
        }) 
        
      } else if (type == "NDBC") {
        
        # NDBC stations 
        # Call API 
        weather <- fread(wind.link, 
                         skip = 2,
                         na.strings = "MM",
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 3), 
                                        rep("factor", 4), rep("numeric", 3), rep("factor", 4)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", 
                                       "Wind.Speed", "Gust", NA, NA, NA, NA, "Pressure", 
                                       "Air.Temp", "Water.Temp", NA, NA, NA, NA))
        
        
        # Subset past 24 hours of observations, clean and format data
        weather <- weather[1:wind.n,] %>%
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
          geom_line(data = weather, aes(x = Time, y = Pressure), linewidth = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        # Avoid wind rose error if all direction data is missing
        if (all(is.na(weather$Wind.Speed)) == FALSE) {
          
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
          
        } else {plot_rct$rose2 <- ggplot() +
          theme_void() +
          geom_text(aes(0,0,label='No wind data'))}
        
        
        # Avoid scale error if all wind data is missing
        if (all(is.na(weather$Wind.Speed)) == FALSE) {
          maxWind <- max(rbind(weather$Wind.Speed, weather$Gust), na.rm = TRUE)
        } else (maxWind <- 10)
        
        
        # Wind plot
        plot_rct$weather.plot2 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wind.Speed), linewidth = 1) +
          geom_line(data = weather, aes(x = Time, y = Gust), color = "#FF0000") +
          theme_bw() +
          labs(
            title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, maxWind, 5)) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Knots") +
          xlab("")
        
        
        # Last reading output
        output$time.label2 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Current weather label output
        output$weather.label2 <- renderText({
          paste0(wind.rose(first(na.omit(weather$Wind.Dir))), " ",
                 round(first(na.omit(weather$Wind.Speed)), 0), " knots ",
                 "(", first(na.omit(weather$Wind.Dir)), "°)")
          
        })
        
        
      } else if (type == "CONT") {
        
        # Continuous wind stations 
        # Call API 
        weather <- fread(paste0(wind.link, ".cwind"), 
                         skip = 2,
                         na.strings = c("MM", 999, 9999),
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 5)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", 
                                       "Wind.Speed", "Gust.Dir", "Gust", 
                                       "Gust.Time"))
        
        
        # Subset past 24 hours of observations, clean and format data
        weather <- weather[1:wind.n,] %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate(Wind.Speed = Wind.Speed * 1.94384) %>%
          mutate(Gust = na_if(Gust, 99)) %>%
          mutate(Gust = Gust *1.94384) %>%
          mutate(Mod.Dir = case_when(Wind.Dir > 350 ~  0,
                                     TRUE ~ Wind.Dir)) %>%
          mutate(Gust.Mod.Dir = case_when(Gust.Dir > 350 ~  0,
                                          TRUE ~ Gust.Dir))
        
        
        # Call API for barometric pressure
        bar <- fread(paste0(wind.link, ".txt"), 
                     skip = 2,
                     na.strings = "MM",
                     encoding = "UTF-8",
                     colClasses = c(rep("character", 5), rep("numeric", 14)),
                     col.names = c("Year", "Month", "Day", "Hour", "Minute", rep(NA, 7), 
                                   "Pressure", rep(NA, 6)))
        
        # Subset past 24 hours of observations, clean and format data for baro
        bar <- bar[1:24,] %>%
          select(c("Year", "Month", "Day", "Hour", "Minute","Pressure")) %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800))
        
        # Pressure plot
        plot_rct$bar.plot2 <- ggplot() + 
          geom_line(data = bar, aes(x = Time, y = Pressure), linewidth = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(bar$Time), max(bar$Time)), expand = c(0, 0))
        
        
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
        
        
        # Avoid scale error if all wind data is missing
        if (all(is.na(weather$Wind.Speed)) == FALSE) {
          maxWind <- max(rbind(weather$Wind.Speed, weather$Gust), na.rm = TRUE)
        } else (maxWind <- 10)
        
        
        # Wind plot
        plot_rct$weather.plot2 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wind.Speed), linewidth = 1) +
          geom_point(data = weather, aes(x = Time, y = Gust), color = "#FF0000") +
          theme_bw() +
          labs(
            title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, maxWind, 5)) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Knots") +
          xlab("")
        
        # Last reading output
        output$time.label2 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Current weather label output
        output$weather.label2 <- renderText({
          paste0(wind.rose(first(na.omit(weather$Wind.Dir))), " ",
                 round(first(na.omit(weather$Wind.Speed)), 0), " knots ",
                 "(", first(na.omit(weather$Wind.Dir)), "°)")
          
        })
        
      } 
      
      
      else {
        
        # APRS stations
        
        # Call API
        url <- paste0("https://weather.gladstonefamily.net/cgi-bin/wxobservations.pl?site=",site,"&days=1")
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
          geom_line(data = aprs, aes(x = dt, y = pressure), linewidth = 1) +
          geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
          theme_bw() +
          labs(title = "**Barometric Pressure**") +
          theme(plot.title = element_markdown()) +
          ylab("Millibars") +
          xlab("") +
          scale_x_datetime(limits = c(min(aprs$dt), max(aprs$dt)), expand = c(0, 0))
        
        # Wind speed plot
        plot_rct$weather.plot2 <- ggplot() +
          geom_line(data = aprs, aes(x = dt, y = wind_speed), linewidth = 1) +
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
    
      }, 
      
      # Error displays
      error = function(e){
        output$weather.label2 <- renderText({
          paste("This station is not available at the moment")
          }) 
        output$time.label2 <- renderText({})
        plot_rct$weather.plot2 <- ggplot()
        plot_rct$bar.plot2 <- ggplot() 
        plot_rct$dir.plot2 <- ggplot()
        plot_rct$rose2 <- ggplot()
        
        })
      
      
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
  
  
  
  ########################
  ### Wave reports ####
  ########################
  
  wave.sites <- aprs.sites %>%
    filter(wave == "TRUE")
  
  output$map3 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 47.77920969878382, lng = -123.61182070598635 , zoom=7) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(wave.sites$lon, wave.sites$lat,
                       layerId = wave.sites$site,
                       popup = wave.sites$name, 
                       color = "#575757",
                       radius = 8,
                       weight = 3)
  })
  
  plot_rct <- reactiveValues()
  
  observeEvent(
    input$map3_marker_click, 
    { 
      click <- input$map3_marker_click
      site <- click$id
      wave.type <- wave.sites$wave.type[wave.sites$site == site]
      name <- wave.sites$name[wave.sites$site == site]
      plot_rct$site <- paste(name, site)
      wind.link <- wave.sites$wind.link[wave.sites$site == site]
      wave.link <- wave.sites$wave.link[wave.sites$site == site]
      wind.n <- wave.sites$wind.n[wave.sites$site == site]
      wave.n <- wave.sites$wave.n[wave.sites$site == site]
      clat <- click$lat
      clng <- click$lng
      
      leafletProxy('map3')
      
      tryCatch({
      
      if (wave.type == "OOI") {
        
        # OOI Stations
        # Call API 
        weather <- fread(wave.link, 
                         skip = 2,
                         na.strings = "MM",
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 14)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", NA, NA, NA, 
                                       "Wave.Height", "Dom.Period", NA, "Mean.Wave.Dir", 
                                       NA, NA, NA, NA, NA, NA, NA))
        
        
        # Subset past 24 hours of observations, clean and format data
        weather <- weather[1:wave.n,] %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate(Wave.Height = Wave.Height * 3.28084) %>%
          mutate(Mod.Mean.Dir = case_when(Mean.Wave.Dir > 350 ~  0,
                                          TRUE ~ Mean.Wave.Dir)) %>%
          filter(!is.na(Wave.Height))
        
        # Wave direction plot
        plot_rct$wave.dir.plot3 <- ggplot() + 
          geom_point(data = weather, aes(x = Time, y = wind.rose(Mean.Wave.Dir)), size = 1) +
          theme_bw() +
          labs(title = "**Mean Wave Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        
        # Wave rose
        plot_rct$rose3 <- ggplot(weather, aes(x = first(na.omit(Mod.Mean.Dir)))) +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(width = 7, color = "gray10", fill = "red") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        
        ymax <- max(rbind(weather$Wave.Height, weather$Dom.Period), na.rm = TRUE)
        
        # Wave height and period plot
        plot_rct$wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wave.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(Dom.Period)), aes(x = Time, y = Dom.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(Mean.Wave.Dir)),
                    aes(x = Time, y = Wave.Height, angle=-Mean.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wave Height, Direction (true)** and<br><span style='color:#FF0000;'>**Dominant Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds")) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("")
        
        
        # Wave last reading output
        output$time.label3 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Wave current weather label output
        output$weather.label3 <- renderUI({
          paste0(wind.rose(first(na.omit(weather$Mean.Wave.Dir))), " ",
                 round(first(na.omit(weather$Wave.Height)), 0), "' ",
                 "at ", first(na.omit(weather$Dom.Period)), " seconds")
          
        })
        
        # Site label output
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        
        # Current time label output
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        
        # Wave plot output
        output$wave.plot3 <- renderPlot({
          plot_rct$wave.plot3
        }) 
        
        # Wave direction plot output
        output$wave.dir.plot3 <- renderPlot({
          plot_rct$wave.dir.plot3
        })
        
        # Wave rose output
        output$rose3 <- renderPlot({
          plot_rct$rose3
        }) 
        
        # Swell plot blank
        output$swell.plot3 <- renderPlot({
        }) 
        
        # Wind wave plot blank
        output$wind.wave.plot3 <- renderPlot({
        }) 
        
        
        
        
      } else if (wave.type == "CA") {
        
        # Environment Canada stations
        # Call API 
        weather <- fread(wave.link, 
                         skip = 2,
                         na.strings = "MM",
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 14)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", NA, NA, NA, 
                                       "Wave.Height", "Dom.Period", NA, NA,NA, NA, NA, NA, 
                                       NA, NA, NA))
        
        # Subset past 24 hours of observations, clean and format data
        weather <- weather %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate(Wave.Height = Wave.Height * 3.28084)
        
        ymax <- max(rbind(weather$Wave.Height, weather$Dom.Period), na.rm = TRUE)
        
        # Wave height and period plot
        plot_rct$wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wave.Height), linewidth = 1) +
          geom_line(data = weather %>%
                      filter(!is.na(Dom.Period)), aes(x = Time, y = Dom.Period), color = "#FF0000") +
          theme_bw() +
          labs(
            title = "**Wave Height** and <span style='color:#FF0000;'>**Dominant Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, ymax, 5),
                             sec.axis = sec_axis(~., name = "Seconds")) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("")
        
        # Wave last reading output
        output$time.label3 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Wave current weather label output
        output$weather.label3 <- renderUI({
          paste0(round(first(na.omit(weather$Wave.Height)), 0), "' ",
                 "at ", first(na.omit(weather$Dom.Period)), " seconds")
          
        })
        
        # Site label output
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        
        # Current time label output
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        
        # Wave plot output
        output$wave.plot3 <- renderPlot({
          plot_rct$wave.plot3
        })
        
        # Wave rose blank
        output$rose3 <- renderPlot({
        }) 
        
        # Swell plot blank
        output$swell.plot3 <- renderPlot({
        }) 
        
        # Wind wave plot blank
        output$wind.wave.plot3 <- renderPlot({
        }) 
        
        # Wave direction plot output
        output$wave.dir.plot3 <- renderPlot({
        })
        
      } else if (wave.type == "NDBCIN") {
        
        #NDBC Inside stations
        # Call API 
        weather <- fread(wave.link, 
                         skip = 2,
                         na.strings = c("MM", "N/A", -99),
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 5), 
                                        rep("character", 2), "factor", rep("numeric", 2)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wave.Height", 
                                       "Swell.Height", "Swell.Period", "W.Wave.Height", 
                                       "W.Wave.Period","Swell.Dir", "W.Wave.Dir", "Steepness", 
                                       "Ave.Period", "Mean.Wave.Dir"))
        
        
        # Subset past 24 hours of observations, clean and format data
          weather <- weather[1:wave.n,] %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate_at(vars(Wave.Height, Swell.Height, W.Wave.Height),
                    ~ .* 3.28084) %>%
          mutate_at(vars(Swell.Dir, W.Wave.Dir), ~ wind.deg(.)) %>%
          mutate(Mod.Swell.Dir = case_when(Swell.Dir > 350 ~  0,
                                           TRUE ~ Swell.Dir)) %>%
          mutate(Mod.W.Wave.Dir = case_when(W.Wave.Dir > 350 ~  0,
                                            TRUE ~ W.Wave.Dir)) %>%
          mutate(Mod.Mean.Dir = case_when(Mean.Wave.Dir > 350 ~  0,
                                          TRUE ~ Mean.Wave.Dir)) 
        
        
        mean.ymax <- max(rbind(weather$Wave.Height, weather$Ave.Period), na.rm = TRUE)
        
        
        # Mean wave height, direction, and period plot
        plot_rct$wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wave.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(Ave.Period)), aes(x = Time, y = Ave.Period),
                    color = "#FF0000", linewidth = 0.5) +
          geom_text(data = weather %>% 
                      filter(!is.na(Mean.Wave.Dir)),
                    aes(x = Time, y = Wave.Height, angle=-Mean.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wave Height, Direction (true),** and<br><span style='color:#FF0000;'>**Average Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("")    
        
        # Wave direction plot
        plot_rct$wave.dir.plot3 <- ggplot() + 
          geom_point(data = weather, aes(x = Time, y = wind.rose(Mean.Wave.Dir)),
                     color = "#FF0000", size = 1) +
          geom_point(data = weather, aes(x = Time, y = wind.rose(Swell.Dir)), 
                     color = "#00eaca", size = 1, position = position_jitter(height = 0.2)) +
          geom_point(data = weather, aes(x = Time, y = wind.rose(W.Wave.Dir)), 
                     size = 1, position = position_jitter(height = 0.2)) +
          geom_jitter(height = 0.6) +
          theme_bw() +
          labs(title = "<span style='color:#FF0000;'>**Mean,**</span></span> <span style='color:#00eaca;'>**Swell,**</span></span> and<br> 
               **Wind Wave** Direction") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        

        
        
        # Swell height, direction, and period plot
        plot_rct$swell.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Swell.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(Swell.Period)), aes(x = Time, y = Swell.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(Swell.Dir)),
                    aes(x = Time, y = Swell.Height, angle=-Swell.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Swell Height, Direction (true),** and<br><span style='color:#FF0000;'>**Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("") 
        
        # Wind wave height, direction, and period plot
        plot_rct$wind.wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = W.Wave.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(W.Wave.Period)), aes(x = Time, y = W.Wave.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(W.Wave.Dir)),
                    aes(x = Time, y = W.Wave.Height, angle=-W.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wind Wave Height, Direction (true),** and<br><span style='color:#FF0000;'>**Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("") 
        
        # Wave last reading output
        output$time.label3 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        if (first(weather$Wave.Height) < 0.9) {
          
        
        # Wave current weather label output
        output$weather.label3 <- renderUI({
          line1 <- paste0("Wind waves < 1' ",
                          "at ", first(na.omit(weather$W.Wave.Period)), " seconds")
          HTML(line1)
          
        })
        
        # Wave rose
        plot_rct$rose3 <- ggplot() +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        }
        
        else {
          # Wave current weather label output
          output$weather.label3 <- renderUI({
            line1 <- paste0("<span style='color:#FF0000;'>Mean </span></span>", wind.rose(first(na.omit(weather$Mean.Wave.Dir))), " ",
                            round(first(na.omit(weather$Wave.Height)), 0), "' ",
                            "at ", first(na.omit(weather$Ave.Period)), " seconds")
            line2 <- paste0("<span style='color:#00eaca;'>Swell </span></span>", wind.rose(first(na.omit(weather$Swell.Dir))), " ",
                            round(first(na.omit(weather$Swell.Height)), 0), "' ",
                            "at ", first(na.omit(weather$Swell.Period)), " seconds")
            line3 <- paste0("Wind waves ", wind.rose(first(na.omit(weather$W.Wave.Dir))), " ",
                            round(first(na.omit(weather$W.Wave.Height)), 0), "' ",
                            "at ", first(na.omit(weather$W.Wave.Period)), " seconds")
            HTML(paste(line1, line2, line3, sep = "<br/>"))
            
          })
          
          # Wave rose
          plot_rct$rose3 <- ggplot() +
            coord_polar(theta = "x", start = -pi/45, direction = 1) +
            geom_bar(data = weather, aes(x = first(na.omit(Mod.Mean.Dir))),
                     width = 7, color = "gray10", fill = "red") +
            geom_bar(data = weather, aes(x = first(na.omit(Mod.Swell.Dir))),
                     width = 7, color = "gray10", fill = "#00eaca") +
            geom_bar(data = weather, aes(x = first(na.omit(Mod.W.Wave.Dir))),
                     width = 7, color = "gray10", fill = "black") +
            scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                               labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                          'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
            theme_minimal() +
            theme(
              axis.text.y = element_blank(),
              axis.title = element_blank())
          
        }
        
    
        
        # Site label output
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        
        # Current time label output
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        
        # Wave plot output
        output$wave.plot3 <- renderPlot({
          plot_rct$wave.plot3
        }) 
        
        # Swell plot output
        output$swell.plot3 <- renderPlot({
          plot_rct$swell.plot3
        }) 
        
        # Wind wave plot output
        output$wind.wave.plot3 <- renderPlot({
          plot_rct$wind.wave.plot3
        }) 
        
        # Wave direction plot output
        output$wave.dir.plot3 <- renderPlot({
          plot_rct$wave.dir.plot3
        })
        
        # Wave rose output
        output$rose3 <- renderPlot({
          plot_rct$rose3
        }) 
        
        
     
        
      } else if (wave.type == "SOFAR") {
        
        # UW SOFAR Buoy
        # Call API 
        weather.page <- read.csv(wave.link)
        
        # Strip and format weather data
        weather <- weather.page %>%
          tail(n= wave.n) %>%
          setNames(c("Time", "Wave.Height", "Mean.Period", "Mean.Wave.Dir")) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate(Wave.Height = Wave.Height * 3.28084) %>%
          mutate(Mod.Mean.Dir = case_when(Mean.Wave.Dir > 350 ~  0,
                                          TRUE ~ Mean.Wave.Dir))
        
        
        # Wave direction plot
        plot_rct$wave.dir.plot3 <- ggplot() + 
          geom_point(data = weather, aes(x = Time, y = wind.rose(Mean.Wave.Dir)), size = 1) +
          theme_bw() +
          labs(title = "**Mean Wave Direction**") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        
        # Wave rose
        plot_rct$rose3 <- ggplot(weather, aes(x = last(na.omit(Mod.Mean.Dir)))) +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(width = 7, color = "gray10", fill = "red") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        
        ymax <- max(rbind(weather$Wave.Height, weather$Mean.Period), na.rm = TRUE)
        
        # Wave height and period plot
        plot_rct$wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wave.Height), linewidth = 0.5) +
          geom_line(data = weather, aes(x = Time, y = Mean.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(Mean.Wave.Dir)),
                    aes(x = Time, y = Wave.Height, angle=-Mean.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wave Height, Direction (true)** and<br><span style='color:#FF0000;'>**Mean Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds")) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("")
        
        
        # Wave last reading output
        output$time.label3 <- renderText({
          paste("Last reading:", format(last(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Wave current weather label output
        output$weather.label3 <- renderUI({
          paste0(wind.rose(last(na.omit(weather$Mean.Wave.Dir))), " ",
                 round(last(na.omit(weather$Wave.Height)), 0), "' ",
                 "at ", last(na.omit(weather$Mean.Period)), " seconds")
          
        })
        
        # Site label output
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        
        # Current time label output
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        
        # Wave plot output
        output$wave.plot3 <- renderPlot({
          plot_rct$wave.plot3
        }) 
        
        # Wave direction plot output
        output$wave.dir.plot3 <- renderPlot({
          plot_rct$wave.dir.plot3
        })
        
        # Wave rose output
        output$rose3 <- renderPlot({
          plot_rct$rose3
        }) 
        
        # Swell plot blank
        output$swell.plot3 <- renderPlot({
        }) 
        
        # Wind wave plot blank
        output$wind.wave.plot3 <- renderPlot({
        }) 
        
        
      } else {
        
        #NDBC stations
        # Call API 
        dom.weather <- fread(wind.link, 
                             skip = 2,
                             na.strings = "MM",
                             encoding = "UTF-8",
                             colClasses = c(rep("character", 5), rep("numeric", 14)),
                             col.names = c("Year", "Month", "Day", "Hour", "Minute", NA, NA, NA, 
                                           "Wave.Height", "Dom.Period", "Ave.Period", "Mean.Wave.Dir", 
                                           NA, NA, NA, NA, NA, NA, NA))
        
        weather <- fread(wave.link, 
                         skip = 2,
                         na.strings = c("MM", "N/A", -99),
                         encoding = "UTF-8",
                         colClasses = c(rep("character", 5), rep("numeric", 5), 
                                        rep("character", 2), "factor", rep("numeric", 2)),
                         col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wave.Height", 
                                       "Swell.Height", "Swell.Period", "W.Wave.Height", 
                                       "W.Wave.Period","Swell.Dir", "W.Wave.Dir", "Steepness", 
                                       "Ave.Period", "Mean.Wave.Dir"))
        
        
        # Subset past 24 hours of observations, clean and format data
        dom.weather <- dom.weather[1:wind.n,] %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          select(Time, Dom.Period)
        
        weather <- weather[1:wave.n,] %>%
          unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
          unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
          unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
          mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
          mutate(Time = case_when(
            dst(Time[1]) == TRUE ~ Time - 25200,
            dst(Time[1]) == FALSE ~ Time - 28800)) %>%
          mutate_at(vars(Wave.Height, Swell.Height, W.Wave.Height),
                    ~ .* 3.28084) %>%
          mutate_at(vars(Swell.Dir, W.Wave.Dir), ~ wind.deg(.)) %>%
          mutate(Mod.Swell.Dir = case_when(Swell.Dir > 350 ~  0,
                                           TRUE ~ Swell.Dir)) %>%
          mutate(Mod.W.Wave.Dir = case_when(W.Wave.Dir > 350 ~  0,
                                            TRUE ~ W.Wave.Dir)) %>%
          mutate(Mod.Mean.Dir = case_when(Mean.Wave.Dir > 350 ~  0,
                                          TRUE ~ Mean.Wave.Dir)) 
        
        
        mean.ymax <- max(rbind(weather$Wave.Height, weather$Ave.Period), na.rm = TRUE)
        
        # Mean wave height, direction, and period plot
        plot_rct$wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Wave.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(Ave.Period)), aes(x = Time, y = Ave.Period),
                    color = "#FF0000", linewidth = 0.5) +
          geom_text(data = weather %>% 
                      filter(!is.na(Mean.Wave.Dir)),
                    aes(x = Time, y = Wave.Height, angle=-Mean.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wave Height, Direction (true),** and<br><span style='color:#FF0000;'>**Average Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("")    
        
        # Wave direction plot
        plot_rct$wave.dir.plot3 <- ggplot() + 
          geom_point(data = weather, aes(x = Time, y = wind.rose(Mean.Wave.Dir)),
                     color = "#FF0000", size = 1) +
          geom_point(data = weather, aes(x = Time, y = wind.rose(Swell.Dir)), 
                     color = "#00eaca", size = 1, position = position_jitter(height = 0.2)) +
          geom_point(data = weather, aes(x = Time, y = wind.rose(W.Wave.Dir)), 
                     size = 1, position = position_jitter(height = 0.2)) +
          geom_jitter(height = 0.6) +
          theme_bw() +
          labs(title = "<span style='color:#FF0000;'>**Mean,**</span></span> <span style='color:#00eaca;'>**Swell,**</span></span> and<br> 
               **Wind Wave** Direction") +
          theme(plot.title = element_markdown()) +
          ylab("") +
          xlab("") +
          scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))
        
        # Wave rose
        plot_rct$rose3 <- ggplot() +
          coord_polar(theta = "x", start = -pi/45, direction = 1) +
          geom_bar(data = weather, aes(x = first(na.omit(Mod.Mean.Dir))),
                   width = 7, color = "gray10", fill = "red") +
          geom_bar(data = weather, aes(x = first(na.omit(Mod.Swell.Dir))),
                   width = 7, color = "gray10", fill = "#00eaca") +
          geom_bar(data = weather, aes(x = first(na.omit(Mod.W.Wave.Dir))),
                   width = 7, color = "gray10", fill = "black") +
          scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                             labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                        'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title = element_blank())
        
        
        # Swell height, direction, and period plot
        plot_rct$swell.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = Swell.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(Swell.Period)), aes(x = Time, y = Swell.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(Swell.Dir)),
                    aes(x = Time, y = Swell.Height, angle=-Swell.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Swell Height, Direction (true),** and<br><span style='color:#FF0000;'>**Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("") 
        
        # Wind wave height, direction, and period plot
        plot_rct$wind.wave.plot3 <- ggplot() + 
          geom_line(data = weather, aes(x = Time, y = W.Wave.Height), linewidth = 0.5) +
          geom_line(data = weather %>%
                      filter(!is.na(W.Wave.Period)), aes(x = Time, y = W.Wave.Period), 
                    linewidth = 0.5, color = "#FF0000") +
          geom_text(data = weather %>% 
                      filter(!is.na(W.Wave.Dir)),
                    aes(x = Time, y = W.Wave.Height, angle=-W.Wave.Dir+270), label="→", size = 8) +
          theme_bw() +
          labs(
            title = "**Wind Wave Height, Direction (true),** and<br><span style='color:#FF0000;'>**Period**</span></span>") +
          theme(plot.title = element_markdown()) +
          scale_y_continuous(breaks = seq(0, mean.ymax, 2),
                             sec.axis = sec_axis(~., name = "Seconds",
                                                 breaks = seq(0, mean.ymax, 2))) +
          scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
          ylab("Feet") +
          xlab("") 
        
        # Wave last reading output
        output$time.label3 <- renderText({
          paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
        }) 
        
        # Wave current weather label output
        output$weather.label3 <- renderUI({
          line1 <- paste0("<span style='color:#FF0000;'>Mean </span></span>", wind.rose(first(na.omit(weather$Mean.Wave.Dir))), " ",
                          round(first(na.omit(weather$Wave.Height)), 0), "' ",
                          "at ", first(na.omit(weather$Ave.Period)), " seconds")
          line2 <- paste0("<span style='color:#00eaca;'>Swell </span></span>", wind.rose(first(na.omit(weather$Swell.Dir))), " ",
                          round(first(na.omit(weather$Swell.Height)), 0), "' ",
                          "at ", first(na.omit(weather$Swell.Period)), " seconds")
          line3 <- paste0("Wind waves ", wind.rose(first(na.omit(weather$W.Wave.Dir))), " ",
                          round(first(na.omit(weather$W.Wave.Height)), 0), "' ",
                          "at ", first(na.omit(weather$W.Wave.Period)), " seconds")
          line4 <- paste("Dominant period", first(na.omit(dom.weather$Dom.Period)), "seconds")
          HTML(paste(line1, line2, line3, line4, sep = "<br/>"))
          
        })
        
        # Site label output
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        
        # Current time label output
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        
        # Wave plot output
        output$wave.plot3 <- renderPlot({
          plot_rct$wave.plot3
        }) 
        
        # Swell plot output
        output$swell.plot3 <- renderPlot({
          plot_rct$swell.plot3
        }) 
        
        # Wind wave plot output
        output$wind.wave.plot3 <- renderPlot({
          plot_rct$wind.wave.plot3
        }) 
        
        # Wave direction plot output
        output$wave.dir.plot3 <- renderPlot({
          plot_rct$wave.dir.plot3
        })
        
        # Wave rose output
        output$rose3 <- renderPlot({
          plot_rct$rose3
        }) 
        
        
        
        
      }
      }, 
      
      # Error handling
      error = function(e){
        
        output$site.label3 <- renderText({
          req(input$map3_marker_click)
          paste0("Station: ", plot_rct$site)
        })
        output$weather.label3 <- renderText({
          paste("This station is not available at the moment")
        }) 
        output$time.current3 <- renderText({
          paste("",format(Sys.time(), "%a %m-%d %H:%M"))
        })
        output$time.label3 <- renderText({})
        plot_rct$wave.plot3 <- ggplot()
        plot_rct$wind.wave.plot3 <- ggplot()
        plot_rct$swell.plot3 <- ggplot() 
        plot_rct$wave.dir.plot3 <- ggplot()
        plot_rct$rose3 <- ggplot()
        
        })
      
    })
  
  
  
  ########################
  ### Ship reports ####
  ########################
  
  # Call API 
  ship.link <- "https://www.ndbc.noaa.gov/data/realtime2/ship_obs.txt"
  ship.weather <- fread(ship.link, 
                        skip = 2,
                        na.strings = "MM",
                        encoding = "UTF-8",
                        colClasses = c(rep("character", 5), rep("numeric", 2),
                                       "character", rep("numeric", 15), "character",
                                       rep("numeric", 11)),
                        col.names = c("Station", "Year", "Month", "Day", "Hour", "Lat", "Lon",
                                      "Wind.Dir", "Wind.Speed", "Gust", "Wave.Height", 
                                      "DPD", "APD", "MWD", "Pressure", "Air.Temp", "Water.Temp",
                                      "Dew.Point", "Vis", "Press.Tend", "TCC", "S1HT", "S1PD",
                                      "S1DIR", "S2HT", "S2PD", "S2DIR", rep(NA, 8)))
  
  
  # Subset local observations, clean and format data
  ship.weather <- ship.weather %>%
    select("Station", "Year", "Month", "Day", "Hour", "Lat", "Lon",
           "Wind.Dir", "Wind.Speed", "Gust", "Wave.Height", "Pressure") %>%
    filter(Station == "SHIP") %>%
    filter(Lat > 42 & Lat < 49 & Lon < -124 & Lon > -130) %>%
    unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
    unite(Time, Date, Hour, sep = " ", remove = TRUE) %>%
    mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H")) %>%
    mutate(Time = case_when(
      dst(Time[1]) == TRUE ~ Time - 25200,
      dst(Time[1]) == FALSE ~ Time - 28800)) %>%
    filter(first(Time) - Time < 21600) %>%
    mutate(Wave.Height = Wave.Height * 3.28084) %>%
    mutate(Wind.Speed = Wind.Speed * 1.94384) %>%
    mutate(Gust = Gust *1.94384)
  
  
  # Make wind arrow icon
  getWindColor <- function(ship.weather) {
    sapply(ship.weather$Wind.Speed , function(Wind.Speed) {
      if (is.na(Wind.Speed)) {
        "black"
      } else if (Wind.Speed <= 10) {
        "white"
      } else if (Wind.Speed <= 15) {
        "lightblue"
      } else if (Wind.Speed <= 20) {
        "blue"
      } else if (Wind.Speed <= 25) {
        "darkblue"
      } else if (Wind.Speed <= 30) {
        "purple"
      } else if (Wind.Speed <= 35) {
        "darkpurple"
      } else if (Wind.Speed <= 45) {
        "red"
      } else if (Wind.Speed <= 60) {
        "darkred"
      } else {"yellow"}
    })
  }
  
  windIcon <- makeAwesomeIcon(icon = "arrow-down",
                              iconRotate = ship.weather$Wind.Dir,
                              squareMarker = TRUE,
                              iconColor = "black",
                              markerColor = getWindColor(ship.weather))
  
  # Make wave height icon 
  wave.weather <- ship.weather %>%
    filter(!is.na(ship.weather$Wave.Height))
  
  getWaveColor <- function(wave.weather) {
    sapply(wave.weather$Wave.Height , function(Wave.Height) {
      if (Wave.Height <= 2) {
        "white"
      } else if (Wave.Height <= 4) {
        "lightblue"
      } else if (Wave.Height <= 6) {
        "blue"
      } else if (Wave.Height <= 8) {
        "darkblue"
      } else if (Wave.Height <= 10) {
        "purple"
      } else if (Wave.Height <= 12) {
        "darkpurple"
      } else if (Wave.Height <= 14) {
        "red"
      } else if (Wave.Height <= 16) {
        "darkred"
      } else {"yellow"}
    })
  }
  
  waveIcon <- makeAwesomeIcon(text = paste0(wave.weather$Wave.Height %>% round(0), "'"),
                              squareMarker = TRUE,
                              iconColor = "black",
                              markerColor = getWaveColor(wave.weather),
                              fontFamily = "helvetica")
  
  # Render ship observation map 
  output$map4 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
      addTiles() %>%
      setView(lat = 47.77920969878382, lng = -123.61182070598635 , zoom=7) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addAwesomeMarkers(ship.weather$Lon, ship.weather$Lat,
                        icon = windIcon,
                        popup = paste(ship.weather$Wind.Speed %>% round(0),
                                      "kt",
                                      wind.rose(ship.weather$Wind.Dir),
                                      ship.weather$Time %>% format("%H:%M"))) %>%
      addAwesomeMarkers(wave.weather$Lon + 0.2,
                        wave.weather$Lat - 0.2,
                        icon = waveIcon,
                        popup = paste0(wave.weather$Wave.Height %>% round(0),"' ",
                                       ship.weather$Time %>% format("%H:%M")))
    
  })
  
  # Ship recency output
  output$ship.recency4 <- renderText({
    paste("", tail(ship.weather$Time, 1) %>% format("%a %m-%d %H:%M"),
          "to", first(ship.weather$Time) %>% format("%H:%M"))
  })
  
  ############################
  ### NWS Marine Forecast ####
  ############################
  
  # Synopsis output
  output$zone.data <- renderUI({
    HTML(marcast(input$zone))
  })
  
  
}

# Execute app
shinyApp(ui, server)




