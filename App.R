library(tidyverse)
library(jsonlite)
library(shiny)
library(scales)
library(ggtext)

# Coon Bay + D West Fog Forecast

wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

Sys.setenv(TZ="America/Los_Angeles")

### Coon Bay Chunk ###


url <- "https://api.openweathermap.org/data/2.5/onecall?lat=48.8877&lon=-122.59758&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
weather.page <- fromJSON(url, flatten = TRUE)
hourly.forecast <- data.frame(weather.page$hourly)
hourly.forecast$dt <- as.POSIXct(hourly.forecast$dt, origin="1970-01-01")
current <- data.frame(weather.page$current)
current$dt <- as.POSIXct(current$dt, origin="1970-01-01")
current$sunrise <- as.POSIXct(current$sunrise, origin="1970-01-01")
current$sunset <- as.POSIXct(current$sunset, origin="1970-01-01")

hourly.forecast <- hourly.forecast %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_gust = wind_gust * 0.868976)

current <- current %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_deg = as.integer(wind_deg)) %>%
  mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                             wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                             TRUE ~ wind_deg))

shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(hourly.forecast$dt, 1)), tail(hourly.forecast$dt, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(hourly.forecast$dt, 1)), head(hourly.forecast$dt, 1)))

rose <- ggplot(current, aes(x = mod_deg)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())


dir.plot <- ggplot() +
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


bar.plot <- ggplot() + 
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


fog.plot <- ggplot() + 
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


weather.plot <- ggplot() +
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

if ("rain.1h" %in% colnames(hourly.forecast)) {
  
  rain.plot <- ggplot() +
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
  rain.plot <- ggplot() +
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

### D West Chunk ###

d.url <- "https://api.openweathermap.org/data/2.5/onecall?lat=48.1345&lon=-123.28568&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
d.weather.page <- fromJSON(d.url, flatten = TRUE)
d.hourly.forecast <- data.frame(d.weather.page$hourly)
d.hourly.forecast$dt <- as.POSIXct(d.hourly.forecast$dt, origin="1970-01-01")
d.current <- data.frame(d.weather.page$current)
d.current$dt <- as.POSIXct(d.current$dt, origin="1970-01-01")
d.current$sunrise <- as.POSIXct(d.current$sunrise, origin="1970-01-01")
d.current$sunset <- as.POSIXct(d.current$sunset, origin="1970-01-01")

d.hourly.forecast <- d.hourly.forecast %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_gust = wind_gust * 0.868976)

d.current <- d.current %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_deg = as.integer(wind_deg)) %>%
  mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                             wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                             TRUE ~ wind_deg))

d.shade <- data.frame(dusk = seq.POSIXt(d.current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(d.current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

d.shade <- d.shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(d.hourly.forecast$dt, 1)), tail(d.hourly.forecast$dt, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(d.hourly.forecast$dt, 1)), head(d.hourly.forecast$dt, 1)))

d.rose <- ggplot(d.current, aes(x = mod_deg)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())


d.dir.plot <- ggplot() +
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_point(data = d.hourly.forecast, aes(x = dt, y = wind.rose(wind_deg)), size = 1) +
  theme_bw() +
  labs(title = "**Wind Direction**") +
  theme(plot.title = element_markdown()) +
  ylab("") +
  xlab("") +
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))


d.bar.plot <- ggplot() + 
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = d.hourly.forecast, aes(x = dt, y = pressure), size = 1) +
  geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
  theme_bw() +
  labs(title = "**Barometric Pressure**") +
  theme(plot.title = element_markdown()) +
  ylab("Millibars") +
  xlab("") +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))


d.fog.plot <- ggplot() + 
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = d.hourly.forecast, aes(x = dt, y = temp), size = 1) +
  geom_line(data = d.hourly.forecast, aes(x = dt, y = dew_point), color = "gray", size = 1) +
  theme_bw() +
  labs(title = "**Temperature and Dew Point**") +
  theme(plot.title = element_markdown()) +
  ylab("°F") +
  xlab("") +
  scale_x_datetime(limits = c(min(d.hourly.forecast$dt), max(d.hourly.forecast$dt)), expand = c(0, 0))


d.weather.plot <- ggplot() +
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = d.hourly.forecast, aes(x = dt, y = wind_speed), size = 1) +
  geom_line(data = d.hourly.forecast, aes(x = dt, y = wind_gust), color = "#FF0000") +
  theme_bw() +
  labs(
    title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
  theme(plot.title = element_markdown()) +
  ylab("Knots") +
  xlab("") + 
  scale_x_datetime(limits = c(min(d.hourly.forecast$dt), max(d.hourly.forecast$dt)), expand = c(0, 0))

if ("rain.1h" %in% colnames(d.hourly.forecast)) {
  
  d.rain.plot <- ggplot() +
    geom_rect(data = d.shade, 
              aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
              fill = 'light grey', alpha = 0.5) +
    geom_line(data = d.hourly.forecast, aes(x = dt, y = pop), size = 1) +
    geom_col(data = d.hourly.forecast, aes(x = dt, y = rain.1h/5), color = "darkgrey", fill = "#28d0eb") +
    geom_text(data = d.hourly.forecast, aes(x = dt, y = rain.1h/5, label = rain.1h), size = 2, vjust = -0.5) +
    theme_bw() +
    labs(
      title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
    theme(plot.title = element_markdown()) +
    ylab("Percent") + 
    xlab("") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0,1)) +
    scale_x_datetime(limits = c(min(d.hourly.forecast$dt), max(d.hourly.forecast$dt)), expand = c(0, 0))
  
} else (
  d.rain.plot <- ggplot() +
    geom_rect(data = d.shade, 
              aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
              fill = 'light grey', alpha = 0.5) +
    geom_line(data = d.hourly.forecast, aes(x = dt, y = pop), size = 1) +
    theme_bw() +
    labs(
      title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
    theme(plot.title = element_markdown()) +
    ylab("Percent") + 
    xlab("") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0,1)) +
    scale_x_datetime(limits = c(min(d.hourly.forecast$dt), max(d.hourly.forecast$dt)), expand = c(0, 0))
)

# Construct UI
ui <- tabsetPanel(
  tabPanel("D West",
           fluidPage(
             h5("WX Monitor", align = "center"),
             h3("D West 48 hour forecast", align = "center"),
             h4(textOutput("d.time.current"), align = "center"),
             h4(textOutput("d.weather.label"), align = "center"),
             fluidRow(column(12, align = "center",
                             plotOutput(outputId = "d.rose", width = "50%", height = "200px"))),
             plotOutput(outputId = "d.weather.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.dir.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.fog.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.rain.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.bar.plot", width = "100%", height = "400px")
           )
  ),
  tabPanel("Coon Bay",
           fluidPage(
             h5("WX Monitor", align = "center"),
             h3("Coon Bay 48 hour forecast", align = "center"),
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
  )
)

#Run r code on server
server <- function(input, output) {
  
  output$d.time.current <- renderText({
    paste("Current time:", Sys.time())
  })
  
  output$time.current <- renderText({
    paste("Current time:", Sys.time())
  })
  
  output$d.weather.label <- renderText({
    paste0(wind.rose(d.current$wind_deg), " ",
           round(d.current$wind_speed, 0), " knots ",
           "(", d.current$wind_deg, "°)")
    
  }) 
  
  output$weather.label <- renderText({
    paste0(wind.rose(current$wind_deg), " ",
           round(current$wind_speed, 0), " knots ",
           "(", current$wind_deg, "°)")
    
  })
  
  output$d.rose <- renderPlot({
    d.rose
  }) 
  
  output$rose <- renderPlot({
    rose
  }) 
  
  output$d.weather.plot <- renderPlot({
    d.weather.plot
  }) 
  
  output$weather.plot <- renderPlot({
    weather.plot
  })

  
  output$d.dir.plot <- renderPlot({
    d.dir.plot
  })
  
  output$dir.plot <- renderPlot({
    dir.plot
  })
  
  
  output$d.rain.plot <- renderPlot({
    d.rain.plot
  })
  
  output$rain.plot <- renderPlot({
    rain.plot
  })
  
  output$d.fog.plot <- renderPlot({
    d.fog.plot
  })
  
  output$fog.plot <- renderPlot({
    fog.plot
  })
  
  output$d.bar.plot <- renderPlot({
    d.bar.plot
  }) 
  
  output$bar.plot <- renderPlot({
    bar.plot
  }) 
  
  
}


# Execute app
shinyApp(ui, server)


