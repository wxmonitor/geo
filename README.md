**WX Monitor web app with leaflet map integrated**
**Version 2.0 incorporating APRS amateur radio wx stations**

  This shiny app displays a leaflet map (initialized on NE Olympic Peninsula) which allows users to pick a location.
  Location is used to pull open source weather data from OpenWeather API.
  Current time, wind speed and direction are displayed. 
  Ggplot objects are generated and displayed for 48 hour forecast data for the following:
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Rainfall chance (%) and accumulation totals (mm)
    Air temperature and dew point (°F)
    Barometric pressure (mb)
   
  Second tab displays leaflet map with Washington amateur radio (APRS) wx stations marked.
  Station selection loads ggplot objects for current and n-day previous conditions (1 < n < 14).
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Barometric pressure (mb)
  
  Web app is deployed on shiny server running on remote ubuntu 18.04 server. 
  Accesible at https://monitor.wxnw.net/geo

