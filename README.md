**WX Monitor web app with leaflet map integrated**
**Version 3.0 incorporating APRS amateur radio wx stations, NBDC coastal buoys, and local ship reports**

  This shiny app displays a leaflet map (initialized on Olympic Peninsula) with the locations of NOAA, USCG, and Washington amateur radio (APRS) wx stations marked.
  Station selection loads ggplot objects for current and 24h previous conditions. APRS stations allow slider selection of n-day previous conditions (1 < n < 14).
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Barometric pressure (mb)
  
  
  Second tab displays locations of active NBDC buoys on WA coast. Buoy selection displays ggplot objects for 24h previous conditions for:
  
    Wave Height (ft)
    Mean Wave Direction (compass points)
    
  Third tab displays past 6h ship reports in WA coastal region. Wind reports show compass point direction and speed mapped to color. Selection of a wind report shows speed, direction, and time of report. Wave height reports are given with height in feet, with height mapped to color. Selection shows height and time of report.
  
  
  Fourth tab allows location selection which is used to pull open source weather data from OpenWeather API.
  Current time, wind speed and direction are displayed. 
  Ggplot objects are generated and displayed for 48 hour forecast data for the following:
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Rainfall chance (%) and accumulation totals (mm)
    Air temperature and dew point (°F)
    Barometric pressure (mb)
   
  
  Web app is deployed on shiny server running on remote ubuntu 18.04 server. 
  Accesible at https://monitor.wxnw.net

