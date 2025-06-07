**WX Monitor web app with leaflet map integrated**
**Version 4 incorporating NBDC coastal buoys, and local ship reports**

  This shiny app displays a leaflet map (initialized on Olympic Peninsula) with the locations of NOAA, USCG, DFO, and amateur wx stations marked.
  Station selection loads ggplot objects for current and 24h previous conditions.
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Barometric pressure (mb)
  
  
  Second tab displays locations of active wave buoys. Buoy selection displays ggplot objects for 24h previous conditions for:
  
    Wave Height (ft)
    Mean Wave Direction (compass points)
    Mean Wave Period (s)
    Swell Height (ft)
    Mean Swell Direction (compass points)
    Mean Swell Period (s)
    Wind Wave Height (ft)
    Mean Wind Wave Direction (compass points)
    Mean Wind Wave Period (s)
    
    
  Third tab displays past 6h ship reports in WA coastal region. Wind reports show compass point direction and speed mapped to color. Selection of a wind report shows speed, direction, and time of report. Wave height reports are given with height in feet, with height mapped to color. Selection shows height and time of report.
  
  
  Fourth tab allows location selection which is used to pull open source weather data from OpenWeather API.
  Current time, wind speed and direction are displayed. 
  Ggplot objects are generated and displayed for 48 hour forecast data for the following:
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Rainfall chance (%) and accumulation totals (mm)
    Air temperature and dew point (°F)
    Barometric pressure (mb)
   
  
  Fifth tab pulls National Weather Service marine area weather forecasts in text format.
  
  Web app is deployed on shiny server running on remote ubuntu 18.04 server. 
  Accesible at https://monitor.wxnw.net

