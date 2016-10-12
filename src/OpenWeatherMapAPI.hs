module OpenWeatherMapAPI where

--data API_KEY

getAPIEndpoint :: String -> String -> String
getAPIEndpoint method city =
  "http://api.openweathermap.org/data/2.5" ++ method ++ "?q=" ++ city

