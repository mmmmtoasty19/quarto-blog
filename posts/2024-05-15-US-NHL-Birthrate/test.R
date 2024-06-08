
library(httr)
library(jsonlite)


username  <- "Belanger"
password  <- "IHq186D77Pbg"

url = "https://opensky-network.org/api/flights/departure"

res = GET(
"https://opensky-network.org/api/flights/departure", 
authenticate(user = username, password = password),
query = list(airport = "KRDU", begin = 1717200000, end = 1717545600))


data = fromJSON(rawToChar(res$content))
