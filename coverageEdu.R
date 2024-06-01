library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Wrangling_________
df.poblacion <- read_excel('poblacion.xlsx')
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')
db.schools <- read_excel('establecimientos/educaciónLatLong.xlsx')

we <- apply(rbs.conecel, 1, function(x){
  lon1 <- as.numeric(x[['LONGITUD']])
  lat1 <- as.numeric(x[['LATITUD']])
  
  # Define lon2 and lat2 inside the function to create vectors
  lon2 <- as.numeric(db.schools[['LONGITUD']][1])
  lat2 <- as.numeric(db.schools[['LATITUD']][1])
  
  # Calculate distance
  t <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  t <- t / 1000
  t
})

we.min <- min(we)
we.min

rbs.conecel.4g <- rbs.conecel |> filter(TECNOLOGIA=='LTE')

calculo.cobertura <- function(db1, lon2, lat2){
  wi <- apply(db1, 1, function(x){
    lon1 <- as.numeric(x[['LONGITUD']])
    lat1 <- as.numeric(x[['LATITUD']])
    
    # Define lon2 and lat2 inside the function to create vectors
    lon2 <- as.numeric(lon2)
    lat2 <- as.numeric(lat2)
    
    # Calculate distance
    t <- distHaversine(c(lon1, lat1), c(lon2, lat2))
    t <- t / 1000
    t
  })
  wi
}

db.schools <- db.schools |>
  rowwise() |>
  mutate(resultado = min(calculo.cobertura(rbs.conecel.4g[1:2,], LONGITUD, LATITUD))
  )




