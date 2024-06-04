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

#------------WRANGLING----------------------
# filter schools without signal or bad signal 4G
db.schools.filt <- db.schools |> 
  filter(conecel_lte == '<Nulo>' | conecel_lte == '-140')

db.schools.filt <- db.schools.filt |>
  filter(otecel_lte == '<Nulo>' | otecel_lte == '-140')

db.schools.filt <- db.schools.filt |>
  filter(cnt_lte == 'Sin cobertura 4G' | cnt_lte == '-140 ≤Cobertura con niveles < -120')

# function to assign considering the probability
set.seed(2024)

addRandomBool <- function(df, p){
  
  n <- ceiling(nrow(df) * p)
  df$Operadora <- sample(rep(c("CONECEL","OTECEL"), times = c(n, nrow(df) - n)))
  
  df
}

db.schools.ope <- Reduce(rbind, 
                         lapply(
                           split(db.schools.filt, db.schools.filt$DPA_DESCAN),
                           addRandomBool,
                           p= 0.5))

db.schools.conecel <- db.schools.ope |> filter(Operadora == 'CONECEL')
db.schools.otecel <- db.schools.ope |> filter(Operadora == 'OTECEL')
nrow(db.schools.conecel)
nrow(db.schools.otecel)


# crossing bases
we <- apply(rbs.conecel, 1, function(x){
  
  #new around
  my_hashmap <- new.env()
  
  lon1 <- as.numeric(x[['LONGITUD']])
  lat1 <- as.numeric(x[['LATITUD']])
  
  # Define lon2 and lat2 inside the function to create vectors
  lon2 <- as.numeric(db.schools[['LONGITUD']][1])
  lat2 <- as.numeric(db.schools[['LATITUD']][1])
  
  # Calculate distance
  t <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  t <- t / 1000
  my_hashmap$distancia <- t
  my_hashmap$lon <- x[['LONGITUD']]
  my_hashmap$lat <- x[['LATITUD']]
  my_hashmap
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




