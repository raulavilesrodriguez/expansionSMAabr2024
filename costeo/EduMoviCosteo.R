library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents

source(here::here('costeo/costosUnidad.R'))

#---Data----
db.fibra.movi <- read_excel('nodos_fibra/nodos_fibra_otecel.xlsx')
db.edu.movi <- read_excel('coberturaEducación/educaciónOTECELCobertura2.xlsx')

db.fibra.movi <- db.fibra.movi |> group_by(cantón) |>
  distinct()

class(db.fibra.movi)

# all strings in UPPER CASE
db.fibra.movi <- db.fibra.movi |> 
  mutate(
    cantón = toupper(cantón)
  )

db.edu.movi <- db.edu.movi |>
  mutate(
    DPA_DESCAN = toupper(DPA_DESCAN)
  )


# all strings without accents
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

db.edu.movi <- db.edu.movi |>
  mutate(
    DPA_DESCAN = stri_trans_general(DPA_DESCAN, "Latin-ASCII")
  )


# all strings without extra spaces
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = str_squish(cantón)
  )


db.edu.movi <- db.edu.movi |>
  mutate(
    DPA_DESCAN = str_squish(DPA_DESCAN)
  )

# looking for schools where are part of an cantón where there are fiber
join_places_fiber <- function(x, y){
  ifelse(x[['DPA_DESCAN']] == y[['cantón']],1,0)
}

matrix.fibra <- apply(db.edu.movi, 1, join_places_fiber, y = db.fibra.movi)

col.fibra <- apply(matrix.fibra, 2, sum)

db.edu.movi$acceso_fibra <- col.fibra

# order base schools Claro in descendent order
db.edu.movi <- db.edu.movi[order(-db.edu.movi$distancia_min_km),]

col.fases <- c(
  rep("Fase 1", times = 676), 
  rep("Fase 2", times = 675),
  rep("Fase 3", times = 675),
  rep("Fase 4", times = 675)
)

db.edu.movi$fases <- col.fases
db.edu.movi$datos_moviles <- as.logical(db.edu.movi$datos_moviles)

db.edu.movi <- db.edu.movi |> 
  mutate(costeo_15 = ifelse(datos_moviles, 0, 
                            ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                   fibkm.min.15*distancia_min_km, plan40gb.15)))

db.edu.movi <- db.edu.movi |> 
  mutate(costeo_20 = ifelse(datos_moviles, 0, 
                            ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                   fibkm.min.20*distancia_min_km, plan40gb.20)))

sum(db.edu.movi$costeo_15)
sum(db.edu.movi$costeo_20)


#Export tibble with companies classified
writexl::write_xlsx(db.edu.movi, 'costeo/educaciónOTECELCosteado.xlsx')

