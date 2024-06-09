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
db.edu.movi <- read_excel('coberturaEducación/educaciónOTECELCobertura.xlsx')

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
db.edu.movi <- db.edu.movi[order(-db.edu.movi$resultado),]

col.fases <- c(
  rep("Fase 1", times = 676), 
  rep("Fase 2", times = 675),
  rep("Fase 3", times = 675),
  rep("Fase 4", times = 675)
)

db.edu.movi$fases <- col.fases


db.edu.movi <- db.edu.movi |> 
  mutate(costeo = ifelse(acceso_fibra == 0 & resultado > 100, 2*micro.min.15,
                         ifelse(acceso_fibra == 0 & resultado < 100, micro.min.15,
                                ifelse(acceso_fibra == 1, fibeq.min.15 + resultado*fibkm.min.15, 0))
  ))

sum(db.edu.movi$costeo)

#Export tibble with companies classified
writexl::write_xlsx(db.edu.movi, 'costeo/educaciónOTECELCosteado.xlsx')

