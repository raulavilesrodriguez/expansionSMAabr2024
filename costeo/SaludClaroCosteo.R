library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents

source(here::here('costeo/costosUnidad.R'))

#---Data----
db.fibra.claro <- read_excel('nodos_fibra/nodos_fibra_conecel.xlsx')
db.salud.claro <- read_excel('coberturaSalud/saludCONECELCobertura.xlsx')

db.fibra.claro <- db.fibra.claro |> group_by(cantón) |>
  distinct()

class(db.fibra.claro)

# all strings in UPPER CASE
db.fibra.claro <- db.fibra.claro |> 
  mutate(
    cantón = toupper(cantón)
  )

db.salud.claro <- db.salud.claro |>
  mutate(
    CAN_DESC = toupper(CAN_DESC)
  )


# all strings without accents
db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

db.salud.claro <- db.salud.claro |>
  mutate(
    CAN_DESC = stri_trans_general(CAN_DESC, "Latin-ASCII")
  )


# all strings without extra spaces
db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = str_squish(cantón)
  )


db.salud.claro <- db.salud.claro |>
  mutate(
    CAN_DESC = str_squish(CAN_DESC)
  )

# looking for schools where are part of an cantón where there are fiber
join_places_fiber <- function(x, y){
  ifelse(x[['CAN_DESC']] == y[['cantón']],1,0)
}

matrix.fibra <- apply(db.salud.claro, 1, join_places_fiber, y = db.fibra.claro)

col.fibra.s <- apply(matrix.fibra, 2, sum)

db.salud.claro$acceso_fibra <- col.fibra.s

# order base schools Claro in descendent order
db.salud.claro <- db.salud.claro[order(-db.salud.claro$resultado),]

col.fases.s <- c(
  rep("Fase 1", times = 8)
)

db.salud.claro$fases <- col.fases.s


db.salud.claro <- db.salud.claro |> 
  mutate(costeo = ifelse(acceso_fibra == 0 & resultado > 100, 2*micro.min.15,
                         ifelse(acceso_fibra == 0 & resultado < 100, micro.min.15,
                                ifelse(acceso_fibra == 1, fibeq.min.15 + resultado*fibkm.min.15, 0))
  ))

sum(db.salud.claro$costeo)

#Export tibble with companies classified
writexl::write_xlsx(db.salud.claro, 'costeo/saludConecelCosteado.xlsx')

