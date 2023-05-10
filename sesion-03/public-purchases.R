library(tidyverse)
library(data.table)

# https://datos-abiertos.chilecompra.cl/descargas/ordenes-y-licitaciones

# Obtener los datos ----

# Opción 1 (descargar por archivos individuales)

url <- "https://transparenciachc.blob.core.windows.net/oc-da/2023-1.zip"
destfile <- "sesion-03/raw-data/2023-1.zip"

download.file(url, destfile)

unzip("sesion-03/raw-data/2023-1.zip", list = TRUE)
unzip("sesion-03/raw-data/2023-1.zip", exdir = "sesion-03/raw-data")

ene_2023 <- fread("sesion-03/raw-data/2023-1.csv", encoding = "Latin-1")


# Opción 2 (descargar múltiples archivos)

meses <- seq(1, 5, 1)

url_list <- sprintf("https://transparenciachc.blob.core.windows.net/oc-da/2023-%s.zip", meses)

for (i in seq_along(url_list)) {
  download.file(url_list[i], paste0("sesion-03/raw-data/2023-", i, ".zip"))
  unzip(paste0("sesion-03/raw-data/2023-", i, ".zip"), exdir = "sesion-03/raw-data")
}

lista_archivos <- list.files("sesion-03/raw-data", pattern = "*.csv", full.names = TRUE) |>
  purrr::map(~ fread(., encoding = "Latin-1"))

# Arrojan un error de tipo de datos:
# rbindlist(ordenes_compra)
# bind_rows(lista_archivos)
# map_df()

ordenes_compra <- plyr::ldply(lista_archivos, data.frame)


# Borramos los datos
unlink("sesion-03/raw-data/*")


# Veamos un poco el dataset ----

glimpse(ordenes_compra)

# Es necesario cambiar la marca de decimal
lista_archivos[[1]] |>
  mutate(
    MontoTotalOC_PesosChilenos = str_replace(MontoTotalOC_PesosChilenos, ",", "."),
    MontoTotalOC_PesosChilenos = as.numeric(MontoTotalOC_PesosChilenos)
  )

# Aplicamos el ajuste del decimal a todo el dataset
ordenes_compra <- ordenes_compra |>
  mutate(
    MontoTotalOC_PesosChilenos = str_replace(MontoTotalOC_PesosChilenos, ",", "."),
    MontoTotalOC_PesosChilenos = as.numeric(MontoTotalOC_PesosChilenos)
  )


oc_san_miguel <- ordenes_compra |>
  filter(OrganismoPublico == "I MUNICIPALIDAD DE SAN MIGUEL")

oc_san_miguel |>
  janitor::tabyl(EsTratoDirecto) |>
  janitor::adorn_totals()

oc_san_miguel |> 
  filter(MontoTotalOC_PesosChilenos == max(MontoTotalOC_PesosChilenos)) |> 
  pull(MontoTotalOC_PesosChilenos)

oc_san_miguel |>
  filter(
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  group_by(NombreProveedor) |>
  summarise(totalMonto = sum(MontoTotalOC_PesosChilenos)) |>
  arrange(desc(totalMonto)) |>
  top_n(20) |>
  ungroup()

# .by Nuevo en dplyr 1.1.0 https://dplyr.tidyverse.org/news/index.html#dplyr-110
oc_san_miguel |>
  filter(
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  summarise(totalMonto = sum(MontoTotalOC_PesosChilenos), .by = NombreProveedor) |>
  arrange(desc(totalMonto)) |>
  top_n(20)


oc_san_miguel |>
  filter(
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  summarise(n = n(), .by = NombreProveedor) |>
  arrange(desc(n)) |>
  top_n(20)


# Top 10 por cada municipio
municipalidades_td <- ordenes_compra |>
  filter(
    sector == "Municipalidades",
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  group_by(OrganismoPublico, NombreProveedor) |>
  summarise(totalMonto = sum(MontoTotalOC_PesosChilenos)) |>
  arrange(desc(totalMonto)) |>
  top_n(10, totalMonto) |>
  ungroup()


municipalidades_td |>
  slice_max(totalMonto)

# Top 10 Proveedores
ordenes_compra |>
  filter(
    sector == "Municipalidades",
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  summarise(totalMonto = sum(MontoTotalOC_PesosChilenos), .by = NombreProveedor) |>
  arrange(desc(totalMonto)) |>
  top_n(10, totalMonto)


# Ejercicio ----

# Explorar los datos del sector salud
oc_salud_salud <- ordenes_compra |>
  filter(sector == "Salud")
