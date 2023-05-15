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
# rbindlist(lista_archivos)
# bind_rows(lista_archivos)
# map_df()

ordenes_compra <- plyr::ldply(lista_archivos, data.frame)


# Borramos los datos
unlink("sesion-03/raw-data/*")


# Veamos un poco el dataset ----

# Aplicamos el ajuste del decimal a todo el dataset y cambiamos los tipos de datos
ordenes_compra <- ordenes_compra |>
  mutate(
    MontoTotalOC_PesosChilenos = str_replace(MontoTotalOC_PesosChilenos, ",", "."),
    MontoTotalOC_PesosChilenos = as.numeric(MontoTotalOC_PesosChilenos),
    across(starts_with("Fecha"), as.Date),
    across(starts_with("fecha"), as.Date)
  )

glimpse(ordenes_compra)

colnames(ordenes_compra)


# Ver valores perdidos 
ordenes_compra |> 
  DataExplorer::plot_missing()

miss_value <- naniar::miss_summary(ordenes_compra)



oc_montos <- ordenes_compra$MontoTotalOC_PesosChilenos

format(max(oc_montos), scientific = FALSE, big.mark = ",")

ordenes_compra |> 
  filter(MontoTotalOC_PesosChilenos == max(MontoTotalOC_PesosChilenos)) |> 
  pull(ID)

format(
ordenes_compra |> 
  filter(EsTratoDirecto == "Si") |> 
  pull(MontoTotalOC_PesosChilenos) |> 
  max()
, big.mark = ",")


# Subseting
oc_san_miguel <- ordenes_compra |>
  filter(OrganismoPublico == "I MUNICIPALIDAD DE SAN MIGUEL")

skimr::skim(oc_san_miguel)


hist(oc_san_miguel$MontoTotalOC_PesosChilenos, breaks = 400)

oc_san_miguel |> 
  ggplot(aes(MontoTotalOC_PesosChilenos)) +
  geom_histogram(bins = 400)

oc_san_miguel |> 
  ggplot(aes(MontoTotalOC_PesosChilenos)) +
  geom_histogram(bins = 400) +
  scale_x_continuous(labels = scales::comma)


oc_san_miguel |>
  janitor::tabyl(EsTratoDirecto) |>
  janitor::adorn_totals()

oc_san_miguel |> 
  filter(MontoTotalOC_PesosChilenos == max(MontoTotalOC_PesosChilenos)) |> 
  pull(ID)

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
top_10_proveedores <- ordenes_compra |>
  filter(
    sector == "Municipalidades",
    Estado != "Cancelacion solicitada",
    EsTratoDirecto == "Si"
  ) |>
  summarise(totalMonto = sum(MontoTotalOC_PesosChilenos), .by = NombreProveedor) |>
  arrange(desc(totalMonto)) |>
  top_n(10, totalMonto)


# Un breve gráfico
top_10_proveedores |>
  ggplot(aes(fct_reorder(NombreProveedor, totalMonto), totalMonto )) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
  


# Ejercicio ----

# Explorar los datos del sector salud
oc_salud_salud <- ordenes_compra |>
  filter(sector == "Salud")
