library(tidyverse)

pediatric_hospitals_rm <- c(
  "Hospital Adalberto Steeger (Talagante)",
  "Hospital San Luis (Buin)",
  "Hospital Clínico de Niños Dr. Roberto del Río (Santiago, Independencia)",
  "Hospital Clínico Metropolitano El Carmen Doctor Luis Valentín Ferrada",
  "Hospital Clínico Metropolitano La Florida Dra. Eloísa Díaz Insunza",
  "Hospital Clínico San Borja Arriarán", 
  "Hospital de Niños Dr. Luis Calvo Mackenna",
  "Hospital Padre Alberto Hurtado (San Ramón)",
  "Hospital San Juan de Dios (Santiago, Santiago)",
  "Complejo Hospitalario Dr. Sótero del Río (Santiago, Puente Alto)",
  "Hospital Dr. Exequiel González Cortés (Santiago, San Miguel)",
  "Hospital Dr. Félix Bulnes Cerda (Santiago, Quinta Normal)"
)

render_fun <- function(hospital){
  rmarkdown::render(
    input = "sesion-10/03-report.Rmd",
    params = list(n_establecimiento = hospital),
    output_file = glue::glue("reports/{hospital}-report.html")
  )
}

data <- data.table::fread("sesion-10/raw-data/AtencionesUrgencia2023.csv", encoding = "Latin-1") |>
  janitor::clean_names()

data |> 
  filter(n_establecimiento %in% pediatric_hospitals_rm) |> 
  distinct(n_establecimiento) |> 
  pull() |> 
  purrr::walk(render_fun)
