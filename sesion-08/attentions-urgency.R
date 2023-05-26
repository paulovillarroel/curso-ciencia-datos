library(tidyverse)
library(janitor)

## Data obtained from DEIS

url <- "https://repositoriodeis.minsal.cl/SistemaAtencionesUrgencia/AtencionesUrgencia2023.zip"

download.file(url, destfile = "sesion-08/raw-data/AtencionesUrgencia2023.zip")

unzip("sesion-08/raw-data/AtencionesUrgencia2023.zip", exdir = "sesion-08/raw-data/")

data <- data.table::fread("sesion-08/raw-data/AtencionesUrgencia2023.csv", encoding = "Latin-1") |>
  clean_names()


# Remove the files

unlink("sesion-08/raw-data/*")


# Resume data

respiratory_cause <- data |>
  filter(
    glosa_causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO",
    semana != max(semana)
  ) |>
  group_by(semana) |>
  summarise(total = sum(total))


respiratory_cause_childs <- data |>
  filter(
    glosa_causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO",
    semana != max(semana)
  ) |> 
  rowwise() |> 
  mutate(ped = sum(menores_1, de_1_a_4, de_5_a_14)) |> 
  group_by(semana) |> 
  summarise(menores_1 = sum(menores_1),
            de_1_a_4 = sum(de_1_a_4),
            de_5_a_14 = sum(de_5_a_14)) |> 
  pivot_longer(cols = -semana,
               names_to = "grupo_etario",
               values_to = "n_atenciones")



# Plot

## All ages

respiratory_cause |>
  ggplot(aes(semana, total)) +
  geom_area(fill = "#e0aaff", color = "#7b2cbf", size = 2) +
  geom_point(color = "#724cf9", size = 3) +
  scale_x_continuous(breaks = 1:nrow(respiratory_cause)) +
  scale_y_continuous(labels = scales::comma) +
  theme_grey() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias. Chile 2023",
    subtitle = "Se incluyen todos los servicios de urgencia del país\nTodas las edades\n",
    x = "\nSemana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: Datos abiertos DEIS"
  ) +
  theme(
    plot.title = element_text(size = 34, face = "bold"),
    plot.subtitle = element_text(size = 22),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "#e9ecef"),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#ced4da", linewidth = 0.2),
    panel.grid.minor = element_line(color = "#ced4da", linewidth = 0.2),
    legend.background = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("sesion-08/plot/respiratory_cause_2023.png", height = 10, width = 15)


## Pediatric

respiratory_cause_childs$grupo_etario <- factor(respiratory_cause_childs$grupo_etario,
                                                labels = c("menores_1", "de_1_a_4", "de_5_a_14"))

respiratory_cause_childs |>
  ggplot(aes(semana, n_atenciones, fill = grupo_etario)) +
  geom_area() +
  scale_x_continuous(breaks = 1:nrow(respiratory_cause_childs)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#f72585", "#7209b7", "#7678ed"), 
                    labels = c("Menores 1 año", "Entre 1 y 4 años", "Entre 5 y 14 años")) +
  theme_grey() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias < 14 años. Chile 2023",
    subtitle = "Se incluyen todos los servicios de urgencia del país\n",
    x = "\nSemana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: Datos abiertos DEIS",
    fill = "Grupo etario"
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 22),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "#e9ecef"),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#ced4da", linewidth = 0.2),
    panel.grid.minor = element_line(color = "#ced4da", linewidth = 0.2),
    legend.background = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("sesion-08/plot/respiratory_cause_childs_2023.png", height = 10, width = 15)
