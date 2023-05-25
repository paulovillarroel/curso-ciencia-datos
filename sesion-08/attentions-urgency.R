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


# Plot

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
