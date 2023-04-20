library(openxlsx)
library(dplyr)
gom_88 <- read.xlsx("data/gam_84.xlsx")
edad_inicio <- 30
edad_retiro <- 65
sueldo <- 123123132
vs <- 0.02
i <- 0.03
rotacion <- 0.01

get_costo_ultimate_r <- function(edad_inicio, rotacion, edad_retiro, sueldo, aporte_pct = 0.1) {
  tabla <- gom_88 %>%
    filter(Edad >= edad_inicio & Edad < edad_retiro) %>%
    mutate(
      p = if_else(Edad == edad_inicio, 1, 1 - Masculino),
      p_desde_x_hasta_t = cumprod(p) * (1 - rotacion) ** -(edad_inicio - Edad),
      f_actualizacion = 1 / (1 + i) ** (Edad - edad_inicio),
      sueldo_t = (1 + vs) ** (Edad - edad_inicio),
      costo = p_desde_x_hasta_t * f_actualizacion * sueldo_t * aporte_pct
    )
  sum(tabla$costo) * (1 + i) ** (edad_retiro - edad_inicio) * (1 / last(tabla$p_desde_x_hasta_t))
}

a_vida <- function(edad_retiro) {
  tabla <- gom_88 %>%
    filter(Edad >= edad_retiro) %>%
    mutate(
      p = if_else(Edad == edad_retiro, 1, 1 - lag(Masculino)),
      p_desde_x_hasta_t = cumprod(p),
      f_actualizacion = 1 / (1 + i) ** (Edad - edad_retiro),
      costo = p_desde_x_hasta_t * f_actualizacion
    )
  sum(tabla$costo)
}

a_vida <- a_vida(edad_retiro)
get_contribucion <- function(a_vida, vs, edad_retiro, edad_inicio, beneficio_objetivo) {
  vs_64 <- (1 + vs) ** (edad_retiro - edad_inicio - 1)
  tabla <- data.frame(
    edad = edad_inicio:(edad_retiro - 1)
  ) %>%
    mutate(
      f_actualizacion = (1 + i) ** (edad_retiro - edad),
      sueldo_t = (1 + vs) ** (edad - edad_inicio),
      costo = f_actualizacion * sueldo_t
    )
  costo <- sum(tabla$costo)
  (beneficio_objetivo * vs_64 * a_vida)/ costo
}

beneficio_objetivo <- 0.32
get_contribucion(a_vida, sueldo, vs, edad_retiro, edad_inicio, beneficio_objetivo)
