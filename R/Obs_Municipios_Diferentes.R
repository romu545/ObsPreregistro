#' @title Observaciones Municipio Distinto

#' @name Obs_Municipios_Diferentes
#'
#' @param Datos.Evaluar.SF Conjunto de datos para evaluar con características espaciales.
#' @param Mpio.shp Capa de municipios
#'
#' @description Esta función permite generar observaciones cuando las coordenadas ubican
#' las coordenadas pre-registras en un municipio distinto al seleccionado.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import tibble
#' @import sf
#' @import glue
#' @importFrom rlang .data
#'
#' @export

Obs_Municipios_Diferentes <- function(Datos.Evaluar.SF, Mpio.shp) {

  Geometrias.Intersectadas <-
    sf::st_join(
      x = Datos.Evaluar.SF,
      y = Mpio.shp,
      join = sf::st_intersects) |>
    tibble::as_tibble()

  Observaciones <-
    Geometrias.Intersectadas |>
    dplyr::filter(
      (tolower(.data$municipio) != tolower(.data$MPIO_CNMBR)) |
        (tolower(.data$departamento) != tolower(.data$DPTO_CNMBR))) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Municipio distinto' =
        glue::glue(
          "Estas coordenadas ubican al punto caracterizado en {MPIO_CNMBR} - {DPTO_CNMBR}.\n",
          "RESPUESTA DEL COLECTOR:"))

  invisible(Observaciones)
}
