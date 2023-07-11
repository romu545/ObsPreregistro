#' @title Observaciones Municipio Distinto

#' @name Obs_Municipios_Diferentes
#'
#' @param Datos_Evaluar_SF Conjunto de datos para evaluar con características espaciales.
#' @param Mpio_shp Capa de municipios
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

Obs_Municipios_Diferentes <- function(Datos_Evaluar_SF, Mpio_shp) {

  Geometrias_Intersectadas <-
    sf::st_join(
      x = Datos_Evaluar_SF,
      y = Mpio_shp,
      join = sf::st_intersects) |>
    tibble::as_tibble()

  Observaciones <-
    Geometrias_Intersectadas |>
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
