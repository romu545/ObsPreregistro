#' @title Observaciones Veredas Distintas

#' @name Obs_Veredas_Diferentes
#'
#' @param Datos.Evaluar.SF Conjunto de datos para evaluar con características espaciales.
#' @param Verda.shp Capa de veredas
#'
#' @description Esta función permite generar observaciones cuando las coordenadas ubican
#' a la upa pre-registrada en una vereda distinta a la seleccionada.
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

Obs_Veredas_Diferentes <- function(Datos.Evaluar.SF, Vereda.shp) {

  Geometrias_Intersectadas <-
    sf::st_join(
      x = Datos.Evaluar.SF,
      y = Vereda.shp,
      join = sf::st_intersects) |>
    tibble::as_tibble()

  Observaciones <-
    Geometrias_Intersectadas |>
    dplyr::filter(tolower(.data$vereda) != tolower(.data$NOMBRE_VER)) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Veredas' =
        glue::glue(
          "Si las coordenadas son correctas, el nombre de la vereda/corregimiento es: ",
          "{toupper(NOMBRE_VER)}"))

  invisible(Observaciones)
}
