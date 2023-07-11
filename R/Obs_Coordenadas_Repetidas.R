#' @title Observaciones Otros Códigos Misma UPA

#' @name Obs_Codigo_Misma_UPA
#'
#' @param Datos_Evaluar Conjunto de datos para evaluar.
#'
#' @description Esta función permite generar observaciones cuando existan más de dos
#' pre-registros con las mismas coordenadas.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export

Obs_Coordenadas_Repetidas <- function(Datos_Evaluar) {

  Observaciones <-
    Datos_Evaluar |>
  dplyr::filter(
    dplyr::n() > 1,
    .by = c("latitud", "longitud")) |>
  dplyr::mutate(
    'registro_menor' = suppressWarnings(min(.data$registro)),
    'Observaciones: Coordenadas repetidas' =
      glue::glue(
        "Este registro tiene las mismas coordenadas con el número de registro ",
        "{registro_menor}.\nRESPUESTA DEL COLECTOR:"),
    .by = c("latitud", "longitud")) |>
  dplyr::filter(.data$registro != .data$registro_menor) |>
  dplyr::select(c("registro", "Observaciones: Coordenadas repetidas"))

  invisible(Observaciones)
}

