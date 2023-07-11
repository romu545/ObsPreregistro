#' @title Observaciones UPA Repetidas

#' @name Obs_UPA_Repetidas
#'
#' @param Datos_Evaluar Conjunto de datos para evaluar.
#' @param Nom_No_Evaluar Vector con nombres que no se tendrán en cuenta en la revisión.
#'
#'
#' @description Esta función permite generar observaciones cuando los valores en los campos
#' Nombre de la UPA, Departamento, Municipio, Centro poblado, Latitud y Longitud aparezcan
#' más de un pre-registro.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export

Obs_UPA_Repetidas <- function(Datos_Evaluar, Nom_No_Evaluar) {

  Observaciones <-
    Datos_Evaluar |>
    dplyr::filter(!.data$nombreupa %in% Nom_No_Evaluar) |>
    dplyr::filter(
      dplyr::n() > 1,
      .by = c("nombreupa", "departamento", "municipio", "centropoblado", "latitud", "longitud")) |>
    dplyr::mutate(
      'registro_menor' = suppressWarnings(min(.data$registro)),
      'Observaciones: UPA repetidas' =
        glue::glue(
          "Esta encuesta ya está guardada con el número de registro {registro_menor}.\n",
          "RESPUESTA DEL COLECTOR:"),
      .by = c("nombreupa", "departamento", "municipio", "centropoblado", "latitud", "longitud")) |>
    dplyr::filter(.data$registro != .data$registro_menor) |>
    dplyr::select(c("registro", "Observaciones: UPA repetidas"))

  invisible(Observaciones)

}

