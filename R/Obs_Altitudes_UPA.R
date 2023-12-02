#' @title Observaciones Altitudes UPA

#' @name Obs_Altitudes_UPA
#'
#' @param Datos.Elevacion Conjunto de datos para evaluar con las elevaciones calculadas.
#'
#' @description Esta función permite generar observaciones cuando las altitudes registradas
#' quedan por fuera del intervalo generado mediante el modelo digital de elevación de AWS.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Altitudes_UPA <- function(Datos.Elevacion) {

  Rev.Altitudes.UPA <- Datos.Elevacion |>
    dplyr::mutate(
      'Col_Aux_1' =
        dplyr::case_when(
          .data$altitud <   100 ~ dplyr::between(.data$altitud, .data$elevation * 0.500, .data$elevation * 1.500),
          .data$altitud <   500 ~ dplyr::between(.data$altitud, .data$elevation * 0.575, .data$elevation * 1.425),
          .data$altitud <  1000 ~ dplyr::between(.data$altitud, .data$elevation * 0.650, .data$elevation * 1.350),
          .data$altitud <  1500 ~ dplyr::between(.data$altitud, .data$elevation * 0.725, .data$elevation * 1.275),
          .data$altitud <  2000 ~ dplyr::between(.data$altitud, .data$elevation * 0.800, .data$elevation * 1.200),
          .data$altitud <  2500 ~ dplyr::between(.data$altitud, .data$elevation * 0.875, .data$elevation * 1.125),
          .data$altitud >= 3000 ~ dplyr::between(.data$altitud, .data$elevation * 0.950, .data$elevation * 1.050),
          .default = TRUE)) |>
    dplyr::filter(!.data$Col_Aux_1) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Altitud' =
        glue::glue(
          "El valor de altitud se encuentra por fuera del intervalo generado mediante ",
          "el modelo digital de elevación de AWS.\n",
          "Verificar este dato o en su defecto los respectivos valores de longitud y latitud.\n",
          "RESPUESTA DEL COLECTOR:"))

  invisible(Rev.Altitudes.UPA)

}
