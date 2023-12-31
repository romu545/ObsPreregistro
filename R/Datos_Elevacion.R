#' @title Datos Elevaciones Datos Evaluados

#' @name Datos_Elevacion
#'
#' @param Datos.Evaluar.SF Conjunto de datos para evaluar con características espaciales.
#'
#' @description Esta función permite obtener la altitud de las coordenadas evaluadas
#' mediante por fuera el modelo digital de elevación de AWS.
#'
#' @return Retorna una tabla con el número de registro y las altitudes registradas y calculadas.
#'
#' @import dplyr
#' @import sf
#' @import elevatr
#' @import tibble
#' @importFrom rlang .data
#'
#' @export


Datos_Elevacion <- function(Datos.Evaluar.SF) {

  Elevaciones <-
    elevatr::get_elev_point(
      Datos.Evaluar.SF,
      prj = "EPSG:4326",
      src = "aws",
      z = 10) |>
    dplyr::select(.data$elevation)

  Datos.Elevacion <-
    cbind(
      Datos.Evaluar.SF,
      Elevaciones) |>
    tibble::as_tibble() |>
    dplyr::select(c("registro", "altitud", "elevation"))

  invisible(Datos.Elevacion)

}
