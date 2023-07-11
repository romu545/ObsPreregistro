#' @title Datos Pre-registro Evaluar SF

#' @name Datos_Evaluar_SF
#'
#' @param Datos_Evaluar Conjunto de datos para evaluar.
#'
#' @description Esta función transforma el conjunto de datos de los pre-registros para
#' evaluar a conjunto con características espaciales.
#'
#'
#' @return Retorna el conjunto de datos de los pre-registros evaluados con características
#' espaciales.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang .data
#'
#' @export


Datos_Evaluar_SF <- function(Datos_Evaluar) {

  Datos_Evaluar_SF <-
    Datos_Evaluar |>
    dplyr::arrange(.data$departamento, .data$municipio, .data$registro) |>
    sf::st_as_sf(coords = c("longituddecimal", "latituddecimal"), crs = 4326)

  invisible(Datos_Evaluar_SF)

}
