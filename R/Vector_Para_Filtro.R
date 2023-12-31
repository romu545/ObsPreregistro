#' @title Vector Booleano Para Filtro

#' @name Vector_Para_Filtro
#'
#' @param Lista.Observaciones.x.Colector Lista de observaciones de cada colector.
#'
#'
#' @description Esta función permite crear un vector booleano que filtrar en primera
#' instancia los pre-registros sin observaciones que se enviaran a habilitar de los
#' que poseen alguna observación para ser validada por los colectores.
#'
#' @return Retorna un vector booleano
#'
#' @import dplyr
#' @import tidyselect
#' @import purrr
#'
#' @export


Vector_Para_Filtro <- function(Lista.Observaciones.x.Colector) {

  # Determinar las filas y columnas que cuentan con observaciones, esto se hace para cada
  #  uno de los dataset de la lista-

  Filas.Con.Observaciones <-
    purrr::map(
      .x = Lista.Observaciones.x.Colector,
      .f = ~ rowSums(is.na(dplyr::select(.x, tidyselect::starts_with("Obs")))))

  Columnas.Con.Observaciones <-
    purrr::map(
      .x = Lista.Observaciones.x.Colector,
      .f = ~ length(dplyr::select(.x, tidyselect::starts_with("Obs"))))

  # Construir un vector booleano para objeto de la listo con la finalidad de filtrar
  # los mismos objeto para así definir que registros se habilitaran y que otros
  # se enviaran para la revisión de los colectores.

  Para.Filtro <- purrr::map2(
    .x = Filas.Con.Observaciones,
    .y = Columnas.Con.Observaciones,
    .f = ~ .x == .y)

  invisible(Para.Filtro)
}
