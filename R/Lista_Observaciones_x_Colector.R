#' @title Lista Observaciones Por Colector

#' @name Lista_Observaciones_x_Colector
#'
#' @param Relacion.General.Observaciones Conjunto de datos del consolidado general de observaciones
#'
#'
#' @description Esta función crea una lista con las observaciones por cada colector.
#'
#' @return Retorna una lista con las observaciones por cada colector
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr group_map
#' @importFrom dplyr select_if
#' @importFrom rlang .data
#'
#' @export



Lista_Observaciones_x_Colector <- function(Relacion.General.Observaciones) {

  Observaciones.x.Colector <- Relacion.General.Observaciones |>
    dplyr::group_by(.data$colector, .add = TRUE) |>
    dplyr::group_map(.f = ~ dplyr::select_if(.x, ~ !all(is.na(.))), .keep = TRUE)

  invisible(Observaciones.x.Colector)
}
