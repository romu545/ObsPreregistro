#' @title Consolidado General Observaciones

#' @name Relacion_General_Observaciones
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar
#'
#'
#' @description Esta función realiza el consolidado de todas las observaciones generadas.
#'
#' @return Retorna una tabla con todos los registros evaluados y sus respectivas observaciones
#'
#' @importFrom purrr list_flatten
#' @importFrom purrr reduce
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr select
#'
#' @export

Relacion_General_Observaciones <- function(Datos.Evaluar) {

  Consolidado.Observaciones <-
    purrr::list_flatten(list(Datos.Evaluar = Datos.Evaluar,
                             mget(ls(pattern = "Obs\\.", sorted = TRUE, envir = .GlobalEnv),
                                  envir = .GlobalEnv))) |>
    purrr::reduce(~ dplyr::left_join(.x, .y, by = dplyr::join_by("registro"))) |>
  dplyr::select(c(1, 5, 35, 29, 6, 27, 34, 7, 32, 8, 33, 9, 24, 10, 11, 30,
                  12, 23, 13, 36, 14, 31, 26, 28, 15, 16, 25, 19, 22, 20, 21))

  invisible(Consolidado.Observaciones)
}
