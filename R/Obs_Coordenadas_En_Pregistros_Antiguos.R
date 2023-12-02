#' @title Observaciones Coordenadas En Pre-registros Antiguos

#' @name Obs_Coordenadas_En_Pregistros_Antiguos
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Preregistros.Antiguos Conjusto de datos de los pre-registros evaluados en cortes
#' anteriores
#'
#' @description Esta función permite generar observaciones cuando las coordenadas de los
#' pre-registros evaluados estén digitadas en pre-registros evaluados en cortes anteriormente.
#'
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @importFrom plyr empty
#' @import purrr
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Coordenadas_En_Pregistros_Antiguos <- function(Datos.Evaluar, Preregistros.Antiguos) {

  Rev.Coordenadas.En.Pregistros.Antiguos <- Datos.Evaluar |>
    dplyr::nest_join(y = Preregistros.Antiguos, by = c("latitud", "longitud")) |>
    dplyr::mutate('es_vacio' = purrr::map_lgl(.data$Preregistros.Antiguos, plyr::empty)) |>
    dplyr::filter(!.data$es_vacio) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'registro_antiguo' =
        purrr::map_chr(
          .x = .data$Preregistros.Antiguos,
          .f = ~glue::glue_collapse(.x, sep = ", ", last = " y ")),
      'Observaciones: Coordenadas repetidas con registros antiguos' =
        glue::glue(
          "Las coordenadas de este preregistro se encuentran repetidas con el o los ",
          "registro(s) {registro_antiguo} que fueron digitados en semanas anteriores a ",
          "la actual.\n¡Por favor! Verifique e indique cuales son las coordenadas ",
          "correctas.\nRESPUESTA DEL COLECTOR:")) |>
    dplyr::select(-2)

  invisible(Rev.Coordenadas.En.Pregistros.Antiguos)

}
