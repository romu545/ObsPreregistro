#' @title Observaciones Distancia Entre UPA Pre-registradas y Digitadas

#' @name Obs_Distancia_Con_UPA_Digitadas
#'
#' @param Datos_Evaluar_SF Conjunto de datos para evaluar con características espaciales.
#' @param Datos_Caracterizacion_SF Datos caracterización con características espaciales
#'
#'
#' @description Esta función permite generar observaciones cuando las coordenadas de los
#' pre-registros evaluados se encuentran cerca de coordenadas registradas en el formulario
#' de caracterición.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import tibble
#' @import sf
#' @import units
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Distancia_Con_UPA_Digitadas <- function(Datos_Evaluar_SF, Datos_Caracterizacion_SF) {

  Join_Coordenadas_Preregitro_y_Digitadas <-
    Datos_Evaluar_SF |>
    dplyr::nest_join(y = Datos_Caracterizacion_SF, by = c("departamento", "municipio")) |>
    tidyr::unnest(cols = c("Datos_Caracterizacion_SF"), names_sep = ".")

  Join_Coordenadas_Preregitro_y_Digitadas$distancia <-
    sf::st_distance(
      purrr::pluck(Join_Coordenadas_Preregitro_y_Digitadas, "geometry"),
      purrr::pluck(Join_Coordenadas_Preregitro_y_Digitadas, "Datos_Caracterizacion_SF.geometry"),
      by_element = TRUE,
      which = "Great Circle") |>
    units::drop_units()

  Observaciones <-
    Join_Coordenadas_Preregitro_y_Digitadas  |>
    tibble::as_tibble() |>
    dplyr::select(!c(
      "anio",
      "diferentecodigoupa",
      "geometry",
      "Datos_Caracterizacion_SF.geometry",
      "altitud")) |>
    dplyr::filter(.data$distancia < 120) |>
    dplyr::arrange(.data$registro, .data$distancia) |>
    dplyr::summarise(
      'n' = dplyr::n(),
      'distancias' =  glue::glue_collapse(round(.data$distancia,2), sep = ", ", last = " y "),
      'codigos' =  glue::glue_collapse(.data$Datos_Caracterizacion_SF.codigoupa, sep = ", ", last = " y "),
      .by = c("registro")) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Distancia a UPA caracterizadas' =
        glue::glue(
          "El punto georreferenciado se encuentra cerca de ",
          "{ifelse(n > 1, 'las UPA caracterizadas con los códigos', 'la UPA caracterizada con el código')} ",
          "{codigos}; que se {ifelse(n > 1, 'encuentran', 'encuentra')} a una distancia de ",
          "{distancias} {ifelse(n > 1, 'metros respectivamente', 'metros')}.\n",
          "RESPUESTA DEL COLECTOR:"))

  invisible(Observaciones)

}
