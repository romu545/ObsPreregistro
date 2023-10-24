#' @title Observaciones UPA Pre-registro vs Asignadas

#' @name Obs_Coordenadas_Preregistro_vs_Asignadas
#'
#' @param Datos.Evaluar.SF Conjunto de datos para evaluar con características espaciales.
#' @param UPA.Asignadas.SF Conjunto de datos de UPA asignadas con características espaciales.
#'
#'
#' @description Esta función permite generar observaciones cuando se encuentra alguna
#' inconsistencia al constrastar las UPA pre-registradas con las asignadas.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import units
#' @import tibble
#' @import stringi
#' @import sf
#' @import stringr
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Coordenadas_Preregistro_vs_Asignadas <- function(Datos.Evaluar.SF, UPA.Asignadas.SF) {

  Join_Coordenadas_Preregistro_y_Asignadas <-
    Datos.Evaluar.SF |>
    dplyr::nest_join(y = UPA.Asignadas.SF, by = c("departamento", "municipio")) |>
    tidyr::unnest(cols = c("UPA.Asignadas.SF"), names_sep = ".")


  Join_Coordenadas_Preregistro_y_Asignadas$distancia <-
  sf::st_distance(
    purrr::pluck(Join_Coordenadas_Preregistro_y_Asignadas, "geometry"),
    purrr::pluck(Join_Coordenadas_Preregistro_y_Asignadas, "UPA.Asignadas.SF.geometry"),
    by_element = TRUE, which = "Great Circle") |>
  units::drop_units()

Observaciones <-
  Join_Coordenadas_Preregistro_y_Asignadas |>
  tibble::tibble() |>
  dplyr::select(
    "registro",
    "codigoupa",
    "UPA.Asignadas.SF.codigoupa",
    "diferentecodigoupa",
    "distancia") |>
  dplyr::mutate(
    'Col_Aux' = dplyr::case_when(
      (.data$codigoupa == .data$UPA.Asignadas.SF.codigoupa & .data$distancia > 120) ~ 1,
      (.data$codigoupa != .data$UPA.Asignadas.SF.codigoupa & .data$codigoupa != "Codigo 0" & .data$distancia < 50) ~ 2,
      (.data$codigoupa == "Codigo 0" & .data$distancia < 80) ~ 3),
    'diferentecodigoupa' = stringr::str_replace_na(.data$diferentecodigoupa, ""),
    'Col_Aux_2' = stringr::str_detect(.data$diferentecodigoupa, as.character(.data$UPA.Asignadas.SF.codigoupa))) |>
  dplyr::filter(!is.na(.data$Col_Aux) & !(.data$Col_Aux == 2 & .data$Col_Aux_2)) |>
  dplyr::filter() |>
  dplyr::reframe(
    'n' = dplyr::n(),
    'Col_Aux_3'= glue::glue_collapse(.data$UPA.Asignadas.SF.codigoupa, sep = ", ", last = " y "),
    'distancia' = max(.data$distancia),
    .by = c(.data$registro, .data$codigoupa, .data$Col_Aux)) |>
  dplyr::transmute(
    "registro" = .data$registro,
    "Observaciones" =
      ifelse(
        .data$Col_Aux == 3,
        glue::glue(
          "Ha referenciado una UPA con un código 0, a pesar de que ",
          "{ifelse(n > 1, 'existen', 'existe')} {n} ",
          "{ifelse(n > 1, 'códigos asignados', 'código asignado')} ({Col_Aux_3}) ",
          "a menos de {round(distancia, 0)} metros.\n",
          "RESPUESTA DEL COLECTOR:"),
        ifelse(
          .data$Col_Aux == 1,
          glue::glue(
            "El punto georreferenciado se encuentra a {round(distancia, 0)} metros de las ",
            "coordenadas asignadas.\nRESPUESTA DEL COLECTOR:"),
          glue::glue(
            "{ifelse(n > 1, 'Existen', 'Existe')} {n} ",
            "{ifelse(n > 1, 'códigos asignados', 'código asignado')} ({Col_Aux_3}) ",
            "a menos de {round(distancia, 0)} metros de estas coordenadas.\n",
            "RESPUESTA DEL COLECTOR:")))) |>
  dplyr::summarise(
    "Col_Aux_3" =
      stringr::str_c(.data$Observaciones, collapse = "\nAdemás; "),
    .by = c("registro")) |>
  dplyr::transmute(
    "registro" = .data$registro,
    "Observaciones: Distancia entre coordenadas pre_registradas y asignadas" =
      stringi::stri_replace(.data$Col_Aux_3, regex = "Además; E", replacement = "Además; e"))

  invisible(Observaciones)
}

