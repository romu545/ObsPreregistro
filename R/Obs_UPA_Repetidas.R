#' @title Observaciones UPA Repetidas

#' @name Obs_UPA_Repetidas
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#'
#' @description Esta función permite generar observaciones cuando los valores en los campos
#' Nombre de la UPA, Departamento, Municipio, Centro poblado, Latitud y Longitud aparezcan
#' más de un pre-registro.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @importFrom dplyr all_of
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr full_join
#' @importFrom dplyr join_by
#' @importFrom purrr reduce
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom stringr str_squish
#' @importFrom glue glue
#' @importFrom rlang .data
#'
#' @export

Obs_UPA_Repetidas <- function(Datos.Evaluar) {

  expregex_1 <- "\\bupa\\b|\\bresponde\\b|\\bdio\\b|\\baplica\\b|inex"
  by_1 <- c("nombreupa", "departamento", "municipio", "centropoblado", "latitud", "longitud")
  by_2 <- c("nombreupa", "departamento", "municipio", "centropoblado")

  Datos.Revisar <- Datos.Evaluar |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ stringr::str_squish(tolower(.x)))) |>
    dplyr::filter(!grepl(expregex_1, .data$nombreupa, ignore.case = TRUE))


  Rev.UPA.Repetidas <- Datos.Revisar |>
    dplyr::filter(
      dplyr::n() > 1,
      .by = dplyr::all_of(by_1)) |>
    dplyr::mutate(
      'registro_menor' = suppressWarnings(min(.data$registro)),
      'Observaciones: UPA repetidas' =
        glue::glue(
          "Esta encuesta ya está guardada con el número de pre-registro {registro_menor}.\n",
          "RESPUESTA DEL COLECTOR:"),
      .by = dplyr::all_of(by_1)) |>
    dplyr::filter(.data$registro != .data$registro_menor) |>
    dplyr::select(c("registro", "Observaciones: UPA repetidas"))


    Rev.Posibles.UPA.Repetidas <- Datos.Revisar |>
      dplyr::filter(
        dplyr::n() > 1,
        .by = dplyr::all_of(by_2)) |>
      dplyr::mutate(
        'registro_menor' = suppressWarnings(min(.data$registro)),
        'Observaciones: Posibles UPA repetidas' =
          glue::glue(
            "Esta encuesta posiblemente se encuentra repetida con la del ",
            "registro {registro_menor}.\n",
            "RESPUESTA DEL COLECTOR:"),
        .by = dplyr::all_of(by_2)) |>
      dplyr::filter(.data$registro != .data$registro_menor) |>
      dplyr::select(c("registro", "Observaciones: Posibles UPA repetidas"))


    Rev.UPA.Coordenadas.Repetidas <- Datos.Evaluar |>
      dplyr::filter(
        dplyr::n() > 1,
        .by = c("latitud", "longitud")) |>
      dplyr::mutate(
        'registro_menor' = suppressWarnings(min(.data$registro)),
        'Observaciones: Coordenadas repetidas' =
          glue::glue(
            "Este registro tiene las mismas coordenadas con el número de registro ",
            "{registro_menor}.\nRESPUESTA DEL COLECTOR:"),
        .by = c("latitud", "longitud")) |>
      dplyr::filter(.data$registro != .data$registro_menor) |>
      dplyr::select(c("registro", "Observaciones: Coordenadas repetidas"))


    list(Rev.UPA.Repetidas, Rev.Posibles.UPA.Repetidas, Rev.UPA.Coordenadas.Repetidas) |>
      purrr::reduce(~ dplyr::full_join(.x, .y, by = dplyr::join_by("registro"))) |>
      invisible()

}
