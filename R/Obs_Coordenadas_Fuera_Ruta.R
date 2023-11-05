#' @title Observaciones Coordenadas Fuera Ruta

#' @name Obs_Coordenadas_Fuera_Ruta
#'
#' @param datos.evaluar.sf Conjunto de datos para evaluar.
#' @param ruta.archivos.gpx Dirección de la carpeta en donde se encuentran los archivos GPX
#' que contienen las rutas hechas por los colectores.
#' @param fecha.tracks fecha desde la cual se tomaran los tracks.
#' @param buffer distancia de influencia de las rutas. Por defecto el valor inicales es de 20.
#'
#' @description Esta función permite generar observaciones cuando existan coordenadas por
#' fuera de las rutas seguidas por los colectores al momento de la caracterización de las
#' UPA.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @importFrom rlang set_names
#' @importFrom rlang .data
#' @importFrom sf st_read
#' @importFrom sf st_as_sf
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom sf st_make_valid
#' @importFrom sf st_filter
#' @importFrom sf st_disjoint
#' @importFrom  sf st_is_empty
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr last_col
#' @importFrom dplyr filter
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_c
#' @importFrom tidyr separate_wider_delim
#' @importFrom lubridate dmy
#'
#' @export

Obs_Coordenadas_Fuera_Ruta <- function(
        datos.evaluar.sf,
        ruta.archivos.gpx,
        fecha.tracks,
        buffer = 20) {

    tracks.colectores <-
        lapply(
            dir(ruta.archivos.gpx, full.names = TRUE),
            sf::st_read, layer = "tracks") |>
        dplyr::bind_rows() |>
        dplyr::mutate(
            'fechas' =
                stringr::str_extract_all(
                    .data$name, "\\b\\d{2}\\s\\w{3}\\s\\d{4}\\b") |>
                stringr::str_replace_all(
                    pattern = c(
                        " ENE " = "01", " FEB " = "02", " MAR " = "03", " ABR " = "04",
                        " MAY " = "05", " JUN " = "06", " JUL " = "07", " AGO " = "08",
                        " SEP " = "09", " OCT " = "10", " NOV " = "11", " DIC " = "12")) |>
                lubridate::dmy()) |>
        dplyr::filter(.data$fechas >= fecha.tracks) |>
        sf::st_buffer(buffer) |>
        sf::st_union() |>
        sf::st_make_valid() |>
        (\(data) {data[!sf::st_is_empty(data)]})()

    rev.pt.fruera.ruta <- sf::st_filter(
        datos.evaluar.sf,
        tracks.colectores,
        .predicate = sf::st_disjoint) |>
        tibble::as_tibble() |>
        dplyr::mutate(
            'registro' = .data$registro,
            .data$colector,
            'Observaciones: Coordenadas fuera de ruta' = stringr::str_c(
                "Las coordenadas de este registro se encuentran alejadas de la ruta recorrida. ",
                "Verificar si existe algún error en las coordenadas o que el archivo GPX cargado",
                "corresponde a la semana evaluda.\n",
                "RESPUESTA DEL COLECTOR:"),
            .keep = "none")

    invisible(rev.pt.fruera.ruta)

}
