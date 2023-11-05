pacman::p_load(sf, dplyr, tidyr, stringr, purrr, ggplot2)

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
#' @importFrom collapse unlist2d
#' @importFrom sf st_read
#' @importFrom sf st_as_sf
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom sf st_make_valid
#' @importFrom sf st_filter
#' @importFrom sf st_disjoint
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
        fecha.rutas,
        buffer = 20) {

    colectores <-  dir(ruta.archivos.gpx) |> stringr::str_remove_all(".gpx|.GPX")

    tracks.colectores <-
        lapply(
            dir(ruta_1, full.names = TRUE),
            sf::st_read, layer = "tracks") |>
        rlang::set_names(colectores) |>
        collapse::unlist2d(idcols = "colector") |>
        dplyr::select(1, 2, dplyr::last_col()) |>
        dplyr::mutate(
            'name' =
                stringr::str_extract_all(name, "\\b\\d{2}\\s\\w{3}\\s\\d{4}\\b") |>
                stringr::str_replace_all(
                    pattern = c(
                        " ENE " = "01", " FEB " = "02", " MAR " = "03", " ABR " = "04",
                        " MAY " = "05", " JUN " = "06", " JUL " = "07", " AGO " = "08",
                        " SEP " = "09", " OCT " = "10", " NOV " = "11", " DIC " = "12")) |>
                lubridate::dmy()) |>
        dplyr::filter(name >= fecha.rutas) |>
        tidyr::separate_wider_delim(
            colector,
            delim = "_",
            names = c("colector", "semana")) |>
        sf::st_as_sf() |>
        sf::st_buffer(buffer) |>
        sf::st_union() |>
        sf::st_make_valid()

    Rev.pt.fruera.ruta <- sf::st_filter(
        datos.evaluar.sf,
        tracks.colectores,
        .predicate = sf::st_disjoint) |>
        dplyr::mutate(
            'registro' = .data$registro,
            'Observaciones: Coordenadas fuera de ruta' = stringr::str_c(
                "Las coordenadas de este registro se encuentran alejadas de la ruta recorrida. ",
                "Verificar si existe algún error en las coordenadas o que el archivo GPX cargado",
                "corresponde a la semana evaluda.\n",
                "RESPUESTA DEL COLECTOR:"))

    invisible(Rev.pt.fruera.ruta)

}
