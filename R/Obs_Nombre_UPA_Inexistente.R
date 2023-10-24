#' @title Observaciones Nombre UPA Inexistente

#' @name Obs_Nombre_UPA_Inexistente
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Nom.No.Evaluar Vector con nombres que no se tendrán en cuenta en la revisión.
#'
#' @description Esta función evalúa los nombres de los sitios en donde no se evidenció
#' activida acuicola con el objeto de verificar que estos sean diligenciado como
#' La UPA no existe.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Nombre_UPA_Inexistente <- function(Datos.Evaluar, Nom.No.Evaluar) {

  Observacion_1 <- Datos.Evaluar |>
      dplyr::filter(.data$existe == "No", !.data$nombreupa %in% Nom.No.Evaluar) |>
      dplyr::transmute(
        'registro' = .data$registro,
        'Observaciones: Nombre UPA inexistente' =
          glue::glue("Cuando no exista una UPA en el sitio visitado; el nombre a registrar ",
                     "en el formulario debe ser 'La UPA no existe'.\n",
                     "RESPUESTA DEL COLECTOR:"))

  Observacion_2 <- Datos.Evaluar |>
    dplyr::filter(.data$existe == "Sí", .data$nombreupa %in% Nom.No.Evaluar[Nom.No.Evaluar != "no informa"]) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Nombre UPA inexistente' =
        glue::glue("Cuando exista una UPA en el sitio visitado; el nombre a registrar ",
                   "en el formulario debe ser distinto 'La UPA no existe' o similares.\n",
                   "RESPUESTA DEL COLECTOR:"))

  invisible(
    rbind(Observacion_1,
          Observacion_2))

}
