#' @title Observaciones Nombre Predio

#' @name Obs_Nombre_Predio
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Nom.Evaluar Vector con nombres que se tendrán en cuenta en la revisión.
#'
#' @description Esta función evalúa los nombres de los predios en donde no se evidenció
#' actividad acuícola con el objeto de verificar que estos sean diligenciados con algún
#' nombre en especifico.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import stringr
#' @importFrom rlang .data
#'
#' @export


Obs_Nombre_Predio <- function(Datos.Evaluar, Nom.Evaluar) {

  Observaciones <- Datos.Evaluar |>
    dplyr::filter(.data$predio %in% Nom.Evaluar[Nom.Evaluar != "no informa"]) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Nombre predio' =
        stringr::str_c(
          "La inexistencia de una granja de acuicultura no está relacionada con el ",
          "nombre del predio, dado que en este se podría adelantar una actividad ",
          "productiva diferente. Si el predio no cuenta con un nombre o el encuestado ",
          "desconoce dicha información, debe colocar “No Informa” o el nombre del ",
          "propietario.\n RESPUESTA DEL COLECTOR:"))

  invisible(Observaciones)

}
