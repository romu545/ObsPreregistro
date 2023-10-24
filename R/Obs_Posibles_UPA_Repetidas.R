#' @title Observaciones Posibles UPA Repetidas

#' @name Obs_Posibles_UPA_Repetidas
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Nom.No.Evaluar Vector con nombres que no se tendrán en cuenta en la revisión.
#'
#' @description Esta función permite generar observaciones cuando los valores en los campos
#' Nombre de la UPA, Departamento, Municipio y Centro poblado aparezcan más de un pre-registro.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export


Obs_Posibles_UPA_Repetidas <- function(Datos.Evaluar, Nom.No.Evaluar) {

   Observaciones <-
     Datos.Evaluar |>
     dplyr::filter(!.data$nombreupa %in% Nom.No.Evaluar) |>
     dplyr::filter(
       dplyr::n() > 1,
       .by = c("nombreupa", "departamento", "municipio", "centropoblado")) |>
    dplyr::mutate(
      'registro_menor' =  suppressWarnings(min(.data$registro)),
      'Observaciones: Posibles UPA repetidas' =
        glue::glue(
          "Esta encuesta posiblemente se encuentra repetida con la del ",
          "registro {registro_menor}.\n",
          "RESPUESTA DEL COLECTOR:"),
      .by = c("nombreupa", "departamento", "municipio", "centropoblado")) |>
     dplyr::filter(.data$registro != .data$registro_menor) |>
     dplyr::select(c("registro", "Observaciones: Posibles UPA repetidas"))

  invisible(Observaciones)

}

