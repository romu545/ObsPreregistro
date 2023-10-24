#' @title Observaciones Nombre UPA

#' @name Obs_Nombre_UPA
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Lista.Referencia.Nombres Listado de nombres catalogados como validados.
#' @param Nom.No.Evaluar Vector con nombres que no se tendrán en cuenta en la revisión.
#'
#' @description Esta función evalúa los nombres de las unidades productivas con el objeto
#' de verificar que estos estén escritos correctamente.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import stringr
#' @import glue
#' @import tidyr
#' @importFrom rlang .data
#'
#' @export


Obs_Nombre_UPA <- function(Datos.Evaluar, Lista.Referencia.Nombres, Nom.No.Evaluar) {

  Observaciones <-
    Datos.Evaluar |>
    dplyr::select(c("registro", "nombreupa")) |>
    dplyr::filter(!.data$nombreupa %in% Nom.No.Evaluar) |>
    dplyr::mutate("Col_Aux_1" = stringr::str_split(.data$nombreupa, pattern = " ")) |>
    tidyr::unchop(.data$Col_Aux_1) |>
    dplyr::anti_join(y = Lista.Referencia.Nombres, by = c("Col_Aux_1" = "Nombres")) |>
    dplyr::summarise(
      "Col_Aux_1" = glue::glue_collapse(stringr::str_to_title(.data$Col_Aux_1), sep = ", ", last = " y "),
      .by = c("registro", "nombreupa")) |>
    dplyr::transmute(
      "registro" = .data$registro,
      "Observaciones: Nombre de la UPA" =
        glue::glue("Verificar si para el nombre de la UPA la(s) palabra(s): ",
                   "{Col_Aux_1} se encuentra(n) escrita(s) correctamente.\n",
                   "RESPUESTA DEL COLECTOR:"))

  invisible(Observaciones)

}
