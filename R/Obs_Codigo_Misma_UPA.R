#' @title Observaciones Otros Códigos Misma UPA

#' @name Obs_Codigo_Misma_UPA
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Datos.Preregistro Conjunto de datos de todos los pre-registros.
#'
#' @description Esta función evalúa que los códigos registrados en el campo para otros id
#' para una misma UPA no hayan sido utilizados en algún pre-registro como el identificador
#' principal.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @import dplyr
#' @import tidyr
#' @import glue
#' @importFrom rlang .data
#'
#' @export

Obs_Codigo_Misma_UPA <- function(Datos.Evaluar, Datos.Preregistro) {

  Codigos.Misma.UPA <-
    Datos.Preregistro |>
    dplyr::select(c("registro", "diferentecodigoupa")) |>
    tidyr::drop_na() |>
    tidyr::separate_wider_delim(
      col = c("diferentecodigoupa"),
      delim = ",",
      names_sep = ".",
      too_few = "align_start") |>
    tidyr::pivot_longer(
      cols = !c("registro"),
      names_to = "atributo",
      values_to = "codigoupa",
      values_drop_na = TRUE) |>
    dplyr::distinct(.data$registro, .data$codigoupa) |>
    dplyr::filter(.data$codigoupa != "Codigo 0") |>
    dplyr::mutate('codigoupa' = as.double(.data$codigoupa))


  Observaciones <-
    Datos.Evaluar |>
    dplyr::filter(.data$codigoupa != "Codigo 0") |>
    dplyr::mutate('codigoupa' = as.double(.data$codigoupa)) |>
    dplyr::select(c("registro", "codigoupa")) |>
    dplyr::inner_join(Codigos.Misma.UPA, by = dplyr::join_by("codigoupa")) |>
    dplyr::reframe(
      'registro.y' = glue::glue_collapse(.data$registro.y, sep = ", ", last = " y "),
      .by = c("registro.x", "codigoupa")) |>
    dplyr::transmute(
      'registro' = .data$registro.x,
      'Observaciones: Otros códigos en la misma UPA' =
        glue::glue(
          "El código {codigoupa} ya fue relacionado como 'otros códigos ",
          "en la misma UPA' en el/los pre-registro(s) {registro.y}.\n",
          "RESPUESTA DEL COLECTOR:"))

  invisible(Observaciones)

}
