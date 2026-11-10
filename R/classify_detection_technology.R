#' Classify Motus tag and receiver technology
#'
#' Internal helper used to determine which validation rules apply.
#'
#' @param det Hit-level Motus detection data.
#'
#' @return Detection data with `tag_technology` and `receiver_type`.
#' @keywords internal
classify_detection_technology <- function(det) {

  det |>
    dplyr::mutate(
      tag_technology = dplyr::case_when(
        mfg == "Lotek" ~ "Lotek",
        mfg == "CTT"   ~ "CTT",
        TRUE           ~ "unknown"
      ),

      receiver_type = dplyr::case_when(
        grepl("^SG-", recv)    ~ "SensorGnome",
        grepl("^CTT-", recv)   ~ "SensorStation",
        grepl("^Lotek-", recv) ~ "Lotek",
        TRUE                   ~ "unknown"
      )
    )
}
