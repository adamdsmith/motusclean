#' Collapse logical flags into readable reasons
#'
#' @param data A data frame containing logical flag columns.
#' @param flag_map Named character vector mapping flag columns to labels.
#'
#' @return Character vector of semicolon-separated reasons.
#' @keywords internal
collapse_flag_reasons <- function(data, flag_map) {

  flags <- names(flag_map)

  purrr::pmap_chr(
    data[flags],
    function(...) {

      vals <- c(...)

      hit <- which(vals %in% TRUE)

      if (length(hit) == 0) {
        return(NA_character_)
      }

      paste(
        unname(flag_map[hit]),
        collapse = "; "
      )
    }
  )
}
