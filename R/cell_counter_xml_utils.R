# generate xpath for marker type n
marker_xpath <- function(n) {
  base <- "//Marker_Type[x]"
  stringr::str_replace(base, "x", as.character(n))
}


get_markers <- function(xpath, obj) {
  xml2::as_list(xml2::xml_contents(xml2::xml_find_first(obj, xpath)))[-c(1, 2)]
}

# create base tibble to fill
gen_base_list <- function(xml_obj, xpath = "//Marker_Type/Name") {
  n_markers <- length(xml2::xml_length(xml2::xml_find_all(xml_obj, xpath)))

  t <- tibble::tibble(
    marker_type_int = 1:n_markers,
    marker_type = purrr::map_chr(marker_type_int, .f = marker_xpath)
  )

  list(t, xml_obj)
}

# extracts markers from xml
extract_xml <- function(xml_base_list) {
  base_tibble <- xml_base_list[[1]]
  xml_obj <- xml_base_list[[2]]

  out <- base_tibble |>
    dplyr::mutate(raw_xml_extract = purrr::map(marker_type, get_markers, xml_obj)) |>
    tidyr::unnest(raw_xml_extract) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      parsed_xml_object = purrr::map(raw_xml_extract, tibble::enframe)
    )

  out
}


clean_xml_tibble <- function(xml_tibble) {
  if (nrow(xml_tibble) != 0) {
    xml_tibble |>
      tidyr::unnest(parsed_xml_object) |>
      dplyr::mutate(
        value = as.numeric(unlist(value)),
        raw_xml_extract = NULL
      ) |>
      tidyr::pivot_wider(
        id_cols = c(marker_type, marker_type_int, id),
        names_from = "name",
        values_from = "value"
      )
  } else {
    xml_tibble |>
      dplyr::mutate(
        MarkerX = numeric(),
        MarkerY = numeric(),
        MarkerZ = numeric(),
        parsed_xml_object = NULL,
        raw_xml_extract = NULL
      )
  }
}


parse_cell_counter_xml <- function(xml_fpath) {
  i <- xml2::read_xml(xml_fpath)
  position <-
    stringr::str_match(stringr::str_match(xml_fpath, pattern = "n[:digit:]{1,3}\\.xml$"),
      pattern = "[:digit:]{1,3}"
    ) |> as.numeric()

  gen_base_list(i) |>
    extract_xml() |>
    clean_xml_tibble() |>
    dplyr::mutate(position = position) |>
    dplyr::rename(
      marker_xpos = MarkerX,
      marker_ypos = MarkerY,
      t.frame = MarkerZ
    )
}

#' Parse cell counter XML output
#'
#' Loads all XML files in directory `dir_path` and parses them
#'
#' @param dir_path character - path to directory containing Cell Counter XML files
#'
#' @return tibble
#' @export
#'
parse_cell_counter_results <- function(dir_path) {
  l <- file.path(
    dir_path,
    dir(path = dir_path, pattern = "xml$")
  )
  tibble::tibble(
    paths = l,
    xml = purrr::map(l, parse_cell_counter_xml)
  ) |>
    tidyr::unnest(xml)
}
