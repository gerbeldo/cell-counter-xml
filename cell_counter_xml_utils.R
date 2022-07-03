library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)

# genero xpath para marker n
marker_xpath <- function(n) {
  base <- "//Marker_Type[x]"
  str_replace(base, "x", as.character(n))
}

# obtengo los markers
get_markers <- function(xpath, obj) {
  as_list(xml_contents(xml_find_first(obj, xpath)))[-c(1, 2)]
}

# creo el tibble base
gen_base_list <- function(xml_obj, xpath = "//Marker_Type/Name") {
  n_markers <- length(xml_length(xml_find_all(xml_obj, xpath)))
  
  t <- tibble(
    marker_type_int = 1:n_markers,
    marker_type = map_chr(marker_type_int, .f = marker_xpath)
  )
  
  list(t, xml_obj)
}

# extraigo los markers del xml
extract_xml <- function(xml_base_list) {
  base_tibble <- xml_base_list[[1]]
  xml_obj <- xml_base_list[[2]]
  
  out <- base_tibble %>%
    mutate(raw_xml_extract = map(marker_type, get_markers, xml_obj)) %>%
    unnest(raw_xml_extract) %>%
    mutate(id = row_number(),
           parsed_xml_object = map(raw_xml_extract, enframe))
  
  out
  
}

# limpio el tibble
clean_xml_tibble <- function(xml_tibble) {
  if (nrow(xml_tibble) != 0) {
    xml_tibble %>%
      unnest(parsed_xml_object) %>%
      mutate(value = as.numeric(unlist(value)),
             raw_xml_extract = NULL) %>%
      pivot_wider(
        id_cols = c(marker_type, marker_type_int, id),
        names_from = "name",
        values_from = "value"
      )
  } else {
    xml_tibble %>%
      mutate(
        MarkerX = numeric(),
        MarkerY = numeric(),
        MarkerZ = numeric(),
        parsed_xml_object = NULL,
        raw_xml_extract = NULL
      )
  }
}


parse_cell_counter_xml <- function(xml_fpath) {
  i <- read_xml(xml_fpath)
  position <-
    str_match(str_match(xml_fpath, pattern = "n[:digit:]{1,3}\\.xml$"),
              pattern = "[:digit:]{1,3}") %>% as.numeric()
  
  gen_base_list(i) %>%
    extract_xml() %>%
    clean_xml_tibble() %>%
    mutate(position = position) %>%
    rename(marker_xpos = MarkerX,
           marker_ypos = MarkerY,
           t.frame = MarkerZ)
}

parse_cell_counter_results <- function(dir_path) {
  l <- file.path(dir_path,
                 dir(path = dir_path, pattern = "xml$"))
  tibble(paths = l,
         xml = map(l, parse_cell_counter_xml)) %>%
    unnest(xml)
  
}