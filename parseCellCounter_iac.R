library(devtools)
library(usethis)

devtools::create(".")
use_mit_license()

use_package("dplyr")
use_package("xml2")
use_package("stringr")
use_package("tibble")
use_package("tidyr")
use_package("purrr")
