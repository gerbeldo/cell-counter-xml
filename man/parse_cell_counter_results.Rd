% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell_counter_xml_utils.R
\name{parse_cell_counter_results}
\alias{parse_cell_counter_results}
\title{Parse cell counter XML output}
\usage{
parse_cell_counter_results(dir_path)
}
\arguments{
\item{dir_path}{character - path to directory containing Cell Counter XML files}
}
\value{
tibble
}
\description{
Loads all XML files in directory \code{dir_path} and parses them
}
