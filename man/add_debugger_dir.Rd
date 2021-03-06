% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/debugger.R
\name{add_debugger_dir}
\alias{add_debugger_dir}
\title{add_debugger_dir}
\usage{
add_debugger_dir(input_dir, output_dir = NULL)
}
\arguments{
\item{input_dir}{Path to the shiny application}

\item{output_dir}{Path to output directory where you want to save shiny application . default value is NULL to overwrite.}
}
\description{
This function takes application input and output directory as input parameter
and update the shiny application code by adding messages for function and reactive expression
if you don't provide the output directory it ask you to overwrite existing directory or
provide output directory location.
}
