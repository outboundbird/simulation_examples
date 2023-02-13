#' ---
#' title: Data simulation
#' author:  SH
#' date: 'created: 2023-02-13 , updated (`r Sys.Date()`)'
#' always_allow_html: true
#' output:
#'   html_document:
#'     css: sanofi.css
#'     code_folding: "hide"
#'     toc: yes
#'     toc_float:
#'       collapse: no
#' ---
#+ setup, include = FALSE
knitr::opts_knit$set(root_dir='/mnt/c/Users/e0482362/Work/simulation_examples/docs')
knitr::opts_chunk$set(echo = T, comment = '',message = F, warning = F, error=F)
options(width = 100)
#+ libs
library(here)
#' # Intro
#' Testing site for now.  TBD
#' # Simulating categorical data
#'
#' [demo](./demo.html)
#'
#' # Simulating numeric data
#'

#' <details><summary>Session Info</summary>
sessionInfo()
#' </details>
# Markdown --------------------------------------------------------
# rmarkdown::render('docs/index.R', output_dir = 'docs')