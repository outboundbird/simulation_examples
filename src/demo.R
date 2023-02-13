#' ---
#' title: Demo of variable simulation
#' subtitle: 'simulating categorical and numeric variables with dependencies'
#' author:  Siying Huang
#' date: 'created: 2023-02-13 , updated (`r Sys.Date()`)'
#' always_allow_html: true
#' output:
#'   html_document:
#'     css: sanofi.css
#'     code_folding: "show"
#'     toc: yes
#'     toc_float:
#'       collapse: no
#' ---
#+ setup, include = FALSE
knitr::opts_knit$set(root_dir = "/mnt/c/Users/e0482362/Work/simulation_examples/src")
knitr::opts_chunk$set(echo = T, comment = "", message = F, warning = F, error = F)
options(width = 100)
#+ libs
library(here)
library(zeallot)
source(file.path(here(), "src/utils/utils.R"))

#' # Generte categorical variables
#' ## Generate categorical variables based on conditional probabilities
#' Generate two categorical variables based on conditional distribution of the two categorical variables
tab <- matrix(c(5, 4, 2, 5, 6, 8), ncol = 2) / 30
dimnames(tab) <- list(c("A", "B", "C"), c("X", "Y"))
print(tab)

c(varA, varB) %<-% gen_2catBy2cat(tab, 100)
prop.table(table(varA, varB))

#' Genrate a set of variables based on correlation structure
#' <details><summary>Session Info</summary>
sessionInfo()
#' </details>
# Markdown --------------------------------------------------------
# rmarkdown::render('src/demo.R', output_dir = 'output')