#' ---
#' title: generate network data
#' subtitle: 'R tutorial'
#' author:  Siying Huang
#' date: 'created: 2023-01-19 , updated (`r Sys.Date()`)'
#' always_allow_html: true
#' output:
#'   html_document:
#'     code_folding: "hide"
#'     css: sanofi.css
#'     toc: yes
#'     toc_float:
#'       collapse: no
#' ---
#+ setup, include = FALSE
# knitr::opts_knit$set(root_dir='/mnt/c/Users/e0482362/Work/simulation_examples/src')
knitr::opts_chunk$set(echo = T, comment = '',message = F, warning = F, error=F)
options(width = 100)
#+ libs
library(knitr)
library(graphsim) #
library("gplots")
library("scales")
library(igraph)
library(DAAG) # b network sim

#' # generate network gene expression data
#' Using [grphsim](https://cran.r-project.org/web/packages/graphsim/vignettes/simulate_expression.html)
#' package. The examples are adapted from the vignettes. Togenerate the network
#' data. One needs to define the network structure first. The gene network is first
#' defined with connected genes.

graph_structure_edges <- rbind(
  c("A", "C"), c("B", "C"), c("C", "D"), c("D", "E"),
  c("D", "F"), c("F", "G"), c("F", "I"), c("H", "I")
)
print(graph_structure)

graph_structure <- graph_from_edgelist(graph_structure_edges, directed = TRUE)

plot_directed(graph_structure,
main = "Simulated network structure",
layout = layout.kamada.kawai)

expr <- generate_expression(100, graph_structure,
  cor = 0.8, mean = 0,
  comm = FALSE, dist = TRUE, absolute = FALSE
)
#' ## Visualize simulated network
heatmap.2(expr,
  scale = "none", trace = "none",
  col = bluered(50), colsep = 1:4, rowsep = 1:4
)

#' ### adjacency matrix
#' > The data can be summarised by an “adjacency matrix” where a one (1) is given between a row i and column j if there is an edge between genes i and j. Otherise it is a zero (0) for genes that are not connected. For an undirected graph, edges are shown in a symmetical matrix.
#'
#' > For a directed graph, the adjacency matrix may be assymetric. A non-zero element adjmat[i,j] represents the presence or weight of the edge from gene i (matrix rows) to gene j (matrix columns).
adj_mat <- make_adjmatrix_graph(graph_structure)
print(adj_mat)


#' # Simulate data with Bayesian networks

#' <details><summary>Session Info</summary>
sessionInfo()
#' </details>
# Markdown --------------------------------------------------------
# rmarkdown::render('src/network.R', output_dir = 'output')
