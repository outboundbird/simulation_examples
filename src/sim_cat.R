#' ---
#' title: Simulating categorical variables
#' subtitle: 'genotypes'
#' author:  Siying Huang 
#' date: 'created: 2023-01-27 , updated (`r Sys.Date()`)'
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
knitr::opts_chunk$set(echo = T, comment = "", message = F, warning = F, error = F)
options(width = 100)
#+ libs
library(knitr)
library(here)
library(dplyr)
library(igraph)
library(graphsim)
library(PhenotypeSimulator)
# simulate genetic pathways
source(file.path(here(), "src/utils.R"))

# generate independent genotype data
edges <- rbind(
  c("G1", "Abs"),
  c("G2", "Abs"),
  c("G3", "BioTrans"),
  c("G4", "BioTrans"),
  c("G5", "BioTrans"),
  c("G6", "ImmRect"),
  c("G7", "ImmRect"),
  c("G8", "ImmRect"),
  c("Abs", "BioTrans"),
  c("BioTrans", "ImmRect"),
  c("ImmRect", "DILI")
)

net <- graph.edgelist(edges, directed = T)
states <- rep(1, 11)

plot_directed(net, states, cex.node = 3)

#' # Generate genotype data
#' ## Generate independent genotype data
# simulate genotype with pkg
set.seed(123)
s1 <- simulateGenotypes(N = 100, NrSNP = 2, frequencies = c(0.2, 0.6))
s2 <- gen_genotype(0.6, 100)

table(s1$genotypes[, 2])
table(s2)
table(s1$genotypes[, 2], s2)

#' ## Generate genotype data based on conditional distribution
g1 <- gen_genotype(0.8, 100)
abs <- sample(c("yes", "no"), 100, replace = T)
p_cond_tab <- table(g1, abs)
prop.table(table(g1, abs))

mars <- gen_2catBy2cat(prop.table(table(g1, abs)), 100)
proportions(table(mars[[1]], mars[[2]]))

cond_prob <- matrix(
  c(
    0.1, 0.2, 0.3,
    0.1, 0, 0.2,
    0, 0, 0.2
  ),
  nrow = 3, ncol = 3,
  dimnames = list(LETTERS[1:3], letters[1:3])
)

num <- gen_2catBy2cat(cond_prob, 100)
prop.table(table(num[[1]], num[[2]]))

#' ## Generated correlated genotype from Gaussian copula
#' ### Generate correlated uniform distributions
# sim genotype samples
# 1. generate correlated uniform vars
df <- gen_gauss_cop(c(0.0, 0.2, 0.5), 1000)
cor(df)
par(mfrow = c(2, 2))
invisible(apply(df, 2, function(x) hist(x)))

#' ### Generate genotype from the uniform distributions
# 2. generate genotype from the correlated mtx with MAF
afs <- c(0.05, 0.1, 0.2, 0.5)
G <- sapply(seq_len(ncol(df)), function(i) {
  unif2genotype(df[, i], afs[i])
}) %>%
  data.frame() %>%
  setNames(paste0("G", 1:ncol(df)))
summary(G)
cat_cor(G)

#' # Simulate phenotype


# currently known genetic networks

#' <details><summary>Session Info</summary>
sessionInfo()
#' </details>
# Markdown --------------------------------------------------------
# rmarkdown::render('src/sim_genes.R', output_dir = 'output')
