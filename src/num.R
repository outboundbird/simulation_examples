#' ---
#' title: Basics on covariate adjustment
#' subtitle: 'SAR: sar , Study: study'
#' author:  Siying Huang (E0482362), Biomarker statistics team
#' date: 'created: 2023-02-02 , updated (`r Sys.Date()`)'
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
library(dplyr)
library(ggpubr)
library(jtools)

#' # IMP effect on endpoint
#' at baseline (t0), the endpoint (ep0) level is associated with age, sex, EOS count, ICS, therapy.
#' at the follow-up oservational time point (t1), the endpoint (ep1) level is depended on
#' baseline ep0 and baseline EOS count
# simulate longitudinal data
set.seed(123)
n_obs <- 200
t <- rep(c(0, 1), each = n_obs)
therapy <- sample(c("mono", "double", "triple"),
  n_obs,
  replace = T, prob = c(0.2, 0.5, 0.3)
) %>% factor(levels = c("mono", "double", "triple"))

eos_bsl <- rbinom(n_obs, 1, 0.3) %>%
  factor(labels = c("<300", ">=300"))

ics <- rbinom(n_obs, 1, 0.5) %>%
  factor(labels = c("non-ICS", "ICS"))
age <- rnorm(n_obs, 40, 10)
sex <- sample(c("m", "f"), n_obs,
  replace = T, prob = c(0.7, 0.3)
) %>%
  factor(levels = c("f", "m"))


# simulate baseline endpoint
ep0 <- 50 - 0.5 * age + as.numeric(sex) - 1.5 * as.numeric(eos_bsl) - as.numeric(ics) + 2 * as.numeric(therapy) + rnorm(n_obs, sd = 5)
# mean(ep0)
# intercept corresponds to treatment effect
ep1 <- 50 - 0.1 * ep0 + 1.5 * as.numeric(eos_bsl) + rnorm(n_obs, sd = 8)

# mean(ep1)
chg <- ep1 - ep0
# mean(chg)
#+ fig.dim = c(8, 8)
par(mfrow = c(2, 2), mar = c(4,4,1,1))

plot(ep0 ~ age)
boxplot(ep0 ~ sex)
boxplot(ep0 ~ eos_bsl)
boxplot(ep0 ~ ics)
boxplot(ep0 ~ therapy)

par(mfrow = c(2, 3), mar = c(4,4,1,1))
plot(ep0, ep1)
boxplot(ep1 ~ sex)
boxplot(ep1 ~ eos_bsl)
boxplot(ep0 ~ ics)
boxplot(ep0 ~ therapy)

par(mfrow = c(2, 3), mar = c(4, 4, 1, 1))
plot(chg ~ age)
boxplot(chg ~ eos_bsl)
boxplot(chg ~ sex)
boxplot(chg ~ ics)
plot(ep0, chg)

#' # endpoing value over time
#' assuming 
#+ fig.dim = c(8,5)
df <- data.frame(
  ep = c(ep0, ep1),
  t = t,
  id = rep(1:20, 2),
  eos_bsl = rep(eos_bsl, 2) %>% factor(labels = c("<300", ">=300")),
  therapy = rep(therapy, 2) %>% as.factor(),
  ics = rep(ics, 2)
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "eos_bsl",
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "ics",
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "therapy",
)



# from binorm to generate double gaussian dist?
# pt with high eosinophils has better lung func under ics therapy

#' # Basics on covarites adjustment
#' ## Confounder
#' model: $Y  \sim 1 + 0.5 *X + 0.5* Z + \epsilon$
# confounder
n <- 1000
set.seed <- 456
X <- rnorm(n, 50, 10)
Z <- 0.8 * X + rnorm(n)
Y <- 1 + 0.5 * X + 0.5 * Z + rnorm(n, sd = 2)
#+ fig.dim = c(3.5,3.5)
dag <- dagitty::dagitty("dag{y <- z -> x}")
ggdag::ggdag(dag, layout = "circle") +
  ggdag::theme_dag_blank()

#+ fig.dim= c(5,4)
p1 <- plot_summs(lm(Y ~ X)) +
  ggplot2::labs(title = "Without adjusting confounder")
p2 <- plot_summs(lm(Y ~ X + Z)) +
  ggplot2::labs(title = "Ajusted confounder")
ggarrange(p1, p2, nrow = 2, ncol = 1)

#' ## Mediator - complete mediation
#+ fig.dim= c(3.5,3.5)
# mediator
X <- rnorm(n, 50, 10)
Z <- 0.8 * X + rnorm(n)
Y <- 1 + 0.6 * Z + rnorm(n, sd = 2)

dag <- dagitty::dagitty("dag{y <- z  <-  x}")
ggdag::ggdag(dag, layout = "circle") +
  ggdag::theme_dag_blank()

#+ fig.dim = c(5,4)
p1 <- plot_summs(lm(Y ~ X)) +
  ggplot2::labs(title = "Without adjusting intermediate")
p2 <- plot_summs(lm(Y ~ X + Z)) +
  ggplot2::labs(title = "Ajusted intermediate")
ggarrange(p1, p2, nrow = 2, ncol = 1)


#' <details><summary>Session Info</summary>
sessionInfo()
#' </details>
# Markdown --------------------------------------------------------
# rmarkdown::render('src/num.R', output_dir = 'output')