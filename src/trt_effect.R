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
library(zeallot)
library(ggpubr)
library(jtools)
library(lmerTest)
#' # IMP effect on endpoint
#' at baseline (t0), the endpoint (ep0) level is associated with age, sex, EOS count, ICS, therapy.
#' at the follow-up oservational time point (t1), the endpoint (ep1) level is depended on
#' baseline ep0 and baseline EOS count
# simulate longitudinal data
set.seed(123)
n_obs <- 2000
t <- rep(c(0, 1), each = n_obs)

# therapy should be correlated with ics
tab <- matrix(c(5, 3, 2, 5, 7, 8), ncol = 2) / 30
dimnames(tab) <- list(c("mono", "double", "triple"), c("non_ICS", "ICS"))
tab
c(therapy, ics) %<-% gen_2catBy2cat(tab, n_obs, 45)
therapy <- factor(therapy,levels = c("mono", "double", "triple"))
ics <- factor(ics, levels=c("non_ICS", "ICS"))
prop.table(table(therapy, ics))


u <- gen_gauss_cop(c(0.6, 0.4, 0.3), n_obs, 3)
colnames(u) <-   c("eos", "ICS", "TRT")
cor(u)
plot(u[, 1], u[, 2])
plot(u[, 1], u[, 3])
plot(u[, 3], u[, 2])
hist(u[,1], breaks = n_obs)

eos <- unif2cat(u[,'eos'], 0.2, c(">300","<=300"))
table(eos)
ics <- unif2cat(u[,'ICS'], 0.6, c("ICS","non_ICS"))
table(ics)
trt <- unif2cat(u[,'TRT'], c(0.4, 0.2), c('mono','double','triple'))
table(trt)

df <- data.frame(eos, ics, trt)
cat_cor(df)
cat_cor(df, 'cramer')

age <- rnorm(n_obs, 40, 10)

sex <- sample(c("m", "f"), n_obs,
  replace = T, prob = c(0.7, 0.3)
) %>%
  factor(levels = c("f", "m"))



# simulate baseline endpoint
ep0 <- 50 - 0.5 * age + as.numeric(sex) - 1.5 * as.numeric(eos) - as.numeric(ics) + 2 * as.numeric(trt) + rnorm(n_obs, sd = 5)
# mean(ep0)
# intercept corresponds to treatment effect
ep1 <- 50 - 0.5 * ep0 + 5 * as.numeric(eos) + rnorm(n_obs, sd = 10)

# mean(ep1)
chg <- ep1 - ep0
mean(chg)
#+ fig.dim = c(8, 6)
par(mfrow = c(2, 3), mar = c(4,4,1,1))

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
ggboxplot(chg, add = "mean_sd")
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
  ics = rep(ics, 2),
  age = rep(age, 2),
  sex = rep(sex, 2)
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

fit1 <- lmer(ep~ t + eos_bsl +(1|id), data = df)

fit2 <- lmer(ep ~ t + eos_bsl + ics + therapy + age + sex + (1 | id), data = df)
plot_summs(fit1, fit2)

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
# rmarkdown::render('src/num.R', output_dir = 'docs')