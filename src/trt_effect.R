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
library(here)
library(dplyr)
library(zeallot)
library(ggpubr)
library(jtools)
library(lmerTest)
library(ggdag)
library(simstudy)
source(file.path(here(),'src/utils/utils.R'))

#' # Interrelationships among variables of interst
#' ## Structure of the study
#' The study is interested in the association between the treatment and endpoint (e.g. gene expression levels, FEV1, etc.)

dag <- dagify(
  eos ~ itep,
  ics ~ eos,
  t123 ~ eos,
  y ~ eos + itep + ics + t123 + age +sex,
  exposure = "itep",
  outcome = "y"
)

ggdag(dag, text_size =6, node_size =18, edge_type = 'link') +
theme_dag_gray()

#' ## Open paths between variables (itep and y)

ggdag_paths_fan(dag, from ='itep', to ='y', shadow = F, spread = 1) +
  theme_dag_gray() +
  ggplot2::theme(legend.position = "bottom")
#+ fig.dim = c(6,8)
ggdag_paths(dag, shadow = T) +
  theme_dag_gray() +
  ggplot2::theme(legend.position = "bottom")

#' ## D-separation between variables
#' direction separated
ggdag_dseparated(dag, from = "itep", to = "y") +
  ggplot2::theme(legend.position = "bottom")

#' ## Adjustement sets
ggdag_adjustment_set(dag, exposure = "itep", outcome = "y", shadow = T) +
  theme_dag_grey()+
  ggplot2::theme(legend.position = "bottom")

# simulating with package
#' At baseline , t0
#' ```
#' Y0 ~ age + sex + eos0
#' Ics_0 ~ eos0
#' T123_0 ~ eos0
#' At t1
#' Y1~ y0 + eos1 + x + age + sex +e
#' X reflects drug effect on Y ~ N(a, s)
#' eos1 ~ x +eos0
#' Ics1 ~ eos1
#' ```
#' assume the mono/double/triple therapies stays the same over the treatment period.


dstr <- defData(varname = "age", dist = "normal", formula = 40, variance = 15) %>%
  defData("sex", dist = "binary", formula = 0.7) %>%
  defData("eos0", dist = "binary", formula = 0.6) %>%
  defData("y0", dist = "normal", formula = "10+age + sex -eos0") %>%
  defData("x", dist = "normal", formula = 5, variance = 10) %>%
  defData("ics0", dist = "binary", formula = "0.7*eos0") %>%
  defData("t1230", dist = "nonrandom", formula = "0.7*eos0") %>%
  defData("eos1", dist = "binary", formula = "eos0 - 0.6*x") %>%
  defData("ics1", dist = "binary", formula = "0.7*eos1") %>%
  defData("y1", dist = "normal", formula = "y0 - eos1 + age + sex + 0.4*x")

prob_t123 <- c(0.2, 0.6, 0.2)
df <- genData(20, dstr) %>%
  genOrdCat(adjVar = "t1230", prob_t123, catVar = "t123")

summary(df)
lm(y0~ age+ sex + eos0, data = df)

panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
upper.panel <- function(x, y) {
  points(x, y, pch = 19, col = my_cols[as.factor(df$eos0)])
}
pairs(df[,-1],  lower.panel = panel.cor, upper.panel = upper.panel)

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

# mvtnorm::rmvnorm(20, sigma)


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
boxplot(ep0 ~ eos)
boxplot(ep0 ~ ics)
boxplot(ep0 ~ therapy)

par(mfrow = c(2, 3), mar = c(4,4,1,1))
plot(ep0, ep1)
boxplot(ep1 ~ sex)
boxplot(ep1 ~ eos)
boxplot(ep0 ~ ics)
boxplot(ep0 ~ therapy)

par(mfrow = c(2, 3), mar = c(4, 4, 1, 1))
ggboxplot(chg, add = "mean_sd")
plot(chg ~ age)
boxplot(chg ~ eos)
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
  eos = rep(eos, 2) %>% factor(labels = c("<300", ">=300")),
  therapy = rep(therapy, 2) %>% as.factor(),
  ics = rep(ics, 2),
  age = rep(age, 2),
  sex = rep(sex, 2)
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "eos",
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "ics",
)

ggline(df, "t", "ep",
  group = "id",
  facet.by = "therapy",
)

fit1 <- lmer(ep~ t + eos +(1|id), data = df)

fit2 <- lmer(ep ~ t + eos + ics + therapy + age + sex + (1 | id), data = df)
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
# rmarkdown::render('src/trt_effect.R', output_dir = 'docs')