# calculate genotype freq based on allele freq
gtype_freq <- function(fa) {
  c(fa^2, 2 * fa * (1 - fa), (1 - fa)^2)
}

# calculate allele frequency based on genotype frequency
gf2af <- function(gf) {
  stopifnot("need three numbers" = length(gf) == 3)
  # A dominant allele, B minor allele
  A <- 2 * gf[1] + gf[2]
  B <- 2 * gf[3] + gf[2]
  N <- 2 * sum(gf)
  c(A / N, B / N)
}

# AA, 2AB, BB
gf = c(9, 8, 2)
gf <- setNames(gf, c("AA", "AB", "BB"))
length(gf)

gf2af(gf)
gf2af(c(9, 6, 4))
gf2af(c(1, 2, 2))
gf2af(c(108, 37, 5))

# generate genotype with a given allele frequency
gen_genotype <- function(fa, n, seed = 123) {
  gfreq <- gtype_freq(fa)
  set.seed(seed)
  sample(c(2, 1, 0), n, replace = T, prob = gfreq)
}

# generate numeric variable with equal variance
# using linear relationship
gen_num_ev <- function(var, b0, b1, sd_resid, ll = NA, ul = NA) {
  # generate value assuming equal variance
  n <- length(var)
  covar <- b0 + b1 * var + rnorm(n, 0, sd_resid)
  if (!is.na(ll)) covar <- ifelse(covar < ll, ll, covar)
  if (!is.na(ul)) covar <- ifelse(covar > ul, ul, covar)
  print(summary(covar))
  covar
}

# generate numeric values based on a categorical variable
# can generate value with unequal variance
gen_numBycat <- function(N, M, SD) {
  # generate numeric value by unequal variance
  all_eq <- do.call(all.equal, list(length(M), length(SD), length(N)))
  stopifnot("N, M,SD need to have equal length" = all_eq)
  x <- mapply(function(n, m, sd) {
    rnorm(n, m, sd)
  }, N, M, SD)
  do.call(c, x)
}


#' this function generate two categorical variables based on the conditional probability given by a cross table.
#'
#' @param p_cond_tab conditional probability tab.
#' @param n number of instance to generate.
#' @param seed seed.
#'
#' @return list of variables
#' @examples
#' see demo
gen_2catBy2cat <- function(p_cond_tab, n, seed = 12345) {
  # probability table
  if (any(p_cond_tab > 1)) stop("Need a probability table")
  labs <- outer(
    dimnames(p_cond_tab)[[1]],
    dimnames(p_cond_tab)[[2]],
    function(x, y) paste(x, y, sep = "-")
  ) %>%
    c()
  p_cond <- as.vector(p_cond_tab)
  cum_p <- c(0, cumsum(p_cond))

  while (length(unique(cum_p)) < length(cum_p)) {
    cum_p[which(duplicated(cum_p))] <- cum_p[which(duplicated(cum_p))] + 1e-6
  }

  set.seed(seed)
  rand <- runif(n)
  cond_val <- cut(rand,
    breaks = cum_p,
    labels = labs
  )
  list(
    stringr::str_extract(cond_val, ".+(?=-)"),
    stringr::str_extract(cond_val, "(?<=-).+")
  )
}


#' FUNCTION_TITLE
#'
#' Generate correlated uniform distribution with Gaussian copula
#'
#' @param r a numeric vector, desired correlation coefficient(s).
#' @param n int, number of samples to be generated.
#'
#' @return matrix of simulated uniform distributions
#' @examples
#' gen_gauss_cop(c(0.1, 0.4), 1000)
gen_gauss_cop <- function(r, n) {
  rho <- 2 * sin(r * pi / 6) # Pearson correlation
  P <- toeplitz(c(1, rho)) # Correlation matrix
  d <- nrow(P) # Dimension
  ## Generate sample
  U <- pnorm(matrix(rnorm(n * d), ncol = d) %*% chol(P))
  return(U)
}


#' FUNCTION_TITLE
#'
#' Generate genotype data from uniform distribution
#'
#' @param x numeric vector follows uniform distribution.
#' @param fa minor allele frequency.
#'
#' @return genotype data
#' @examples
#' unif2genotype(runif(100), 0.5)
unif2genotype <- function(x, fa) {
  mat <- cut(x,
    cumsum(c(0, gtype_freq(fa))),
    include.lowest = T,
    labels = c(2, 1, 0)
  )
  as.numeric(mat) - 1
}


#' FUNCTION_TITLE
#'
#' contingency coefficient
#'
#' @param data numeric data frame or matrix.
#' @param ... argument in `chisq.test` function
#' @return corrected contingency coefficient
#' @examples
#' # ADD_EXAMPLES_HERE
cont_coeff <- function(data, corrected = T, ...) {
  tab <- table(data)
  X2 <- unname(chisq.test(tab, ...)$stat)
  n <- nrow(data)
  C <- sqrt(X2 / (X2 + n))

  if (corrected) {
    k <- min(NROW(tab), NCOL(tab))
    C_max <- sqrt((k - 1) / k)
    return(C / C_max)
  }
  C
}


#' Cramer’s V
#'
#' correlation coefficient of categorical vars
#'
#' @param data dataframe or matrix.
#'
#' @return Cramer's V coeff
#' @examples
#' # ADD_EXAMPLES_HERE
cramer_v <- function(data, ...) {
  tab <- table(data, ...)
  n <- nrow(data)
  x2 <- unname(chisq.test(tab)$stat)
  k <- min(NROW(tab), NCOL(tab))
  sqrt(x2 / (n * (k - 1)))
}


#' call function of categorical correlation calculation
#'
#' @param data data.frame.
#' @param method method for correlation calculation, default "cc" -contingency coefficient. Available options "creamer" - Creamer's V.
#' @param ... other arguments in the individual functions.
#'
#' @return Null
call_cor <- function(data, method, ...) {
  switch(method,
    cramer = cramer_v(data, ...),
    cc = cont_coeff(data, ...)
  )
}


#' Calculate categorical correlation coefficient
#'
#' @param data data frame. must be numeric and categorical.
#' @param method defult contingency coefficient "cc", other options: "cramer".
#' @param ... other arguments for cor methods
#'
#' @return correlation matrix of the data set
#' @examples
#' # ADD_EXAMPLES_HERE
cat_cor <- function(data, method = "cc", ...) {
  ccor <- sapply(seq_len(ncol(data) - 1), function(i) {
    sapply((i + 1):ncol(data), function(j) {
      call_cor(data[, c(i, j)], method, ...)
    })
  }) %>%
    do.call("c", .)
  mtx <- matrix(, ncol(data), ncol(data))
  colnames(mtx) <- rownames(mtx) <- names(data)
  mtx[lower.tri(mtx, diag = F)] <- ccor
  diag(mtx) <- 1
  mtx[upper.tri(mtx, diag = F)] <- t(mtx)[upper.tri(t(mtx), diag = F)]
  mtx
}

denote_p <- function(p) {
  thresh <- setNames(
    c(1, 0.1, 0.05, 0.01, 0.001, 0.0001),
    c("ns", ".", "*", "**", "***", "****")
  )
  names(thresh)[max(which(p <= thresh))]
}
