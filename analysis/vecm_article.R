library(readxl)
library(urca)
library(vars)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1) Reading data
# Expecting columns: year, profit, surplusvalue, occ (rename to what you have)
data <- read_xlsx("mydata.xlsx", sheet = "Sheet1")
summary(data)


# --- 2) Building time series
# Example for annual data from 1940 to 2023:
Y <- ts(data[, c("profit","surplusvalue","occ")],
        start = min(data$year), frequency = 1)

# --- 3) Selecting lags
sel <- VARselect(Y, lag.max = 6, type = "const")
sel$selection
K <- as.numeric(sel$selection["AIC(n)"])  # pick AIC/HQ/SC per your preference
summary(sel)

# --- 4) Performing Johansen test
joh <- ca.jo(Y, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(joh)
# Decide cointegration rank r from the trace tests:
r <- 1

# --- 5) Estimating VECM 
vecm_fit <- cajorls(joh, r = r)
alpha <- joh@V[, 1:r, drop = FALSE]
beta  <- joh@W[, 1:r, drop = FALSE]
alpha; beta

# Extracting coefficients 
coef_table <- coef(vecm_fit$rlm)
print(coef_table)

# --- 6) Converting to VAR form and compute IRFs
vec_as_var <- vec2var(joh, r = r)

set.seed(123)
ir <- irf(vec_as_var, n.ahead = 20, boot = TRUE, runs = 1000, ci = 0.95, ortho = TRUE)

# --- 7) IRF plots 
make_irf_df <- function(ir_obj) {
  out <- list()
  impulses <- names(ir_obj$irf)
  for (imp in impulses) {
    mat   <- ir_obj$irf[[imp]]
    lower <- ir_obj$Lower[[imp]]
    upper <- ir_obj$Upper[[imp]]
    for (resp in colnames(mat)) {
      out[[length(out) + 1]] <- data.frame(
        h        = 0:(nrow(mat) - 1),
        irf      = mat[, resp],
        lower    = lower[, resp],
        upper    = upper[, resp],
        impulse  = imp,
        response = resp
      )
    }
  }
  bind_rows(out)
}

ir_df <- make_irf_df(ir)
  
  ggplot(ir_df, aes(x = h, y = irf)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    facet_grid(response ~ impulse, scales = "free_y") +
    labs(x = "Horizon", y = "IRF",
         title = "IRFs from VECM (converted to VAR form)",
         subtitle = paste0("K (levels lags) = ", K, ", rank r = ", r, 
                           ", ordering = ", paste(colnames(Y), collapse = " → "))) +
    theme_minimal(base_size = 12)

summary(joh)
summary(irf)

# --- 8) CUMULATIVE IRFs 
irc <- irf(vec_as_var, n.ahead = 20, boot = TRUE, runs = 500, ci = 0.95, ortho = TRUE, cumulative = TRUE)
plot(irc)

# --- 9) Forecast Error Variance Decomposition
fe <- fevd(vec_as_var, n.ahead = 20)
plot(fe)
