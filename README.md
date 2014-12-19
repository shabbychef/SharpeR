


# SharpeR

A number of utilities for dealing with Sharpe ratio, the Sharpe ratio of the
Markowitz portfolio, and, in general, overfit of trading strategies based on
(in-sample) Sharpe statistics.

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package may be installed from CRAN; the latest version may be
found on [github](https://www.github.com/shabbychef/SharpeR "SharpeR")
via devtools:


```r
if (require(devtools)) {
    # latest greatest
    install_github(repo = "SharpeR", username = "shabbychef", 
        ref = "master")
    # last release:
    install_github(repo = "SharpeR", username = "shabbychef", 
        ref = "r0.1310")
}
```


# Basic Usage

## Using sr


```r
require(SharpeR)
# suppose you computed the Sharpe of your strategy
# to be 1.3 / sqrt(yr), based on 1200 daily
# observations.  an object can be instanatiated as
# follows
my.sr <- sr(sr = 1.3, df = 1200 - 1, ope = 252, epoch = "yr")
print(my.sr)
```

```
##        SR/sqrt(yr) Std. Error t value Pr(>t)   
## Sharpe        1.30       0.46     2.8 0.0023 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


And using real data:

```r
require(quantmod)
options(getSymbols.warning4.0 = FALSE)

# get price data, compute log returns on adjusted
# closes
get.ret <- function(sym, warnings = FALSE, ...) {
    # getSymbols.yahoo will barf sometimes; do a
    # trycatch
    trynum <- 0
    while (!exists("OHCLV") && (trynum < 7)) {
        trynum <- trynum + 1
        try(OHLCV <- getSymbols(sym, auto.assign = FALSE, 
            warnings = warnings, ...), silent = TRUE)
    }
    adj.names <- paste(c(sym, "Adjusted"), collapse = ".", 
        sep = "")
    if (adj.names %in% colnames(OHLCV)) {
        adj.close <- OHLCV[, adj.names]
    } else {
        # for DJIA from FRED, say.
        adj.close <- OHLCV[, sym]
    }
    rm(OHLCV)
    # rename it
    colnames(adj.close) <- c(sym)
    adj.close <- adj.close[!is.na(adj.close)]
    lrets <- diff(log(adj.close))
    # chop first
    lrets[-1, ]
}
get.rets <- function(syms, ...) {
    some.rets <- do.call("cbind", lapply(syms, get.ret, 
        ...))
}
```




```r
some.rets <- get.rets(c("IBM", "AAPL", "XOM"), from = "2004-01-01", 
    to = "2013-08-01")
print(as.sr(some.rets))
```

```
##      SR/sqrt(yr) Std. Error t value  Pr(>t)    
## IBM         0.43       0.32     1.3 0.08948 .  
## AAPL        1.05       0.32     3.3 0.00056 ***
## XOM         0.42       0.32     1.3 0.09606 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Inference on the Markowitz Portfolio

The (negative) Markowitz portfolio appears in the inverse of
the uncentered second moment matrix of the 'augmented' vector
of returns. Via the Central Limit Theorem and the delta method
the asymptotic distribution of the Markowitz portfolio can
be found. From this, Wald statistics on the individual portfolio
weights can be computed. Here I perform this computation on the
portfolio consisting of three large cap stocks, and find
that the Markowitz weighting of AAPL is significantly non-zero
(modulo the selection biases in universe construction). The
results are little changed when using a 'robust' covariance
estimator.


```r
some.rets <- get.rets(c("IBM", "AAPL", "XOM"), from = "2004-01-01", 
    to = "2013-08-01")

ism.wald <- function(X, vcov.func = vcov) {
    # negating returns is idiomatic to get + Markowitz
    ism <- ism_vcov(-as.matrix(X), vcov.func = vcov.func)
    ism.mu <- ism$mu[1:ism$p]
    ism.Sg <- ism$Ohat[1:ism$p, 1:ism$p]
    retval <- ism.mu/sqrt(diag(ism.Sg))
    dim(retval) <- c(ism$p, 1)
    rownames(retval) <- rownames(ism$mu)[1:ism$p]
    return(retval)
}

wald.stats <- ism.wald(some.rets)
print(t(wald.stats))
```

```
##        IBM AAPL  XOM
## [1,] -0.22  2.9 0.15
```

```r

if (require(sandwich)) {
    wald.stats <- ism.wald(some.rets, vcov.func = sandwich::vcovHAC)
    print(t(wald.stats))
}
```

```
##        IBM AAPL  XOM
## [1,] -0.21  2.8 0.16
```

