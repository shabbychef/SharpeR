dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", 
    role=c("aut","cre"),
    email="shabbychef@gmail.com",
    comment = c(ORCID = "0000-0002-4197-6195")))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Statistical Significance of the Sharpe Ratio
BugReports: https://github.com/shabbychef/SharpeR/issues
Description: A collection of tools for analyzing significance of assets,
    funds, and trading strategies, based on the Sharpe ratio and overfit 
    of the same. Provides density, distribution, quantile and random generation 
    of the Sharpe ratio distribution based on normal returns, as well
    as the optimal Sharpe ratio over multiple assets. Computes confidence intervals
    on the Sharpe and provides a test of equality of Sharpe ratios based on 
    the Delta method. The statistical foundations of the Sharpe can be found in
    the author's Short Sharpe Course  <doi:10.2139/ssrn.3036276>.
Depends: 
    R (>= 3.0.0)
Imports: 
    matrixcalc,
    zoo,
    epsiwal,
    methods
dnl sadists (>= 0.2.0)
Suggests: 
    xtable,
    xts,
    timeSeries,
    quantmod,
    MASS,
    TTR,
    testthat, 
    sandwich,
    txtplot,
    knitr
URL: https://github.com/shabbychef/SharpeR
VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4:et
