dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Author: Steven E. Pav
Authors@R: c(person("Steven", "Pav", role=c("aut","cre"), 
    email="shabbychef@gmail.com"))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Statistical significance of Sharpe ratio
Description: a collection of tools for analyzing significance of trading 
    strategies, based on the Sharpe ratio and overfit of the same.
Depends: 
    R (>= 3.0.0)
Suggests: 
    quantmod,
    MASS,
    TTR,
    testthat, 
    sandwich,
    knitr
URL: http://www.r-project.org, https://github.com/shabbychef/SharpeR
VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4
