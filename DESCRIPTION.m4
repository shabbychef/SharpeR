define(`m4_CHOMP',`substr($1,0,eval(len($1)-1))')dnl
define(`m4_CHOMP_SYS',`m4_CHOMP(`esyscmd($1)')')dnl
dnl holy fuck. http://mbreen.com/m4.html#quotemacro 
define(`LQ',`changequote(<,>)`dnl'
changequote`'')
define(`RQ',`changequote(<,>)dnl`
'changequote`'')
define(`m4_R_FILES',`m4_CHOMP_SYS(`ls  R/*.r | perl -pe "s{^R/}{    `'RQ()};s{$}{`'RQ()};";')')dnl
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
    R (>= 2.4.0)
Suggests: 
    quantmod,
    MASS,
		TTR,
    testthat, 
    knitr
URL: http://www.r-project.org, https://github.com/shabbychef/SharpeR
VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4
