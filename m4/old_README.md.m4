dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
# SharpeR

A number of utilities for dealing with Sharpe ratio, the Sharpe ratio of the
Markowitz portfolio, and, in general, overfit of trading strategies based on
(in-sample) Sharpe statistics.

-- Steven E. Pav, shabbychef@gmail.com

## Installation

The package is being shipped to CRAN. It can also be installed from github via devtools:

LQ()LQ()LQ()
# latest greatest
if (require(devtools))
	install_github(repo='SharpeR',username='shabbychef',ref='master')
# last release:
if (require(devtools))
	install_github(repo='SharpeR',username='shabbychef',ref='r`'VERSION()`'')
LQ()LQ()LQ()

