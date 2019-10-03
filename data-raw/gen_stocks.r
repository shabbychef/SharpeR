# /usr/bin/r
#
# Copyright 2019-2019 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav 
#
# This file is part of SharpeR.
#
# SharpeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SharpeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SharpeR.  If not, see <http://www.gnu.org/licenses/>.
#
# Created: 2019.10.02
# Copyright: Steven E. Pav, 2019
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

suppressMessages(library(docopt))       # we need docopt (>= 0.3) as on CRAN

doc <- "Usage: gen_stocks.r [-v] [-O <OUTFILE>]

-O OUTFILE --outfile=OUTFILE     Location of output data [default: stocks.csv]
-v --verbose                     Be more verbose
-h --help                        show this help text"

opt <- docopt(doc)

suppressMessages({
	library(readr)
	library(dplyr)
	library(tidyr)
	library(magrittr)
	library(quantmod)
	library(usethis)
})

# ugly but it works
get.ret <- function(sym,...) {
	OHLCV <- getSymbols(sym,auto.assign=FALSE,...)
	lrets <- diff(log(OHLCV[,paste(c(sym,"Adjusted"),collapse=".",sep="")]))
	names(lrets) <- sym
	# chomp first NA!
	lrets[-1,]
}
get.rets <- function(syms,...) { 
	some.rets <- do.call("cbind",lapply(syms,get.ret,...)) 
}
stock_returns <- get.rets(c("IBM","AAPL","SPY","XOM"),from='2000-01-01',to='2018-12-31')

usethis::use_data(stock_returns,overwrite=TRUE)

stock_returns %>% 
	as.data.frame() %>% 
	tibble::rownames_to_column(var='date') %>%
	readr::write_csv(opt$outfile)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
