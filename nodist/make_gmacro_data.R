# Copyright 2012-2013 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

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

require(timeSeries)
require(timeDate)
require(xts)

# * Fri Dec 20 2013 21:26:41 PM Steven E. Pav <steven@cerebellumcapital.com>
# precache any market data we need for the vignette

read.https.csv <- function(url,...) {
	tpf <- tempfile()
	download.file(url,destfile=tpf, method="curl")
	retval <- read.csv(tpf,...)
	return(retval)
}

shiller.data <- read.https.csv("https://raw.github.com/datasets/s-and-p-500/master/data/data.csv",
															 stringsAsFactors=FALSE)
shill.xts <- xts(shiller.data[,!colnames(shiller.data) %in% c("Date")],
								 order.by=alignMonthly(as.Date(shiller.data$Date),include.weekends=TRUE))


# 
# load data from quandl.
get.quandl <- function(urlend,date.col='Date',colClasses=NULL,...) {
	furl <- paste('http://www.quandl.com/api/v1/datasets/',
								urlend,sep="")

	if (is.null(colClasses)) {
		colClasses <- c('Date')
		names(colClasses) <- date.col
	}

	df.dat <- read.csv(furl, colClasses=colClasses, ...)
	df.xts <- xts(df.dat[,!colnames(df.dat) %in% date.col],
								order.by=df.dat[,date.col])
	return(df.xts)
}

ff10.xts <- get.quandl('KFRENCH/10_IND_PORTF_M.csv?&auth_token=EsppzNgJHtEGaH2gqepZ&trim_start=1926-07-31&trim_end=2012-12-31&sort_order=desc')
ff5.xts <- get.quandl('KFRENCH/5_IND_PORTF_M.csv?&auth_token=EsppzNgJHtEGaH2gqepZ&trim_start=1926-07-31&trim_end=2012-12-31&sort_order=desc')
fff.xts <- get.quandl('KFRENCH/FACTORS_M.csv?&auth_token=EsppzNgJHtEGaH2gqepZ&trim_start=1926-07-31&trim_end=2013-10-31&sort_order=desc', date.col='Month')

dp.xts <- get.quandl('YALE/SP_PER10Y.csv?&auth_token=EsppzNgJHtEGaH2gqepZ&trim_start=1881-12-31&trim_end=2013-12-31&sort_order=desc', date.col='Year')


# o.... 2FIX
save(ff5.xts,ff10.xts,fff.xts,dp.xts,shill.xts,file='gmacro_data.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
