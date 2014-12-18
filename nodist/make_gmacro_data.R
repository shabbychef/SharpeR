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

#curl -o data.csv 'https://raw.github.com/datasets/s-and-p-500/master/data/data.csv'
#shiller.data <- read.https.csv("https://raw.github.com/datasets/s-and-p-500/master/data/data.csv",
															 #stringsAsFactors=FALSE)

shiller.data <- read.csv("sp500_data.csv",
															 stringsAsFactors=FALSE)
shill.xts <- xts(shiller.data[,!colnames(shiller.data) %in% c("Date")],
								 order.by=alignMonthly(as.Date(shiller.data$Date),include.weekends=TRUE))


require(Quandl)

# Quandl
if (require(utils) && require(Quandl)) {
	quandl.auth <- Sys.getenv('QUANDL_AUTH')
	options(quandl.auth = ifelse(nchar(quandl.auth),quandl.auth,""))
	rm(quandl.auth)
	Quandl.auth(options()$quandl.auth)
}

dp.xts <- Quandl("YALE/SP_PER10Y",
	start_date="1881-12-31",end_date="2013-12-31",
	type="xts")
ff10.xts <- Quandl("KFRENCH/10_IND_PORTF_M",
	start_date="1926-07-31",end_date="2012-12-31",
	type="xts")
ff5.xts <- Quandl("KFRENCH/5_IND_PORTF_M",
	start_date="1926-07-31",end_date="2012-12-31",
	type="xts")
fff.xts <- Quandl("KFRENCH/FACTORS_M",
	start_date="1926-07-31",end_date="2013-10-31",
	type="xts")

# o.... 2FIX
save(ff5.xts,ff10.xts,fff.xts,dp.xts,shill.xts,file='gmacro_data.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
