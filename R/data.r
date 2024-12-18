# Copyright 2019-2025 Steven E. Pav. All Rights Reserved.
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
#
# Created: 2019.10.02
# Copyright: Steven E. Pav, 2019-2025
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @title Stock Returns Data
#' @description Nineteen years of daily log returns on three stocks and an ETF.
#' @usage data(stock_returns)
#' @format An \code{xts} object with 4777 observations and 4 columns. 
#' 
#' The columns are the daily log returns for the tickers IBM, AAPL, SPY and XOM, 
#' as sourced from Yahoo finance using the \code{quantmod} package.
#' Daily returns span from January, 2000 through December, 2018.  
#' Returns are \sQuote{log returns}, which are the differences of the logs of
#' daily adjusted closing price series, as defined by Yahoo finance (thus presumably
#' including adjustments for splits and dividends). Dates of observations are
#' the date of the second close defining the return, not the first.
#' 
#' @source 
#' Data were collected on October 2, 2019, from Yahoo finance using the
#' \code{quantmod} package.
#' 
#' @template etc
#' @note The author makes no guarantees regarding correctness of this data.
#' @name stock_returns
#' @rdname stock_returns
#' @docType data
#' @keywords data
#' @examples
#' if (require(xts)) {
#'  data(stock_returns)
#'  as.sr(stock_returns)
#' }
"stock_returns"

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
