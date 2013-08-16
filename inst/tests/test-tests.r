# Copyright 2013 Steven E. Pav. All Rights Reserved.
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

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2013.08.14
# Copyright: Steven E. Pav, 2013-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav

# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
THOROUGHNESS <- getOption('test.thoroughness',1.0)
#UNFOLD

context("test hypothesis tests")#FOLDUP
test_that("sr_equality_test uniformity",{#FOLDUP
	ngen <- ceiling(THOROUGHNESS * 32)
	alpha.floor = 0.001 + 0.003 * (THOROUGHNESS / (1 + THOROUGHNESS))

	set.char.seed("0b144107-4de8-4e00-95f7-d746db3aef8e")
	ope <- 253
	for (nyr in c(2,4)) {
		nday <- ceiling(ope * nyr)
		for (nstock in c(2,4)) {
			for (mu in c(0,0.1)) {
				pvs <- rep(0,ngen)
				for (iii in (1:ngen)) {
					X <- matrix(rnorm(nday * nstock,mean=mu),ncol=nstock)
					etc <- sr_equality_test(X)
					pvs[iii] <- etc$p.value
				}
				meta.test <- ks.test(pvs,punif)
				expect_true(meta.test$p.value > alpha.floor)
			}
		}
	}

	if (require(sandwich)) {
		ngen <- ceiling(THOROUGHNESS * 16)

		set.char.seed("d1cbe090-dca6-4543-8332-1da96f1665a8")
		ope <- 253
		for (nyr in c(1,2)) {
			nday <- ceiling(ope * nyr)
			for (nstock in c(2)) {
				for (mu in c(0,0.1)) {
					pvs <- rep(0,ngen)
					for (iii in (1:ngen)) {
						X <- matrix(rnorm(nday * nstock,mean=mu),ncol=nstock)
						etc <- sr_equality_test(X,vcov.func=vcovHAC)
						pvs[iii] <- etc$p.value
					}
					meta.test <- ks.test(pvs,punif)
					expect_true(meta.test$p.value > alpha.floor)
				}
			}
		}
	}


})#UNFOLD

test_that("sr_test uniformity",{#FOLDUP
	ngen <- ceiling(THOROUGHNESS * 64)
	alpha.floor = 0.001 + 0.003 * (THOROUGHNESS / (1 + THOROUGHNESS))

	set.char.seed("3e4249f2-18d4-4d5c-98be-c32cc05e74c7")
	ope <- 253
	for (nyr in c(2,4)) {
		nday <- ceiling(ope * nyr)
		for (psi in c(0,0.1)) {
			for (sg in c(0.01,0.02)) {
				pvs <- rep(0,ngen)
				for (iii in (1:ngen)) {
					X <- rnorm(nday,mean=psi*sg,sd=sg)
					etc <- sr_test(X,zeta=psi)
					pvs[iii] <- etc$p.value
				}
				meta.test <- ks.test(pvs,punif)
				expect_true(meta.test$p.value > alpha.floor)
			}
		}
	}
})#UNFOLD

#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
