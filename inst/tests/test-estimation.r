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
# Created: 2013.04.26
# Copyright: Steven E. Pav, 2013-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav


# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
THOROUGHNESS <- getOption('test.thoroughness',1.0)
#UNFOLD

context("estimation functions: confint coverage")#FOLDUP
test_that("confint.sr coverage",{#FOLDUP
	set.char.seed("066dfa96-6dd7-4d14-ab74-49e81b3afd83")

	ngen <- ceiling(THOROUGHNESS * 64)
	alpha.tol = 0.05 + 0.10 / THOROUGHNESS

	ope <- 253
	for (nyr in c(3,6,9)) {
		df <- ceiling(ope * nyr)
		for (type in c("exact","t","Z")) {
			for (zeta in c(-1.0,0.0,1.0,2.0)) {
				rvs <- rsr(ngen, df, zeta, ope)
				roll.own <- sr(sr=rvs,df=df,c0=0,ope=ope)
				for (nominal.coverage in c(0.90,0.95)) {
					aci <- confint(roll.own,level=nominal.coverage,type=type)
					coverage <- 1 - mean((zeta < aci[,1]) | (aci[,2] < zeta))
					expect_equal(coverage,nominal.coverage,tolerance=alpha.tol,scale=1)
				}
			}
		}
	}
})#UNFOLD

test_that("confint.sropt coverage",{#FOLDUP
	set.char.seed("50c7aa74-9cec-4fef-980e-c25bfede8260")

	ngen <- ceiling(THOROUGHNESS * 64)
	alpha.tol = 0.05 + 0.10 / THOROUGHNESS

	ope <- 253
	for (df1 in c(2,4,8,16)) {
		for (nyr in c(3,6,9)) {
			df2 <- ceiling(ope * nyr)
			for (zeta.s in c(0.0,1.0,3.0)) {
				rvs <- rsropt(ngen, df1, df2, zeta.s, ope)
				roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope)
				for (nominal.coverage in c(0.90,0.95)) {
					aci <- confint(roll.own,level=nominal.coverage)
					coverage <- 1 - mean((zeta.s < aci[,1]) | (aci[,2] < zeta.s))
					#cat(sprintf('df1=%d,df2=%d,zeta.s=%g: nominal: %.2g%%; achieved: %.2g%%\n',df1,df2,zeta.s,
											#100*nominal.coverage,100*coverage))
					expect_equal(coverage,nominal.coverage,tolerance=alpha.tol,scale=1)
				}
			}
		}
	}
})#UNFOLD
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
