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

context("estimation functions")

set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}

test_that("confint.sr coverage",{#FOLDUP
	set.char.seed("066dfa96-6dd7-4d14-ab74-49e81b3afd83")

	ope <- 253
	df <- ope * 6
	zeta <- 1.0
	rvs <- rsr(500, df, zeta, ope)
	roll.own <- sr(sr=rvs,df=df,c0=0,ope=ope)
	test.coverage <- 0.95
	aci <- confint(roll.own,level=test.coverage)
	coverage <- 1 - mean((zeta < aci[,1]) | (aci[,2] < zeta))
	expect_equal(coverage,test.coverage,tolerance=0.05)
})#UNFOLD

test_that("confint.sropt coverage",{#FOLDUP
	set.char.seed("a5b0fb75-b50a-41a4-85d6-cd990ac0b9b2")

	ope <- 253
	df1 <- 6
	df2 <- ope * 6
	zeta.s <- 1.0
	rvs <- rsropt(800, df1, df2, zeta.s, ope)
	roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope)
	test.coverage <- 0.95
	aci <- confint(roll.own,level=test.coverage)
	coverage <- 1 - mean((zeta.s < aci[,1]) | (aci[,2] < zeta.s))
	expect_equal(coverage,test.coverage,tolerance=0.05)
})#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
