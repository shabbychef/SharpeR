# Copyright 2013 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of Ratarb.
#
# Ratarb is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Ratarb is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Ratarb.  If not, see <http://www.gnu.org/licenses/>.

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2013.01.10
# Copyright: Steven E. Pav, 2013-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

test_that("p function monotonicity",{
	set.seed(as.integer(charToRaw("1ccb4a05-fd09-43f7-a692-80deebfd67f4")))
	
	df <- 128
	q1 <- 0.2
	q2 <- 0.4
	for (opy in c(1,12,52,253)) {
		p1 <- psr(q1,df,opy=opy)
		p2 <- psr(q2,df,opy=opy)
		expect_true(p1 <= p2)
	}
})
test_that("qlambdap sensible",{
	set.seed(as.integer(charToRaw("77141b84-05df-4e90-a726-a7788b9d89bf")))
	
	df <- 128
	true.ncp <- 3
	tvals <- rt(4096,df,true.ncp)

	for (p in c(0.05,0.25,0.5,0.75,0.95)) {
		tstat <- sapply(tvals,function(t) { return(qlambdap(p,df,t)) })
		expect_equal(mean(tstat >= true.ncp),p,tolerance=0.05)
	}
})

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
