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

# helper
is.sorted <- function(xs,pragma=c("ascending","descending")) {
	pragma <- match.arg(pragma)
	retv <- switch(pragma,
								 ascending = !is.unsorted(xs),
								 descending = !is.unsorted(rev(xs)))
	return(retv)
}

test_that("pfoo/qfoo monotonicity",{#FOLDUP
	set.seed(as.integer(charToRaw("1ccb4a05-fd09-43f7-a692-80deebfd67f4")))
	
	# psr
	ps <- seq(0.1,0.9,length.out=9)
	for (df in c(256,1024)) {
		for (snr in c(0,1)) {
			for (opy in c(1,52)) {
				for (lp in c(TRUE,FALSE)) {
					if (lp) { checkps <- log(ps) } else { checkps <- ps }
					for (lt in c(TRUE,FALSE)) {
						qs <- qsr(checkps, df, snr, opy, lower.tail=lt, log.p=lp)
						if (lt) { 
							expect_true(is.sorted(qs,pragma="ascending"))
						} else {
							expect_true(is.sorted(qs,pragma="descending"))
						}
						pret <- psr(qs, df, snr, opy, lower.tail=lt, log.p=lp)
						expect_equal(checkps,pret)
					}
				}
			}
		}
	}
	# pT2
	ps <- seq(0.1,0.9,length.out=9)
	for (df1 in c(2,4,8)) {
		for (df2 in c(256,1024)) {
			for (delta2 in c(0,0.02)) {
				for (lp in c(TRUE,FALSE)) {
					if (lp) { checkps <- log(ps) } else { checkps <- ps }
					for (lt in c(TRUE,FALSE)) {
						qs <- qT2(checkps, df1, df2, delta2, lower.tail=lt, log.p=lp)
						if (lt) { 
							expect_true(is.sorted(qs,pragma="ascending"))
						} else {
							expect_true(is.sorted(qs,pragma="descending"))
						}
						pret <- pT2(qs, df1, df2, delta2, lower.tail=lt, log.p=lp)
						expect_equal(checkps,pret)
					}
				}
			}
		}
	}
	# psrstar
	ps <- seq(0.1,0.9,length.out=9)
	for (df1 in c(2,4,8)) {
		for (df2 in c(256,1024)) {
			for (snrstar in c(0,0.05)) {
				for (opy in c(1,2)) {
					for (drag in c(0,0.1)) {
						for (lp in c(TRUE,FALSE)) {
							if (lp) { checkps <- log(ps) } else { checkps <- ps }
							for (lt in c(TRUE,FALSE)) {
								qs <- qsrstar(checkps, df1, df2, snrstar, opy, drag, lower.tail=lt, log.p=lp)
								if (lt) { 
									expect_true(is.sorted(qs,pragma="ascending"))
								} else {
									expect_true(is.sorted(qs,pragma="descending"))
								}
								pret <- psrstar(qs, df1, df2, snrstar, opy, drag, lower.tail=lt, log.p=lp)
								expect_equal(checkps,pret)
							}
						}
					}
				}
			}
		}
	}
	# plambdap
	ps <- seq(0.1,0.9,length.out=9)
	for (df in c(4,8,16)) {
		for (tstat in c(-1,0,1)) {
			for (lp in c(TRUE,FALSE)) {
				if (lp) { checkps <- log(ps) } else { checkps <- ps }
				for (lt in c(TRUE,FALSE)) {
					qs <- qlambdap(checkps, df, tstat, lower.tail=lt, log.p=lp)
					if (lt) { 
						expect_true(is.sorted(qs,pragma="ascending"))
					} else {
						expect_true(is.sorted(qs,pragma="descending"))
					}
					pret <- plambdap(qs, df, tstat, lower.tail=lt, log.p=lp)
					expect_equal(checkps,pret,tolerance=1e-4)
				}
			}
		}
	}
})#UNFOLD

test_that("pfoo/qfoo parameter monotonicity",{#FOLDUP
	set.seed(as.integer(charToRaw("5274138f-8db4-46bc-b015-08442166f22c")))
	
	# psr:
	# snrs
	snrs <- seq(-1,1,length.out=11)
	ps <- 0.5
	for (df in c(256,1024)) {
		for (opy in c(1,12)) {
			for (lp in c(TRUE,FALSE)) {
				if (lp) { checkps <- log(ps) } else { checkps <- ps }
				for (lt in c(TRUE,FALSE)) {
					qs <- qsr(checkps, df, snrs, opy, lower.tail=lt, log.p=lp)
					expect_true(is.sorted(qs,pragma="ascending"))
				}
			}
		}
	}
	# opy
	# in this case the nct ncp is decreasing so the bias is as well...
	opy <- c(1,2,4,12,52,253)
	ps <- 0.5
	for (df in c(256,1024)) {
		for (snr in c(0,1)) {
			for (lp in c(TRUE,FALSE)) {
				if (lp) { checkps <- log(ps) } else { checkps <- ps }
				for (lt in c(TRUE,FALSE)) {
					qs <- qsr(checkps, df, snr, opy, lower.tail=lt, log.p=lp)
					expect_true(is.sorted(qs,pragma="descending"))
				}
			}
		}
	}
	# df
	# in this case the bias should decrease in the df...
	# see also Ghosh, B. K. "Some monotonicity theorems for χ 2, 
	# F and t distributions with applications." Journal of the Royal 
	# Statistical Society. Series B (Methodological) (1973): 480-492.
	df <- c(24,52,256,512,1024)
	ps <- 0.5
	for (opy in c(52,256)) {
		for (snr in c(0,1)) {
			for (lp in c(TRUE,FALSE)) {
				if (lp) { checkps <- log(ps) } else { checkps <- ps }
				for (lt in c(TRUE,FALSE)) {
					qs <- qsr(checkps, df, snr, opy, lower.tail=lt, log.p=lp)
					expect_true(is.sorted(qs,pragma="descending"))
				}
			}
		}
	}
	
	## pT2
	#ps <- seq(0.1,0.9,length.out=9)
	#for (df1 in c(2,4,8)) {
		#for (df2 in c(256,1024)) {
			#for (delta2 in c(0,0.02)) {
				#for (lp in c(TRUE,FALSE)) {
					#if (lp) { checkps <- log(ps) } else { checkps <- ps }
					#for (lt in c(TRUE,FALSE)) {
						#qs <- qT2(checkps, df1, df2, delta2, lower.tail=lt, log.p=lp)
						#if (lt) { 
							#expect_true(!is.unsorted(qs))
						#} else {
							#expect_true(!is.unsorted(rev(qs)))
						#}
						#pret <- pT2(qs, df1, df2, delta2, lower.tail=lt, log.p=lp)
						#expect_equal(checkps,pret)
					#}
				#}
			#}
		#}
	#}
	## psrstar
	#ps <- seq(0.1,0.9,length.out=9)
	#for (df1 in c(2,4,8)) {
		#for (df2 in c(256,1024)) {
			#for (snrstar in c(0,0.05)) {
				#for (opy in c(1,2)) {
					#for (drag in c(0,0.1)) {
						#for (lp in c(TRUE,FALSE)) {
							#if (lp) { checkps <- log(ps) } else { checkps <- ps }
							#for (lt in c(TRUE,FALSE)) {
								#qs <- qsrstar(checkps, df1, df2, snrstar, opy, drag, lower.tail=lt, log.p=lp)
								#if (lt) { 
									#expect_true(!is.unsorted(qs))
								#} else {
									#expect_true(!is.unsorted(rev(qs)))
								#}
								#pret <- psrstar(qs, df1, df2, snrstar, opy, drag, lower.tail=lt, log.p=lp)
								#expect_equal(checkps,pret)
							#}
						#}
					#}
				#}
			#}
		#}
	#}
	## plambdap
	#ps <- seq(0.1,0.9,length.out=9)
	#for (df in c(4,8,16)) {
		#for (tstat in c(-1,0,1)) {
			#for (lp in c(TRUE,FALSE)) {
				#if (lp) { checkps <- log(ps) } else { checkps <- ps }
				#for (lt in c(TRUE,FALSE)) {
					#qs <- qlambdap(checkps, df, tstat, lower.tail=lt, log.p=lp)
					#if (lt) { 
						#expect_true(!is.unsorted(qs))
					#} else {
						#expect_true(!is.unsorted(rev(qs)))
					#}
					#pret <- plambdap(qs, df, tstat, lower.tail=lt, log.p=lp)
					#expect_equal(checkps,pret,tolerance=1e-4)
				#}
			#}
		#}
	#}
})#UNFOLD
# 2FIX: monotonicity wrt parameters...

test_that("qlambdap sensible",{#FOLDUP
	set.seed(as.integer(charToRaw("77141b84-05df-4e90-a726-a7788b9d89bf")))
	
	df <- 128
	true.ncp <- 3
	tvals <- rt(4096,df,true.ncp)

	for (p in c(0.05,0.25,0.5,0.75,0.95)) {
		tstat <- sapply(tvals,function(t) { return(qlambdap(p,df,t)) })
		expect_equal(mean(tstat >= true.ncp),p,tolerance=0.05)
	}
})#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
