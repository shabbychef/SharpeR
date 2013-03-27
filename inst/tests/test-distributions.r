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
# Created: 2013.01.10
# Copyright: Steven E. Pav, 2013-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

# helpers
is.sorted <- function(xs,pragma=c("ascending","descending")) {
	pragma <- match.arg(pragma)
	retv <- switch(pragma,
								 ascending = !is.unsorted(xs),
								 descending = !is.unsorted(rev(xs)))
	return(retv)
}
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}

context("distribution functions")

test_that("psr/qsr monotonicity",{#FOLDUP
	set.char.seed("1ccb4a05-fd09-43f7-a692-80deebfd67f4")
	
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
})#UNFOLD

test_that("pT2/qT2 monotonicity",{#FOLDUP
	set.char.seed("f28b5cd4-dcdb-4e8e-9398-c79fb9038351")
	
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
})#UNFOLD

test_that("psrstar/qsrstar monotonicity",{#FOLDUP
	set.char.seed("22ad9afb-49c4-4f37-b32b-eab413b32750")
	
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
})#UNFOLD

test_that("plambdap/qlambdap monotonicity",{#FOLDUP
	set.char.seed("a5ae65f3-257a-47a3-af8e-46b34dfcebb0")
	
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

test_that("pcosrstar/qcosrstar monotonicity",{#FOLDUP
	set.char.seed("161f0496-0229-4013-a65e-ff7c8b236f4a")
	
	# cosrstar
	ps <- seq(0.05,0.95,length.out=9)
	for (df1 in c(2,4,8)) {
		for (df2 in c(256,1024)) {
			for (delta2 in c(0.72,1.2)) {  # this is the observed SR stat
				for (lp in c(TRUE,FALSE)) {
					if (lp) { checkps <- log(ps) } else { checkps <- ps }
					for (lt in c(TRUE,FALSE)) {
						qs <- qcosrstar(checkps, df1, df2, z.s=delta2, opy=1,
														lower.tail=lt, log.p=lp)
						if (lt) { 
							expect_true(is.sorted(qs,pragma="ascending"))
						} else {
							expect_true(is.sorted(qs,pragma="descending"))
						}
						pret <- pcosrstar(qs, df1, df2, z.s=delta2, opy=1,
															lower.tail=lt, log.p=lp)
						expect_true(ifelse(lp,all(pret <= 0),
															 all(0 <= pret) && all(pret <= 1)))
						expect_equal(checkps,pret,tolerance=0.001)
					}
				}
			}
		}
	}
})#UNFOLD

test_that("psr/qsr parameter monotonicity",{#FOLDUP
	set.char.seed("adc578c1-381c-428a-baae-8d5607732176")
	
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
	
})#UNFOLD
# 2FIX: other distributions monotonicity wrt parameters...
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

test_that("qlambdap sensible",{#FOLDUP
	set.char.seed("f54698f1-ec37-49a4-8463-d4209f25afbc")
	
	df <- 128
	true.ncp <- 3
	tvals <- rt(4096,df,true.ncp)

	for (p in c(0.05,0.25,0.5,0.75,0.95)) {
		tstat <- sapply(tvals,function(t) { return(qlambdap(p,df,t)) })
		expect_equal(mean(tstat >= true.ncp),p,tolerance=0.05)
	}

	# edge cases
	expect_true(Inf == qlambdap(1,df,1,lower.tail=TRUE))
	expect_true(-Inf == qlambdap(1,df,1,lower.tail=FALSE))
	expect_true(-Inf == qlambdap(0,df,1,lower.tail=TRUE))
	expect_true(Inf == qlambdap(0,df,1,lower.tail=FALSE))

	expect_true(1 == plambdap(Inf,df,1,lower.tail=TRUE))
	expect_true(1 == plambdap(-Inf,df,1,lower.tail=FALSE))
	expect_true(0 == plambdap(-Inf,df,1,lower.tail=TRUE))
	expect_true(0 == plambdap(Inf,df,1,lower.tail=FALSE))
})#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
