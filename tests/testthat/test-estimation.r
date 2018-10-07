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

context("code runs at all")#FOLDUP
test_that("basic sr functionality, numeric",{# FOLDUP
	set.char.seed("50e02c82-fdfc-4423-93ef-cbd350a65391")
	xret <- rnorm(100)
	expect_error(mysr <- as.sr(xret,higher_order=FALSE),NA)
	expect_error(print(mysr),NA)
	expect_error(print(format(mysr)),NA)
	expect_error(dummy <- reannualize(mysr,new.ope=2),NA)
	expect_true(is.sr(mysr))

	expect_error(myse <- se(mysr,"t"),NA)
	expect_error(myse <- se(mysr,"Lo"),NA)
	# these should throw b/c they do not have cumulants
	expect_error(myse <- se(mysr,"Mertens"))
	expect_error(myse <- se(mysr,"Bao"))
	# unknown method:
	expect_error(myse <- se(mysr,"FJNORD"))

	expect_error(ci1 <- confint(mysr),NA)
	expect_error(ci2 <- confint(mysr, level=0.9),NA)
	expect_error(ci3 <- confint(mysr, level.lo=0.1, level.hi=0.95),NA)
	expect_error(ci4 <- confint(mysr, type="exact"),NA)
	expect_error(ci5 <- confint(mysr, type="t"),NA)
	expect_error(ci6 <- confint(mysr, type="Z"),NA)
	# these should throw b/c they do not have cumulants
	expect_error(dci7 <- confint(mysr, type="Mertens"))
	expect_error(dci8 <- confint(mysr, type="Bao"))

	# compute cumulants and try higher order methods
	expect_error(mysr_h <- as.sr(xret,higher_order=TRUE),NA)
	expect_error(ci7 <- confint(mysr_h, type="Mertens"),NA)
	expect_error(ci8 <- confint(mysr_h, type="Bao"),NA)

	expect_error(myse <- se(mysr_h,"Mertens"),NA)
	expect_error(myse <- se(mysr_h,"Bao"),NA)
})# UNFOLD
test_that("basic sr functionality, Matrix",{# FOLDUP
	set.char.seed("4f080841-01cc-4d35-8e5f-cd3f9b786dc0")
	X <- matrix(rnorm(100*2),ncol=4)

	expect_error(mysr <- as.sr(X,higher_order=FALSE),NA)
	expect_error(print(mysr),NA)
	expect_error(print(format(mysr)),NA)
	expect_error(dummy <- reannualize(mysr,new.ope=2),NA)
	expect_true(is.sr(mysr))

	expect_error(myse <- se(mysr,"t"),NA)
	expect_error(myse <- se(mysr,"Lo"),NA)
	# these should throw b/c they do not have cumulants
	expect_error(myse <- se(mysr,"Mertens"))
	expect_error(myse <- se(mysr,"Bao"))
	# unknown method:
	expect_error(myse <- se(mysr,"FJNORD"))

	expect_error(ci1 <- confint(mysr),NA)
	expect_error(ci2 <- confint(mysr, level=0.9),NA)
	expect_error(ci3 <- confint(mysr, level.lo=0.1, level.hi=0.95),NA)
	expect_error(ci4 <- confint(mysr, type="exact"),NA)
	expect_error(ci5 <- confint(mysr, type="t"),NA)
	expect_error(ci6 <- confint(mysr, type="Z"),NA)
	# these should throw b/c they do not have cumulants
	expect_error(dci7 <- confint(mysr, type="Mertens"))
	expect_error(dci8 <- confint(mysr, type="Bao"))

	# compute cumulants and try higher order methods
	expect_error(mysr_h <- as.sr(X,higher_order=TRUE),NA)
	expect_error(ci7 <- confint(mysr_h, type="Mertens"),NA)
	expect_error(ci8 <- confint(mysr_h, type="Bao"),NA)

	expect_error(myse <- se(mysr_h,"Mertens"),NA)
	expect_error(myse <- se(mysr_h,"Bao"),NA)
})# UNFOLD
test_that("Matrix computations work in parallel",{# FOLDUP
	set.char.seed("4f080841-01cc-4d35-8e5f-cd3f9b786dc0")
	X <- matrix(rnorm(100*2),ncol=4)

	# now break them apart and combine them together
	expect_error(mysr_h <- as.sr(X,higher_order=TRUE),NA)
	expect_error(myse_t <- se(mysr_h,"t"),NA)
	expect_error(myse_l <- se(mysr_h,"Lo"),NA)
	expect_error(myse_m <- se(mysr_h,"Mertens"),NA)
	expect_error(myse_b <- se(mysr_h,"Bao"),NA)

	expect_error(myci_t <- confint(mysr_h, type="t"),NA)
	expect_error(myci_z <- confint(mysr_h, type="Z"),NA)
	expect_error(myci_m <- confint(mysr_h, type="Mertens"),NA)
	expect_error(myci_b <- confint(mysr_h, type="Bao"),NA)

	for (iii in 1:ncol(X)) {
		expect_error(subsr_h <- as.sr(X[,iii],higher_order=TRUE),NA)
		expect_error(subse_t <- se(subsr_h,"t"),NA)
		expect_error(subse_l <- se(subsr_h,"Lo"),NA)
		expect_error(subse_m <- se(subsr_h,"Mertens"),NA)
		expect_error(subse_b <- se(subsr_h,"Bao"),NA)

		expect_error(subci_t <- confint(subsr_h, type="t"),NA)
		expect_error(subci_z <- confint(subsr_h, type="Z"),NA)
		expect_error(subci_m <- confint(subsr_h, type="Mertens"),NA)
		expect_error(subci_b <- confint(subsr_h, type="Bao"),NA)

		expect_equal(as.numeric(subse_t),as.numeric(myse_t[iii]))
		expect_equal(as.numeric(subse_l),as.numeric(myse_l[iii]))
		expect_equal(as.numeric(subse_m),as.numeric(myse_m[iii]))
		expect_equal(as.numeric(subse_b),as.numeric(myse_b[iii]))

		expect_equal(as.numeric(subci_t),as.numeric(myci_t[iii,]))
		expect_equal(as.numeric(subci_z),as.numeric(myci_z[iii,]))
		expect_equal(as.numeric(subci_m),as.numeric(myci_m[iii,]))
		expect_equal(as.numeric(subci_b),as.numeric(myci_b[iii,]))
	}

})# UNFOLD
test_that("basic sr functionality, others",{# FOLDUP
	set.char.seed("76bd2b1d-1de1-4e93-b133-6a22310510e6")

	# via data.frame
	xdf <- data.frame(a=rnorm(500,1),b=rnorm(500,1));
	expect_error(mysr <- as.sr(xdf),NA)
	expect_error(print(mysr),NA)
	expect_error(print(format(mysr)),NA)
	expect_true(is.sr(mysr))

	# via lm
	set.char.seed("acd4df9a-6ce5-471f-bf7b-38899ea13f3f")
	xdf <- data.frame(AAPL=rnorm(500,1),SPY=rnorm(500,1));
	mylm <- lm(AAPL ~ SPY,data=xdf)
	expect_error(mysr <- as.sr(mylm),NA)
	expect_error(print(mysr),NA)
	expect_error(print(format(mysr)),NA)
	expect_true(is.sr(mysr))

	# via xts
	if (require(xts)) {
		set.char.seed("b2e7fbe0-0b6e-4231-a0dc-86d4a61f7022")
		X <- matrix(rnorm(100*2),ncol=4)
		xxts <- xts(X,order.by=as.Date(1:nrow(X),origin="1990-01-01"))
		expect_error(mysr <- as.sr(xxts),NA)
		expect_error(print(mysr),NA)
		expect_error(print(format(mysr)),NA)
		expect_true(is.sr(mysr))
		expect_error(dummy <- reannualize(mysr,new.ope=2),NA)

		# submit problems with Debian that I assume will go away...
		skip_on_cran()
		if (require(timeSeries)) {
			ats <- as.timeSeries(xxts)
			expect_error(mysr <- as.sr(ats),NA)
			expect_error(print(mysr),NA)
			expect_error(print(format(mysr)),NA)
			expect_true(is.sr(mysr))
			expect_error(dummy <- reannualize(mysr,new.ope=2),NA)
		}
	}
})# UNFOLD
test_that("basic sropt functionality",{# FOLDUP
	set.char.seed("943c439d-8f66-4f5e-a2a5-a9af7cf1b787")
	x <- matrix(rnorm(1000*10),ncol=10)
	mysr <- as.sropt(x)
	print(mysr)
	print(format(mysr))
	dummy <- reannualize(mysr,new.ope=2)
	expect_true(is.sropt(mysr))
	expect_error(ci1 <- confint(mysr),NA)
	expect_error(ci2 <- confint(mysr, level=0.9),NA)
	expect_error(ci3 <- confint(mysr, level.lo=0.1, level.hi=0.95),NA)

	expect_error(z1 <- inference(mysr, type="KRS"),NA)
	expect_error(z2 <- inference(mysr, type="MLE"),NA)
	expect_error(z3 <- inference(mysr, type="unbiased"),NA)

	sricv <- sric(mysr)
	sricv <- sric(dummy)

	# via xts
	if (require(xts)) {
		xxts <- xts(x,order.by=as.Date(1:(dim(x)[1]),origin="1990-01-01"))
		mysr <- as.sropt(xxts)
		print(mysr)
		print(format(mysr))
		expect_true(is.sropt(mysr))
		expect_error(dummy <- reannualize(mysr,new.ope=2),NA)
	}
})
test_that("basic del_sropt functionality",{
	set.char.seed("cdbfbd76-29f4-454f-b8a1-e3502ba287d7")
	x <- matrix(rnorm(1000*10,mean=3e-4),ncol=10)
	G <- matrix(rnorm(2*10),ncol=10)
	mysr <- as.del_sropt(x,G,drag=0,ope=1,epoch="yr")
	print(mysr)
	print(format(mysr))
	# not yet:
	#dummy <- reannualize(mysr,new.ope=2)
	expect_true(is.del_sropt(mysr))
	expect_error(ci1 <- confint(mysr),NA)
	expect_error(ci2 <- confint(mysr, level=0.9),NA)
	expect_error(ci3 <- confint(mysr, level.lo=0.1, level.hi=0.95),NA)

	expect_error(z1 <- inference(mysr, type="KRS"),NA)
	expect_error(z2 <- inference(mysr, type="MLE"),NA)
	expect_error(z3 <- inference(mysr, type="unbiased"),NA)

	# via xts
	if (require(xts)) {
		xxts <- xts(x,order.by=as.Date(1:(dim(x)[1]),origin="1990-01-01"))
		mysr <- as.del_sropt(xxts,G,drag=0,ope=1,epoch="yr")
		print(mysr)
		print(format(mysr))
		expect_true(is.del_sropt(mysr))
		#dummy <- reannualize(mysr,new.ope=2)
	}
})# UNFOLD
test_that("basic sr_vcov functionality",{# FOLDUP
	set.char.seed("de096679-85cc-438b-8335-96a9940a9021")
	for (p in c(1:3)) {
		X <- matrix(rnorm(1000*p,mean=3e-4),ncol=p)
		expect_error(S <- sr_vcov(X),NA)
	}
	X <- rnorm(1000)
	expect_error(S <- sr_vcov(X),NA)
})# UNFOLD
#UNFOLD
context("higher order moments")#FOLDUP
test_that("basic as.sr usage",{
	set.char.seed("a11f6c3d-c0f1-4282-a4ba-d6ebf990bdef")
	x <- rnorm(100)
	X <- matrix(rnorm(100*2),ncol=4)
	expect_error(mysr1 <- as.sr(x,higher_order=FALSE),NA)
	expect_error(mysr2 <- as.sr(x,higher_order=TRUE),NA)
	expect_equal(mysr1$sr,mysr2$sr)

	expect_error(mysr1 <- as.sr(X,higher_order=FALSE),NA)
	expect_error(mysr2 <- as.sr(X,higher_order=TRUE),NA)
	expect_equal(mysr1$sr,mysr2$sr)

	# via data.frame
	xdf <- data.frame(a=rnorm(500,1),b=rnorm(500,1));
	expect_error(mysr1 <- as.sr(xdf,higher_order=FALSE),NA)
	expect_error(mysr2 <- as.sr(xdf,higher_order=TRUE),NA)
	expect_equal(mysr1$sr,mysr2$sr)

	# via lm
	xdf <- data.frame(AAPL=rnorm(500,1),SPY=rnorm(500,1))
	mylm <- lm(AAPL ~ SPY,data=xdf)
	expect_error(mysr1 <- as.sr(mylm,higher_order=FALSE),NA)
	expect_error(mysr2 <- as.sr(mylm,higher_order=TRUE),NA)
	expect_warning(mysr2 <- as.sr(mylm,higher_order=TRUE))
	expect_equal(mysr1$sr,mysr2$sr)

	# via xts
	if (require(xts)) {
		xxts <- xts(X,order.by=as.Date(1:nrow(X),origin="1990-01-01"))
		expect_error(mysr1 <- as.sr(xxts,higher_order=FALSE),NA)
		expect_error(mysr2 <- as.sr(xxts,higher_order=TRUE),NA)
		expect_equal(mysr1$sr,mysr2$sr)

		# submit problems with Debian that I assume will go away...
		skip_on_cran()
		if (require(timeSeries)) {
			ats <- as.timeSeries(xxts)
			expect_error(mysr1 <- as.sr(ats,higher_order=FALSE),NA)
			expect_error(mysr2 <- as.sr(ats,higher_order=TRUE),NA)
			expect_equal(mysr1$sr,mysr2$sr)
		}
	}
})
# basically make sure each of the types gives about the same results
test_that("se not confounded by ope",{
	set.char.seed("8eb9aa6a-4435-46ba-bb60-1c8c39593218")
	x <- rnorm(1000)
	expect_error(mysr_h <- as.sr(x,higher_order=TRUE),NA,ope=252)

	expect_error(myse_t <- se(mysr_h,"t"),NA)
	expect_error(myse_l <- se(mysr_h,"Lo"),NA)
	expect_error(myse_m <- se(mysr_h,"Mertens"),NA)
	expect_error(myse_b <- se(mysr_h,"Bao"),NA)
	expect_lt(abs(log(myse_t / myse_l)),0.2)
	expect_lt(abs(log(myse_t / myse_m)),0.2)
	expect_lt(abs(log(myse_t / myse_b)),0.2)

	set.char.seed("8eb9aa6a-4435-46ba-bb60-1c8c39593218")
	x <- rnorm(1000,mean=4)
	expect_error(mysr_h <- as.sr(x,higher_order=TRUE),NA,ope=252)

	expect_error(myci_e <- confint(mysr_h,type="exact"),NA)
	expect_error(myci_t <- confint(mysr_h,type="t"),NA)
	expect_error(myci_z <- confint(mysr_h,type="Z"),NA)
	expect_error(myci_m <- confint(mysr_h,type="Mertens"),NA)
	expect_error(myci_b <- confint(mysr_h,type="Bao"),NA)

	expect_lt(max(abs(log(myci_e / myci_t))),0.2)
	expect_lt(max(abs(log(myci_e / myci_z))),0.2)
	expect_lt(max(abs(log(myci_e / myci_m))),0.2)
	expect_lt(max(abs(log(myci_e / myci_b))),0.2)

})

#UNFOLD
context("estimation functions: confint coverage")#FOLDUP
test_that("confint.sr coverage",{#FOLDUP
	set.char.seed("066dfa96-6dd7-4d14-ab74-49e81b3afd83")

	ngen <- ceiling(THOROUGHNESS * 32)
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
context("estimation functions: prediction intervals")#FOLDUP
test_that("predint right output",{#FOLDUP
	set.char.seed("0919609e-4e99-42f2-b04c-89e2d2faaa4f")

	for (nasset in c(1,2,4,8)) {
		x <- matrix(rnorm(100*nasset),ncol=nasset)
		srx <- as.sr(x)
		aci <- predint(srx,oosdf=500)
		expect_equal(nrow(aci),nasset)
		expect_equal(ncol(aci),2)
		expect_true(is.matrix(aci))
	}
})#UNFOLD
test_that("predint runs at all",{#FOLDUP
	set.char.seed("080c6f73-834e-4d10-a6fa-4b27dc266b24")

	ngen <- ceiling(THOROUGHNESS * 32)
	alpha.tol = 0.05 + 0.10 / THOROUGHNESS

	isn <- 200
	oosn <- 100

	ope <- 253
	sg <- 0.013
	for (nyr in c(3,6,9)) {
		df <- ceiling(ope * nyr)
		for (zeta in c(-1.0,0.0,1.0,2.0)) {
			x <- rnorm(isn,mean=(zeta/sqrt(ope))*sg,sd=sg)
			#y <- rnorm(oosn,mean=(zeta/sqrt(ope))*sg,sd=sg)
			for (nominal.coverage in c(0.90,0.95)) {
				aci <- predint(x,oosdf=oosn-1,ope=1,level=nominal.coverage)
				aci2 <- predint(as.sr(x),oosdf=oosn-1,ope=1,level=nominal.coverage)
				# no ope:
				noaci <- predint(x,oosdf=oosn-1,level=nominal.coverage)
			}
			# corner cases:
			iinf <- predint(x,oosdf=oosn-1,level.lo=0,level.hi=1)
			expect_true(all(is.infinite(iinf)))
		}
	}
})#UNFOLD
test_that("predint not fooled by annualization",{#FOLDUP
	set.char.seed("94cbb2a6-b497-48d4-9139-e987364f8a0f")

	isn <- 200
	oosn <- 100
	ope <- 253
	sg <- 0.013
	zeta <- 1
	x <- rnorm(isn,mean=(zeta/sqrt(ope))*sg,sd=sg)
	srx1 <- as.sr(x,ope=1)
	srx2 <- as.sr(x,ope=ope)
	nominal.coverage <- 0.90
	aci1 <- predint(srx1,oosdf=oosn-1,ope=1,level=nominal.coverage)
	aci2 <- predint(srx2,oosdf=oosn-1,ope=1,level=nominal.coverage)
	errs <- unlist(aci1) - unlist(aci2)

	expect_less_than(max(abs(errs)),1e-4)
})#UNFOLD
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
