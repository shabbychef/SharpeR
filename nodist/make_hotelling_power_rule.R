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

# * Sun Oct 13 2013 08:51:52 PM Steven E. Pav <steven@cerebellumcapital.com>
# pre-run the hotelling study b/c it takes too damn long.

mc.resolution <- 500
dpy <- 253
lseq <- function(from,to,length.out) { 
	exp(seq(log(from),log(to),length.out = length.out))
}

# the power of an f-test 
f_fpower <- function(df1,df2,ncp,alpha = 0.05) {
	pf(qf(alpha,df1=df1,df2=df2,ncp=0,lower.tail=FALSE),df1 = df1,df2 = df2,ncp = ncp,lower.tail = FALSE)
}

# the power of the Hotelling test
f_hpower <- function(n,p,rhosq,alpha = 0.05) {
	f_fpower(df1 = p,df2 = n - p,ncp = n * rhosq,alpha = alpha)
}
# find the sample size for a given power of the univariate hotelling test
f_hreqsize <- function(rhosq,p,powr = 0.80,alpha = 0.05) {
	#2FIX: get a sane upper bound!
	zz <- uniroot(function(n,p,rhosq,alpha,powr)(f_hpower(n,p,rhosq,alpha) - powr),
								c(max(8,p+1),2000000 * 10 / (rhosq)),p = p,rhosq = rhosq,powr = powr,alpha = alpha)
	return(zz$root)
}
#run the sample size computation for multiple values of rhosq and p
f_pows <- function(powr,rhosq,pvals) {
	ssizes <- sapply(pvals,function(p) {
									sapply(rhosq,f_hreqsize,p=p,powr=powr,alpha=0.05) } )
	ssizes <- as.vector(ssizes)
	allp <- as.vector(kronecker(t(pvals),matrix(1,1,length(rhosq))))
	allr <- as.vector(kronecker(rhosq,matrix(1,1,length(pvals))))
	allpow <- as.vector(kronecker(powr,matrix(1,1,length(allr))))
	retme = list(ssizes,allp,allr,allpow)
}

#run a regression on the results of such a study;
f_pow_reg <- function(powr,rhosq,pvals) {
	all_data <- f_pows(powr,rhosq,pvals)
	ssizes <- unlist(all_data[1])
	allp <- unlist(all_data[2])
	allr <- unlist(all_data[3])
	lap2 <- log(allp) ^ 2
	best_model <- lm(log(ssizes) + log(allr) ~ log(allp) + lap2)
}

#Hotelling power law
rhosq <- seq(0,2.5 / sqrt(dpy),length.out = 100) ^ 2
rhosq <- rhosq[rhosq != 0]
rhosq <- lseq(rhosq[1],rhosq[length(rhosq)],length.out = ceiling(1.024 * mc.resolution))

pvals <- lseq(1,512,length.out = ceiling(64 * mc.resolution / 1000))

mod25 <- f_pow_reg(0.25,rhosq,pvals)
mod50 <- f_pow_reg(0.50,rhosq,pvals)
mod80 <- f_pow_reg(0.80,rhosq,pvals)

#hot.power <- data.frame("inter" = c(mod25$coefficients[1],mod50$coefficients[1],mod80$coefficients[1]),
#									"p" = c(mod25$coefficients[2],mod50$coefficients[2],mod80$coefficients[2]),
#									"logp" = c(mod25$coefficients[3],mod50$coefficients[3],mod80$coefficients[3]),
#									row.names = c("power = 0.25","power = 0.50","power = 0.80"))
hot.power <- data.frame("numerator" = c(
														 sprintf('$%.2f p^{%.3f + %.3f \\log p}$',round(exp(mod25$coefficients[1]),digits=2),
																		 round(mod25$coefficients[2],digits=3),round(mod25$coefficients[3],digits=4)),
														 sprintf('$%.2f p^{%.3f + %.3f \\log p}$',round(exp(mod50$coefficients[1]),digits=2),
																		 round(mod50$coefficients[2],digits=3),round(mod50$coefficients[3],digits=4)),
														 sprintf('$%.2f p^{%.3f + %.3f \\log p}$',round(exp(mod80$coefficients[1]),digits=2),
																		 round(mod80$coefficients[2],digits=3),round(mod80$coefficients[3],digits=4)) ),
									row.names = c("power = 0.25","power = 0.50","power = 0.80"))

save(hot.power,mc.resolution,
		 file='hotelling_power_rule.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
