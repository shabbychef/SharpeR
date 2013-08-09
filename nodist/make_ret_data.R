# * Wed Aug 07 2013 01:01:22 PM Steven E. Pav <steven@cerebellumcapital.com>
# precache any market data we need for the vignette

require(quantmod)
options("getSymbols.warning4.0"=FALSE)

clear.symbolLookup <- function() {
	get.us <- names(getSymbolLookup())
	lapply(get.us,function(sym) { # what bother
				 symlist <- list(NULL)
				 names(symlist) <- c(sym)
				 setSymbolLookup(symlist) })
}

clear.symbolLookup()

yahoo.src <- c("IBM","XOM","SPY","AAPL",
							 "XLY","XLE","XLP","XLF","XLV",
							 "XLI","XLB","XLK","XLU")
blah <- lapply(yahoo.src,function(n) { 'yahoo' })
names(blah) <- yahoo.src
setSymbolLookup(blah)
rm(blah)

## do not preload; ack.
#get.us <- names(getSymbolLookup())
#lapply(get.us,function(sym) {
			 #myd <- getSymbols(sym,from='1999-01-01',auto.assign=FALSE)
			 #fname <- paste(sym,'.rda',sep='')
			 #save(myd,file=fname)
			#})

#lapply(get.us,function(sym) { # what bother
			 #symlist <- list('RData')
			 #names(symlist) <- c(sym)
			 #setSymbolLookup(symlist) })

# fuck
setSymbolLookup(DJIA='FRED')
get.us <- names(getSymbolLookup())

get.ret <- function(sym,warnings=FALSE,max.try=10,...) {
	# getSymbols.yahoo will barf sometimes; do a trycatch
  trynum <- 0
	while (!exists("OHCLV") && (trynum < max.try)) {
		trynum <- trynum + 1
		try(OHLCV <- getSymbols(sym,auto.assign=FALSE,
														warnings=warnings,...),silent=TRUE)
  }
	adj.names <- paste(c(sym,"Adjusted"),collapse=".",sep="")
	if (adj.names %in% colnames(OHLCV)) {
		adj.close <- OHLCV[,adj.names]
	} else {
		# for DJIA from FRED, say. 
		adj.close <- OHLCV[,sym]
	}
	rm(OHLCV)
	adj.close <- adj.close[!is.na(adj.close)]
	# rename it
	colnames(adj.close) <- c(sym)
	lrets <- diff(log(adj.close))
	#chop first
	lrets[-1,]
}
get.rets <- function(syms,...) { 
	some.rets <- do.call("cbind",lapply(syms,get.ret,...)) }

my.from <- '2003-01-01'
my.to <- '2013-01-01'
all.ret <- get.rets(get.us,from=my.from,to=my.to)
all.ret <- all.ret[paste(my.from,my.to,sep='::'),]

save(all.ret,file='ret_data.rda',compress='bzip2')

