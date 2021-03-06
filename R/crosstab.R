# Author: Robert J. Hijmans
# Date : March 2009
# Version 1.0
# Licence GPL v3

# revised April 2011

if (!isGeneric("crosstab")) {
	setGeneric("crosstab", function(x, y, ...)
		standardGeneric("crosstab"))
}


setMethod('crosstab', signature(x='Raster', y='Raster'), 
	function(x, y, digits=0, long=FALSE, useNA=FALSE, progress='', ...) {
		x <- stack(x, y)
		crosstab(x, digits=digits, long=long, useNA=useNA, progress=progress, ...) 
	}
)


setMethod('crosstab', signature(x='RasterStackBrick', y='missing'), 
	function(x, digits=0, long=FALSE, useNA=FALSE, progress='', ...) {

		nl <- nlayers(x)
		if (nl < 2) {
			stop('crosstab needs at least 2 layers')
		}
		nms <- names(x)
		
		if (canProcessInMemory(x)) {
			res <- getValues(x)
			res <- lapply(1:nl, function(i) round(res[, i], digits=digits))
			res <- do.call(table, c(res, useNA='always'))
			res <- as.data.frame(res)
			
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='crosstab', progress=progress)	
			res <- NULL
			for (i in 1:tr$n) {
				d <- getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])
				d <- lapply(1:nl, function(i) round(d[, i], digits=digits))
				d <- do.call(table, c(d, useNA='always'))
				d <- as.data.frame(d)
				res <- rbind(res, d)
				pbStep(pb, i)
			}
			pbClose(pb)
			
			if (nrow(res) == 0) {
				res <- data.frame(matrix(nrow=0, ncol=length(nms)+1))
			} 
			colnames(res) <- c(nms, 'Freq')
			
			if (! useNA ) {
				i <- which(apply(res, 1, function(x) sum(is.na(x))>0))				
				res <- res[-i,  ,drop=FALSE]
			}
 
			# keep NA classes if there are any
			for (i in 1:(ncol(res)-1)) {
				if (any(is.na(res[,i]))) {
					res[,i] <- factor(res[,i], levels=c(levels(res[,i]), NA), exclude=NULL) 
				}
			}
			f <- eval(parse(text=paste('Freq ~ ', paste(nms , collapse='+'))))
			res <- xtabs(f, data=res)
			
		}
		
		if (long) {
			if (nrow(res) > 1) {
				res <- data.frame(res)
				colnames(res) <- c(nms, 'Freq')	
				res <- res[res$Freq > 0,  ,drop=FALSE]
			}
		} 
		return(res)
	}
)




.oldcrosstab <- function(x, y, digits=0, long=FALSE, progress, ...) {
# old function, not used any more	
		compareRaster(c(x, y))
		if (missing(progress)) { progress <- .progress() }

		if (canProcessInMemory(x, 3) | ( inMemory(x) & inMemory(y) )) {
			res <- table(first=round(getValues(x), digits=digits), second=round(getValues(y), digits=digits), ...) 
		} else {
			res <- NULL
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='crosstab', progress=progress)	
			for (i in 1:tr$n) {
			
				d <- table( round(getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]), digits=digits), round(getValuesBlock(y, row=tr$row[i], nrows=tr$nrows[i]), digits=digits), ...)
				if (length(dim(d))==1) {
					first = as.numeric(names(d))
					second = first
					d <- matrix(d)
				} else {
					first = as.numeric(rep(rownames(d), each=ncol(d)))
					second = as.numeric(rep(colnames(d), times=nrow(d)))
				}
				count = as.vector(t(d))
				res = rbind(res, cbind(first, second, count))
				pbStep(pb, i)
			}
			pbClose(pb)
			res = xtabs(count~first+second, data=res)
		}
		
		if (long) {
			return( as.data.frame(res) )
		} else {
			return(res)
		}
}
