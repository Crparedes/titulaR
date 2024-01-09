pround <- function (x, digits = 4) {
  if (round(x, digits) == 0) {
    return(paste0('< 1 e-0', digits))
  } else {
  	return(round(x, digits))
  }
}

seedNorm <- function (n = 500, mean = 0, sd = 1, seed = 0) {
  set.seed(seed)
  return(rnorm(n = n, mean = mean, sd = sd))
}

CNTR <- function(x) {return(column(12, align="center", x))}

cnclsn <- function(x, y = 'ref'){
  if(y == 'ref') {
    if (x == 'two.sided') return('diferente del valor de referencia.')
    if (x == 'less')      return('menor al valor de referencia.')
    if (x == 'greater')   return('mayor al valor de referencia.')
  } else {
    if (x == 'two.sided') return('diferente de la del segundo grupo.')
    if (x == 'less')      return('menor de la del segundo grupo.')
    if (x == 'greater')   return('mayor de la del segundo grupo.')
  }
}

mode <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found on RSiteSearch)	

x <- data
lim.inf=min(x)-1; lim.sup=max(x)+1
#hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.2))
s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
#n<-length(s$y)
#v1<-s$y[1:(n-2)];
#v2<-s$y[2:(n-1)];
#v3<-s$y[3:n]
#ix<-1+which((v1<v2)&(v2>v3))
#lines(s$x,s$y,col="red")
#points(s$x[ix],s$y[ix],col="blue")
md <- s$x[which(s$y==max(s$y))] 
md
}


Col2Num <- function(x) as.numeric(gsub("[^0-9]", "",  x))

ifelseNV <- function(test, yes, no) {
  if (test) return(yes) else return(no)
}
