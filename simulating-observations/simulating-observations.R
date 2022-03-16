##
## simulate an age-structured population through time
## and obtain observations of age-length and length increments
## from population samples

## one realisation of the simulation consists in a list containing
## 1 - a matrix of the true"numbers-at-age
## 2 - age-length observations
## 3 - growth increment observations


simulate.pop.fct <- function(){

  set.seed(1973)

  ages <- 0:20
  years <- 1:120

  n.at.age <- matrix(rep(NA, length(ages)*(length(years))), nc=length(years), nr=length(ages))
  #dim(n.at.age)
  ## age0, i.e. recruitment
  #n.at.age[1,] <- rpois(ncol(n.at.age),100) * 1E06
  n.at.age[1,] <- rnbinom(ncol(n.at.age),size=2,mu=100) * 1E06

  ## natural mortality
  m <- 0.2

  # numbers-at-age matrix
  n.ages <- length(ages)
  n.years <- length(years)
  for(a in 2:n.ages){
    for(y in 2:n.years){
      n.at.age[a,y] = round(n.at.age[a-1,y-1] * exp(-m))
    }
  }

  out.mat <- n.at.age[,n.ages:n.years]
dimnames(out.mat) <- list(age=ages, year=1:100)

  return(out.mat)

}

x <- simulate.pop.fct()
dim(x)

pal.fire=c(	rgb(255/255, 246/255, 143/255, 1),
            rgb(255/255, 215/255, 0/255, 1),
            rgb(255/255, 100/255, 2/255, 1),
            rgb(221/255, 8/255, 6/255, 1),
            rgb(139/255, 0/255, 0/255, 1))

br <- c(0, 1E04, 1E05, 1E06, 1E07, 1E08, 1E09)

image(x=0:100, y=0:21, z=t(x), col=c("white",pal.fire), breaks=br, xlab="", ylab="", axes=FALSE)

grid()
box()
axis(side=1, at=seq(1,100,10), padj=-1)
axis(side=1, at=1:100, labels = FALSE, tck=-0.01)
axis(side=2, at=seq(0,21), padj=1)
