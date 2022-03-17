##
## simulate an age-structured population through time
## and obtain observations of age-length and length increments
## from population samples

## one realisation of the simulation consists in a list containing
## 1 - a matrix of the simulated numbers-at-age by year
## 2 - a data frame of the number of fish captured in a survey
## 3 - a data frame of the age, length and weight of the sampled fish
## 4 - age-length observations
## 5 - growth increment observations

vb.fct <- function(a,Linf=100,k=0.1,t0=-1){
  return(Linf*(1-exp(-k*(a-t0))))
}

lw.fct <- function(length, a=0.001,b=3.1){
  return(a*(length^b))
}

simulate.pop.fct <- function(m=0.2){

  set.seed(1973)

  ages <- 0:20
  years <- 1:120

  n.at.age <- matrix(rep(NA, length(ages)*(length(years))), nc=length(years), nr=length(ages))
  #dim(n.at.age)
  ## age0, i.e. recruitment
  #n.at.age[1,] <- rpois(ncol(n.at.age),100) * 1E06
  n.at.age[1,] <- rnbinom(ncol(n.at.age),size=2,mu=100) * 1E06

  # numbers-at-age matrix
  n.ages <- length(ages)
  n.years <- length(years)
  for(a in 2:n.ages){
    for(y in 2:n.years){
      ## if this is the oldest age, turn into a plus group
      ##
      if(a==max(ages)) {n.at.age[a,y] <- round(n.at.age[a-1,y-1] * exp(-m))}
      else {n.at.age[a,y] = round(n.at.age[a-1,y-1] * exp(-m))}
    }

  }

  out.mat <- n.at.age[,n.ages:n.years]
  dimnames(out.mat) <- list(age=ages, year=1:100)

  ## simulate yearly sample from a survey
  yearly.df <- colSums(out.mat)

  obs.df <- NULL
  ibm.df <- NULL

  for(i in 1:100){
    this.q <- 0.0001
    this.n <- round(yearly.df[i] * this.q)
    t.df <- data.frame(year=years[i], n.captured=this.n)
    obs.df <- rbind(obs.df,t.df)
    ## for the individuals captured, assign an age based on the
    ## age structure in the simulated population
    t2.df <- data.frame(year=years[i], fish.no=1:this.n)
    #dim(ibm.df)
    prop.at.age <- out.mat[,i]/yearly.df[i]
    t2.df$age <- sample(ages, this.n, replace=TRUE, prob=prop.at.age)

    t2.df$length <- vb.fct(t2.df$age, Linf=100, k=0.1, t0=-1)
    t2.df$weight <- lw.fct(t2.df$length, a=0.001,b=3.1)

    ibm.df <- rbind(ibm.df,t2.df)
      }

  ## growth trajectories



  return(list(mat=out.mat, obs=obs.df, ibm=ibm.df))
} #end function





l <- simulate.pop.fct(m=0.5)
x <- l[[1]]

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

## total number of individuals per year
plot(colSums(x), type='b', pch=19, ylim=c(0, max(colSums(x))))
par(new=TRUE)
y <- l[[2]]
plot(y, type='b', pch=19)

cor(colSums(x),y$n.captured)

range(y$n.captured)

## total weight of individuals per year



z <- l[[3]]


head(z)
agg.df <- aggregate(fish.no~age+year, data=z, length)
agg.df[agg.df$year==21,]
idx <- sample(1:nrow(z),1000)
plot(z[idx,"age"], z[idx,"length"])
plot(z[idx,"length"], z[idx,"weight"])
