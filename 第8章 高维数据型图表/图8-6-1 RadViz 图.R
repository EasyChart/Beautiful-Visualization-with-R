
#EasyCharts团队出品，如有商用必究，
#如需使用与深入学习，请联系微信：EasyCharts


#--------------------------图7-6-1 Radviz图 (b)多数据系列----------------------------------
library(ggplot2)
library(wesanderson)
library(Radviz)

data(iris)
das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
S <- make.S(das)
scaled <- apply(iris[,das],2,do.L)
rv <- do.radviz(scaled,S)

sim.mat <- cosine(scaled)
in.da(S,sim.mat) 
new <- do.optim(S,sim.mat,iter=10,n=100)
new.S <- make.S(get.optim(new))
new.rv <- do.radviz(scaled,new.S)

pop.cols <- setNames(c(wes_palette(n=3, name="Darjeeling1")),levels(iris$Species))

p<-bubbleRadviz(new.rv,
                bubble.color=c(wes_palette(n=3, name="Darjeeling1"))
                [as.integer(iris$Species)],
                bubble.fg='black',
                scale=0.05,
                decreasing=TRUE
)

# prepare "circle data"
radius <- 0.95
theta <- seq(0, 2 * pi, length = 200)
p<-p+lines(x = radius * cos(theta), y = radius * sin(theta))

legend( "bottomright",
          legend=names(pop.cols),
            col=pop.cols,
            cex=1,
            pch=15,
            bty='n')

p

#--------------------------图7-6-1 Radviz图 (a)高密度单数据系列----------------------------------
library(bodenmiller)
data(refPhenoMat)
data(refFuncMat)
refMat <- cbind(refPhenoMat,refFuncMat)
norm <- apply(refMat,2,do.L,fun=function(x) quantile(x,c(0.025,0.975)))
ct.S <- make.S(dimnames(refPhenoMat)[[2]])
## compute the similarity matrix
ct.sim <- cosine(norm)
## the current radviz-independent measure of projection efficiency
in.da(ct.S,ct.sim)
## the current radviz-independent measure of projection efficiency
rv.da(ct.S,ct.sim)
ct.rv <- do.radviz(norm,ct.S)

p<-smoothRadviz(ct.rv)
radius <- 0.95
theta <- seq(0, 2 * pi, length = 200)

p<-p+lines(x = radius * cos(theta), y = radius * sin(theta))
