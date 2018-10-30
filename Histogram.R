x.norm<-rnorm(n=200,m=10,sd=2)
hist(x.norm,main="Histogram of observed data")
plot(density(x.norm),main="Density estimate of data")

plot(ecdf(x.norm),main="Empirical cumulative distribution function")

z.norm<-(x.norm-mean(x.norm))/sd(x.norm) ## standardized data
qqnorm(z.norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line

observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed,
           p = expected)

num_of_samples = 1000
x <- rgamma(num_of_samples, shape = 10, scale = 3)
x <- x + rnorm(length(x), mean=0, sd = .1)

p1 <- hist(x,breaks=50, include.lowest=FALSE, right=FALSE)
