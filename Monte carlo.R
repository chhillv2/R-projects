install.packages('MonteCarlo')
library(MonteCarlo)

ttest<-function(n,loc,scale){
  
  # generate sample:
  sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(list("decision"=decision))
}

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
summary(MC_result)
MakeTable(output=MC_result, rows="n", cols=c("loc","scale"), digits=2, include_meta=FALSE)

ttest<-function(n,loc,scale){
  
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  return(list("stat"=stat))
}
MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
df<-MakeFrame(MC_result)
head(df)

library(dplyr)
library(ggplot2)
tbl <- tbl_df(df)
ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))