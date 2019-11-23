outcome=sample(c(1,0),100,replace = T)
score=runif(100)

cutoff=seq(0,1,length=1000)

cutoff1=0.2
prediction=as.numeric(score>cutoff1)

table(outcome,prediction)
