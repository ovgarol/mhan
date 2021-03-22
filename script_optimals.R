source('model2.R')

n.total = 31
model.list = NULL
optimal.list = NULL

for(i in 1:100){
  I = 10**(runif(1,-1,3)) # 0.1 to 1000 uE m-2 d-1 
  x = 10**(runif(1,-1,3))*1e-6 # 0.1 to 1000 umol-C L-1
  N = 10**(runif(1,-1,3))*1e-6 # 0.1 to 1000 umol-N L-1 

  model.list = NULL
  for(i in 1:n.total)for(j in 1:n.total){
    f.A = i/(n.total)
    f.H = j/(n.total)
    f.N = 1-f.A-f.H
    if(f.N>0) {
      m = the.model()
      if(!is.null(m)){
        model.list = c(model.list,m)
      }
    }
  }
  model.list = t(matrix(unlist(model.list),nrow=length(m)))
  model.list = as.data.frame(model.list)
  names(model.list) = names(m)
  optimal.list = c(optimal.list,model.list[which.max(model.list$mu),])
}

optimal.list = t(matrix(unlist(optimal.list),nrow=length(m)))
optimal.list = as.data.frame(optimal.list)
names(optimal.list) = names(m)
