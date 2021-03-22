n.total = 31
model.list = NULL
optimal.list = NULL

I.mean = 50 # mean light intensity uE/m2/d
x.mean = 10e-6 # prey biomass concentration mol/L
N.mean = 8.82e-6 # mean nutrient concentration mol-N/L

for(i in 1:1000){
  I = runif(1,1,1000) #I.mean*rlnorm(1,0,1) 
  x = runif(1,1,1000)*1e-6 #x.mean*rlnorm(1,0,1)
  N = runif(1,1,1000)*1e-6 #N.mean*rlnorm(1,0,1) 

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
