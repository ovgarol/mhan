mu.list = NULL
fA.list = NULL
fH.list = NULL
model.list = NULL

the.label = paste('L = ',I,'\nx = ',x*1e6,'\nN = ',N*1e6)
plot(0,xlim=c(0,1),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
#text(.75,1,the.label,adj=c(0,1))

for(i in 1:n.total)for(j in 1:n.total){
  f.A = i/(n.total)
  f.H = j/(n.total)
  f.N = 1-f.A-f.H
  if(f.N>0) {
    m = the.model()
    if(!is.null(m)){
      if(!is.nan(m$mu)&m$mu>1e-2){
        colores = as.integer(5*m$mu)+1
        tamano = cex.min+cex.max*m$mu
        points(f.A,f.H,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
      }
      if(!is.nan(m$mu)&m$mu<1e-2&m$mu>0) points(f.A,f.H,cex=cex.min,pch=1,col=col.white,lwd=0.5)
      mu.list = c(mu.list,m$mu)
      fA.list = c(fA.list,f.A)
      fH.list = c(fH.list,f.H)
      model.list = c(model.list,m)
    }
  }
}

axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)

f.A.max = fA.list[which.max(mu.list)]
f.H.max = fH.list[which.max(mu.list)]
#points(weighted.mean(fA.list,mu.list),weighted.mean(fH.list,mu.list),pch=19,col='black')
points(f.A.max,f.H.max,pch=0,col='red',lwd=3,cex=2)
points(f.A.max,f.H.max,pch=0,col='black',lwd=1,cex=2)

text(0.8,1,paste0('(',round(f.A.max,digits=2),', ',round(f.H.max,digits=2),')'),adj=c(1,1))
mu.max = round(max(mu.list,na.rm=T),digits=2)
mu.label = TeX(paste('$\\mu_{max} = $', mu.max, '$d^{-1}$'))
mtext(mu.label,cex=1,adj=0)

model.list = matrix(unlist(model.list),nrow=length(m))

if(F){ # to plot a border around a value
  mu.limit = 0.01
  k=chull(fA.list[mu.list>mu.limit],fH.list[mu.list>mu.limit])
  polygon(fA.list[mu.list>mu.limit][k],fH.list[mu.list>mu.limit][k],border=alpha('gray',0.5),lwd=2)
}
