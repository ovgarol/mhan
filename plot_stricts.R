library(scales)
library(latex2exp)

is.pdf = T
to.run = T

if(to.run){
  tau.A = 0
  tau.H = 0
  source('script_optimals.R', echo=F)
  optimal.M = optimal.list
  
  tau.A = 0.5
  tau.H = 0
  source('script_optimals.R', echo=F)
  optimal.A = optimal.list
  
  tau.A = 0
  tau.H = 0.5
  source('script_optimals.R', echo=F)
  optimal.H = optimal.list
  
  tau.A = 0.5
  tau.H = 0.5
  source('script_optimals.R', echo=F)
  optimal.AH = optimal.list
  
  code.name = paste0(format(Sys.time(),"%y%m%d-%H%M%S"),'-',round(1000*runif(1)))
  file.name = paste0('data/MC-optimal-',code.name,'.RData')
  save(optimal.M,optimal.A,optimal.H,optimal.AH,file=file.name)
}

if(!to.run){
  code.name = '210322-172640-11'
  file.name = paste0('data/MC-optimal-',code.name,'.RData')
  load(file.name)
} 

optimal.M = optimal.M[optimal.M$mu>1e-2,]
optimal.A = optimal.A[optimal.A$mu>1e-2,]
optimal.H = optimal.H[optimal.H$mu>1e-2,]
optimal.AH = optimal.AH[optimal.AH$mu>1e-2,]

file.name = paste0('test/MC-results-',code.name,'.pdf')
if(is.pdf) pdf(file.name,width=5,height=5)

par(mfrow=c(2,2),mai=0.2+c(0,0.2,0.2,0),oma=c(2,2,0,0))

## plot AH-mu diagram

palette(rev(hcl.colors(8,'Viri')))
col.white = alpha('black',0.25)

cex.min = 0.5
cex.max = 0.5

optimal.list = optimal.M
colores = 1+as.integer(5*optimal.list$mu)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.A,optimal.list$f.H,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,col=col.white,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(1,1,'-',adj=c(1,1),cex=2)

#points(0.7+0*1:3,0.1+c(0.5,0.6,0.7),pch=21,cex=1,bg=c(1,5,10),col='black',lwd=0.5)
#text(0.75+0*1:3,0.1+c(0.5,0.6,0.7),labels=c('Hetero','Mixo','Auto'),pch=the.pch,adj=c(0,0.5),cex=0.9)

optimal.list = optimal.A
colores = 1+as.integer(5*optimal.list$mu)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.A,optimal.list$f.H,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,col=col.white,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(1,1,'A',adj=c(1,1),cex=2)

optimal.list = optimal.H
colores = 1+as.integer(5*optimal.list$mu)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.A,optimal.list$f.H,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,col=col.white,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(1,1,'H',adj=c(1,1),cex=2)

optimal.list = optimal.AH
colores = 1+as.integer(5*optimal.list$mu)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.A,optimal.list$f.H,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,col=col.white,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(1,1,'AH',adj=c(1,1),cex=2)

mtext(side=1,TeX('autotrophic allocation, $\\phi_A$'),outer=T,line = 1)
mtext(side=2,TeX('heterotrophic allocation, $\\phi_H$'),outer=T,line = 0)

dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
points(0.8+0*1:6,0.2+dd/2.5,pch=the.pch,cex=dd,bg=(1+c(1,3,6,9,12,15))/2,lwd=0.5,col=col.white)
text(0.85+0*1:6,0.2+dd/2.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
text(0.8,0.8,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)

## plot AH qN

cex.min = .5
cex.max = .5

optimal.list = optimal.M
colores = 'gray' #1+as.integer(9*optimal.list$f.H)# alpha('green3',optimal.list$f.A)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.H,optimal.list$q.N,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(0,0,'-',adj=c(0,0),cex=2)

dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
points(0.8+0*1:6,0.+dd/2.5,pch=the.pch,cex=dd,col='black',bg='gray',lwd=0.5)
text(0.85+0*1:6,0.+dd/2.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
text(0.8,0.6,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)

optimal.list = optimal.A
colores = 'gray' #1+as.integer(9*optimal.list$f.H)# alpha('green3',optimal.list$f.A)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.H,optimal.list$q.N,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(0,0,'A',adj=c(0,0),cex=2)

optimal.list = optimal.H
colores = 'gray' #1+as.integer(9*optimal.list$f.H)# alpha('green3',optimal.list$f.A)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.H,optimal.list$q.N,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(0,0,'H',adj=c(0,0),cex=2)


optimal.list = optimal.AH
colores = 'gray' #1+as.integer(9*optimal.list$f.H)# alpha('green3',optimal.list$f.A)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$f.H,optimal.list$q.N,log='',pch=21,lwd=0.5,bg=colores,cex=tamano,
     ylim=c(0,1),xlim=c(0,1),xaxt='n',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
text(0,0,'AH',adj=c(0,0),cex=2)

mtext(side=1,TeX('heterotrophic allocation, $\\phi_H$'),outer=T,line = 1)
mtext(side=2,TeX('internal Nitrogen quota, $\\q_N$'),outer=T,line = 0)


## mu Light

palette(rev(hcl.colors(16,'Dynamic')))
palette(c('gray','gray'))
cex.min = .5
cex.max = .0

optimal.list = optimal.M
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$I,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.A
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$I,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'A',adj=c(1,0),cex=2)

optimal.list = optimal.H
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$I,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'H',adj=c(1.05,0),cex=2.1,col='white')
text(1000,0,'H',adj=c(0.95,0),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,0.05),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,-0.05),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,0),cex=2)

optimal.list = optimal.AH
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$I,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'AH',adj=c(1.05,0),cex=2.1,col='white')
text(1000,0,'AH',adj=c(0.95,0),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,0.05),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,-0.05),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,0),cex=2)

mtext(side=1,TeX('light intensity, L ($\\mu$E m$^{-2}$ s$^{-1}$)'),outer=T,line = 1)
mtext(side=2,TeX('growth rate, $\\mu$ (d$^{-1}$)' ),outer=T,line = 0)

#dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
#points(2+0*1:6,0.2+dd/1.5,pch=the.pch,cex=dd,col='black',bg=1+c(0,3,6,9,12,15),lwd=0.5)
#text(2.5+0*1:6,0.2+dd/1.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
#text(2,1.2,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)


## mu Prey

palette(rev(hcl.colors(16,'Dynamic')))
palette(c('gray','gray'))
cex.min = .5
cex.max = .0

optimal.list = optimal.M
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000)*1e-6,ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.A
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000)*1e-6,ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'A',adj=c(1,0),cex=2)

optimal.list = optimal.H
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000)*1e-6,ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'H',adj=c(1.05,0),cex=2.1,col='white')
text(1000,0,'H',adj=c(0.95,0),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,0.05),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,-0.05),cex=2.1,col='white')
text(1000,0,'H',adj=c(1,0),cex=2)

optimal.list = optimal.AH
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x,optimal.list$mu,log='x',pch=21,lwd=0.5,bg=colores,cex=tamano,
     xlim=c(1,1000)*1e-6,ylim=c(0,1.5),
     xaxt='l',yaxt='n',
     las=1, xlab='', ylab='')
axis(side=2,c(0,0.5,1,1.5),las=1,cex=0.25)
text(1000,0,'AH',adj=c(1.05,0),cex=2.1,col='white')
text(1000,0,'AH',adj=c(0.95,0),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,0.05),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,-0.05),cex=2.1,col='white')
text(1000,0,'AH',adj=c(1,0),cex=2)

mtext(side=1,TeX('light intensity, L ($\\mu$E m$^{-2}$ s$^{-1}$)'),outer=T,line = 1)
mtext(side=2,TeX('growth rate, $\\mu$ (d$^{-1}$)' ),outer=T,line = 0)

dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
points(2+0*1:6,0.2+dd/1.5,pch=the.pch,cex=dd,col='black',bg=1+c(0,3,6,9,12,15),lwd=0.5)
text(2.5+0*1:6,0.2+dd/1.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
text(2,1.2,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)

## L,x mu

palette((hcl.colors(10,'Temp')))
cex.min = 1.0
cex.max = 0.

optimal.list = optimal.M
colores = 1+as.integer(9*optimal.list$f.H)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$I,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.A
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$I,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.H
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$I,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.AH
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$I,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

mtext(side=1,TeX('prey biomass, x ($\\mu$mol-C L$^{-1}$ )'),outer=T,line = 1)
mtext(side=2,TeX('light intensity, L ($\\mu$E m$^{-2}$ s$^{-1}$)'),outer=T,line = 0)


## N,x mu

palette((hcl.colors(10,'Temp')))
cex.min = 1.0
cex.max = 0.

optimal.list = optimal.M
colores = 1+as.integer(9*optimal.list$f.H)
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$N*1e6,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.A
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$N*1e6,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.H
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$N*1e6,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

optimal.list = optimal.AH
colores = as.integer(10*optimal.list$mu)+1
tamano = cex.min + cex.max*optimal.list$mu
plot(optimal.list$x*1e6,optimal.list$N*1e6,log='xy',pch=21,lwd=0.,bg=colores,cex=tamano,
     xlim=c(1,1000),ylim=c(1,1000),
     xaxt='l',yaxt='l',
     las=1, xlab='', ylab='')
text(1000,0,'-',adj=c(1,0),cex=2)

mtext(side=1,TeX('prey biomass, x ($\\mu$mol-C L$^{-1}$ )'),outer=T,line = 1)
mtext(side=2,TeX('Nitrogen concentration, R$_N$ ($\\mu$mol-N L$^{-1}$)'),outer=T,line = 0)

####

optimal.list = optimal.M
plot(optimal.list$I,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$I,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.A
plot(optimal.list$I,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$I,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.H
plot(optimal.list$I,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$I,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.AH
plot(optimal.list$I,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$I,optimal.list$f.H),col='red',lwd=2)


####

optimal.list = optimal.M
plot(optimal.list$x*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$x*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.A
plot(optimal.list$x*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$x*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.H
plot(optimal.list$x*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$x*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.AH
plot(optimal.list$x*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$x*1e6,optimal.list$f.H),col='red',lwd=2)

####

optimal.list = optimal.M
plot(optimal.list$N*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$N*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.A
plot(optimal.list$N*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$N*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.H
plot(optimal.list$N*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$N*1e6,optimal.list$f.H),col='red',lwd=2)

optimal.list = optimal.AH
plot(optimal.list$N*1e6,optimal.list$f.H,log='x',ylim=c(0,1),xlim=c(1,1000))
lines(supsmu(optimal.list$N*1e6,optimal.list$f.H),col='red',lwd=2)

if(is.pdf) dev.off()
