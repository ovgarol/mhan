source('model2.R')


par(mfrow=c(4,4),mai=0.2+c(0,0.2,0.2,0),oma=c(2,2,0,0))
palette(rev(hcl.colors(8,'Viri')))

n.total = 31
cex.max = .5
cex.min = .5
the.pch = 21
col.white = alpha('black',0.25)

if(!exists("super.optimal")){
  super.optimal = NULL
  super.optimal$fA =NULL
  super.optimal$fH = NULL
  super.optimal$mu = NULL
  super.optimal$I = NULL
  super.optimal$x = NULL
  super.optimal$N = NULL
  super.optimal$size = size
}

### constants

dummy.i = 1
for(dummy.i in 1:(3*16)){

size = seq(log(8),log(60),length.out = 16)[1+dummy.i%%16] #log(runif(1,5,100)) # cell size in um

Photo.rate = (1./0.5*1/(1+(0.2*0.5*exp(size))*(0.25/0.5*(pi/6*exp(size)**3/8)**0.11)**(1./3.))-.5/(pi/6*exp(size)**3/8)**0.11)
L.opt = -1.83 + size - 0.02*size**2 + 1.7 - 0.7/(1+exp(5*(size-log(20))))
a.opt = 0.4*(size-L.opt)
Inges.rate = 1*173*exp(a.opt+(2-a.opt)*L.opt+(a.opt-3)*size)

beta = 0.001+0*runif(1,0,0.002) # photoinibition coeficient m2 d/uE
n.Star = 3+0*runif(1,1,10) # independence
Q.N.min = 0.032*exp(-0.33*size) # minimum quota mol-N/mol-C
Q.N.max = 0.183*exp(-0.06*size) # maximum quota mol-N/mol-C
A.x = 0.2e6+0*runif(1,0.1,0.5)*1e6  # prey affinity L/mol-C/d
A.I = 0.024+0*runif(1,0.01,0.05) # light absorption m2 d/uE 
A.N = 1.983*1e6*exp(-0.3*size) # nutrient afinity L/mol-C/d
V.H.max = Inges.rate+0*runif(1,1.0,10.) # maximum ingestion rate 1/d
V.A.max = 2.5*Photo.rate+0*runif(1,0.5,1.5) # maximum photosinthesis rate 1/d
V.N.max = 2*0.619*exp(-0.24*size) # maximum nutrient uptake rate mol-N/mol-C/d 
tau.A = 1*sample(c(0,1),1)*runif(1,0.5,10.)  # photo growth production factor 1/d
tau.H = 1*sample(c(0,1),1)*runif(1,0.5,10.) # prey growth ingestion factor 1/d
epsilon = 0.2+0*runif(1,0.1,0.5) # asimilation efficiency
zeta.N = 1.5+0*runif(1,0.,3.)# assimilation cost mol-C/mol-N
zeta.A = 0.+0*runif(1,0.,1.) # photoinhibition reparation cost
zeta.H = 0.1+0*runif(1,0.,.5) # grazing respiration mol-C/d
xi.N = 0.5+0*runif(1,0.,1.) # exudation cost mol-C/mol-N
Q.N.prima = 0.15+0*runif(1,.1,.3) # nutrient quota (redfield for N) mol-N/mol-C
E.N.max = runif(1,0.,1.)*Q.N.max # maximum exudation rate 1/d

### allocation initial conditions

f.A = 0.33 # autotrophic allocation
f.H = 0.33 # heterotrophi allocation
f.N = 1.-f.A-f.H # nutrient allocation 

### initial conditions

Q.N = .5*Q.N.max # initial nutrient quota mol-N/mol-C
I = 0*200+1*exp(runif(1,log(1),log(200))) # light intensity uE/m2/d
x = 0*1000e-6+1*exp(runif(1,log(1),log(200)))*1e-6 # prey biomass concentration mol-C/L
N = 0*1000e-6+1*exp(runif(1,log(1),log(200)))*1e-6 # nutrient concentration mol-N/L

### 

source('script_square.R', echo=F)

limits = c('')
if(tau.A) limits = paste0(limits,'A')
if(tau.H) limits = paste0(limits,'H')
text(1,1,limits,adj=c(1,1),cex=2)

text(1,0.8,TeX(paste0('ESD = ',round(exp(size)),' $\\mu$m')),adj=c(1,1),cex=0.8)
text(1,0.7,paste0('L = ',round(I)),adj=c(1,1),cex=0.8)
text(1,0.6,paste0('x = ',round(x*1e6)),adj=c(1,1),cex=0.8)
text(1,0.5,paste0('N = ',round(N*1e6)),adj=c(1,1),cex=0.8)

super.optimal$fA = c(super.optimal$fA,f.A.max) 
super.optimal$fH = c(super.optimal$fH,f.H.max)
super.optimal$mu = c(super.optimal$mu,max(mu.list,na.rm=T))
super.optimal$I = c(super.optimal$I,I)
super.optimal$x = c(super.optimal$x,x)
super.optimal$N = c(super.optimal$N,N)
super.optimal$size = c(super.optimal$size,size)

}

#plot(1:100,1/0.5*1/(1+(0.28*0.5*1:100)*(0.25/0.5*(pi/6*(1:100)**3/8)**0.181)**(1./3.))-.5/(pi/6*(1:100)**3/8)**0.181,log='x')
#abline(h=0)

size = seq(log(8),log(60),length.out = 16) # cell size in um

Photo.rate = 2.5*(1/0.5*1/(1+(0.2*0.5*exp(size))*(0.25/0.5*(pi/6*exp(size)**3/8)**0.181)**(1./3.))-.5/(pi/6*exp(size)**3/8)**0.181)
L.opt = -1.83 + size - 0.02*size**2 + 1.7 - .70/(1+exp(5*(size-log(20))))
a.opt = 0.4*(size-L.opt)
Inges.rate = 1*173*exp(a.opt+(2-a.opt)*L.opt+(a.opt-3)*size)

plot(size,Photo.rate,log='',type='l',col='green3',xaxt='n',las=2,ylim=c(0,2))
axis(1,log(c(1,5,10,20,50,100)),labels=c(1,5,10,20,50,100))
lines(size,epsilon*Inges.rate)

plot(size,L.opt,xlim=c(1,5),ylim=c(0,4))
abline(a=0,b=1)

###
par(mfrow=c(2,2),mai=0.2+c(0,0.2,0.2,0),oma=c(2,2,0,0))
colores = as.integer(8*super.optimal$mu)+1
tamano = cex.min+5*cex.max*super.optimal$mu

plot(0,xlim=c(0,1),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n')
lines(c(0,1),c(1,0),lwd=0.5,col='darkgray')
points(super.optimal$fA,super.optimal$fH,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=2,cex=0.25)
axis(side=1,c(0,0.25,0.5,0.75,1),labels=c('0','','','','1'),las=1,cex=0.25)
mtext('fA',side=1,line=2)
mtext('fH',side=2,line=2)

plot(0,xlim=c(1,200),ylim=c(1,200),type='n',xlab='',xaxt='n',ylab='',yaxt='n',log='xy')
colores2 = as.integer(10*super.optimal$fH)+1
palette(hcl.colors(10,'Temp'))
points(super.optimal$I,super.optimal$x*1e6,cex=tamano,pch=the.pch,bg=colores2,col=col.white,lwd=0.5)
axis(side=2,200*c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(side=1,200*c(0,0.25,0.5,0.75,1),las=1,cex=0.25)
mtext('light',side=1,line=2)
mtext('prey',side=2,line=2)

plot(0,xlim=c(1,200),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n', log='x')
points(super.optimal$I,super.optimal$fH,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(side=1,c(1,2,5,10,20,50,100,200),las=1,cex=0.25)
mtext('light',side=1,line=2)
mtext('fH',side=2,line=2)

plot(0,xlim=c(1,200),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n',log='x')
points(super.optimal$x*1e6,super.optimal$fH,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(side=1,c(1,2,5,10,20,50,100,200),las=1,cex=0.25)
mtext('prey',side=1,line=2)
mtext('fH',side=2,line=2)

plot(0,xlim=c(1,200),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n',log='x')
points(super.optimal$N*1e6,super.optimal$fH,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(side=1,c(1,2,5,10,20,50,100,200),las=1,cex=0.25)
mtext('nutrient',side=1,line=2)
mtext('fH',side=2,line=2)

plot(0,xlim=c(5,60),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n')
points(exp(super.optimal$size),super.optimal$fH,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(1,c(1,5,10,20,30,40,50,60))


plot(0,xlim=c(5,60),ylim=c(0,1),type='n',xlab='',xaxt='n',ylab='',yaxt='n')
points(exp(super.optimal$size),super.optimal$mu,cex=tamano,pch=the.pch,bg=colores,col=col.white,lwd=0.5)
axis(side=2,c(0,0.25,0.5,0.75,1),las=2,cex=0.25)
axis(1,c(1,5,10,20,30,40,50,60))
