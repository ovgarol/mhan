setwd("~/Documents/AAA-mixotrophy")
library(latex2exp)
library(scales)

weighted.mean = function(...) stats::weighted.mean(...,na.rm=T)

gn = function(r,n=2) (r-r^(1+n))/(1-r^(1+n)+1e-9)

Cn = function(q1,q2,n=2){
  dd = q2*gn(q1/q2,n)*(1+q1*q2/n+log(4^(-1/n)+0.5/n))
  return(gn(dd,n))
  #if(n>2) return(q1*q2)
  #if(n<1) return(min(c(q1,q2)))
  #else return(1/(1/q1+1/q2))
} 

### constants

beta = 0.001 # photoinibition coeficient m2 d/uE
n.Star = 3 # independence
Q.N.min = 0.02 # minimum quota mol-N/mol-C
Q.N.max = 0.2 # maximum quota mol-N/mol-C
A.x = 0.2e6 # prey affinity L/mol-C/d
A.I = 0.024 # light absorption m2 d/uE 
A.N = 1.5e6 # nutrient afinity L/mol-C/d
V.H.max = 3*3 # maximum ingestion rate 1/d
V.A.max = 3*1.5 # maximum photosinthesis rate 1/d
V.N.max = 3*0.5 # maximum nutrient uptake rate mol-N/mol-C/d 
tau.A = .0 # photo growth production factor 1/d
tau.H = .0 # prey growth ingestion factor 1/d
epsilon = 0.3 # asimilation efficiency
zeta.N = 1.5 # assimilation cost mol-C/mol-N
zeta.A = .1 # photoinhibition reparation cost
zeta.H = 0.1 # grazing respiration mol-C/d
xi.N = 0.5 # exudation cost mol-C/mol-N
Q.N.prima = 0.15 # nutrient quota (redfield for N) mol-N/mol-C
E.N.max = 1.5 # maximum exudation rate 1/d

f.A = 0.33 # autotrophic allocation
f.H = 0.33 # heterotrophi allocation
f.N = 1.-f.A-f.H # nutrient allocation 

### initial conditions
Q.N = .5*Q.N.max # initial nutrient quota mol-N/mol-C
I = 250 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)

### plot functions

the.model = function(...,steps=1000){
  if((f.A+f.H+f.N)>1) return(NULL)
  if(f.A<0 | f.H<0 | f.N<0) return(NULL)
  
  for(dummy.i in 1:steps){
    q.N = (Q.N-Q.N.min)/(Q.N.max-Q.N.min)

    n.A = n.Star*(1+q.N)
    n.H = n.Star*(1+q.N)
    n.N = n.Star*(1+q.N)
    
    x.norm = A.x*x/V.H.max
    I.norm = A.I*I/V.A.max
    N.norm = A.N*N/V.N.max
    
    P.inib = exp(-beta*I/(f.A*V.A.max))
    
    Ing = f.H*V.H.max*gn(x.norm/f.H,n.H) 
    Phot = f.A*V.A.max*(1-exp(-I.norm/f.A))*P.inib
    Uptake = f.N*V.N.max*gn(N.norm/f.N,n.N)
    
    mu0 = 0.9*(epsilon*Ing+Phot)
    
    q.A = (Phot+1e-5)/(Phot+1e-5+mu0/tau.A)
    q.H = (Ing+1e-5)/(Ing+1e-5+mu0/tau.H)
    
    if(tau.A == 0) q.A=1
    if(tau.H == 0) q.H=1
    
    s.N = 1-gn(q.N,n.N)
    E.N = E.N.max*(q.N>1)
    
    V.H = epsilon*Cn(q.N,q.A,n.H)*Ing
    #V.H = epsilon*q.A*Ing
    V.A = Cn(q.N,q.H,n.A)*Phot
    V.N = s.N*Uptake
    
    rho = zeta.N*V.N + xi.N*E.N*Q.N + zeta.H*V.H/epsilon + zeta.A*P.inib
    mu = V.H + V.A - rho
    G.N = V.N + f.H*V.H*Q.N.prima - E.N*Q.N - mu*Q.N
    
    Q.N = Q.N + .2*G.N
    
    if(is.nan(mu)) break
    Q.N.old = Q.N
    
  }
  
    
  resul = NULL
  resul$mu = mu
  resul$rho = rho
  resul$q.N = q.N
  resul$q.A = q.A
  resul$q.H = q.H
  resul$V.H = V.H
  resul$V.A = V.A
  resul$V.N = V.N
  resul$s.N = s.N
  resul$G.N = G.N
  resul$Ing = Ing
  resul$f.A = f.A
  resul$f.H = f.H
  resul$f.N = 1-f.A-f.H
  resul$I = I
  resul$x = x
  resul$N = N
  #print(dummy.i)
  
  return(resul)
}



################

par(mfrow=c(2,2),mai=0.2+c(0,0.2,0.2,0),oma=c(2,2,0,0))
palette(rev(hcl.colors(8,'Dy')))
palette(rev(hcl.colors(8,'Viri')))

n.total = 31
cex.max = .5
cex.min = .5
the.pch = 21
col.white = alpha('black',0.25)


I = 500 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('~/Documents/AAA-mixotrophy/plot square.R', echo=TRUE)
text(1,1,'A',adj=c(1,1),cex=2)

I = 500 # light intensity uE/m2/d
x = 0e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('~/Documents/AAA-mixotrophy/plot square.R', echo=TRUE)
text(1,1,'B',adj=c(1,1),cex=2)

I = 0 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('~/Documents/AAA-mixotrophy/plot square.R', echo=TRUE)
text(1,1,'C',adj=c(1,1),cex=2)

I = 500 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 0.e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('~/Documents/AAA-mixotrophy/plot square.R', echo=TRUE)
text(1,1,'D',adj=c(1,1),cex=2)

mtext(side=1,TeX('autotrophic allocation, $\\phi_A$'),outer=T,line = 1)
mtext(side=2,TeX('heterotrophic allocation, $\\phi_H$'),outer=T,line = 0)

dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
points(0.8+0*1:6,0.2+dd/2.5,pch=the.pch,cex=dd,col=col.white,bg=(1+c(1,3,6,9,12,15))/2,lwd=0.5)
text(0.85+0*1:6,0.2+dd/2.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
text(0.8,0.8,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)


#source('~/Documents/AAA-mixotrophy/plot vars.R', echo=TRUE)
