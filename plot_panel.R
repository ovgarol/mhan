source('model2.R')

is.pdf = T

code.name = paste0(format(Sys.time(),"%y%m%d-%H%M%S"),'-',round(1000*runif(1)))
file.name = paste0('test/AH.diag',code.name,'.pdf')

################

if(is.pdf) pdf(file.name,width=5,height=5)

par(mfrow=c(2,2),mai=0.2+c(0,0.2,0.2,0),oma=c(2,2,0,0))
palette(rev(hcl.colors(8,'Viri')))

n.total = 31
cex.max = .5
cex.min = .5
the.pch = 21
col.white = alpha('black',0.25)

I = 500 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('script_square.R', echo=TRUE)
text(1,1,'A',adj=c(1,1),cex=2)

I = 500 # light intensity uE/m2/d
x = 0e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('script_square.R', echo=TRUE)
text(1,1,'B',adj=c(1,1),cex=2)

I = 0 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 8.82e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('script_square.R', echo=TRUE)
text(1,1,'C',adj=c(1,1),cex=2)

I = 500 # light intensity uE/m2/d
x = 1000e-6 # prey biomass concentration mol/L
N = 0.e-4 # nutrient concentration mol-N/L (for f/2 medium)
source('script_square.R', echo=TRUE)
text(1,1,'D',adj=c(1,1),cex=2)

mtext(side=1,TeX('autotrophic allocation, $\\phi_A$'),outer=T,line = 1)
mtext(side=2,TeX('heterotrophic allocation, $\\phi_H$'),outer=T,line = 0)

dd=seq(cex.min,cex.min+1.5*cex.max,length.out = 6)
points(0.8+0*1:6,0.2+dd/2.5,pch=the.pch,cex=dd,col=col.white,bg=(1+c(1,3,6,9,12,15))/2,lwd=0.5)
text(0.85+0*1:6,0.2+dd/2.5,labels=c('0.0','0.3','0.6','0.9','1.2','1.5'),pch=the.pch,adj=c(0,0.5),cex=0.7)
text(0.8,0.8,TeX('$\\mu$ (d$^{-1}$)'),adj=c(0.,0.5),cex=0.9)

if(is.pdf) dev.off()