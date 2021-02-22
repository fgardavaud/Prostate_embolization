# test de simulation pour comprendre p-value avec H0 en hypothèse nulle
#mu1=mu2=8.1; sigma=3.2; n1=400; n2=300 # H0
mu1=8.1; mu2=1.1*mu1; sigma=3.2; n1=400; n2=300 # H1

set.seed(123)
nrep=2000 # 2000 tirages
s=rep(NA,nrep)
for (rep in 1:nrep) {

  x1=rnorm(n1,mu1,sigma)
  x2=rnorm(n2,mu2,sigma)
  s[rep]=abs(mean(x1)-mean(x2))
}

plot(density(s))
hist(s,col="grey",nc=30)

thres=quantile(s,prob=0.95) # seuil

pv.emp=mean(s>0.5)
pv.emp # p-value


p=0.12
n=40

set.seed(12)
x=sample(0:1,replace=TRUE,size=n,prob=c(1-p,p)) # ici jeu de données simulée
# que l'on aura en réel dans les embolies


p.hat=mean(x) # proba estimée

# loi pour faire les IC dans les graphiques
qbinom(c(0.5,0.05,0.95),size =n,prob=p.hat)/length(x)
qnorm(c(0.5,0.05,0.95),mean=p.hat,sd=sqrt(p.hat*(1-p.hat)/length(x)))

x=rexp(n,rate =1/0.05)

mu.hat=mean(x)
sd.hat=sd(x)

qnorm(c(0.5,0.05,0.95),mean=mu.hat,sd=sd.hat/sqrt(length(x)))


