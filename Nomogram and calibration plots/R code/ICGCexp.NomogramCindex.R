###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#install.packages("Hmisc")
#install.packages("lattice")
#install.packages("Formula")
#install.packages("ggplot2")
#install.packages("foreign")
#install.packages("rms")

library(rms)

library(Hmisc)

library(lattice)

library(survival)

library(Formula)

library(ggplot2)

library(foreign)



setwd("C:\\Users\\lenovo\\Desktop\\��Ъ�Խ�ʳ\\�ǽͽ�\\�鰩\\����ͼ")                         #���ù���Ŀ¼
rt=read.table("input2.txt",sep="\t",header=T,row.names=1,check.names=F)             #��ȡ�����ļ�

#���ݴ��
bc <- na.omit(rt)
dd <- datadist(rt)
options(datadist="dd")

#���ɺ���
f <- cph(Surv(futime,fustat) ~ Age+Stage+riskScore, x=T, y=T, surv=T, data=rt, time.inc=1)
surv <- Survival(f)

#����nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(3, x), function(x) surv(5, x)), 
                lp=F, funlabel=c("1-year survival", "3-year survival", "5-year survival"), 
                maxscale=100, 
                fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2,0.1,0.05))  

#nomogram���ӻ�
pdf(file="nomogram2.pdf",height=6,width=10)
plot(nom)
dev.off()

#C-indexֵ

validate(f, method="boot", B=1000, dxy=T)
rcorrcens(Surv(futime,fustat) ~ predict(f), data = bc)

�ڲ���֤
f3<- cph(Surv(futime,fustat) ~ Age+Stage+riskScore, x=T, y=T, surv=T, data=rt,time.inc=36)
cal3 <- calibrate(f3, cmethod="KM", method="boot", u=36, m=220, B=1000)
plot(cal3)

�ⲿ��֤
rt=read.table ("input2.txt",header=T) 
fev3 <- cph(Surv(futime,fustat) ~predict(f, newdata=rt), x=T, y=T, surv=T, data=rt, time.inc=4.6)

calev3 <- calibrate(fev3, cmethod="KM", method="boot", u=4.6, m=200, B=1000)

plot(calev3)
###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######������ѧ��: http://www.biowolf.cn/
######�������䣺2749657388@qq.com
######����΢��: 18520221056