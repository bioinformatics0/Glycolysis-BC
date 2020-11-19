###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

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



setwd("C:\\Users\\lenovo\\Desktop\\间歇性禁食\\糖酵解\\乳癌\\列线图")                         #设置工作目录
rt=read.table("input2.txt",sep="\t",header=T,row.names=1,check.names=F)             #读取输入文件

#数据打包
bc <- na.omit(rt)
dd <- datadist(rt)
options(datadist="dd")

#生成函数
f <- cph(Surv(futime,fustat) ~ Age+Stage+riskScore, x=T, y=T, surv=T, data=rt, time.inc=1)
surv <- Survival(f)

#建立nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(3, x), function(x) surv(5, x)), 
                lp=F, funlabel=c("1-year survival", "3-year survival", "5-year survival"), 
                maxscale=100, 
                fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2,0.1,0.05))  

#nomogram可视化
pdf(file="nomogram2.pdf",height=6,width=10)
plot(nom)
dev.off()

#C-index值

validate(f, method="boot", B=1000, dxy=T)
rcorrcens(Surv(futime,fustat) ~ predict(f), data = bc)

内部验证
f3<- cph(Surv(futime,fustat) ~ Age+Stage+riskScore, x=T, y=T, surv=T, data=rt,time.inc=36)
cal3 <- calibrate(f3, cmethod="KM", method="boot", u=36, m=220, B=1000)
plot(cal3)

外部验证
rt=read.table ("input2.txt",header=T) 
fev3 <- cph(Surv(futime,fustat) ~predict(f, newdata=rt), x=T, y=T, surv=T, data=rt, time.inc=4.6)

calev3 <- calibrate(fev3, cmethod="KM", method="boot", u=4.6, m=200, B=1000)

plot(calev3)
###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056