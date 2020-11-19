######Video source: http://ke.biowolf.cn
######生信自学网: http://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056

#install.packages("glmnet")
#install.packages("survival")


library("glmnet")
library("survival")

setwd("C:\\Users\\lenovo\\Desktop\\间歇性禁食\\糖酵解\\20.uniCox")                #设置工作目录
rt=read.table("uniSigExp.txt",header=T,sep="\t",row.names=1,check.names=F)     #读取文件
rt$futime[rt$futime<=0]=1

x=as.matrix(rt[,c(3:ncol(rt))])
y=data.matrix(Surv(rt$futime,rt$fustat))

fit <- glmnet(x, y, family = "cox", maxit = 1000)
pdf("lambda.pdf")
plot(fit, xvar = "lambda", label = TRUE)
dev.off()

cvfit <- cv.glmnet(x, y, family="cox", maxit = 1000)
pdf("cvfit.pdf")
plot(cvfit)
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty="dashed")
dev.off()

coef <- coef(fit, s = cvfit$lambda.min)
index <- which(coef != 0)
actCoef <- coef[index]
lassoGene=row.names(coef)[index]
lassoGene=c("futime","fustat",lassoGene)
lassoSigExp=rt[,lassoGene]
lassoSigExp=cbind(id=row.names(lassoSigExp),lassoSigExp)
write.table(lassoSigExp,file="lassoSigExp.txt",sep="\t",row.names=F,quote=F)


######Video source: http://ke.biowolf.cn
######生信自学网: http://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056