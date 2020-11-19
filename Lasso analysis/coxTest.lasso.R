######Video source: http://ke.biowolf.cn
######������ѧ��: http://www.biowolf.cn/
######΢�Ź��ںţ�biowolf_cn
######�������䣺2749657388@qq.com
######����΢��: 18520221056

#install.packages("glmnet")
#install.packages("survival")


library("glmnet")
library("survival")

setwd("C:\\Users\\lenovo\\Desktop\\��Ъ�Խ�ʳ\\�ǽͽ�\\20.uniCox")                #���ù���Ŀ¼
rt=read.table("uniSigExp.txt",header=T,sep="\t",row.names=1,check.names=F)     #��ȡ�ļ�
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
######������ѧ��: http://www.biowolf.cn/
######΢�Ź��ںţ�biowolf_cn
######�������䣺2749657388@qq.com
######����΢��: 18520221056