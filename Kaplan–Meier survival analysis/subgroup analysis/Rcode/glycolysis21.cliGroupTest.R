######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056

#install.packages("survival")
#install.packages("survminer")

library(survival)
library(survminer)

setwd("C:\\Users\\lenovo\\Desktop\\间歇性禁食\\糖酵解\\乳癌\\分层分析")                 #设置工作目录
risk=read.table("riskTrain.txt",header=T,sep="\t",check.names=F,row.names=1)         #读取风险文件
cli=read.table("clinical.txt",sep="\t",check.names=F,header=T,row.names=1)      #读取临床文件
sameSample=intersect(row.names(cli),row.names(risk))
risk=risk[sameSample,]
cli=cli[sameSample,]
data=cbind(futime=risk[,1],fustat=risk[,2],cli,risk=risk[,"risk"])

#对临床信息进行循环
for(i in colnames(data[,3:(ncol(data)-1)])){
    rt=data[,c("futime","fustat",i,"risk")]
    rt=rt[(rt[,i]!="unknow"),]
    colnames(rt)=c("futime","fustat","clinical","risk")
	tab=table(rt[,"clinical"])
	tab=tab[tab!=0]
	#对每个临床信息里面的分类进行循环
	for(j in names(tab)){
		rt1=rt[(rt[,"clinical"]==j),]
		tab1=table(rt1[,"risk"])
		tab1=tab1[tab1!=0]
		labels=paste0(names(tab1)," risk(n=",tab1,")")
		if(length(labels)==2){
			titleName=j
			if((i=="age") | (i=="Age") | (i=="AGE")){
				titleName=paste0("age",j)
			}
			diff=survdiff(Surv(futime, fustat) ~risk,data = rt1)
			pValue=1-pchisq(diff$chisq,df=1)
			if(pValue<0.001){
				pValue="p<0.001"
			}else{
				pValue=paste0("p=",sprintf("%.03f",pValue))
			}
			fit <- survfit(Surv(futime, fustat) ~ risk, data = rt1)
			#绘制生存曲线
			surPlot=ggsurvplot(fit, 
			           data=rt1,
			           pval=pValue,
			           pval.size=6,
			           legend.labs=labels,
			           legend.title=titleName,
			           font.legend=13,
			           xlab="Time(years)",
			           break.time.by = 1,
			           palette=c("red","blue") )
		    #输出图片
		    j=gsub(">=","ge",j);j=gsub("<=","le",j);j=gsub(">","gt",j);j=gsub("<","lt",j)
			pdf(file=paste0("survival.",i,"_",j,".pdf"),onefile = FALSE,
			       width = 5,             #图片的宽度
			       height =4.5)           #图片的高度
			print(surPlot)
			dev.off()
		}
	}
}


######Video source: https://ke.biowolf.cn
######生信自学网: https://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：biowolf@foxmail.com
######答疑微信: 18520221056
