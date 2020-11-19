######Video source: http://ke.biowolf.cn
######生信自学网: http://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056
library(RColorBrewer)
library(pheatmap)
setwd("C:\\Users\\lenovo\\Desktop\\间歇性禁食\\糖酵解\\卵巢癌\\实验组\\22")     #设置工作目录

#绘制train组风险图
rt=read.table("Train.txt",header=T,sep="\t",check.names=F,row.names=1)     #读取train输入文件
rt=rt[order(rt$riskScore),]
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScoreTrain.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("#2E9FDF",lowLength),
     rep("#E7B800",highLength)))
trainMedianScore=median(rt$riskScore)
abline(h=trainMedianScore,v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
dev.off()
#绘制train组风险状态图
color=as.vector(rt$fustat)
color[color==1]="#E7B800"
color[color==0]="#2E9FDF"
pdf(file="survStatTrain.pdf",width = 12,height = 5)
plot(rt$futime,
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topright", c("Dead", "Alive"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

#绘制train组热图(排序)
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmaptrain.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         fontsize_row=10,
         show_colnames = F,
         fontsize_col=3,
         color =colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(50) )
dev.off()

#组图
#plot_grid(plot.point, plot.sur, plot.h$gtable,
#          labels = c("A", "B","C"),
 #         label_x=0,
   #       label_y=1,
    #      align = 'v',ncol = 1,axis="t")

#绘制test组风险图
rt=read.table("riskTest.txt",header=T,sep="\t",check.names=F,row.names=1)       #读取test输入文件
rt=rt[order(rt$riskScore),]
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScoreTest.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("#2E9FDF",lowLength),
     rep("#E7B800",highLength)))
abline(h=trainMedianScore,v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
dev.off()

#绘制test组风险状态图
color=as.vector(rt$fustat)
color[color==1]="#E7B800"
color[color==0]="#2E9FDF"
pdf(file="survStatTest.pdf",width = 12,height = 5)
plot(rt$futime,
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topright", c("Dead", "Alive"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

#绘制test组热图
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmaptest.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         fontsize_row=10,
         show_colnames = F,
         fontsize_col=3,
         color =colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(50) )
dev.off()

#绘制all组风险图
rt=read.table("riskall.txt",header=T,sep="\t",check.names=F,row.names=1)       #读取test输入文件
rt=rt[order(rt$riskScore),]
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScoreall.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("#2E9FDF",lowLength),
           rep("#E7B800",highLength)))
abline(h=trainMedianScore,v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
dev.off()
#绘制all组风险状态图
color=as.vector(rt$fustat)
color[color==1]="#E7B800"
color[color==0]="#2E9FDF"
pdf(file="survStatall.pdf",width = 12,height = 5)
plot(rt$futime,
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topright", c("Dead", "Alive"),bty="n",pch=19,col=c("#E7B800","#2E9FDF"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

#绘制all组热图
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmapall.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         fontsize_row=10,
         show_colnames = F,
         fontsize_col=3,
         color =colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(50) )
dev.off()

######Video source: http://ke.biowolf.cn
######生信自学网: http://www.biowolf.cn/
######微信公众号：biowolf_cn
######合作邮箱：2749657388@qq.com
######答疑微信: 18520221056