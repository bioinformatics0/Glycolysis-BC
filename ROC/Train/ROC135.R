install.packages("timeROC")


library(survival)
library(timeROC)

dir="C:\\Users\\lenovo\\Desktop\\间歇性禁食\\糖酵解\\列线图后ROC"
setwd(dir)
miRNA<-read.table("input.txt",header=T,sep="\t")
predict_1_year<- 1
predict_3_year<- 3
predict_5_year<- 5

ROC<-timeROC(T=miRNA$futime,delta=miRNA$fustat,
                  marker=miRNA$riskScore,cause=1,
                  weighting="marginal",
                  times=c(predict_1_year,predict_3_year,predict_5_year),ROC=TRUE)

pdf("ROC135.pdf")
plot(ROC,time=predict_1_year,col="blue",title=FALSE,lwd=3)
plot(ROC,time=predict_3_year,col="green",add=TRUE,title=FALSE,lwd=3)
plot(ROC,time=predict_5_year,col="red",add=TRUE,title=FALSE,lwd=3)
legend("bottomright",
       c(paste("AUC of 1 year survival: ",round(ROC$AUC[1],3)),
         paste("AUC of 3 year survival: ",round(ROC$AUC[2],3)),
         paste("AUC of 5 year survival: ",round(ROC$AUC[3],3))),col=c("blue","green","red"),lwd=3)
dev.off()

#AUC
0.5      #没???魏?预????锟斤拷
0.51-0.7 #??准确??
0.71-0.9 #?械?准确??
>0.9     #??准确??   颜色 #2E9FDF   #E7B800
