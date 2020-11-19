######Video source: http://ke.biowolf.cn
######??????Ñ§??: http://www.biowolf.cn/
######Î¢?Å¹??ÚºÅ£?biowolf_cn
######???????ä£?2749657388@qq.com
######????Î¢??: 18520221056

#install.packages("survival")

setwd("C:\\Users\\yangsi\\Desktop\\·ºËØ»¯¸Î°©\\¸Î°©·ºËØ»¯»ùÒò\\5")   #????Ä¿Â¼?????Þ¸Ä£?


library(survival)
library(dplyr)
library(plyr)
library(survival)
library(ggplot2)
library(gplots)
library(ggpubr)
library(magrittr)
library(survminer)
library(readr)
library(rbsurv)
library(edgeR)
library(survivalROC)
library(MASS)
library(caret)
library(pheatmap)
library(RColorBrewer)

#????train??????????
rt=read.table("risk.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)
fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)
summary(fit)    #?é¿???????????
pdf(file="survival.pdf",width=5.5,height=5)
ggsurvplot(fit, 
           conf.int=F,    # ????????
           #fun="pct",
           pval=TRUE,
           palette =  c("#E7B800", "#2E9FDF"),
           pval.method = T,
           risk.table =T, 
           ncensor.plot = F,
           ggtheme = theme_bw(),
           surv.median.line="hv",
           legend.labs=c("high risk","low risk"))+
  labs(x = "Time (year)")
dev.off()



#????test??????????
rt=read.table("riskTest.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)
fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)
summary(fit)    #?é¿???????????
pdf(file="survivalTest.pdf",width=5.5,height=5)
ggsurvplot(fit, 
           conf.int=F,    # ????????
           #fun="pct",
           pval=TRUE,
           palette =  c("#E7B800", "#2E9FDF"),
           pval.method = T,
           risk.table =T, 
           ncensor.plot = F,
           ggtheme = theme_bw(),
           surv.median.line="hv",
           legend.labs=c("high risk","low risk"))+
  labs(x = "Time (year)")
dev.off()


rt=read.table("riskall.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)
fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)
summary(fit)    #?é¿???????????
pdf(file="survivalall.pdf",width=5.5,height=5)
ggsurvplot(fit, 
           conf.int=F,    # ????????
           #fun="pct",
           pval=TRUE,
           palette =  c("#E7B800", "#2E9FDF"),
           pval.method = T,
           risk.table =T, 
           ncensor.plot = F,
           ggtheme = theme_bw(),
           surv.median.line="hv",
           legend.labs=c("high risk","low risk"))+
  labs(x = "Time (year)")
dev.off()


ggsurvplot(fit,
           conf.int = T,
           ggtheme = theme_bw(), 
           palette =  c("#E7B800", "#2E9FDF"),
           fun = "cumhaz",
   legend.labs=c("high risk","low risk"))+labs(x = "Time (year)")
ggsave(paste('cox_Cumul_hazard.pdf', sep = ""),width = 5.5,height = 5)

multi_sign_gene=colnames(rt[4:11])

for(sign_gene in multi_sign_gene){
  pdf(paste('boxplot_',sign_gene,'.pdf',sep = ""),width = 6,height = 5,onefile = F)
  p <- ggboxplot(rt,x = "Strata", y= sign_gene, color="Strata", 
                 palette=c("#E7B800","#2E9FDF"), add="jitter", shape="Strata")
  my_comparisons <- list(c("high risk",'low risk'))
  p <- p +  stat_compare_means(comparisons = my_comparisons)
  print(p)
  dev.off()
}


######Video source: http://ke.biowolf.cn
######??????Ñ§??: http://www.biowolf.cn/
######Î¢?Å¹??ÚºÅ£?biowolf_cn
######???????ä£?2749657388@qq.com
######????Î¢??: 18520221056