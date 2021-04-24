## ANOVA_AB.R
## Xiangfei Han
## 两因素分析
## 2021/01/23


# 加载Packages
# 1. agricolae包中有LSD检验
library(agricolae)          # install.packages('agricolae')
# 2. 数据框初始化
library(plyr)               # install.packages('plyr')
# 3.数据导入和处理
library(rio)                         # install.packages("rio")
library(tidyverse)                   # install.packages("tidyverse")


ANOVA_AXX <- function(dat, StartColumnNo, EndColumnNo)
{
  # 缺失参数赋值
  if(missing(EndColumnNo))
    EndColumnNo=length(dat)
  
  # 数据起始、终止列
  n = StartColumnNo
  m = EndColumnNo
  
  name_A = ddply(dat,.(A),summarize,number=length(A))
  
  for(j in 1:nrow(name_A))
  {
    B_AOV = ddply(dat,.(B),summarize,number=length(B))
    B_AVG = ddply(dat,.(B),summarize,number=length(B))
    B_STD = ddply(dat,.(B),summarize,number=length(B))
    PB_Value = data.frame(Trt = c("B          "))
    FB_Value = data.frame(Trt = c("B          "))
    
    dat %>%
      filter(A == name_A$A[j]) -> dat_A
    
    for(i in n:m)
    {
      #aov函数、LSD.test函数
      B.aov = aov(dat_A[,i] ~ B, dat_A)
      B.LSD = LSD.test(B.aov, "B", p.adj="none")
      B_summary = summary(B.aov)
      
      #导出 均值、STD和LSD
      DFB_AVG = data.frame(B = row.names(B.LSD[["means"]]), AVG = B.LSD[["means"]][["dat_A[, i]"]])
      DFB_STD = data.frame(B = row.names(B.LSD[["means"]]), STD = B.LSD[["means"]][["std"]])
      DFB_AOV = data.frame(B = row.names(B.LSD[["groups"]]), AOV = B.LSD[["groups"]][["groups"]])
      
      #导出 P值和F值
      DF_F = data.frame(Trt = row.names(B_summary[[1]]), x = B_summary[[1]][["F value"]])
      DF_P = data.frame(Trt = row.names(B_summary[[1]]), y = B_summary[[1]][["Pr(>F)"]])
      
      #列名赋值
      AVG_NAME = paste(names(dat_A[i]), "_AVG", sep="")
      STD_NAME = paste(names(dat_A[i]), "_STD", sep="")
      AOV_NAME = paste(names(dat_A[i]), "_AOV", sep="")
      F_NAME = paste(names(dat_A[i]), "_F_Value", sep="")
      P_NAME = paste(names(dat_A[i]), "_P_Value", sep="")
      
      #列名修改
      names(DFB_AVG) = c("B",  AVG_NAME)
      names(DFB_STD) = c("B",  STD_NAME)
      names(DFB_AOV) = c("B",  AOV_NAME)
      names(DF_F) = c("Trt", F_NAME)
      names(DF_P) = c("Trt", P_NAME)
      
      #数据合并
      B_AOV = merge(B_AOV, DFB_AOV, by="B", all = TRUE)
      B_AVG = merge(B_AVG, DFB_AVG, by="B", all = TRUE)
      B_STD = merge(B_STD, DFB_STD, by="B", all = TRUE)
      PB_Value = merge(PB_Value, DF_P, by="Trt", all = TRUE)
      FB_Value = merge(FB_Value, DF_F, by="Trt", all = TRUE)
    }
    
    write.table(paste("Significance of Bi in factor ", name_A$A[j], ".", sep = ""), "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(B_AVG, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(B_AOV, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(B_STD, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(PB_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(FB_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  }    
}


ANOVA_BXX <- function(dat, StartColumnNo, EndColumnNo)
{
  # 缺失参数赋值
  if(missing(EndColumnNo))
    EndColumnNo=length(dat)
  
  # 数据起始、终止列
  n = StartColumnNo
  m = EndColumnNo
  
  name_B = ddply(dat,.(B),summarize,number=length(B))
  
  for(j in 1:nrow(name_B))
  {
    A_AOV = ddply(dat,.(A),summarize,number=length(A))
    A_AVG = ddply(dat,.(A),summarize,number=length(A))
    A_STD = ddply(dat,.(A),summarize,number=length(A))
    PA_Value = data.frame(Trt = c("A          "))
    FA_Value = data.frame(Trt = c("A          "))
    
    dat %>%
      filter(B == name_B$B[j]) -> dat_B
    
    for(i in n:m)
    {
      #aov函数、LSD.test函数
      A.aov = aov(dat_B[,i] ~ A, dat_B)
      A.LSD = LSD.test(A.aov, "A", p.adj="none")
      A_summary = summary(A.aov)
      
      #导出 均值、STD和LSD
      DFA_AVG = data.frame(A = row.names(A.LSD[["means"]]), AVG = A.LSD[["means"]][["dat_B[, i]"]])
      DFA_STD = data.frame(A = row.names(A.LSD[["means"]]), STD = A.LSD[["means"]][["std"]])
      DFA_AOV = data.frame(A = row.names(A.LSD[["groups"]]), AOV = A.LSD[["groups"]][["groups"]])
      
      #导出 P值和F值
      DF_F = data.frame(Trt = row.names(A_summary[[1]]), x = A_summary[[1]][["F value"]])
      DF_P = data.frame(Trt = row.names(A_summary[[1]]), y = A_summary[[1]][["Pr(>F)"]])
      
      #列名赋值
      AVG_NAME = paste(names(dat_B[i]), "_AVG", sep="")
      STD_NAME = paste(names(dat_B[i]), "_STD", sep="")
      AOV_NAME = paste(names(dat_B[i]), "_AOV", sep="")
      F_NAME = paste(names(dat_B[i]), "_F_Value", sep="")
      P_NAME = paste(names(dat_B[i]), "_P_Value", sep="")
      
      #列名修改
      names(DFA_AVG) = c("A",  AVG_NAME)
      names(DFA_STD) = c("A",  STD_NAME)
      names(DFA_AOV) = c("A",  AOV_NAME)
      names(DF_F) = c("Trt", F_NAME)
      names(DF_P) = c("Trt", P_NAME)
      
      #数据合并
      A_AOV = merge(A_AOV, DFA_AOV, by="A", all = TRUE)
      A_AVG = merge(A_AVG, DFA_AVG, by="A", all = TRUE)
      A_STD = merge(A_STD, DFA_STD, by="A", all = TRUE)
      PA_Value = merge(PA_Value, DF_P, by="Trt", all = TRUE)
      FA_Value = merge(FA_Value, DF_F, by="Trt", all = TRUE)
    }
    
    write.table(paste("Significance of Ai in factor ", name_B$B[j], ".", sep = ""), "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(A_AVG, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(A_AOV, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(A_STD, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(PA_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
    write.table(FA_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  }    
}


## 两因素分析
ANOVA_AB <- function(dat, StartColumnNo, EndColumnNo)
{
  # 缺失参数赋值
  if(missing(EndColumnNo))
    EndColumnNo=length(dat)
  
  # 数据起始、终止列
  n = StartColumnNo
  m = EndColumnNo
  
  # 数据框初始化
  A_AOV = ddply(dat,.(A),summarize,number=length(A))
  A_AVG = ddply(dat,.(A),summarize,number=length(A))
  A_STD = ddply(dat,.(A),summarize,number=length(A))
  B_AOV = ddply(dat,.(B),summarize,number=length(B))
  B_AVG = ddply(dat,.(B),summarize,number=length(B))
  B_STD = ddply(dat,.(B),summarize,number=length(B))
  P_Value = data.frame(Trt = c("A          "))
  F_Value = data.frame(Trt = c("A          "))
  
  # 开始分析
  for(i in n:m)
  {
    #aov函数、LSD.test函数
    AB.aov = aov(dat[,i] ~ A*B, dat)
    A.LSD = LSD.test(AB.aov, "A", p.adj="none")
    B.LSD = LSD.test(AB.aov, "B", p.adj="none")
    ABC_summary = summary(AB.aov)
    
    #导出 均值、STD和LSD
    DFA_AVG = data.frame(A = row.names(A.LSD[["means"]]), AVG = A.LSD[["means"]][["dat[, i]"]])
    DFA_STD = data.frame(A = row.names(A.LSD[["means"]]), STD = A.LSD[["means"]][["std"]])
    DFA_AOV = data.frame(A = row.names(A.LSD[["groups"]]), AOV = A.LSD[["groups"]][["groups"]])
    DFB_AVG = data.frame(B = row.names(B.LSD[["means"]]), AVG = B.LSD[["means"]][["dat[, i]"]])
    DFB_STD = data.frame(B = row.names(B.LSD[["means"]]), STD = B.LSD[["means"]][["std"]])
    DFB_AOV = data.frame(B = row.names(B.LSD[["groups"]]), AOV = B.LSD[["groups"]][["groups"]])
    
    #导出 P值和F值
    DF_F = data.frame(Trt = row.names(ABC_summary[[1]]), x = ABC_summary[[1]][["F value"]])
    DF_P = data.frame(Trt = row.names(ABC_summary[[1]]), y = ABC_summary[[1]][["Pr(>F)"]])
    
    #列名赋值
    AVG_NAME = paste(names(dat[i]), "_AVG", sep="")
    STD_NAME = paste(names(dat[i]), "_STD", sep="")
    AOV_NAME = paste(names(dat[i]), "_AOV", sep="")
    F_NAME = paste(names(dat[i]), "_F_Value", sep="")
    P_NAME = paste(names(dat[i]), "_P_Value", sep="")
    
    #列名修改
    names(DFA_AVG) = c("A",  AVG_NAME)
    names(DFA_STD) = c("A",  STD_NAME)
    names(DFA_AOV) = c("A",  AOV_NAME)
    names(DFB_AVG) = c("B",  AVG_NAME)
    names(DFB_STD) = c("B",  STD_NAME)
    names(DFB_AOV) = c("B",  AOV_NAME)
    names(DF_F) = c("Trt", F_NAME)
    names(DF_P) = c("Trt", P_NAME)
    
    #数据合并
    A_AOV = merge(A_AOV, DFA_AOV, by="A", all = TRUE)
    A_AVG = merge(A_AVG, DFA_AVG, by="A", all = TRUE)
    A_STD = merge(A_STD, DFA_STD, by="A", all = TRUE)
    B_AOV = merge(B_AOV, DFB_AOV, by="B", all = TRUE)
    B_AVG = merge(B_AVG, DFB_AVG, by="B", all = TRUE)
    B_STD = merge(B_STD, DFB_STD, by="B", all = TRUE)
    P_Value = merge(P_Value, DF_P, by="Trt", all = TRUE)
    F_Value = merge(F_Value, DF_F, by="Trt", all = TRUE)
  }
  
  #导出数据
  write.table(Sys.time(), "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(A_AVG, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(A_AOV, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(A_STD, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(B_AVG, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(B_AOV, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(B_STD, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(P_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  write.table(F_Value, "ANOVA.csv", row.names=FALSE, sep=",", append=TRUE)
  
  ANOVA_AXX(dat, EndColumnNo)
  ANOVA_BXX(dat, EndColumnNo)
  
}

