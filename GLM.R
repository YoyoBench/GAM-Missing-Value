UDSPRO = read_sav("X:/Research/Yolanda/Data/UDSPRO V30Q1_V30Q4_2020.sav")
##################################
### choose CMS's 18 vars, filter impairment groups & medicare & missing cost
UDSPRO$dummycase = data.frame(1:nrow(UDSPRO))
UDSPRO$IMPGRPS = as.numeric(UDSPRO$IMPGRPS)
UDSPRO$AV14_ADM_H0350_BLADDER_NUM = as.numeric(UDSPRO$AV14_ADM_H0350_BLADDER)
UDSPRO$AV14_ADM_H0400_BOWEL_NUM = as.numeric(UDSPRO$AV14_ADM_H0400_BOWEL)
UDSPRO = UDSPRO %>% mutate(BLADDER = recode(AV14_ADM_H0350_BLADDER_NUM,`0`=6,`1`=5,`2`=3,`3`=2,`4`=1,`5`=0,`9`=0)) %>%
  mutate(BOWEL = recode(AV14_ADM_H0400_BOWEL_NUM,`0`=6,`1`=3,`2`=2,`3`=1,`9`=0))
DATAFT = UDSPRO %>%
  select(AV14_GG0130A_SC_EATING:AV14_GG0170K_MBL_WALK150FT,
         AV14_GG0170M_MBL_1STAIR, AV14_ADM_H0350_BLADDER, AV14_ADM_H0400_BOWEL,
         AV14_GG0130A_SC_EATING_NUM:AV14_GG0170K_MBL_WALK150FT_NUM,
         
         AV14_GG0170M_MBL_1STAIR_NUM, BLADDER, BOWEL, #AV14_ADM_H0350_BLADDER_NUM, AV14_ADM_H0400_BOWEL_NUM,
         dummycase, ADMITAGE, TIER, TOTALADJUSTEDFPP, IMPGRPS, PRIMPAY, SECPAY) %>%
  select(-AV14_GG0170A_MBL_ROLL, -AV14_GG0170G_MBL_CARTX, -AV14_GG0170H_MBL_PATIENTWALK,
         -AV14_GG0170A_MBL_ROLL_NUM, -AV14_GG0170G_MBL_CARTX_NUM)
IMP_select = data.frame(xtabs(~IMPGRPS, data = DATAFT))
IMP_select = IMP_select %>% filter(Freq > nrow(DATAFT) * .003) %>% select(IMPGRPS)
IMP_select = as.numeric(IMP_select[,1])
DATAFT = DATAFT %>%
  #filter(IMPGRPS %in% IMP_select) %>%
  filter(PRIMPAY %in% c("51","02")|SECPAY %in% c("51","02")) %>%
  filter(!is.na(TOTALADJUSTEDFPP))
DATA2 = DATAFT %>% select(-PRIMPAY, -SECPAY) %>% filter(IMPGRPS %in% IMP_select)
##################################
### GAM ##################
### modify GG Vars
n = 18
DATA2[,1:n] = as.data.frame(apply(DATA2[,1:n], 2, as.numeric))
DATA2 = mutate_at(DATA2, vars(1:n),list(~replace(., . %in% c(0,1,2,3,4,5,6), 0)))
DATA2[,1:n][DATA2[,1:n] != 0] = 1
DATA2[,1:n][is.na(DATA2[,1:n])] = 1
#temp = filter(DATA2, AV14_GG0170M_MBL_1STAIR %in% c(1,0))
DATA2[,1:n] = as.data.frame(apply(DATA2[,1:n], 2, as.factor))
DATA2[,(n+1):(n*2)] = as.data.frame(apply(DATA2[,(n+1):(n*2)], 2, as.numeric))
DATA2[,(n+1):(n*2)][is.na(DATA2[,(n+1):(n*2)])] = 1
DATA2$motorscore = rowSums(DATA2[,(n+1):(n*2)])
#names(DATA2)
#temp = filter(DATA2, is.na(motorscore))
DATA2$TIER = as.factor(DATA2$TIER)
DATA2$LogCost = log(DATA2$TOTALADJUSTEDFPP)
### fit model: logcost = f1(motor) + f2(age) + ??{tier}
gam1 = gam(LogCost ~ bs(motorscore,7) + bs(ADMITAGE,7) + TIER ,data = DATA2)
#summary(gam1)
#gam2 = gam(LogCost ~ ns(motorscore,4) + AGEGRPS + TIER ,data = DATA2)
#summary(gam2)
#gam3 = gam(LogCost ~ ns(motorscore,4) + TIER ,data = DATA2)
#summary(gam3)
#anova(gam3, gam1, gam2, test="F")
#plot(gam1, se=TRUE, col="green ")
#library(ggplot2)
#qplot(x = ADMITAGE, y = LogCost, data = DATA2)

### for i-th var ###
result = data.frame(x = 1, y = 1, z = 1, k = 1)
k=0
#summary(DATA2$IMPGRPS)
for (j in IMP_select) {
  DATAIMP = filter(DATA2, IMPGRPS == j)
  k = k+1
  temp1 = select(DATAIMP, 1:n, dummycase)
  temp2 = DATAIMP[(n+1):(n*2)]
  for (i in 1:n) {
    ### fit model: logcost = f1(motor-i) + f2(age) + ??1{tier} + ??2{missing-i} + ??1*motori + ??2*0
    ### logcost - f1(motor-i) - f2(age) - intercept = ??1{tier} + ??2{missing-i} + ??1*motori
    Gamma2 = temp1 %>% select(-i) %>% filter_all(all_vars(. != "1")) %>% select(dummycase) %>% mutate(MV=0)
    DATAIMP$motors_i = rowSums(temp2[,-i])
    DATA3 = DATAIMP %>% select(i, i+n, motors_i, dummycase, ADMITAGE, TIER, LogCost) %>%
      filter(DATAIMP[,all_of(i)] =='0') %>%
      left_join(Gamma2, by = "dummycase")
    names(DATA3)[2] = "VAR_NUM"
    DATA3$MV = as.factor(ifelse(is.na(DATA3$MV), 1, 0))
    #gam1$coefficients[1]
    #gam1$terms
    PCT_Motor = quantile(DATAIMP$motorscore, c(.2, .4, .6, .8))
    PCT_Age = quantile(DATAIMP$ADMITAGE, c(.2, .4, .6, .8))
    DATA3 = mutate(DATA3, Y = LogCost - gam1$coefficients[1]
                   - (gam1$coefficients[2]*motors_i + gam1$coefficients[3] *motors_i^2 + gam1$coefficients[4]*motors_i^3
                      + gam1$coefficients[5]*(motors_i - PCT_Motor[1])^3 + gam1$coefficients[6]*(motors_i - PCT_Motor[2])^3
                      + gam1$coefficients[7]*(motors_i - PCT_Motor[3])^3 + gam1$coefficients[8]*(motors_i - PCT_Motor[4])^3)
                   - (gam1$coefficients[9]*ADMITAGE + gam1$coefficients[10] *ADMITAGE^2 + gam1$coefficients[11]*ADMITAGE^3
                      + gam1$coefficients[12]*(ADMITAGE - PCT_Age[1])^3 + gam1$coefficients[13]*(ADMITAGE - PCT_Age[2])^3
                      + gam1$coefficients[14]*(ADMITAGE - PCT_Age[3])^3 + gam1$coefficients[15]*(ADMITAGE - PCT_Age[4])^3))
    #TEMP = filter(DATA3, is.na(Y))
    gam2 = gam(Y ~ TIER + MV + VAR_NUM, data = DATA3)
    #summary(gam2)
    #gam2$coefficients
    #preds=predict(gam1, newdata = DATA3)
    #summary(preds)
    ### fit model: logcost = f1(motor-i) + f2(age) + ??1{tier} + ??2{missing-i} + ??1*motori + ??2*missingi
    
    ### logcost - (f1(motor-i) + f2(age) + ??1{tier} + ??2{missing-i} + ??1) = ??2
    DATA4 = DATAIMP %>% select(i, i+n, motors_i, dummycase, ADMITAGE, TIER, LogCost, IMPGRPS) %>%
      filter(DATAIMP[,i] =='1') %>%
      left_join(Gamma2, by = "dummycase")
    DATA4$MV = ifelse(is.na(DATA4$MV), 1, 0)
    DATA4$TIER1 = ifelse(DATA4$TIER == "1", 1,0)
    DATA4$TIER2 = ifelse(DATA4$TIER == "2", 1,0)
    DATA4$TIER3 = ifelse(DATA4$TIER == "3", 1,0)
    DATA4 = mutate(DATA4, Y = LogCost - gam1$coefficients[1] - gam2$coefficients[1]
                   - (gam1$coefficients[2]*motors_i + gam1$coefficients[3] *motors_i^2 + gam1$coefficients[4]*motors_i^3
                      + gam1$coefficients[5]*(motors_i - PCT_Motor[1])^3 + gam1$coefficients[6]*(motors_i - PCT_Motor[2])^3
                      + gam1$coefficients[7]*(motors_i - PCT_Motor[3])^3 + gam1$coefficients[8]*(motors_i - PCT_Motor[4])^3)
                   - (gam1$coefficients[9]*ADMITAGE + gam1$coefficients[10] *ADMITAGE^2 + gam1$coefficients[11]*ADMITAGE^3
                      + gam1$coefficients[12]*(ADMITAGE - PCT_Age[1])^3 + gam1$coefficients[13]*(ADMITAGE - PCT_Age[2])^3
                      + gam1$coefficients[14]*(ADMITAGE - PCT_Age[3])^3 + gam1$coefficients[15]*(ADMITAGE - PCT_Age[4])^3)
                   - (gam2$coefficients[2]*TIER1 + gam2$coefficients[3]*TIER2 + gam2$coefficients[4]*TIER3)
                   - gam2$coefficients[5]*MV - gam2$coefficients[6])
    #hist(DATA4$Y)
    #boxplot(DATA4$Y)
    result[i+n*(k-1),] = data.frame(x = 1 + mean(DATA4$Y) /gam2$coefficients[6], y = names(DATA4)[1], z = mean(DATA4$IMPGRPS), k = nrow(DATAIMP))
  }}
result[,1][result[,1] > 6] = 6
result[,1][result[,1] < 1] = 1
names(result) = c("GG", "GG Items", "IMPGRP", "Counts")
result2 = result %>% group_by(`GG Items`) %>% summarise_at(vars(`GG`), median)
##################################
#### PLOT
DATAPLOT = read.csv("X:/Research/Yolanda/ACRM Conference/RAND_PLOT.csv")
DATAPLOT = filter(DATAPLOT, GG.Items %in% c('Eating', 'Toileting',
                                            'BedChairTransfer', 'ToiletTransfer'))
names(DATAPLOT) = c('GG.Items','2017','2018','2019','2020','Total')
temp = gather(DATAPLOT, FiscalYear, GGScore, '2017':Total, factor_key=TRUE)
ggplot(temp, aes(x = GG.Items, y = GGScore, group = FiscalYear, color = FiscalYear)) +
  geom_col(aes(fill = FiscalYear), position = "dodge", linetype=0) +
  scale_fill_manual(values = c('steelblue2','dodgerblue1','skyblue1','deepskyblue1','dodgerblue3')) +
  theme(legend.position="bottom") + #ylim(0, 6)
  scale_y_continuous(breaks=seq(0, 6, 1), limits = c(0, 6))