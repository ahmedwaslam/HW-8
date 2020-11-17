# HW-7

By Antonio Sosa, Ahmed Aslam, Mirajul Fahim 

````
load("/cloud/project/NHIS_2014.RData")
> data_use1$earn_lastyr <- as.factor(data_use1$ERNYR_P)
> 
> levels(data_use1$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
> 
> dat2 <- subset(data_use1, ((AGE_P >= 17) & (AGE_P <= 75)))
> 
> covprop <- dat2$NOTCOV 
> 
> covtable <- table(covprop)
> 
> agecovtable <- table(dat2$AGE_P, dat2$NOTCOV)
> 
> round((prop.table(covtable)[1])*100, digits = 0) 
 0 
85 
> 
> model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + REGION + region_born,family = binomial, data = dat2)
> 
> summary(model_logit1)

Call:
glm(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + 
    RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
    educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
    REGION + region_born, family = binomial, data = dat2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9027  -0.5923  -0.3807  -0.1889   3.3181  

Coefficients:
                                Estimate Std. Error
(Intercept)                   -3.475e+00  1.056e-01
AGE_P                          1.455e-01  5.235e-03
I(AGE_P^2)                    -2.031e-03  6.192e-05
female                        -2.804e-01  2.216e-02
AfAm                          -1.443e-01  3.299e-02
Asian                         -2.245e-01  7.539e-02
RaceOther                      5.108e-01  6.640e-02
Hispanic                       3.185e-01  3.456e-02
educ_hs                       -2.222e-01  2.988e-02
educ_smcoll                   -6.288e-01  3.465e-02
educ_as                       -7.498e-01  4.271e-02
educ_bach                     -1.442e+00  4.378e-02
educ_adv                      -2.087e+00  7.160e-02
married                       -6.677e-01  2.751e-02
widowed                       -3.463e-02  8.582e-02
divorc_sep                    -4.753e-02  3.892e-02
veteran_stat                  -5.787e-01  5.962e-02
REGIONMidwest                  2.882e-01  4.068e-02
REGIONSouth                    6.940e-01  3.522e-02
REGIONWest                     2.874e-01  3.721e-02
region_bornMex Cent Am Caribb  1.072e+00  3.851e-02
region_bornS Am                9.464e-01  8.515e-02
region_bornEur                 3.699e-01  1.024e-01
region_bornformer USSR         9.618e-01  2.057e-01
region_bornAfrica              8.155e-01  1.092e-01
region_bornMidE                4.319e-01  1.753e-01
region_bornIndia subc          9.019e-01  1.209e-01
region_bornAsia                9.204e-01  1.136e-01
region_bornSE Asia             4.811e-01  1.041e-01
region_bornElsewhere           2.878e-01  1.626e-01
region_bornunknown            -9.682e-02  1.912e-01
                              z value Pr(>|z|)    
(Intercept)                   -32.897  < 2e-16 ***
AGE_P                          27.791  < 2e-16 ***
I(AGE_P^2)                    -32.808  < 2e-16 ***
female                        -12.653  < 2e-16 ***
AfAm                           -4.375 1.21e-05 ***
Asian                          -2.977 0.002907 ** 
RaceOther                       7.694 1.43e-14 ***
Hispanic                        9.216  < 2e-16 ***
educ_hs                        -7.438 1.02e-13 ***
educ_smcoll                   -18.148  < 2e-16 ***
educ_as                       -17.556  < 2e-16 ***
educ_bach                     -32.944  < 2e-16 ***
educ_adv                      -29.152  < 2e-16 ***
married                       -24.271  < 2e-16 ***
widowed                        -0.404 0.686554    
divorc_sep                     -1.221 0.222005    
veteran_stat                   -9.707  < 2e-16 ***
REGIONMidwest                   7.084 1.40e-12 ***
REGIONSouth                    19.704  < 2e-16 ***
REGIONWest                      7.724 1.13e-14 ***
region_bornMex Cent Am Caribb  27.843  < 2e-16 ***
region_bornS Am                11.115  < 2e-16 ***
region_bornEur                  3.613 0.000303 ***
region_bornformer USSR          4.675 2.94e-06 ***
region_bornAfrica               7.469 8.05e-14 ***
region_bornMidE                 2.464 0.013741 *  
region_bornIndia subc           7.458 8.80e-14 ***
region_bornAsia                 8.104 5.32e-16 ***
region_bornSE Asia              4.623 3.79e-06 ***
region_bornElsewhere            1.770 0.076771 .  
region_bornunknown             -0.506 0.612529    
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 67348  on 78049  degrees of freedom
Residual deviance: 55501  on 78019  degrees of freedom
  (1522 observations deleted due to missingness)
AIC: 55563

Number of Fisher Scoring iterations: 6

> d_region <- data.frame(model.matrix(~ dat2$REGION))
> 
> d_region_born <- data.frame(model.matrix(~ factor(dat2$region_born)))
> 
> dat_for_analysis_sub <- data.frame(dat2$NOTCOV,dat2$AGE_P,dat2$female,dat2$AfAm,dat2$Asian,dat2$RaceOther,dat2$Hispanic,dat2$educ_hs,dat2$educ_smcoll,dat2$educ_as,dat2$educ_bach,dat2$educ_adv,dat2$married,dat2$widowed,dat2$divorc_sep,d_region[,2:4],d_region_born[,2:12]) 
> 
> names(dat_for_analysis_sub) <- c("NOTCOV","Age", "female", "AfAm","Asian","RaceOther","Hispanic","educ_hs","educ_smcoll","educ_as","educ_bach","educ_adv","married","widowed","divorc_sep","Region.Midwest","Region.South", "Region.West","born.Mex.CentAm.Carib","born.S.Am","born.Eur", "born.f.USSR", "born.Africa", "born.MidE","born.India.subc", "born.Asia","born.SE.Asia","born.elsewhere","born.unknown")

summary(model_OLS1)

Call:
lm(formula = sobj$formula, data = sobj$data)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.60732 -0.17995 -0.09605 -0.00931  1.11770 

Coefficients:
                         Estimate Std. Error t value
(Intercept)             0.5574261  0.0642340   8.678
Age                    -0.0396285  0.0032580 -12.163
female1                -0.0144371  0.0027148  -5.318
AfAm1                  -0.0191034  0.0041975  -4.551
Asian1                 -0.0158563  0.0087896  -1.804
RaceOther1              0.0421389  0.0100458   4.195
Hispanic1               0.0193547  0.0047790   4.050
educ_hs1               -0.0002775  0.0043536  -0.064
educ_smcoll1           -0.0315379  0.0046966  -6.715
educ_as1               -0.0267065  0.0054248  -4.923
educ_bach1             -0.0554831  0.0048984 -11.327
educ_adv1              -0.0656082  0.0057682 -11.374
married1               -0.0295291  0.0034983  -8.441
widowed1               -0.0098762  0.0089221  -1.107
divorc_sep1            -0.0018255  0.0051154  -0.357
Region.Midwest1         0.0110894  0.0044993   2.465
Region.South1           0.0363097  0.0040484   8.969
Region.West1            0.0129634  0.0042273   3.067
born.Mex.CentAm.Carib1  0.1060590  0.0058450  18.145
born.S.Am1              0.0398128  0.0132271   3.010
born.Eur1               0.0161050  0.0112666   1.429
born.f.USSR1            0.0726396  0.0268625   2.704
born.Africa1            0.0635681  0.0152544   4.167
born.MidE1              0.0528245  0.0207550   2.545
born.India.subc1        0.0508833  0.0144997   3.509
born.Asia1              0.0368217  0.0137450   2.679
born.SE.Asia1           0.0413706  0.0121460   3.406
born.elsewhere1         0.0125449  0.0190534   0.658
born.unknown1           0.0333367  0.0259793   1.283
                       Pr(>|t|)    
(Intercept)             < 2e-16 ***
Age                     < 2e-16 ***
female1                1.06e-07 ***
AfAm1                  5.38e-06 ***
Asian1                 0.071252 .  
RaceOther1             2.75e-05 ***
Hispanic1              5.15e-05 ***
educ_hs1               0.949186    
educ_smcoll1           1.94e-11 ***
educ_as1               8.61e-07 ***
educ_bach1              < 2e-16 ***
educ_adv1               < 2e-16 ***
married1                < 2e-16 ***
widowed1               0.268334    
divorc_sep1            0.721194    
Region.Midwest1        0.013723 *  
Region.South1           < 2e-16 ***
Region.West1           0.002169 ** 
born.Mex.CentAm.Carib1  < 2e-16 ***
born.S.Am1             0.002617 ** 
born.Eur1              0.152896    
born.f.USSR1           0.006856 ** 
born.Africa1           3.10e-05 ***
born.MidE1             0.010933 *  
born.India.subc1       0.000451 ***
born.Asia1             0.007394 ** 
born.SE.Asia1          0.000661 ***
born.elsewhere1        0.510288    
born.unknown1          0.199440    
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3382 on 15840 degrees of freedom
Multiple R-squared:  0.1195,	Adjusted R-squared:  0.118 
F-statistic: 76.79 on 28 and 15840 DF,  p-value: < 2.2e-16

summary(model_OLS1)

> pred_vals_OLS <- suppressWarnings(predict(model_OLS1, s_dat_test))
> 
> pred_model_OLS1 <- (pred_vals_OLS > 0.45)
> 
> pred1OLStable <- table(pred = pred_model_OLS1, true = dat_test$NOTCOV)
> 
> pred1OLStable
       true
pred        0     1
  FALSE 53304  8648
  TRUE    600  1151
  
  goodolspred <- sum((prop.table(pred1OLStable)[1,1])+(prop.table(pred1OLStable)[2,2]))
> 
> model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
> 
> summary(model_logit1)

Call:
glm(formula = sobj$formula, family = binomial, data = sobj$data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6894  -0.5900  -0.4118  -0.2520   3.1371  

Coefficients:
                       Estimate Std. Error z value
(Intercept)             1.44575    0.53384   2.708
Age                    -0.36174    0.02999 -12.061
female1                -0.12429    0.02388  -5.204
AfAm1                  -0.12353    0.03711  -3.328
Asian1                 -0.11368    0.08023  -1.417
RaceOther1              0.27658    0.07286   3.796
Hispanic1               0.13967    0.03768   3.707
educ_hs1                0.02732    0.03222   0.848
educ_smcoll1           -0.21846    0.03727  -5.862
educ_as1               -0.18047    0.04535  -3.979
educ_bach1             -0.54977    0.04779 -11.505
educ_adv1              -0.89516    0.08050 -11.120
married1               -0.23345    0.03015  -7.743
widowed1               -0.05472    0.08773  -0.624
divorc_sep1             0.03613    0.04319   0.836
Region.Midwest1         0.11440    0.04444   2.574
Region.South1           0.33384    0.03819   8.741
Region.West1            0.14031    0.04022   3.488
born.Mex.CentAm.Carib1  0.61318    0.04189  14.639
born.S.Am1              0.37133    0.10013   3.708
born.Eur1               0.16564    0.10977   1.509
born.f.USSR1            0.67979    0.20268   3.354
born.Africa1            0.54964    0.12071   4.553
born.MidE1              0.53365    0.16845   3.168
born.India.subc1        0.53581    0.12765   4.197
born.Asia1              0.37627    0.12526   3.004
born.SE.Asia1           0.37923    0.10844   3.497
born.elsewhere1         0.13292    0.17891   0.743
born.unknown1           0.32710    0.20333   1.609
                       Pr(>|z|)    
(Intercept)            0.006765 ** 
Age                     < 2e-16 ***
female1                1.95e-07 ***
AfAm1                  0.000873 ***
Asian1                 0.156476    
RaceOther1             0.000147 ***
Hispanic1              0.000210 ***
educ_hs1               0.396536    
educ_smcoll1           4.58e-09 ***
educ_as1               6.92e-05 ***
educ_bach1              < 2e-16 ***
educ_adv1               < 2e-16 ***
married1               9.72e-15 ***
widowed1               0.532830    
divorc_sep1            0.402927    
Region.Midwest1        0.010039 *  
Region.South1           < 2e-16 ***
Region.West1           0.000486 ***
born.Mex.CentAm.Carib1  < 2e-16 ***
born.S.Am1             0.000209 ***
born.Eur1              0.131292    
born.f.USSR1           0.000797 ***
born.Africa1           5.28e-06 ***
born.MidE1             0.001535 ** 
born.India.subc1       2.70e-05 ***
born.Asia1             0.002665 ** 
born.SE.Asia1          0.000470 ***
born.elsewhere1        0.457496    
born.unknown1          0.107674    
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 13587  on 15868  degrees of freedom
Residual deviance: 11728  on 15840  degrees of freedom
AIC: 11786

Number of Fisher Scoring iterations: 6

> pred_vals <- suppressWarnings(predict(model_logit1, s_dat_test, type = "response"))
> 
> pred_model_logit1 <- (pred_vals > 0.5)
> 
> pred1Logtable <- table(pred = pred_model_logit1, true = dat_test$NOTCOV)
> 
> pred1Logtable
       true
pred        0     1
  FALSE 53268  8661
  TRUE    636  1138

goodlogpred <- sum((prop.table(pred1Logtable)[1,1])+(prop.table(pred1Logtable)[2,2]))

> goodlogpred <- sum((prop.table(pred1Logtable)[1,1])+(prop.table(pred1Logtable)[2,2]))
> require('randomForest')
> set.seed(54321)
> model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
> print(model_randFor)

Call:
 randomForest(formula = as.factor(NOTCOV) ~ ., data = sobj$data,      importance = TRUE, proximity = TRUE) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 5

        OOB estimate of  error rate: 13.77%
Confusion matrix:
      0   1 class.error
0 13207 220   0.0163849
1  1952 398   0.8306383
> round(importance(model_randFor),2)
                          0      1 MeanDecreaseAccuracy MeanDecreaseGini
Age                   37.76  41.85                52.82           335.30
female                 5.57   7.51                 8.85            36.76
AfAm                   0.47  21.61                16.04            26.19
Asian                  9.58   1.69                11.19            14.46
RaceOther              5.90   8.32                 9.75            19.79
Hispanic              -9.81  31.35                36.33           123.57
educ_hs               17.99  -9.37                14.97            33.46
educ_smcoll           13.63  14.93                22.66            29.61
educ_as               14.35  11.69                19.96            21.13
educ_bach             20.35  26.99                33.81            43.40
educ_adv              25.42  27.85                36.67            40.04
married               28.37 -16.28                27.19            52.56
widowed                5.99   0.61                 6.61             9.90
divorc_sep             8.74  -2.07                 7.25            20.14
Region.Midwest         0.73   7.99                 7.03            18.81
Region.South           4.47  17.84                19.03            37.55
Region.West           -2.24  10.65                 8.65            24.74
born.Mex.CentAm.Carib -1.70  61.87                48.10           201.37
born.S.Am              6.60  12.33                13.10            13.77
born.Eur               2.27  -2.56                 1.05             7.80
born.f.USSR           -5.49  -4.43                -6.35             2.56
born.Africa           -1.78   5.20                -0.03            10.47
born.MidE             -4.93  -1.39                -5.12             2.69
born.India.subc       13.10   1.79                14.34             8.60
born.Asia              2.24   1.32                 2.77             9.81
born.SE.Asia           9.40  -2.38                 9.19             8.69
born.elsewhere         2.25   1.81                 2.76             7.43
born.unknown          -1.84  -0.89                -2.17             2.78
> varImpPlot(model_randFor)
> pred_model1 <- predict(model_randFor,  s_dat_test)
> RFpredtable <- table(pred = pred_model1, true = dat_test$NOTCOV)
> RFpredtable
    true
pred     0     1
   0 52996  8269
   1   920  1610
> RFproppred <- prop.table(RFpredtable)
> RFgoodpred <- sum((RFproppred[1,1])+(RFproppred[2,2]))
> RFfalsepos <- RFproppred[2,1]
> RFfalseneg <- RFproppred[1,2]
> install.packages("e1071")
Error in install.packages : Updating loaded packages
> install.packages("e1071")
trying URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/e1071_1.7-4.tgz'
Content type 'application/x-gzip' length 896254 bytes (875 KB)
==================================================
downloaded 875 KB


The downloaded binary packages are in
	/var/folders/9h/qrwc6h4s64v7xr5ncqrlvd_80000gn/T//RtmpeN6Rox/downloaded_packages
> require(e1071)
> svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
> svm.pred <- predict(svm.model, s_dat_test)
> SVMpredtable <- table(pred = svm.pred, true = dat_test$NOTCOV)
> SVMpredtable
    true
pred     0     1
   0 52412  7986
   1  1504  1893
> SVMproppred <- prop.table(SVMpredtable)
> SVMproppred
    true
pred          0          1
   0 0.82156909 0.12518222
   1 0.02357552 0.02967317
> SVMgoodpred <- sum((SVMproppred[1,1])+(SVMproppred[2,2]))
> SVMgoodpred
[1] 0.8512423
> 
> 
> svm.model2 <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10)
> svm.pred2 <- predict(svm.model2, s_dat_test)
> SVMpredtable2 <- table(pred = svm.pred2, true = dat_test$NOTCOV)
> SVMpredtable2
    true
pred     0     1
   0 52884  8124
   1  1032  1755
> SVMproppred2 <- prop.table(SVMpredtable2)
> SVMproppred2
    true
pred          0          1
   0 0.82896779 0.12734540
   1 0.01617682 0.02750999
> SVMgoodpred2 <- sum((SVMproppred2[1,1])+(SVMproppred2[2,2]))
> SVMgoodpred2
[1] 0.8564778
> SVMproppred
    true
pred          0          1
   0 0.82156909 0.12518222
   1 0.02357552 0.02967317
   

All the predictions of all the various algorithims were roughly 84 percent prediction success rate relative to the training data. The OLS, logit and vector models were all predicting the similar outcomes. 


````


