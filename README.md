# HW-7

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
