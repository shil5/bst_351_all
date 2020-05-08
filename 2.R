library(survival)                # for Surv(), survfit()
## global estimate
KM0 <- survfit(Surv(obsT, status) ~ 1,  type="kaplan-meier", conf.type="log", data=dfSurv)

## separate estimate for all strata
(KM <- survfit(Surv(obsT, status) ~ IV, type="kaplan-meier", conf.type="log", data=dfSurv))
Call: survfit(formula = Surv(obsT, status) ~ IV, data = dfSurv, type = "kaplan-meier", 
              conf.type = "log")
142
149
320
560
805
1720
5230
6890

        time n.max n.start events median 0.95LCL 0.95UCL   
IV=A          60      60     53   13.0       8      29
IV=B      60    60      60     46   35.0      20      58
IV=C      60    60      60     58    9.5       4      13