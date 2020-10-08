clear
estimates clear
set more off
log using "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\LogCovidnas.smcl", replace
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\BCovidnas.dta", clear

*analyzing the variables
describe covid lstay age nas cd ci ct
sum covid lstay nas age cd ci ct
by covid, sort : summarize cd, detail
by covid, sort : summarize ci, detail
by covid, sort : summarize ct, detail

*analyzing the variables between covid and non-covid groups
by covid, sort: summarize lstay, detail
by covid, sort: summarize age
by covid, sort: summarize nas
by covid, sort: summarize cd
by covid, sort: summarize ci
by covid, sort: summarize ct

*analyzing the sample distribution and applying SWilk Test 
histogram lstay, by(covid) normal
by covid, sort: swilk lstay
histogram age, by(covid) normal
by covid, sort: swilk age
histogram nas, by(covid) normal
by covid, sort: swilk nas
histogram cd, by(covid) normal
by covid, sort: swilk cd
histogram ci, by(covid) normal
by covid, sort: swilk ci
histogram ct, by(covid) normal
by covid, sort: swilk ct

*Median Test: MANN-WHITNEY
ranksum lstay, by(covid)porder
ranksum age, by(covid)porder
ranksum nas, by(covid)porder
ranksum cd, by(covid)porder
ranksum ci, by(covid)porder
ranksum ct, by(covid)porder

*Variance Test: KOLMOGOROV SMIRNOV
ksmirnov lstay, by(covid)
ksmirnov age, by(covid)
ksmirnov nas, by(covid)
ksmirnov cd, by(covid)
ksmirnov ci, by(covid)
ksmirnov ct, by(covid)

* Analyzing the sample costs patients per day
clear
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\bcovidcostsday.dta", clear

describe covid cd ci ct
sum cd ci ct
by covid, sort : summarize cd, detail
by covid, sort : summarize ci, detail
by covid, sort : summarize ct, detail
histogram cd, by(covid) normal
histogram ci, by(covid) normal
histogram ct, by(covid) normal
by covid, sort: swilk cd
by covid, sort: swilk ci
by covid, sort: swilk ct
ranksum cd, by(covid)porder
ranksum ci, by(covid)porder
ranksum ct, by(covid)porder
ksmirnov cd, by(covid)
ksmirnov ci, by(covid)
ksmirnov ct, by(covid)

*Analyzing the sample direct costs complements (Drugs and Materials)
clear
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\bcovidcostscomp.dta", clear

describe covid qtdmed cmmed qtdmat cmmat
sum covid qtdmed cmmed qtdmat cmmat
by covid, sort : summarize qtdmed
by covid, sort : summarize cmmed
by covid, sort : summarize qtdmat
by covid, sort : summarize cmmat
histogram qtdmed, by(covid) normal
histogram cmmed, by(covid) normal
histogram qtdmat, by(covid) normal
histogram cmmat, by(covid) normal

* Econometrics model - MQO/OLS
clear
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\BCovidnas.dta", clear

*Correlate Matrix 
pwcorr lstay age nas, star(0.05) sig

*OLS#1 = Dependent variable Direct Costs
estimates clear
reg cd covid age lstay nas
predict residualcd, r
predict yhatcd, xb
twoway lfit cd yhatcd|| scatter cd yhatcd, ytitle(cd) xtitle(yhat)
histogram residualcd, kdensity normal

* VIF Test: Checking multicollinearity in the model
vif
  
* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest
 
* Ramsey RESET test using powers of the fitted values of direct costs
estat ovtest 

* Analyzing Outliers
lvr2plot, mlabel (covid)
predict residualcd1 if e(sample), resid
predict cookcd if e(sample) , cooksd
predict ecd if e(sample), resid
list cd ecd cookcd if cook>4/544
* observations with high leverage and weight = cook's D > 4/n
sum residualcd1
sort residualcd1
sort cookcd

drop in 544/544
reg cd covid lstay age nas
estat hettest
ovtest
predict yhatcdoutlieroff
twoway lfit cd yhatcdoutlieroff|| scatter cd yhatcdoutlieroff, ytitle(cd) xtitle(yhat without outlier)

*we decided not to exclude any observations and to use other methods to improve the fit of the model.
	
clear
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\BCovidnas.dta", clear
reg cd covid lstay age nas

*Checking the effect if winsorize the variables at 1%
winsor lstay, gen(lstayw1) h(1)
winsor age, gen(agew1) h(1)
winsor nas, gen(nasw1) h(1)
winsor cd, gen(cdw1) h(1)
winsor ci, gen(ciw1) h(1)
winsor ct, gen(ctw1) h(1)
 
reg cdw1 covid lstayw1 agew1 nasw1
predict residualcdw, r
predict yhatcdw1, xb
twoway lfit cdw1 yhatcdw1|| scatter cdw1 yhatcdw1 , ytitle(cdw1) xtitle(yhat winsor)
 
hettest
ovtest

*checking if transforming variables into LN
gen lncd=ln(cd)
gen lnage=ln(age)
gen lnlstay=ln(lstay)
gen lnnas=ln(nas)

estimates clear
reg lncd lnage lnlstay lnnas covid
estat hettest
ovtest

*For better model adjustment and heteroscedasticity correction, we will use in robust matrices
estimates clear
reg lncd lnage lnlstay lnnas covid,r
ovtest
predict residuoslncdr, r
predict yhatlncdr, xb
twoway lfit lncd yhatlncdr|| scatter lncd yhatlncdr, ytitle(Direct Costs) xtitle(yhat in LN and Robust)
histogram residuoslncdr, kdensity normal

*No observation excluded, we do not winsorized, just we apply robust White matrices with logarithmic scale variables.

*OLS#2: Dependent variable = Indirect Costs
estimates clear
reg ci covid lstay age nas
predict residualci, r
predict yhatci, xb
twoway lfit ci yhatci|| scatter ci yhatci, ytitle(Custos Indiretos) xtitle(yhat)
histogram residualci, kdensity normal

* VIF Test: Checking multicollinearity in the model
vif
  
* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest
 
* Ramsey RESET test using powers of the fitted values of indirect costs
estat ovtest 

* Analyzing Outliers
lvr2plot, mlabel (covid)
predict residualci2 if e(sample), cooksd
predict cookci if e(sample), cooksd
predict eci if e(sample), resid
list ci eci cookci if cookci>4/544
sum residualci2
sort residualci2
sort cookci

*We will adapt the model in LN variables
gen lnci=ln(ci)

*For better model adjustment and heteroscedasticity correction, we will use in robust matrices
estimates clear
reg lnci covid lnage lnlstay lnnas, r
ovtest
predict residuoslncir, r
predict yhatlncir, xb
twoway lfit lnci yhatlncir|| scatter lnci yhatlncir, ytitle(Indirect Costs) xtitle(yhat in LN and Robust)
histogram residuoslncir, kdensity normal


*OLS#3: Dependent variable Total Costs
estimates clear
reg ct covid lstay age nas
predict residualct, r
predict yhatct, xb
twoway lfit ct yhatct|| scatter ct yhatct, ytitle(Total Costs) xtitle(yhat)
histogram residualct, kdensity normal

* VIF Test: Checking multicollinearity in the model
vif
	  
* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest
	 
* Ramsey RESET test using powers of the fitted values of indirect costs
estat ovtest 

* Analyzing Outliers
lvr2plot, mlabel (covid)
predict residualct1 if e(sample), resid
predict cookct if e(sample) , cooksd
predict ect if e(sample), resid
list ct ect cookct if cookct>4/544
sum residualct1
sort residualct1
sort cookct
predict yhatct1
twoway (scatter ct yhatct1) (lfit ct yhatct1)

reg ctw1 covid agew1 lstayw1 nasw1
hettest
ovtest

**We will adapt the model in LN variables
gen lnct=ln(ct)

reg lnct covid lnage lnlstay lnnas
ovtest
hettest

*For better model adjustment and heteroscedasticity correction, we will use in robust matrices
estimates clear
reg lnct covid lnage lnlstay lnnas, r
ovtest
predict residuoslnctr, r
predict yhatlnctr, xb
twoway lfit lnct yhatlnctr|| scatter lnct yhatlnctr, ytitle(Total Costs) xtitle(yhat in LN and Robust)
histogram residuoslnctr, kdensity normal

estimates clear
clear

*Applying the methodology just in covid patient
use "C:\Users\cacia\Carreira Acadêmica\Artigos\2020\Covid\bcovidnas1.dta", clear

gen lnlstay=ln(lstay)
gen lnage=ln(age)
gen lnnas=ln(nas)
gen lncd=ln(cd)
gen lnci=ln(ci)
gen lnct=ln(ct)

estimates clear
reg lncd lnlstay lnage lnnas, r
predict residuoslncdcovid, r
predict yhatlncdcovid, xb
twoway lfit lncd yhatlncdcovid|| scatter lncd yhatlncdcovid, ytitle(Direct Costs) xtitle(yhat in LN and Robust)
histogram residuoslncdcovid, kdensity  normal

estimates clear
reg lnci lnlstay lnage lnnas, r
predict residuoslncicovid, r
predict yhatlncicovid, xb
twoway lfit lnci yhatlncicovid|| scatter lnci yhatlncicovid, ytitle(Indirect Costs) xtitle(yhat in LN and Robust)
histogram residuoslncicovid, kdensity  normal

estimates clear
reg lnct lnlstay lnage lnnas, r
predict residuoslnctcovid
predict yhatlnctcovid
twoway lfit lnct yhatlnctcovid|| scatter lnct yhatlnctcovid, ytitle(Total Costs) xtitle(yhat in LN and Robust)
histogram residuoslnctcovid, kdensity  normal
	 
log close
clear
exit
