*******************************************************************************************
*This program runs the wage regressions in Appendix I. The coefficients obtained here can
*be plugged back into the Summary Index to make benefit-cost calculations based on
*projections from the 1979 cohort. See the paper for details. See the README file for
*details on how to obtain the NLSY source data.
*******************************************************************************************

clear
capture log close
set mem 50m
set more off
log using data_AEJ_appendix_deming_HeadStart, text replace
# delimit ;

use Appendix_2008_0217.dta, clear;

use data_AEJ_appendix_deming_HeadStart.dta, clear;


*Normalize wages to 2007 dollars*;

replace Wages80=Wages80*2.52;
replace Wages81=Wages81*2.28;
replace Wages82=Wages82*2.15;
replace Wages83=Wages83*2.08;
replace Wages84=Wages84*2.00;
replace Wages85=Wages85*1.93;
replace Wages86=Wages86*1.89;
replace Wages87=Wages87*1.83;
replace Wages88=Wages88*1.75;
replace Wages89=Wages89*1.67;
replace Wages90=Wages90*1.59;
replace Wages91=Wages91*1.52;
replace Wages92=Wages92*1.48;
replace Wages93=Wages93*1.43;
replace Wages94=Wages94*1.40;
replace Wages96=Wages96*1.32;
replace Wages98=Wages98*1.27;
replace Wages00=Wages00*1.20;
replace Wages02=Wages02*1.15;
replace Wages04=Wages04*1.10;

gen EducAt19= HighGrade79 if Age==19;
replace EducAt19= HighGrade80 if Age==18;
replace EducAt19= HighGrade81 if Age==17;
replace EducAt19= HighGrade82 if Age==16;
replace EducAt19= HighGrade83 if Age==15;
replace EducAt19= HighGrade84 if Age==14;
foreach var of varlist HealthAssess_40- EducAt19 {;
	replace `var'=. if `var'<0;
};
gen HSGrad_at19=1 if EducAt19>=12 & EducAt19!=.;
replace HSGrad_at19=0 if HSGrad_at19!=1 & EducAt19!=.;
gen someCollege_at19=1 if EducAt19>=13 & EducAt19!=.;
replace someCollege_at19=0 if someCollege_at19!=1 & EducAt19!=.;
gen Idle_at19=1 if Age==19 & Wages79==0 & Enroll79!=2 & Enroll79!=3 & Enroll79!=.;
replace Idle_at19=0 if Age==19 & Idle_at19!=1 & Wages79!=. & Enroll79!=.;
replace Idle_at19=1 if Age==18 & Wages80==0 & Enroll80!=2 & Enroll80!=3 & Enroll80!=.;
replace Idle_at19=0 if Age==18 & Idle_at19!=1 & Wages80!=. & Enroll80!=.;
replace Idle_at19=1 if Age==17 & Wages81==0 & Enroll81!=2 & Enroll81!=3 & Enroll81!=.;
replace Idle_at19=0 if Age==17 & Idle_at19!=1 & Wages81!=. & Enroll81!=.;
replace Idle_at19=1 if Age==16 & Wages82==0 & Enroll82!=2 & Enroll82!=3 & Enroll82!=.;
replace Idle_at19=0 if Age==16 & Idle_at19!=1 & Wages82!=. & Enroll82!=.;
replace Idle_at19=1 if Age==15 & Wages83==0 & Enroll83!=2 & Enroll83!=3 & Enroll83!=.;
replace Idle_at19=0 if Age==15 & Idle_at19!=1 & Wages83!=. & Enroll83!=.;
replace Idle_at19=1 if Age==14 & Wages84==0 & Enroll84!=2 & Enroll84!=3 & Enroll84!=.;
replace Idle_at19=0 if Age==14 & Idle_at19!=1 & Wages84!=. & Enroll84!=.;
egen CrimeAt19=rowmax( Charged80- Sentenced80) if Age<=19 & Age!=.;
gen TeenPreg_at19=1 if Kids79==1 & Age<=19 & Age!=.;
replace TeenPreg_at19=1 if Kids80==1 & Age<=18 & Age!=.;
replace TeenPreg_at19=1 if Kids81==1 & Age<=17 & Age!=.;
replace TeenPreg_at19=1 if (Kids82_M==1 | Kids82_F==1) & Age<=16 & Age!=.;
replace TeenPreg_at19=1 if (Kids83_M==1 | Kids83_F==1) & Age<=15 & Age!=.;
replace TeenPreg_at19=1 if (Kids84_M==1 | Kids83_F==1) & Age<=14 & Age!=.;
replace TeenPreg_at19=0 if TeenPreg_at19!=1 & Age<=19 & Age!=.;
gen Health_at19=1 if HealthLim79==1 & Age<=19 & Age!=.;
replace Health_at19=1 if HealthLim80==1 & Age<=18 & Age!=.;
replace Health_at19=1 if HealthLim81==1 & Age<=17 & Age!=.;
replace Health_at19=1 if HealthLim82==1 & Age<=16 & Age!=.;
replace Health_at19=1 if HealthLim83==1 & Age<=15 & Age!=.;
replace Health_at19=1 if HealthLim84==1 & Age<=14 & Age!=.;
replace Health_at19=0 if Health_at19!=1 & Age<=19 & Age!=.;

gen GED_at19=1 if (GED79==2 | GED79==3) & Age<=19 & Age!=.;
replace GED_at19=1 if (GED80==2 | GED80==3) & Age<=18 & Age!=.;
replace GED_at19=1 if (GED81==2 | GED81==3) & Age<=17 & Age!=.;
replace GED_at19=1 if (GED82==2 | GED82==3) & Age<=16 & Age!=.;
replace GED_at19=1 if (GED83==2 | GED83==3) & Age<=15 & Age!=.;
replace GED_at19=1 if (GED84==2 | GED84==3) & Age<=14 & Age!=.;
replace GED_at19=0 if GED_at19!=1 & Age<=19 & Age!=.;

gen Educ_cat=0 if HSGrad_at19==0 & GED_at19==0;
replace Educ_cat=1 if GED_at19==1;
replace Educ_cat=2 if Educ_cat!=1 & HSGrad_at19==1;
replace Educ_cat=3 if Educ_cat==2 & someCollege_at19==1;
foreach x in Wages NetFamInc {;
	gen `x'_at20=`x'80 if Age==19;
	replace `x'_at20=`x'81 if Age==18;
	replace `x'_at20=`x'82 if Age==17;
	replace `x'_at20=`x'83 if Age==16;
	replace `x'_at20=`x'84 if Age==15;
	replace `x'_at20=`x'85 if Age==14;
};
foreach x in Wages NetFamInc {
	gen `x'_at30=`x'90 if Age==19;
	replace `x'_at30=`x'91 if Age==18;
	replace `x'_at30=`x'92 if Age==17;
	replace `x'_at30=`x'93 if Age==16;
	replace `x'_at30=`x'94 if Age==15;
	replace `x'_at30=`x'96 if Age==14;
};

foreach var of varlist HSGrad_at19 someCollege_at19 Idle_at19 CrimeAt19 TeenPreg_at19 Health_at19 {;
	egen `var'_std=std(`var'), mean(0) std(1);
};
replace Idle_at19_std=-Idle_at19_std;
replace CrimeAt19_std=-CrimeAt19_std;
replace TeenPreg_at19_std=-TeenPreg_at19_std;
replace Health_at19_std=-Health_at19_std;
egen Sum=rowmean( HSGrad_at19_std- Health_at19_std) 
		if HSGrad_at19_std!=. & someCollege_at19_std!=. & Idle_at19_std!=. 
		& CrimeAt19_std!=. & TeenPreg_at19_std!=. & Health_at19_std!=.;

egen Sum_std=std(Sum), mean(0) std(1);

egen Wage_avg_19=rowmean(	Wages80 Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 
					Wages90 Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==19;
egen Wage_avg_18=rowmean(	Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 
					Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==18;
egen Wage_avg_17=rowmean(	Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91
					Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==17;
egen Wage_avg_16=rowmean(	Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 
					Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==16;
egen Wage_avg_15=rowmean(	Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 
					Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==15;
egen Wage_avg_14=rowmean(	Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 Wages93 
					Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==14;

egen Wage_ct_19=rownonmiss(	Wages80 Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 
					Wages90 Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==19;
egen Wage_ct_18=rownonmiss(	Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 
					Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==18;
egen Wage_ct_17=rownonmiss(	Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 
					Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==17;
egen Wage_ct_16=rownonmiss(	Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 
					Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==16;
egen Wage_ct_15=rownonmiss(	Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 Wages93 
					Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==15;
egen Wage_ct_14=rownonmiss(	Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 Wages93 Wages94 
					Wages96 Wages98 Wages00 Wages02 Wages04) if Age==14;

gen Wage_ct=Wage_ct_19 if Age==19;
foreach x in 14 15 16 17 18 {;
	replace Wage_ct=Wage_ct_`x' if Age==`x';
};
gen Wage_avg=Wage_avg_19 if Age==19;
foreach x in 14 15 16 17 18 {;
	replace Wage_avg=Wage_avg_`x' if Age==`x';
};
drop Wage_avg_19-Wage_avg_14;

egen Wage_sum_19=rowtotal(	Wages80 Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 
					Wages90 Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==19;
egen Wage_sum_18=rowtotal(	Wages81 Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 
					Wages91 Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==18;
egen Wage_sum_17=rowtotal(	Wages82 Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 
					Wages92 Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==17;
egen Wage_sum_16=rowtotal(	Wages83 Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 
					Wages93 Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==16;
egen Wage_sum_15=rowtotal(	Wages84 Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 Wages93 
					Wages94 Wages96 Wages98 Wages00 Wages02 Wages04) if Age==15;
egen Wage_sum_14=rowtotal(	Wages85 Wages86 Wages87 Wages88 Wages89 Wages90 Wages91 Wages92 Wages93 Wages94 
					Wages96 Wages98 Wages00 Wages02 Wages04) if Age==14;

gen Wage_sum=Wage_sum_19/Wage_ct_19 if Age==19;
foreach x in 14 15 16 17 18 {;
	replace Wage_sum=Wage_sum_`x'/Wage_ct_`x' if Age==`x';
};
drop Wage_sum_19-Wage_sum_14;
drop Wage_ct_19-Wage_ct_14;
gen logWage_avg=ln(Wage_avg);
gen logWage_sum=ln(Wage_sum);

*Age-normalize AFQT score*;

gen temp=AFQT*(40.95/32.08) if Age==14;
replace temp=AFQT*(40.95/33.80) if Age==15;
replace temp=AFQT*(40.95/36.51) if Age==16;
replace temp=AFQT*(40.95/38.74) if Age==17;
replace temp=AFQT*(40.95/39.37) if Age==18;
replace temp=AFQT*(40.95/43.22) if Age==19;
replace temp=AFQT*(40.95/48.94) if Age==20;
replace temp=AFQT*(40.95/48.13) if Age==21;
replace temp=AFQT*(40.95/51.82) if Age==22;
egen AFQT_std=std(temp), mean(0) std(1);
drop temp;
gen AFQT_sq=AFQT_std^2;
gen Hispanic=Race==1;
gen Black=Race==2;
gen NonBlack=Race!=2;
gen Sample=1 if 	HSGrad_at19_std!=. & someCollege_at19_std!=. & Idle_at19_std!=. 
			& CrimeAt19_std!=. & TeenPreg_at19_std!=. & Health_at19_std!=. & AFQT_std!=. &  logWage_avg!=.;
drop if Sample!=1;
xi: reg logWage_avg 	HSGrad_at19_std someCollege_at19_std Idle_at19_std 
				CrimeAt19_std TeenPreg_at19_std Health_at19_std i.Age Black Hispanic Sex if Age<=19, robust;

*Sum the coefficients for each case*;

di .2726+.1471+.3030+.0703+.0345+.0436
/*0.8711*/;

xi: reg logWage_avg 	HSGrad_at19_std someCollege_at19_std Idle_at19_std CrimeAt19_std 
				TeenPreg_at19_std Health_at19_std AFQT_std AFQT_sq i.Age Black Hispanic Sex if Age<=19, robust;

di .1672+.0991+.2917+.0701+.0273+.0419

/*0.6973*/;

*Now generate summary index that weights by coefficients in wage regression*;

gen HSGrad_at19_wage_notest=HSGrad_at19_std*(.2726*(6/0.8711));
gen someCollege_at19_wage_notest=someCollege_at19_std*(.1471*(6/0.8711));
gen Idle_at19_wage_notest=Idle_at19_std*(.3030*(6/0.8711));
gen Crime_at19_wage_notest=CrimeAt19_std*(.0703*(6/0.8711));
gen TeenPreg_at19_wage_notest=TeenPreg_at19_std*(.0345*(6/0.8711));
gen Health_at19_wage_notest=Health_at19_std*(.0419*(6/0.8711));

gen HSGrad_at19_wage=HSGrad_at19_std*(.1672*(6/0.6973));
gen someCollege_at19_wage=someCollege_at19_std*(.0991*(6/0.6973));
gen Idle_at19_wage=Idle_at19_std*(.2917*(6/0.6973));
gen Crime_at19_wage=CrimeAt19_std*(.0701*(6/0.6973));
gen TeenPreg_at19_wage=TeenPreg_at19_std*(.0273*(6/0.6973));
gen Health_at19_wage=Health_at19_std*(.0419*(6/0.6973));


egen temp=rowmean( HSGrad_at19_wage_- Health_at19_wage) 
			if HSGrad_at19_wage!=. & someCollege_at19_wage!=. & Idle_at19_wage!=. 
			& Crime_at19_wage!=. & TeenPreg_at19_wage!=. & Health_at19_wage!=.;

egen Sum_at19_wage=std(temp), mean(0) std(1);
drop temp;

egen temp=rowmean( HSGrad_at19_wage_notest- Health_at19_wage_notest) 
			if HSGrad_at19_wage_notest!=. & someCollege_at19_wage_notest!=. & Idle_at19_wage_notest!=. 
			& Crime_at19_wage_notest!=. & TeenPreg_at19_wage_notest!=. & Health_at19_wage_notest!=.;

egen Sum_at19_wage_notest=std(temp), mean(0) std(1);
drop temp;

xi: reg logWage_avg Sum_at19_wage_notest i.Age Black Hispanic Sex if Age<=19, robust;

*0.524*;

xi: reg logWage_avg Sum_at19_wage AFQT_std AFQT_sq i.Age Black Hispanic Sex if Age<=19, robust;

*0.423*;

impute AFQT_std Hispanic Black Sex, g(AFQT_std_imp);
gen lowAFQT= AFQT_std<=-1 & AFQT_std!=.;
gen highAFQT=AFQT_std>-1 & AFQT_std!=.;
replace lowAFQT=. if AFQT_std==.;
replace highAFQT=. if AFQT_std==.;
gen SumXlowAFQT=  Sum_at19_wage*lowAFQT;
gen SumXhighAFQT=  Sum_at19_wage*highAFQT;
gen Male=Sex==1;
gen Female=Sex==2;
gen SumXMale=Sum_at19_wage*Male;
gen SumXFemale=Sum_at19_wage*Female;
gen SumXBlack=Sum_at19_wage*Black;
gen SumXNonblack=Sum_at19_wage*NonBlack;

xi: reg logWage_avg SumXlowAFQT SumXhighAFQT AFQT_std AFQT_sq i.Age Black Hispanic Sex if Age<=19, robust;
*low AFQT=0.607; high AFQT=0.351*;

xi: reg logWage_avg SumXBlack SumXNonblack AFQT_std AFQT_sq i.Age Black Hispanic Sex if Age<=19, robust;
*Black=0.442; Nonblack=0.405*;

xi: reg logWage_avg SumXMale SumXFemale AFQT_std AFQT_sq i.Age Black Hispanic Sex if Age<=19, robust;
*Male=0.327; Female=0.487*;

*Multiply these coefficients by the (reweighted) coefficients in Table 4 for each subgroup*;
*This gives the projected impact of Head Start on adult wages, in log points*;

