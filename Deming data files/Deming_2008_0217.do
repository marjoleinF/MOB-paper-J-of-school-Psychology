*******************************************************************************************
*This program defines sample eligibility variables and generates the covariates and 
*outcomes used in the paper. It also creates text files that correspond to Tables 1-5. See 
*the readme file on how to obtain the variables in this data set from the original NLSY 
*and CNLSY source data. You can run this program and it will send the text files to
*your default storage directory.
*******************************************************************************************

clear
capture log close
set mem 400m
set more off
log using data_AEJ_deming_HeadStart, text replace
# delimit ;

use data_Deming_2008_0217.dta, clear;

*NLSY codes missing values as negatives - so drop all non-answers;

foreach var of varlist ChildID- hhID {;
	replace `var'=. if `var'<0;
};

*First create rules that establish sample eligibility;

*Rule is 5 years old by 1990, so that they will be 19 by 2004;
*Next restrict the sample to families with at least 2 age-eligible children;

*Finally restrict to families where at least one (but not all) children were in Head Start;

********************************************************************************************
*I create 3 sets of variables that define preschool participation. The first simply uses   
*the age variable and excludes those with any missing data. The second substitutes the PPVT
*age (ie age at test) when the original variable is unavailable, and also substitutes past 
*or future age variables from other survey years (plus or minus 24 months). The third is   
*the most restrictive definition. It codes inconsistent or missing responses across years  
*as zeros, as well as for children for whom mothers report "3 months or less" participation
*in the program. The variables are coded HS1, HS2, HS3 and Pre1, Pre2, Pre3. In general    
*different rules have very little impact on the estimates. I use #2 in the paper.          
********************************************************************************************

*Replace missing age values with PPVT Age or age reported from the nearest survey year instead;
 
local years "86 88 90 92 94 96 98 100 102 104";
foreach y of local years {;
	gen Age2_Mo`y'=Age_Mo`y';
	replace Age2_Mo`y'=PPVTAge`y' if Age2_Mo`y'==.;
};

replace Age2_Mo86= Age2_Mo88-24 if  Age2_Mo86==. &  Age2_Mo88>=25 & Age2_Mo88!=.;
replace Age2_Mo88= Age2_Mo86+24 if  Age2_Mo88==. & Age2_Mo86!=.;
replace Age2_Mo88= Age2_Mo90-24 if  Age2_Mo88==. &  Age2_Mo90>=25 & Age2_Mo90!=.;
replace Age2_Mo90= Age2_Mo88+24 if  Age2_Mo90==. & Age2_Mo88!=.;
replace Age2_Mo90= Age2_Mo92-24 if  Age2_Mo90==. &  Age2_Mo92>=25 & Age2_Mo92!=.;
replace Age2_Mo92= Age2_Mo90+24 if  Age2_Mo92==. & Age2_Mo90!=.;
replace Age2_Mo92= Age2_Mo94-24 if  Age2_Mo92==. &  Age2_Mo94>=25 & Age2_Mo94!=.;
replace Age2_Mo94= Age2_Mo92+24 if  Age2_Mo94==. & Age2_Mo92!=.;
replace Age2_Mo94= Age2_Mo96-24 if  Age2_Mo94==. &  Age2_Mo96>=25 & Age2_Mo96!=.;
replace Age2_Mo96= Age2_Mo94+24 if  Age2_Mo96==. & Age2_Mo94!=.;
replace Age2_Mo96= Age2_Mo98-24 if  Age2_Mo96==. &  Age2_Mo98>=25 & Age2_Mo98!=.;
replace Age2_Mo98= Age2_Mo96+24 if  Age2_Mo98==. & Age2_Mo96!=.;
replace Age2_Mo98= Age2_Mo100-24 if  Age2_Mo98==. &  Age2_Mo100>=25 & Age2_Mo100!=.;
replace Age2_Mo100= Age2_Mo98+24 if  Age2_Mo100==. & Age2_Mo98!=.;
replace Age2_Mo100= Age2_Mo102-24 if  Age2_Mo100==. &  Age2_Mo102>=25 & Age2_Mo102!=.;
replace Age2_Mo102= Age2_Mo100+24 if  Age2_Mo102==. & Age2_Mo100!=.;
replace Age2_Mo102= Age2_Mo104-24 if  Age2_Mo102==. &  Age2_Mo104>=25 & Age2_Mo104!=.;
replace Age2_Mo104= Age2_Mo102+24 if  Age2_Mo104==. & Age2_Mo102!=.;

*Create age in years (rather than months) variable*;

foreach x of local years {;
	gen Age_Yr`x'=0 if Age_Mo`x'<12;
	gen Age2_Yr`x'=0 if Age2_Mo`x'<12;
};
foreach x of local years {;
	forvalues y = 1(1)37 {;
		replace Age_Yr`x'=`y' if Age_Mo`x'>=12*`y' & Age_Mo`x'<(12*`y')+12;
		replace Age2_Yr`x'=`y' if Age2_Mo`x'>=12*`y' & Age2_Mo`x'<(12*`y')+12;
	};
};

*Create dummy for sample eligibility, by year*;
*Minimum of 2 siblings age 54 months or more, in each sample year*;
*Want to account in some way for the fact that people are surveyed at different times of the year*;
*But also don't want to include siblings who subsequently enroll*;
*So I chose 4 years, 6 months - these kids are almost certainly too old to enroll if they are not already in the program*;
*Estimates in the paper are not sensitive to this rule*;

foreach y in 86 88 90 {;
	bysort MotherID: egen Elig1_`y'=count(Age_Mo`y') if Age_Mo`y'>47 & Age_Mo`y'!=.;
	bysort MotherID: egen Elig2_`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;
	replace Elig1_`y'=0 if Elig1_`y'==1;
	replace Elig1_`y'=1 if Elig1_`y'>1 & Elig1_`y'!=.;
	replace Elig1_`y'=0 if Elig1_`y'!=1;
	replace Elig2_`y'=0 if Elig2_`y'==1;
	replace Elig2_`y'=1 if Elig2_`y'>1 & Elig2_`y'!=.;
	replace Elig2_`y'=0 if Elig2_`y'!=1;
};

*Exclude small number of kids who died prior to eligibility*;

foreach x in 86 88 90 {;
	gen byte Dead`x'=Res`x'==8;
	replace Elig1_`x'=. if Dead`x'==1;
	replace Elig2_`x'=. if Dead`x'==1;
};
gen Deceased=Res104==8;
drop Dead*;

*Create Head Start, Other Preschool, and No Preschool categories*;
*Create variables equal to one if respondent ever indicated enrollment in HS or other preschools in 1988-1992* ;
*Question was not asked in 1986*;

foreach y in 1 2 {;
	egen HS`y'_90=rowmax(Ever_HS88 Ever_HS90) if Elig`y'_90==1;
	egen Pre`y'_90=rowmax(Ever_Preschool88 Ever_Preschool90) if Elig`y'_90==1;
	foreach x in 90 {;
		replace HS`y'_`x'=0 if Elig`y'_`x'==1 & HS`y'_`x'!=1;
		replace Pre`y'_`x'=0 if Elig`y'_`x'==1 & Pre`y'_`x'!=1;
	
		*I code "yes" responses to both HS and preschool as Head Start (Rule 3 does it the other way)*;

		replace Pre`y'_`x'=0 if HS`y'_`x'==1;
		gen byte None`y'_`x'=Elig`y'_`x'==1 ;
		replace None`y'_`x'=. if Elig`y'_`x'==0;
		replace None`y'_`x'=0 if (HS`y'_`x'==1 | Pre`y'_`x'==1);
	};
};


*Alternative participation definition - code inconsistent responses across years as missing or zeros*;
*Also code as zero those who report being in Head Start for less than 3 months*;

foreach x in 90 {;
	foreach y in HS Pre None {;
		gen `y'3_`x'=`y'2_`x';
	};
};
replace HS3_90=0 if (Ever_HS88==1 & Ever_HS90==0) | (HowLong_HS88==1 | HowLong_HS90==1) & HS3_90!=.;
replace Pre3_90=0 if (Ever_Preschool88==1 & Ever_Preschool90==0) & Pre3_90!=.;
replace None3_90=1 if HS3_90==0 & Pre3_90==0;

*Create dummy for "fixed effects" sample - families where HS participation varies across siblings*;

foreach y in 86 88 90 {;
	bysort MotherID: egen Num1Elig`y'=count(Age_Mo`y') if Age_Mo`y'>47 & Age_Mo`y'!=.;
	bysort MotherID: egen Num2Elig`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;
	bysort MotherID: egen Num3Elig`y'=count(Age2_Mo`y') if Age2_Mo`y'>47 & Age2_Mo`y'!=.;

	*This creates a count of eligible siblings per household*;
};

foreach x in 1 2 3 {;
	foreach y in 90 {;

		*For each family, count the number of kids in each category (HS, Pre, None)*
		*Then set equal to missing if all kids in the family are in the same category (and thus ineligible)*;
		
		bysort MotherID: egen temp= count(ChildID) if HS`x'_`y'==1;
		replace temp=. if Num`x'Elig`y'==temp;
		bysort MotherID: egen temp2= count(ChildID) if Pre`x'_`y'==1;
		replace temp2=. if Num`x'Elig`y'==temp2;
		bysort MotherID: egen temp3= count(ChildID) if None`x'_`y'==1;
		replace temp3=. if Num`x'Elig`y'==temp3;

		*Now create dummy vars with counts*;

		gen byte HS`x'_FE`y'=1 if temp!=.;
		gen byte Pre`x'_FE`y'=1 if temp2!=.;
		gen byte None`x'_FE`y'=1 if temp3!=.;
		replace HS`x'_FE`y'=0 if (Pre`x'_FE`y'==1 | None`x'_FE`y'==1);
		replace Pre`x'_FE`y'=0 if (HS`x'_FE`y'==1 | None`x'_FE`y'==1);
		replace None`x'_FE`y'=0 if (HS`x'_FE`y'==1 | Pre`x'_FE`y'==1);
		drop temp temp2 temp3;
	};
};

gen byte PreK=1 if HS2_90==1;
replace PreK=2 if Pre2_90==1;
replace PreK=3 if None2_90==1;
gen byte PreK_FE=1 if HS2_FE90==1;
replace PreK_FE=2 if Pre2_FE90==1;
replace PreK_FE=3 if None2_FE90==1;

*END SAMPLE ELIGIBILITY SECTION*;

********************************************************************************************
*Now I create the covariates in Tables 1 and 2 and in the regressions in the rest of the
*paper.
********************************************************************************************
;
gen byte Hispanic=Race_Child==1;
gen byte Black=Race_Child==2;
gen byte White=Race_Child==3;
gen byte NonBlack=Race_Child!=2 & Race_Child!=.;

*Generate basic demographic variables in Table 1*
*Permanent Income, Mother<HS, Mother some college, Maternal AFQT, Grandmother's Education*

*Create yearly income in constant 2004 dollars, as well as "permanent income" measure*;

replace NetFamInc78=NetFamInc78*2.82;
replace NetFamInc79=NetFamInc79*2.54;
replace NetFamInc80=NetFamInc80*2.24;
replace NetFamInc81=NetFamInc81*2.03;
replace NetFamInc82=NetFamInc82*1.90;
replace NetFamInc83=NetFamInc83*1.85;
replace NetFamInc84=NetFamInc84*1.78;
replace NetFamInc85=NetFamInc85*1.71;
replace NetFamInc86=NetFamInc86*1.68;
replace NetFamInc87=NetFamInc87*1.62;
replace NetFamInc88=NetFamInc88*1.55;
replace NetFamInc89=NetFamInc89*1.48;
replace NetFamInc90=NetFamInc90*1.41;
replace NetFamInc91=NetFamInc91*1.35;
replace NetFamInc92=NetFamInc92*1.31;
replace NetFamInc93=NetFamInc93*1.27;
replace NetFamInc95=NetFamInc95*1.21;
replace NetFamInc97=NetFamInc97*1.15;
replace NetFamInc99=NetFamInc99*1.10;
replace NetFamInc101=NetFamInc101*1.04;
egen PermInc=rowmean( NetFamInc78 NetFamInc79 NetFamInc80 NetFamInc81 NetFamInc82 NetFamInc83 NetFamInc84 
			    NetFamInc85 NetFamInc86 NetFamInc87 NetFamInc88 NetFamInc89 NetFamInc90 NetFamInc91 
			    NetFamInc92 NetFamInc93 NetFamInc95 NetFamInc97 NetFamInc99 NetFamInc101 NetFamInc103);
gen lnPermInc=ln(PermInc);
egen PermInc_std=std(PermInc), mean(0) std(1);

foreach var of varlist HighGrade_Moth* {;
	replace `var'=. if `var'==95;
};
egen MothED=rowmax( HighGrade_Moth*);
gen byte MomDropout=MothED<12;
replace MomDropout=. if MothED==.;
gen byte MomHS=MothED==12;
replace MomHS=. if MothED==.;
gen byte MomSomeColl=MothED>=13 & MothED!=.;
replace MomSomeColl=. if MothED==.;

*Create age-adjusted maternal AFQT score*;

gen AgeAFQT=AFQT_Pct81_REV;
replace AgeAFQT=AgeAFQT*(35.60881/28.79544) if Age_Mom79==14;
replace AgeAFQT=AgeAFQT*(35.60881/32.86273) if Age_Mom79==15;
replace AgeAFQT=AgeAFQT*(35.60881/32.86273) if Age_Mom79==16;
replace AgeAFQT=AgeAFQT*(35.60881/36.3544) if Age_Mom79==17;
replace AgeAFQT=AgeAFQT*(35.60881/33.45777) if Age_Mom79==18;
replace AgeAFQT=AgeAFQT*(35.60881/36.84) if Age_Mom79==19;
replace AgeAFQT=AgeAFQT*(35.60881/41.84536) if Age_Mom79==20;
replace AgeAFQT=AgeAFQT*(35.60881/40.95177) if Age_Mom79==21;
replace AgeAFQT=AgeAFQT*(35.60881/42.82069) if Age_Mom79==22;
egen AgeAFQT_std=std(AgeAFQT), mean(0) std(1);

*For a small number of mothers, AFQT score is missing - impute missings for use as a covariate later*;

impute AgeAFQT_std Black Hispanic Age_Moth_Birth, gen(impAFQT_std);

*But use non-imputed values only for Table 1*

*LIST OF VARIABLES FOR TABLE 1 - PermInc MomDropout MomBA AgeAFQT_std HighGrade_GMom79*;

foreach x in Black NonBlack {;
tabexport 	PermInc 
		MomDropout 
		MomSomeColl 
		AgeAFQT_std 
		HighGrade_GMom79 if `x'==1 using table1-`x'.txt, replace s(mean sd count) by(PreK) f(%9.2f);

tabexport 	PermInc 
		MomDropout 
		MomSomeColl 
		AgeAFQT_std 
		HighGrade_GMom79 if `x'==1 using table1-`x'FE.txt, replace s(mean sd count) by(PreK_FE) f(%9.2f);
};

*Generate within-sibling difference covariates in Table 2*;

*Reside in same household, preexisting health limitation, low birthweight, Attrited from LT sample*;

*Reside in same household as mother - code zero if they did not reside with mom in any eligible year*;

forvalues x = 79(1)90 {;
	gen byte MomHH`x'=1 if Res`x'==1;
	replace MomHH`x'=0 if Res`x'!=1 & Res`x'!=.;
};
gen Res_0to3=MomHH90 if Age2_Yr104==14;
egen temp1=rowmin(MomHH89-MomHH90) if Age2_Yr104==15;
egen temp2=rowmin(MomHH88-MomHH90) if Age2_Yr104==16;
egen temp3=rowmin(MomHH87-MomHH90) if Age2_Yr104==17;
egen temp4=rowmin(MomHH86-MomHH89) if Age2_Yr104==18;
egen temp5=rowmin(MomHH85-MomHH88) if Age2_Yr104==19;
egen temp6=rowmin(MomHH84-MomHH87) if Age2_Yr104==20;
egen temp7=rowmin(MomHH83-MomHH86) if Age2_Yr104==21;
egen temp8=rowmin(MomHH82-MomHH85) if Age2_Yr104==22;
egen temp9=rowmin(MomHH81-MomHH84) if Age2_Yr104==23;
egen temp10=rowmin(MomHH80-MomHH83) if Age2_Yr104==24;
egen temp11=rowmin(MomHH79-MomHH82) if Age2_Yr104==25;
egen temp12=rowmin(MomHH79-MomHH81) if Age2_Yr104==26;
egen temp13=rowmin(MomHH79-MomHH80) if Age2_Yr104==27;
forvalues x = 1(1)13 {;
	replace Res_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace Res_0to3=MomHH79 if Age2_Yr104==28;
drop temp* MomHH*;

*Health Limitations that are reported prior to age 5*;

gen Ear86=.;
gen Blood86=.;
gen Epilepsy86=.;
local Health     "Brain 
			Hyper 
			Asthma 
			Resp 
			Speech 
			Deaf 
			Blind 
			Disturb 
			Allergy 
			Crippled 
			Retard 
			Heart 
			Nerve 
			Ear 
			Blood 
			Epilepsy 
			OtherLim";
foreach x of local Health {;
	forvalues y = 86(2)90 {;
		gen temp`x'`y'=.;
		replace temp`x'`y'=1 if `x'`y'>0 & `x'`y'!=.;
		*replace temp`x'`y'=0 if HealthCond`y'!=. & temp`x'`y'!=1*;
	};
	egen `x'=rowmax(temp`x'*);
	gen `x'_before=.;
	forvalues y =86(2)90  {;
		replace `x'_before=1 if temp`x'`y'==1 & Age2_Mo`y'<60;
	};
	drop temp*;
};
egen HealthCond_before=rowmax(*before);
replace HealthCond_before=0 if HealthCond_before!=1;

*Some children are too old and so pre-Head Start info is unavailable for them*;

replace HealthCond_before=. if Res_0to3==.;

*Low Birthweight*;

gen int Low_BW=BirthWeight<88;
replace Low_BW=. if BirthWeight==.;
gen int VLow_BW=BirthWeight<53;
replace VLow_BW=. if BirthWeight==.;

*Attrition - If respondent disappears from sample before age 19*;

gen Attrit=0 if YA_LastInterview==2004;
replace Attrit=1 if YA_LastInterview!=2004 & YA_LastInterview!=.;

*Count those who attrit before 2004 but after they turn 19 as being in the sample*;

replace Attrit=0 if Attrit==1 & YA_LastInterview==2002 & Age2_Yr102>=19;
replace Attrit=0 if Attrit==1 & YA_LastInterview==2000 & Age2_Yr100>=19;
foreach x in 94 96 98 {;
	replace Attrit=0 if Attrit==1 & YA_LastInterview==19`x' & Age2_Yr`x'>=19;
};

*Create dummies for estimation sample*;

foreach x in 1 2 3 {;
	gen Sample90_`x'=1 if HS`x'_FE90!=. & SampleID!=12 & Attrit==0 & Age2_Yr104>=19;

	*Also include those who are 18 but have their 19th birthday in the first 7 months of 2004, for the school year*;

	replace Sample90_`x'=1 if HS`x'_FE90!=. & Attrit==0 & Sample90_`x'!=1 & DOB_Yr_Child==1985 & DOB_Mo_Child<8;
};

*Separate kids that are in Head Start at age 3 from later*;

gen byte Three=(Age_1stHS88<=3 | Age_1stHS90<=3);
gen byte NotThree=(Age_1stHS88>3 & Age_1stHS88!=.) | (Age_1stHS90>3 & Age_1stHS90!=.);
gen HS_3=1 if Three==1;
replace HS_3=2 if NotThree==1;
replace HS_3=0 if HS_3==.;
gen PreK_FE_3=PreK_FE;
replace PreK_FE_3=0 if PreK_FE_3==1 & HS_3==1;
tabstat PermInc MomDropout MomSomeColl AgeAFQT_std Hispanic Black White, by(PreK_FE_3) s(mean semean);

*Log Income Ages 0-3, Log Income at Age 3*;

forvalues x = 78(1)90 {;
	gen Income`x'=NetFamInc`x';
};
gen Income_0to3=Income90 if Age2_Yr104==14;
egen temp1=rowmean(Income89-Income90) if Age2_Yr104==15;
egen temp2=rowmean(Income88-Income90) if Age2_Yr104==16;
egen temp3=rowmean(Income87-Income90) if Age2_Yr104==17;
egen temp4=rowmean(Income86-Income89) if Age2_Yr104==18;
egen temp5=rowmean(Income85-Income88) if Age2_Yr104==19;
egen temp6=rowmean(Income84-Income87) if Age2_Yr104==20;
egen temp7=rowmean(Income83-Income86) if Age2_Yr104==21;
egen temp8=rowmean(Income82-Income85) if Age2_Yr104==22;
egen temp9=rowmean(Income81-Income84) if Age2_Yr104==23;
egen temp10=rowmean(Income80-Income83) if Age2_Yr104==24;
egen temp11=rowmean(Income79-Income82) if Age2_Yr104==25;
egen temp12=rowmean(Income79-Income81) if Age2_Yr104==26;
egen temp13=rowmean(Income78-Income80) if Age2_Yr104==27;
egen temp14=rowmean(Income78-Income79) if Age2_Yr104==28;
forvalues x = 1(1)14 {;
	replace Income_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace Income_0to3=Income78 if Age2_Yr104==29;
drop temp* Income78-Income90;
gen LogInc_0to3=ln(Income_0to3);

gen IncAt3=NetFamInc78 if Age2_Yr104==29;
replace IncAt3=NetFamInc79 if Age2_Yr104==28;
replace IncAt3=NetFamInc80 if Age2_Yr104==27;
replace IncAt3=NetFamInc81 if Age2_Yr104==26;
replace IncAt3=NetFamInc82 if Age2_Yr104==25;
replace IncAt3=NetFamInc83 if Age2_Yr104==24;
replace IncAt3=NetFamInc84 if Age2_Yr104==23;
replace IncAt3=NetFamInc85 if Age2_Yr104==22;
replace IncAt3=NetFamInc86 if Age2_Yr104==21;
replace IncAt3=NetFamInc87 if Age2_Yr104==20;
replace IncAt3=NetFamInc88 if Age2_Yr104==19;
replace IncAt3=NetFamInc89 if Age2_Yr104==18;
replace IncAt3=NetFamInc90 if Age2_Yr104==17;

gen LogIncAt3=ln(IncAt3);

*First Born and Gender*;

gen byte FirstBorn=BirthOrder==1;
replace FirstBorn=. if BirthOrder==.;
gen byte Male=Sex_Child==1;

*PPVT score at age 3*;

gen PPVTat3=PPVT_Raw86 if (PPVTAge86>=36 & PPVTAge86<47);
replace PPVTat3=PPVT_Raw88 if (PPVTAge88>=36 & PPVTAge88<47) & PPVTat3==.;
replace PPVTat3=PPVT_Raw90 if (PPVTAge90>=36 & PPVTAge90<47) & PPVTat3==.;

*HOME score*;

egen HOME_Pct_0to3= rowmean(HOME_Pct86 HOME_Pct88) if Age2_Yr104<=19 & Age2_Yr104>=16;
egen temp1 = rowmean(HOME_Pct88 HOME_Pct90) if Age2_Yr104<=15 & Age2_Yr104>=14;
replace HOME_Pct_0to3=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
replace HOME_Pct_0to3=HOME_Pct86 if Age2_Yr104>=20 & Age2_Yr104<=21;
drop temp1;

*Mom worked 0-3, Avg. wks. stopped work before birth (cond. on having a job), Mom hrs worked/wk 0-1*;

egen temp=rowmean(Moth_HrsWorked*Before);
gen Moth_HrsWorked_BefBirth=temp/13;
drop temp;
egen Moth_HrsWorked_0to3=rowmean(Moth_HrsWorked*Qtr);
egen Moth_HrsWorked_Avg_0to3=rowmean(Moth_HrsWorked*Avg);
egen Moth_HrsWorked_0to1=rowmean(Moth_HrsWorked_1_Avg Moth_HrsWorked_2_Avg Moth_HrsWorked_3_Avg Moth_HrsWorked_4_Avg);

*Father and/or Grandmother present, ages 0-3*;

egen Father_HH_0to3= rowmean(Father_HH90 Father_HH92 Father_HH93) if Age2_Yr104==14;
egen temp1 =rowmean(Father_HH89 Father_HH90) if Age2_Yr104==15;
egen temp2 =rowmean(Father_HH88 Father_HH89 Father_HH90) if Age2_Yr104==16;
egen temp3= rowmean(Father_HH87 Father_HH88 Father_HH89 Father_HH90) if Age2_Yr104==17;
egen temp4= rowmean(Father_HH86 Father_HH87 Father_HH88 Father_HH89) if Age2_Yr104==18;
egen temp5= rowmean(Father_HH85 Father_HH86 Father_HH87 Father_HH88) if Age2_Yr104==19;
egen temp6= rowmean(Father_HH84 Father_HH85 Father_HH86 Father_HH87) if Age2_Yr104==20;
egen temp7= rowmean(Father_HH84 Father_HH85 Father_HH86) if Age2_Yr104==21;
egen temp8= rowmean(Father_HH84 Father_HH85) if Age2_Yr104==22;
replace Father_HH_0to3=temp1 if Age2_Yr104==15 ;
replace Father_HH_0to3=temp2 if Age2_Yr104==16 ;
replace Father_HH_0to3=temp3 if Age2_Yr104==17 ;
replace Father_HH_0to3=temp4 if Age2_Yr104==18;
replace Father_HH_0to3=temp5 if Age2_Yr104==19;
replace Father_HH_0to3=temp6 if Age2_Yr104==20;
replace Father_HH_0to3=temp7 if Age2_Yr104==21;
replace Father_HH_0to3=temp8 if Age2_Yr104==22;
replace Father_HH_0to3=Father_HH84 if Age2_Yr104==23;
drop temp*;

forvalues x = 79(1)90 {;
	gen byte GMom`x'=1 if Grandmother`x'==1;
	replace GMom`x'=0 if Grandmother`x'!=1 & Grandmother`x'!=.;
};
gen GMom_0to3=GMom90 if Age2_Yr104==14;
egen temp1=rowmean(GMom89-GMom90) if Age2_Yr104==15;
egen temp2=rowmean(GMom88-GMom90) if Age2_Yr104==16;
egen temp3=rowmean(GMom87-GMom90) if Age2_Yr104==17;
egen temp4=rowmean(GMom86-GMom89) if Age2_Yr104==18;
egen temp5=rowmean(GMom85-GMom88) if Age2_Yr104==19;
egen temp6=rowmean(GMom84-GMom87) if Age2_Yr104==20;
egen temp7=rowmean(GMom83-GMom86) if Age2_Yr104==21;
egen temp8=rowmean(GMom82-GMom85) if Age2_Yr104==22;
egen temp9=rowmean(GMom81-GMom84) if Age2_Yr104==23;
egen temp10=rowmean(GMom80-GMom83) if Age2_Yr104==24;
egen temp11=rowmean(GMom79-GMom82) if Age2_Yr104==25;
egen temp12=rowmean(GMom79-GMom81) if Age2_Yr104==26;
egen temp13=rowmean(GMom79-GMom80) if Age2_Yr104==27;
forvalues x = 1(1)13 {;
	replace GMom_0to3=temp`x' if Age2_Yr104==`x'+14;
};
replace GMom_0to3=GMom79 if Age2_Yr104==28;
drop temp* GMom79-GMom90;

*Mom Care 0-3, Relative Care 0-3, Nonrelative care 0-3*;

rename ChildCare_1stYr ChildCare_1_Yr;
rename ChildCare_Type_1stYr ChildCare_Type_1_Yr;
rename ChildCare_2ndYr ChildCare_2_Yr;
rename ChildCare_Type_2ndYr ChildCare_Type_2_Yr;
rename ChildCare_3rdYr ChildCare_3_Yr;
rename ChildCare_Type_3rdYr ChildCare_Type_3_Yr;
foreach y in 1 2 3 {;
	gen RelCare_`y'_Yr=1 if  ChildCare_Type_`y'_Yr!=. &  ChildCare_Type_`y'_Yr<=10;
	replace RelCare_`y'_Yr=0 if  ChildCare_`y'_Yr!=. & RelCare_`y'_Yr!=1;
	gen NonRelCare_`y'_Yr=1 if  ChildCare_Type_`y'_Yr!=. &  ChildCare_Type_`y'_Yr>10;
	replace NonRelCare_`y'_Yr=0 if  ChildCare_`y'_Yr!=. & NonRelCare_`y'_Yr!=1;
	gen MomCare_`y'_Yr=1 if  (RelCare_`y'_Yr==0 &  NonRelCare_`y'_Yr==0);
	replace MomCare_`y'_Yr=0 if  MomCare_`y'_Yr!=1 & ( RelCare_`y'_Yr!=. &  NonRelCare_`y'_Yr!=.);
};
egen RelCare=rowmean( RelCare*);
egen NonRelCare=rowmean( NonRelCare*);
egen MomCare=rowmean(MomCare*);

*Mom smoked, Mom drank, Breastfed, Doctor's visit in last 3 months, Dentist ever, Weight Change during preg*;

gen byte Alc_BefBirth= Freq_Alc_BefBirth>=3 & Freq_Alc_BefBirth!=.;
foreach x in Doctor Dentist {;
	egen `x'_temp= rowmean(Last_`x'86 Last_`x'88) if Age2_Yr104<=19 & Age2_Yr104>=16;
	egen temp1=rowmean(Last_`x'88 Last_`x'90) if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_temp=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_temp=Last_`x'86 if Age2_Yr104>=20 & Age2_Yr104<=21;
	drop temp1;
};
gen Doctor_0to3=1 if Doctor_temp<=2;
replace Doctor_0to3=0 if Doctor_temp>2 & Doctor_temp!=.;
gen Dentist_0to3=1 if Dentist_temp<7;
replace Dentist_0to3=0 if Dentist_temp==7;
drop Doctor_temp Dentist_temp;

*UNCHANGED - Moth_Smoke_BefBirth, Breastfed, Moth_WeightChange*;

*Illness in 1st year, Premature birth, Birthweight, Priv Health Insurance 0-3, Medicaid 0-3*;

gen byte Premature=BornEarlyorLate==1;
replace Premature=. if BornOnTime==.;

foreach x in Insurance Medicaid {;
	egen `x'_0to3= rowmean(`x'86 `x'88) if Age2_Yr104<=19 & Age2_Yr104>=16;
	egen temp1=rowmean(`x'88 `x'90) if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_0to3=temp1 if Age2_Yr104<=15 & Age2_Yr104>=14;
	replace `x'_0to3=`x'86 if Age2_Yr104>=20 & Age2_Yr104<=21;
	drop temp1;
};

gen logBW=ln(BirthWeight);

*LIST OF VARIABLE NAMES FOR TABLE 2*;

*Attrit Res_0to3 HealthCond_before logBW VLow_BW LogInc_0to3 LogIncAt3 FirstBorn Male Age2_Yr104*
*PPVTAge3 HOME_Pct_0to3 Moth_HrsWorked_BefBirth Moth_HrsWorked_0to1* 
*Father_HH_0to3 GMom_0to3 MomCare RelCare NonRelCare* 
*Breastfed Moth_Smoke_BefBirth Alc_BefBirth Doctor_0to3 Dentist_0to3 Moth_WeightChange* 
*Illness_1stYr Premature Insurance_0to3 Medicaid_0to3 PreTreatIndex*

*Create index of covariates for summary test*;

unab Covariates: 	Res_0to3 HealthCond_before logBW LogInc_0to3 LogIncAt3 
			FirstBorn Male Age2_Yr104 HOME_Pct_0to3 Moth_HrsWorked_BefBirth 
			Moth_HrsWorked_0to1 Father_HH_0to3 GMom_0to3 MomCare RelCare NonRelCare 
			Moth_Smoke_BefBirth Alc_BefBirth Breastfed Doctor_0to3 Dentist_0to3 Moth_WeightChange 
			Illness_1stYr Premature Insurance_0to3 Medicaid_0to3;
foreach x of local Covariates {;
egen `x'_CV=std(`x') if Sample90_2==1, mean(0) std(1);
};

*Reverse sign so that positive=good outcomes*;

foreach var of varlist 	HealthCond_before_CV Male_CV Age2_Yr104_CV GMom_0to3_CV MomCare_CV RelCare_CV 
				Moth_Smoke_BefBirth_CV Alc_BefBirth_CV  Illness_1stYr_CV Premature_CV Medicaid_0to3_CV {;
				replace `var'=-`var';
};
egen temp=rowmean(*_CV) if Sample90_2==1;
egen PreTreatIndex=std(temp), mean(0) std(1);
drop temp *_CV;

*Run regs for Table 2*;

*Sample is slightly different for Attrit and PPVTat3 than the rest of the covariates*;

xtset MotherID;
xtreg Attrit HS2_FE90 Pre2_FE90 
	if HS2_FE90!=. & SampleID!=12 & (Age2_Yr104>=19 | (DOB_Yr_Child==1985 & DOB_Mo_Child<9)), fe vce(cluster MotherID);
test HS2_FE90=Pre2_FE90;
estadd scalar Difference = r(p);
estimates store Attrit;
estout Attrit using Table2-Rev.txt, varlabels(_cons None) 
						starlevels(* 0.10 ** 0.05 *** 0.01) 
						stardrop(_cons) 
						cells(b(star fmt(3)) 
						se(par([ ]) fmt(3))) 
						stats(Difference N, star(Difference) 
						fmt(3 0) 
						labels("P-value of Diff" "Sample Size")) 
						label append;
drop _est_Attrit;
xtreg PPVTat3 HS2_FE90 Pre2_FE90 Male FirstBorn Age2_Mo90 if Sample90_2==1, fe vce(cluster MotherID);
test HS2_FE90=Pre2_FE90;
estadd scalar Difference = r(p);
estimates store PPVTat3;
estout PPVTat3 using Table2-Rev.txt, 	varlabels(_cons None) 
							starlevels(* 0.10 ** 0.05 *** 0.01) 
							stardrop(_cons) 
							cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
							stats(Difference N, star(Difference) 
							fmt(3 0) 
							labels("P-value of Diff" "Sample Size")) 
							label append;
drop _est_PPVTat3;

*Now loop over all the covariates for respondents in the estimation sample*;

unab Covariates: 	Res_0to3 HealthCond_before VLow_BW logBW LogInc_0to3 LogIncAt3 FirstBorn Male 
			Age2_Yr104 HOME_Pct_0to3 Moth_HrsWorked_BefBirth Moth_HrsWorked_0to1 Father_HH_0to3 
			GMom_0to3 MomCare RelCare NonRelCare Moth_Smoke_BefBirth Alc_BefBirth Breastfed 
			Doctor_0to3 Dentist_0to3 Moth_WeightChange Illness_1stYr Premature Insurance_0to3 
			Medicaid_0to3 PreTreatIndex;

foreach y of local Covariates {;
	xtreg `y' HS2_FE90 Pre2_FE90 if Sample90_2==1, fe vce(cluster MotherID);
	test HS2_FE90=Pre2_FE90;
	estadd scalar Difference = r(p);
	estimates store `y';
	estout `y' using Table2-Rev.txt, 	varlabels(_cons None) 
							starlevels(* 0.10 ** 0.05 *** 0.01) 
							stardrop(_cons) 
							cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
							stats(Difference N, star(Difference) 
							fmt(3 0) 
							labels("P-value of Diff" "Sample Size")) 
							label append;
	drop _est_`y';
};

*Generate control means in Table 2*;

foreach x of local Covariates {;
	tabstat `x' if Sample90_2==1, by(PreK_FE) s(mean sd count);
};

*Generate imputed values for missing covariate data, based on race and gender only*;

unab Covariates: 	Res_0to3 HealthCond_before VLow_BW logBW LogInc_0to3 LogIncAt3 FirstBorn PPVTat3 
			HOME_Pct_0to3 Moth_HrsWorked_BefBirth Moth_HrsWorked_Avg_0to3 Moth_HrsWorked_0to1 
			Father_HH_0to3 GMom_0to3 MomCare RelCare NonRelCare Moth_Smoke_BefBirth Alc_BefBirth 
			Breastfed Doctor_0to3 Dentist_0to3 Moth_WeightChange Illness_1stYr Premature Insurance_0to3 
			Medicaid_0to3;

foreach x of local Covariates {;
	impute `x' Male Black Hispanic if Sample90_2==1, gen(`x'_imp);
	gen `x'_miss=1 if `x'==. & `x'_imp!=.;
	replace `x'_miss=0 if `x'_miss!=1 & `x'_imp!=.;
};

*END COVARIATE SECTION*;

*START TEST SCORE ANALYSIS*;

*First create age-at-test variable, in months and years*;

foreach x in 86 88 90 92 94 96 98 100 102 104 {;
	gen AgeTest_Mo`x'=PPVTAge`x';
	
	*Create age in years (rather than months) variable*;

	gen AgeTest_Yr`x'=0 if AgeTest_Mo`x'<12;
	forvalues y = 1(1)35 {;
		replace AgeTest_Yr`x'=`y' if AgeTest_Mo`x'>=12*`y' & AgeTest_Mo`x'<(12*`y')+12;
	};
};

*Create Test score composite*;

forvalues x=86(2)104 {;
	egen Test_Pct`x'=rowmean(PPVT_Pct`x' PIATMT_Pct`x' PIATRR_Pct`x');
};

*Now organize the data as long, for longitudinal test score analysis*;

preserve;
unab Covariates: *_imp *_miss;
keep if Sample90_2==1;
keep 	ChildID MotherID Attrit Age2_Yr104 Black NonBlack Male PermInc_std MomHS MomSomeColl impAFQT_std 
	PreK_FE PreK_FE_3 HS2_FE90 Pre2_FE90 `Covariates' PreTreatIndex PIATMT_Pct* PIATMT_Raw* PIATRR_Pct* 
	PIATRR_Raw* PPVT_Pct* PPVT_Raw* Test_Pct* BPI_Pct* BPIAS_Pct* AgeTest_Yr*;

reshape long 	PPVT_Raw PIATMT_Raw PIATRR_Raw PPVT_Pct PIATMT_Pct PIATRR_Pct 
			Test_Pct BPI_Pct BPIAS_Pct AgeTest_Yr, i(ChildID) j(year);

*Drop observations with test scores outside the range*;

drop if AgeTest_Yr<5 | AgeTest_Yr>14;

*Generate age groups, spaced in 2 year increments to keep the panel balanced*;

gen Group_5to6=AgeTest_Yr<7;
gen Group_7to10=AgeTest_Yr>=7 & AgeTest_Yr<=10;
gen Group_11to14=AgeTest_Yr>=11;

*Create group-standardized measures of each test score*;

foreach g in 5to6 7to10 11to14 {;
	foreach test in PIATMT PIATRR PPVT BPI BPIAS {;
		egen `test'_std_`g'=std(`test'_Pct) if Group_`g'==1;
	};
	egen temp=rowmean( PIATMT_std_`g' PIATRR_std_`g' PPVT_std_`g');
	egen Test_std_`g'=std(temp), mean(0) std(1);
	drop temp;
};

*Now combine the groups into one variable, for the overall (age 5 to 14) analysis*;

foreach test in PPVT PIATMT PIATRR BPI BPIAS Test {;
	gen `test'_std= `test'_std_5to6 if Group_5to6==1;
	replace `test'_std=`test'_std_7to10 if Group_7to10==1;
	replace `test'_std=`test'_std_11to14 if Group_11to14==1;
};

*Create subgroup categories for tables*;

*Name them `x' Non`x' to facilitate the loop*;

gen NonMale=Male==0;
gen lowAFQT=impAFQT_std<=-1 & impAFQT_std!=.;
gen NonlowAFQT=lowAFQT==0;

foreach a in 5to6 7to10 11to14 {;
	foreach x in HS Pre {;
		gen `x'_`a'=1 if `x'2_FE90==1 & Group_`a'==1;
		replace `x'_`a'=0 if `x'_`a'!=1 & `x'2_FE90!=.;
	};
};

foreach g in Male NonMale Black NonBlack lowAFQT NonlowAFQT {;
	foreach x in HS Pre {;
		gen `x'_`g'=1 if `x'2_FE90==1 & `g'==1;
		replace `x'_`g'=0 if `x'_`g'!=1 & `x'2_FE90!=.;
	};
};

foreach a in 5to6 7to10 11to14 {;
	foreach g in Male NonMale Black NonBlack lowAFQT NonlowAFQT {;
		foreach x in HS Pre {;
			gen `x'_`g'_`a'=1 if `x'2_FE90==1 & `g'==1 & Group_`a'==1;
			replace `x'_`g'_`a'=0 if `x'_`g'_`a'!=1 & `x'2_FE90!=.;
		};
	};
};

*Create Group5to14=1 for everyone, just to facilitate the loop*;

gen Group_5to14=1;

*Create Table 3 - Test Scores by Age Group*;

unab Covariates: *_imp *_miss;

*Column 1 - no controls*;

xi: reg 	Test_std HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
		Male i.year Group* i.AgeTest_Yr, vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS_1 = r(p);
estimates store Test_1;
estout Test_1 using Table3.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS* Pre*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_HS_1 r2 N, fmt(3 3 0) 
						labels("p(All age effects equal)" "R squared" "Sample Size")) 
						label append;

*Column 2 - Add pre-treatment covariates*;

xi: reg Test_std 	HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
			Male i.year Group* i.AgeTest_Yr `Covariates', vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS_2 = r(p);
estimates store Test_2;
estout Test_2 using Table3.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS* Pre*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_HS_2 r2 N, fmt(3 3 0) 
						labels("p(All age effects equal)" "R squared" "Sample Size")) 
						label append;

*Column 3 - Add income, AFQT etc.*;

xi: reg Test_std 	HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
			Male PermInc_std impAFQT_std MomHS MomSomeColl 
			i.year Group* i.AgeTest_Yr `Covariates', vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS_3 = r(p);
estimates store Test_3;
estout Test_3 using Table3.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS* Pre* PermInc_std impAFQT_std MomHS MomSomeColl) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_HS_3 r2 N, fmt(3 3 0) 
						labels("p(All age effects equal)" "R squared" "Sample Size")) 
						label append;

*Column 4 - add family FE, without covariates*;

xtset MotherID;

xi: xtreg Test_std 	HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
				Male i.year Group* i.AgeTest_Yr, fe vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS_4 = r(p);
estimates store Test_4;
estout Test_4 using Table3.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS* Pre*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_HS_4 r2 N, fmt(3 3 0) 
						labels("p(All age effects equal)" "R squared" "Sample Size")) 
						label append;

*Column 5 - family FE plus covariates*;

xi: xtreg 	Test_std HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
		Male i.year Group* i.AgeTest_Yr `Covariates', fe vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS_5 = r(p);
estimates store Test_5;
estout Test_5 using Table3.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS* Pre*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_HS_5 r2 N, fmt(3 3 0) 
						labels("p(All age effects equal)" "R squared" "Sample Size")) 
						label append;
drop _est_*;


*Now create first 4 columns of Table 4 - Overall, then by race, gender and maternal AFQT score*;

*Overall*;

unab Covariates: *_imp *_miss;
xi: xtreg Test_std 	HS_5to6 HS_7to10 HS_11to14 Pre_5to6 Pre_7to10 Pre_11to14 
				Male i.year Group* i.AgeTest_Yr `Covariates', fe vce(cluster MotherID);
test HS_5to6=HS_7to10=HS_11to14;
estadd scalar Difference_HS = r(p);
test HS_5to6=Pre_5to6;
estadd scalar Difference_5to6 = r(p);
test HS_7to10=Pre_7to10;
estadd scalar Difference_7to10 = r(p);
test HS_11to14=Pre_11to14;
estadd scalar Difference_11to14 = r(p);
estimates store Test;
estout Test using Table4.txt, starlevels(* 0.10 ** 0.05 *** 0.01) 
					keep(HS* Pre*) 
					cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
					stats(Difference_HS Difference_5to6 Difference_7to10 Difference_11to14 r2 N, fmt(3 3 3 3 3 0)
					labels("p(All age effects equal)" "p(HS=Pre - 5 to 6)" "p(HS=Pre - 7 to 10)" "p(HS=Pre - 11 to 14)" "R squared" "Sample Size")) 
					label append;

*By subgroups*;

foreach g in Black Male lowAFQT {;
	xi: xtreg Test_std 	HS_`g'_5to6 HS_`g'_7to10 HS_`g'_11to14 
					HS_Non`g'_5to6 HS_Non`g'_7to10 HS_Non`g'_11to14 
					Pre_`g'_5to6 Pre_`g'_7to10 Pre_`g'_11to14 
					Pre_Non`g'_5to6 Pre_Non`g'_7to10 Pre_Non`g'_11to14 
					Male i.Age2_Yr104 `Covariates', fe vce(cluster MotherID);
	test HS_`g'_5to6=HS_Non`g'_5to6;
	estadd scalar Diff_5to6_`g' = r(p);
	test HS_`g'_7to10=HS_Non`g'_7to10;
	estadd scalar Diff_7to10_`g' = r(p);
	test HS_`g'_11to14=HS_Non`g'_11to14;
	estadd scalar Diff_11to14_`g' = r(p);
	estimates store HS_`g';
	estout HS_`g' using Table4.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
							keep(HS_* Pre_*) 
							cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
							stats(Diff_5to6_`g' Diff_7to10_`g' Diff_11to14_`g' r2 N, fmt(3 3 3 3 0) 
							labels("p(5 to 6 (`g'))" "p(7 to 10 `g'))" "p(11 to 14 `g')" "R squared" "Sample Size")) 
							label append;
	drop _est_*;
};

foreach g in Black Male lowAFQT {;
	xi: xtreg Test_std HS_`g' HS_Non`g' Pre_`g' Pre_Non`g' Male i.Age2_Yr104 `Covariates', fe vce(cluster MotherID);
	test HS_`g'=Pre_`g';
	estadd scalar Diff_`Test'_`g' = r(p);
	estimates store `Test'_`g';
	test HS_Non`g'=Pre_Non`g';
	estadd scalar Diff_`Test'_Non`g' = r(p);
	estimates store `Test'_Non`g';
	test HS_`g'=HS_Non`g';
	estadd scalar HS_`Test'_`g' = r(p);
	estimates store HS_`Test'_`g';
	estout HS_`Test'_`g' using Table4.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
								keep(HS_* Pre_*) 
								cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
								stats(Diff_`Test'_`g' Diff_`Test'_Non`g' HS_`Test'_`g' r2 N, fmt(3 3 3 3 0)
								labels("p(HS=Pre (`g'))" "p(HS=Pre Non`g'))" "p(HS_`g'=HS_Non`g')" "R squared" "Sample Size")) 
								label append;
	drop _est_*;
};


restore;

*END TEST SCORE ANALYSIS*;

*THIS SECTION CREATES THE NON-TEST SCORE OUTCOME VARIABLES*;

*Create subgroup categories for tables*;

*Name them `x' Non`x' to facilitate the loop*;

gen NonMale=Male==0;
gen lowAFQT=impAFQT_std<=-1 & impAFQT_std!=.;
gen NonlowAFQT=lowAFQT==0;

foreach g in Male NonMale Black NonBlack lowAFQT NonlowAFQT {;
	foreach x in HS Pre {;
		gen `x'_`g'=1 if `x'2_FE90==1 & `g'==1;
		replace `x'_`g'=0 if `x'_`g'!=1 & `x'2_FE90!=.;
	};
};

*Grade repetition and learning disabilities*;

*Fix random error first*;

replace Repeat92=. if Repeat92==6;

*Create yearly indicators, then take the max across years to get "ever repeat" and "ever LD" measures*;

*Note that, particulary for Repeat, some kids are already too old by the 1st survey year, so data is missing for them*;

egen Repeat94=rowmax( 	Repeat_K94 Repeat_194 Repeat_294 Repeat_394 Repeat_494 Repeat_594 Repeat_694 Repeat_794 Repeat_894);
egen Repeat96=rowmax( 	Repeat_K96 Repeat_196 Repeat_296 Repeat_396 Repeat_496 Repeat_596 Repeat_696 Repeat_796 Repeat_896 
				Repeat_996 Repeat_1096 Repeat_1196 Repeat_1296);
replace Repeat94=0 if Repeat_None94==1 & Repeat94!=1;
replace Repeat96=0 if Repeat_None96==1 & Repeat96!=1;
rename Repeat_None98 RepeatNone98;
drop Repeat_YA98;
foreach var of varlist Repeat_*98 {;
	gen byte `var'_temp=1 if `var'==1;
};
egen Repeat98=rowmax(Repeat_*98_temp);
replace Repeat98=0 if RepeatNone98==1;
drop Repeat*temp;

egen Repeat=rowmax(Repeat88 Repeat90 Repeat92 Repeat94 Repeat96 Repeat98 Repeat100 Repeat102 Repeat104);

forvalues x = 86(2)100 {;
	gen tempLD`x'=LD`x';
	replace tempLD`x'=0 if HealthCond`x'!=. & tempLD`x'!=1;
};
egen LD=rowmax(tempLD*);

*Want to exclude small number of kids that were diagnosed with an LD before age 5*;

gen LD_before=.;
forvalues x =86(2)100  {;
	replace LD_before=1 if tempLD`x'==1 & Age2_Yr`x'<5;
};
replace LD=. if LD_before==1;
drop temp*;

*Long-Term Outcomes - High School Graduation, College Attendance, Idle, Crime, Teen Parenthood, Health Status*;

*Code educational attainment outcomes using the recoded and cleaned variable YA_Educ, rather than other measures*;
*Other measures are sometimes only reported if the respondent answers yes or no certain screener questions*;

*HS Graduation, with and without GED*;

gen HSGrad=YA_Educ104>=12 & YA_Educ104!=.;
replace HSGrad=. if YA_Educ104==.;
egen GED=rowmax(GED*);
gen HSGrad_GED=HSGrad;
replace HSGrad_GED=. if HSGrad_GED==1 & GED==2;

*At least one year of college attempted*;

egen HighGradeAtt=rowmax(HighGrade_Att*);

gen someCollAtt=YA_Educ104>=13 & YA_Educ104!=.;
replace someCollAtt=. if YA_Educ104==.;
replace someCollAtt=1 if HighGradeAtt>12 & HighGradeAtt!=.;

*Labor Force Participation*;
*Define "Idle" as Not in school AND zero wages in 2004 or most recent interview year*;

gen Wages=Wages104;
replace Wages=Wages102 if Wages==. & YA_LastInterview==2002;
replace Wages=Wages100 if Wages==. & YA_LastInterview==2000;
replace Wages=Wages98 if Wages==. & YA_LastInterview==1998;
replace Wages=Wages96 if Wages==. & YA_LastInterview==1996;
replace Wages=Wages94 if Wages==. & YA_LastInterview==1994;
foreach x in 104 102 100 98 96 94 {;
gen PosWages`x'=Wages`x'>0 & Wages`x'!=.;
replace PosWages`x'=. if Wages`x'==.;
};

*If respondent did not know wages, but did report an estimate, code as 0 for lowest estimated category and 1 for all others*;

gen PosWages=PosWages104;
replace PosWages=0 if PosWages104==. & Wages_Est104==1;
replace PosWages=1 if PosWages104==. & Wages_Est104!=. & Wages_Est104>1;
replace PosWages=PosWages102 if PosWages==. & YA_LastInterview==2002;
replace PosWages=0 if PosWages==. & PosWages102==. & Wages_Est102==1 & YA_LastInterview==2002;
replace PosWages=1 if PosWages==. & PosWages102==. & Wages_Est102!=. & Wages_Est102>1;
replace PosWages=PosWages100 if PosWages==. & YA_LastInterview==2000;
replace PosWages=0 if PosWages==. & PosWages100==. & Wages_Est100==1 & YA_LastInterview==2000;
replace PosWages=1 if PosWages==. & PosWages100==. & Wages_Est100!=. & Wages_Est100>1;

*There is no "estimate" code for earlier years*;

replace PosWages=PosWages98 if PosWages==. & YA_LastInterview==1998;
replace PosWages=PosWages96 if PosWages==. & YA_LastInterview==1996;
replace PosWages=PosWages94 if PosWages==. & YA_LastInterview==1994;

gen InSchool=InSchool104;
replace InSchool=InSchool102 if InSchool==. & YA_LastInterview==2002;
replace InSchool=InSchool100 if InSchool==. & YA_LastInterview==2000;
replace InSchool=InSchool98 if InSchool==. & YA_LastInterview==1998;
replace InSchool=InSchool96 if InSchool==. & YA_LastInterview==1996;
replace InSchool=InSchool94 if InSchool==. & YA_LastInterview==1994;

gen Idle=1 if InSchool==0 & PosWages==0;
replace Idle=0 if Idle!=1 & InSchool!=.;

*Crime - Ever convicted, been on probation, sentenced or in jail at time of interview*;

egen Convicted=rowmax(Convicted*);
egen Probation=rowmax(Probation*);
egen Sentenced=rowmax(Sentenced*);
foreach x in 94 96 98 100 102 104 {;
gen Prison`x'=.;
replace Prison`x'=1 if Resid`x'==5;
replace Prison`x'=0 if Resid`x'!=. & Prison`x'!=1;
};
egen Prison=rowmax(Prison*);
egen Crime=rowmax(Convicted Probation Sentenced Prison);

*Teen Parenthood*;

gen TeenPreg=1 if Ageat1stBirth<20;
replace TeenPreg=0 if TeenPreg!=1 & YA_NumKids!=.;

*Poor Health - Self-reported status*;

egen HealthReport=rowmean(Health_Report*);
gen PoorHealth=1 if HealthReport<3 & HealthReport!=.;
replace PoorHealth=0 if HealthReport!=. & PoorHealth!=1;

*Variable Names - HSGrad HSGrad_GED someCollAtt Idle Crime TeenPreg PoorHealth*;

*Now start compiling individual outcome measures, overall and by subgroup, for Table 5*;

*Overall*;

unab Covariates: *_imp *_miss;
xtset MotherID;
foreach o in Repeat LD HSGrad HSGrad_GED someCollAtt Idle Crime TeenPreg PoorHealth {;
	xi: xtreg `o' HS2_FE90 Pre2_FE90 Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);
	test HS2_FE90=Pre2_FE90;
	estadd scalar Difference_`o' = r(p);
	estimates store `o';
	estout `o' using Table5.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS2* Pre2*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_`o' r2 N, fmt(3 3 0) labels("p(Head Start=Other Preschools)" "R squared" "Sample Size")) 
						label append;
	drop _est_*;
};

*By subgroup*;

foreach o in Repeat LD HSGrad HSGrad_GED someColl Idle Crime TeenPreg PoorHealth {;
	foreach g in Black Male lowAFQT {;
		xi: xtreg `o' 	HS_`g' HS_Non`g' Pre_`g' Pre_Non`g' 
					Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);
		test HS_`g'=Pre_`g';
		estadd scalar Diff_`o'_`g' = r(p);
		estimates store `o'_`g';
		test HS_Non`g'=Pre_Non`g';
		estadd scalar Diff_`o'_Non`g' = r(p);
		estimates store `o'_Non`g';
		test HS_`g'=HS_Non`g';
		estadd scalar HS_`o'_`g' = r(p);
		estimates store HS_`o'_`g';
		estout HS_`o'_`g' using Table5.txt, starlevels(* 0.10 ** 0.05 *** 0.01) 
								keep(HS_* Pre_*) 
								cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
								stats(Diff_`o'_`g' Diff_`o'_Non`g' HS_`o'_`g' r2 N, fmt(3 3 3 3 0) 
								labels("p(HS=Pre (`g'))" "p(HS=Pre Non`g'))" "p(HS_`g'=HS_Non`g')" "R squared" "Sample Size")) 
								label append;
		drop _est_*;
	};
};

*Now create summary indices*;

unab Outcomes: LD Repeat HSGrad someCollAtt Idle Crime Prison TeenPreg PoorHealth HSGrad_GED; 
foreach o of local Outcomes { ;
	egen `o'_std=std(`o') if Sample90_2==1, mean(0) std(1);
};

*Reverse sign so that positive = "good" outcome*;

replace Repeat_std=-(Repeat_std);
replace LD_std=-(LD_std);
replace Idle_std=-(Idle_std);
replace Prison_std=-(Prison_std);
replace Crime_std=-(Crime_std);
replace TeenPreg_std=-(TeenPreg_std);
replace PoorHealth_std=-(PoorHealth_std);

*Create index of young adult outcomes*;

egen temp=rowmean(HSGrad_std someCollAtt_std Idle_std Crime_std TeenPreg_std PoorHealth_std) if Sample90_2==1;

*Now restandardize so that index has mean(0) sd(1)*;

egen Sum_Adult=std(temp), mean(0) std(1);
drop temp;

*Now create Noncognitive index*;

egen temp=rowmean(Repeat_std LD_std) if Sample90_2==1;
egen Noncog_std=std(temp), mean(0) std(1);
drop temp;

*Now estimate effects on each index, for same subgroups as above, and Append to Table 4*;

unab Covariates: *_imp *_miss;
xtset MotherID;
foreach o in Noncog_std Sum_Adult {;
	xi: xtreg `o' HS2_FE90 Pre2_FE90 Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);
	test HS2_FE90=Pre2_FE90;
	estadd scalar Difference_`o' = r(p);
	estimates store `o';
	estout `o' using Table4.txt, 	starlevels(* 0.10 ** 0.05 *** 0.01) 
						keep(HS2* Pre2*) 
						cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
						stats(Difference_`o' r2 N, fmt(3 3 0) 
						labels("p(Head Start=Other Preschools)" "R squared" "Sample Size")) 
						label append;
	drop _est_*;
};

foreach o in Noncog_std Sum_Adult {;
	foreach g in Black Male lowAFQT {;
		xi: xtreg `o' 	HS_`g' HS_Non`g' Pre_`g' Pre_Non`g' 
					Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);
		test HS_`g'=Pre_`g';
		estadd scalar Diff_`o'_`g' = r(p);
		estimates store `o'_`g';
		test HS_Non`g'=Pre_Non`g';
		estadd scalar Diff_`o'_Non`g' = r(p);
		estimates store `o'_Non`g';
		test HS_`g'=HS_Non`g';
		estadd scalar HS_`o'_`g' = r(p);
		estimates store HS_`o'_`g';
		estout HS_`o'_`g' using Table4.txt, starlevels(* 0.10 ** 0.05 *** 0.01) 
								keep(HS_* Pre_*) 
								cells(b(star fmt(3)) se(par([ ]) fmt(3))) 
								stats(Diff_`o'_`g' Diff_`o'_Non`g' HS_`o'_`g' r2 N, fmt(3 3 3 3 0) 
								labels("p(HS=Pre (`g'))" "p(HS=Pre Non`g'))" "p(HS_`g'=HS_Non`g')" "R squared" "Sample Size")) 
								label append;
		drop _est_*;
	};
};


*Now do appendix calculations - See Appendix do file for NLSY regressions*;
*You should be able to obtain the numbers below from that file* ;

gen HSGrad_wage=HSGrad_std*(.1672*(6/0.6973));
gen someCollAtt_wage=someCollAtt_std*(.0991*(6/0.6973));
gen Idle_wage=Idle_std*(.2917*(6/0.6973));
gen Crime_wage=Crime_std*(.0701*(6/0.6973));
gen TeenPreg_wage=TeenPreg_std*(.0273*(6/0.6973));
gen PoorHealth_wage=PoorHealth_std*(.0419*(6/0.6973));
egen temp=rowmean( HSGrad_wage-PoorHealth_wage) 
		if HSGrad_wage!=. & someCollAtt_wage!=. & Idle_wage!=. & Crime_wage!=. & TeenPreg_wage!=. & PoorHealth_wage!=.;
egen Sum_wage=std(temp), mean(0) std(1);
drop temp;

unab Covariates: *_imp *_miss;
xi: xtreg Sum_wage HS2_FE90 Pre2_FE90 Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);

*0.248 overall when index is reweighted*;

foreach g in Black Male lowAFQT {;
	xi: xtreg 	Sum_wage HS_`g' HS_Non`g' Pre_`g' Pre_Non`g' 
			Male i.Age2_Yr104 `Covariates' if Sample90_2==1, fe vce(cluster MotherID);
};

*Coefficients on each subgroup should be multiplied by the coefficients in the wage regressions*;




