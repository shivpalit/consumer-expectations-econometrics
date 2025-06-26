*Eco 205 final  - Shiv Palit;

Proc Import out=work.one
Datafile ="C:\Users\shpalit\Desktop\final_data.xls"
DBMS=XLS Replace;
Sheet="final_data"; 
Getnames=Yes;
run;


ODS TAGSETS.EXCELXP
file='saswork.xls'
STYLE=printer
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100'
embedded_titles = 'yes');

Data two;
	Set one;
	SP_ch2 = SP500_PCH*SP500_PCH;
	UNud_UNch = UNRATE_PCH*UNRATE_UD;
	logSP500 = log(SP500);
	logSP500L1 = log(SP500_LAG1);
	logINCOME = log(INCOME);
	logINVAMT = log(INVAMT);
	BUS2 = BUS12r*BUS12r;
	if SP500_UD=1 then SP_U=1;
	if SP500_UD=-1 then SP_U=0;
	SPudSP_ch = SP500_PCH*SP_U;
	if UNRATE_UD = -1 then UNRATE_UD = 0;
run;


proc means N mean stderr min Q1 median Q3 max data = two;
	var BUS12r logSP500 SP_U SP500_PCH UNRATE UNRATE_UD UNRATE_PCH CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK;
	title1 'Proc Means';
run;

proc corr data = two;
	VAR BUS12r logSP500 SP_U SP500_PCH UNRATE UNRATE_UD UNRATE_PCH CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK;
	title1 'Proc Corr';
run;
	

*MODELS;

	*Model1;
	proc reg data=two;
		Model_1: Model BUS12r = logSP500 SP_U SP500_PCH UNRATE UNRATE_UD UNRATE_PCH 
		CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT 
		AGE NUMKID PINC2 PSTK/acov vif;
		output out=model1 p=pred r=resid;
		Title1 'Model 1 - Overall';
	run;
	*AIC = -0.3183784;

	*Model2;
	proc reg data = two;
		Model2: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch 
		CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov vif;
		output out=model2 p=pred r=resid;
		Title1 'Model 2 - With interaction terms';
	run; 
	*AIC = -0.3183862;


	*Model3;
	proc reg data = two;
		Model3: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch 
		CPI PEXPr PAGO5r PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT PINC2 PSTK/acov vif;
		output out=model3 p=pred r=resid;
		Title1 'Model 3 - With restrictions (financials only)';
	run; 
	*AIC = -0.2986422;

*RAMSEYS;

	*Ramsey1;
	proc autoreg data = two;
		Ramsey1: Model BUS12r = logSP500 SP_U SP500_PCH UNRATE UNRATE_UD UNRATE_PCH CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/reset;
		Title1 'Ramsey test model 1';
	run; 

	*Ramsey2;
	proc autoreg data = two;
		Ramsey2: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/reset;
		Title1 'Ramsey test model 2';
	run; 

	*Ramsey3;
	proc autoreg data = two;
		Ramsey3: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT PINC2 PSTK/reset;
		Title1 'Ramsey test model 3';
	run; 

*HETEROSKEDASTICITY;
	Data BPG;
		set model2;
		error2 = resid**2;
		pred2 = pred**2;
	run;

	proc reg data = BPG;
		One: Model error2 = pred; *NR2 = 24.5042;
		Two: Model error2 = pred2; *NR2 = 1127.1932;
		Three: Model error2 = pred pred2; *NR2 = 1128.9435;
		Title 'BPG Heteroskedasticity check';
	run;


*CORRECTING FOR HETEROSKEDASTICITY;
	*using /acov;
	proc reg data = two;
		Model2: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov;
		Title 'Model 2 Results with White SE';
	run;

*SUBSET F-TESTS;

	*subset F-test personal (personal financial, personal non-financial, external);
	proc reg data = two;
		Model2: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK;
		test logSP500, SP_U, SP500_PCH, SPudSP_ch, UNRATE, UNRATE_UD, UNRATE_PCH, UNud_UNch, CPI;
		test PEXPr, PAGO5r, PEXP5r, INEXQ2, INEXQ1r, logINCOME, logINVAMT, PINC2, PSTK;
		test EDUC_college, MARRYd, SEXd, NUMKID, AGE;
		Title1 'subset F-tests';
	run;


*CHOW TEST;
	Data three;
		set two;
		IF SP_U = 1;
		
	proc means data=three;
	Title1 'Proc Means data 3'; 
	run;

	Data four;
		set two;
		IF SP_U = 0;

	proc means data=three;
	Title1 'Proc Means data 3'; 
	run;

	proc reg data=two;
		RESTRICTED: Model BUS12r = logSP500 SP500_PCH UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK;
		Title1 'Restricted model 2 for chow test of SP_U = 0 vs SP_U =1';
	run;

	proc reg data=three;
		SP_U: Model BUS12r = logSP500 SP500_PCH UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov;
		test logSP500, SP500_PCH, UNRATE, UNRATE_UD, UNRATE_PCH, UNud_UNch, CPI;
		test PEXPr, PAGO5r, PEXP5r, INEXQ2, INEXQ1r, logINCOME, logINVAMT, PINC2, PSTK;
		test EDUC_college, MARRYd, SEXd, NUMKID, AGE;
		Title1 'SP_U = 1 model 2 for chow test of SP_U = 0 vs SP_U =1';
	run;

	proc reg data=four;
		SP_D: Model BUS12r = logSP500 SP500_PCH UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov;
		test logSP500, SP500_PCH, UNRATE, UNRATE_UD, UNRATE_PCH, UNud_UNch, CPI;
		test PEXPr, PAGO5r, PEXP5r, INEXQ2, INEXQ1r, logINCOME, logINVAMT, PINC2, PSTK;
		test EDUC_college, MARRYd, SEXd, NUMKID, AGE;
		Title1 'SP_U = 0 model 2 for chow test of SP_U = 0 vs SP_U =1';
	run;

	*Chow = 2.14196;
	*F-crit = 1.811435 @ 0.01;  


*Model 2 after splitting dataset;

	proc reg data=three;
		SP_U: Model BUS12r = logSP500 SP500_PCH UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov;
		Title1 'SP_U model 2 for chow test of SP_U vs SP_D';
	run;

	proc reg data=four;
		SP_D: Model BUS12r = logSP500 SP500_PCH UNRATE UNRATE_UD UNRATE_PCH UNud_UNch CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK/acov;
		Title1 'SP_D model 2 for chow test of SP_U vs SP_D';
	run;

	*joint AIC = -0.3183862;
	*joint adj.R^2 = 0.2014;


*New;
Data five;
	set two;
	logSP_SPU = logSP500*SP_U;
	UNR_SPU = UNRATE_UD*SP_U;
	UNRch_SPU = UNRATE_PCH*SP_U;
	PEXP5_SPU = PEXP5r*SP_U;
	INEX_SPU = INEXQ1r*SP_U;
	PAGO_SPU = PAGO5r*SP_U;
	PCRY_SPU = PCRYr*SP_U;
	AGE_SPU = AGE*SP_U;
	run;
proc means data = five;
	Title1 'Proc Means data 5' ;
run;



*New Model - includes interactions with SP_U based on the analysis of results from the chow test;
proc reg data = five;
		Model4: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch 
		CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK
		logSP_SPU UNR_SPU UNRch_SPU PEXP5_SPU INEX_SPU PAGO_SPU PCRY_SPU AGE_SPU/vif acov;
		output out=model4 p=pred r=resid;
		test logSP500, SP_U, SP500_PCH, SPudSP_ch, UNRATE, UNRATE_UD, UNRATE_PCH, UNud_UNch, CPI;
		test PEXPr, PAGO5r, PEXP5r, INEXQ2, INEXQ1r, logINCOME, logINVAMT, PINC2, PSTK;
		test EDUC_college, MARRYd, SEXd, NUMKID, AGE;
		Title1 'Model 4 - Model 2 With extra interaction terms based on chow test';
	run; 

	*AIC = -0.3191278;

*HETEROSKEDASTICITY2;
	Data BPG2;
		set model4;
		error2 = resid**2;
		pred2 = pred**2;
	run;

	proc reg data = BPG2;
		One: Model error2 = pred; *NR2 = 26.2545;
		Two: Model error2 = pred2; *NR2 = 1125.4429;
		Three: Model error2 = pred pred2; *NR2 = 1128.9435;
		Title 'BPG Heteroskedasticity check Model 4';
	run;


*Ramsey Reset;
proc autoreg data = five;
		Model4: Model BUS12r = logSP500 SP_U SP500_PCH SPudSP_ch UNRATE UNRATE_UD UNRATE_PCH UNud_UNch 
		CPI PEXPr PAGO5r PCRYr EDUC_college MARRYd SEXd PEXP5r INEXQ2 INEXQ1r logINCOME logINVAMT AGE NUMKID PINC2 PSTK
		logSP_SPU UNR_SPU UNRch_SPU PEXP5_SPU INEX_SPU PAGO_SPU PCRY_SPU AGE_SPU/reset;
		Title1 'Model 4 - Model 2 With extra interaction terms based on chow test';
	run; 

ods tagsets.excelxp close;


