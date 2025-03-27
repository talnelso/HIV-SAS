/* 
   This primary program calls in three sub-programs. The third sub-program is based on Marion County
   Public Health Department's (Indianapolis) in-house RISE database of Ryan White (RW) Part A/MAI
   clients. You’ll have to adapt the code to use your own RW Db if you want to compare viral load
   between RW clients and non-clients. If not, revise the code to exclude it.
*/

%LET RequestID=DR3495;
%LET FolderName=&RequestID. Viral Load Disparity Abstract;
%LET ProgramNam=&FolderName.;
%LET DataSource=eHARS, RISE, CDC;
** DataSource goes into the output TITLEs, if you use the example below. See S:\EPI\SAS Programs\Common Data Sources for MCHD Epi analyses.txt ;
** Created by Tammie Nelson on 2018-01-26;
** Validated by _name_ on 2015-0x-xx;
** Purpose: To identify disparities in viral load among people living with HIV in the TGA.;
** Production schedule: (if program is to be run on a regular basis);
** Limitations and Wa-rnings;
** Program derived from "S\EPI\Data Requests\DR3254 RWSP Viral Load Protocol for Providers\DR3254 RWSP Viral Load Protocol for Providers.sas";
** Program Flow Description (high level review of the steps of the program);
**  1) Select years of viral load analysis ;
**  2) Assign macro variables ;
**  3) Create eHARS data sets ;
**  4) Sort and merge incidence data sets ;
**  5) Sort and merge estimated prevalence data sets ;
**  6) Create RISE data sets ;
**  7) Sort eHARS and RISE data sets pre-merge ;
**  8) Merge eHARS and RISE data sets by ehars_uid_rwg ;
**  9) Sort and merge data sets by UID and year, retaining only those vars necessary for VL protocol analyses ;
** 10) Calculate Log10 of most recent individual viral load during &Year. ;
** 11) Generate community geometric mean viral loads with 95% CI for variables known among all PLWH/A in the TGA ;
** 12) Generate community geometric mean viral loads with 95% CI for variables known among only RWSP clients in the TGA ;
** 13) Generate descriptive statistics of community arithmetic mean viral load with 95% CI for variables known among all PLWH/A in the TGA ;
** 14) Retain only needed variables and set LCL to 0 if it is a negative figure ;
** 15) Combine geometric and arithmetic mean data sets for reporting ;
** 16) Create indicator variable for suppressed vs. non-suppressed viral load ;
** 17) Create output files ;
** 18) Clean up. ;
** Remember to include data like this, somewhere on the output: ;
** The standard reference, in the form: "Created ddMMMyyyy, Marion County Health Department, (MCHD reference info). Source: Data source.";
** For example: ;
**TITLE5 HEIGHT=0.7 j=l "Created &SYSDATE9. &SYSTIME., Epidemiology &RequestID.. Source: &DataSource.";
**TITLE6 HEIGHT=0.7 j=l "Prepared by Joe Gibson, Epidemiology Dept., Marion County Public Health Department, Epidemiology@MarionHealth.org" ;
** These next sections (INPUTS, TEMP..., and OUTPUTS) will help anyone who edits or borrows from this program in the future. ;
**   INPUTS shows the dependencies this program. ;
**   TEMPORARY CREATED indicate various names that are used, so someone editting the program will know not to use one of those names again. ;
**   OUTPUTS shows the outputs, and indicates names that should not be re-used. ;
** INPUTS: (Requires the following libnames, datasets, Macro variables, called programs and macros, ...) ;
**   NONE;
** TEMPORARY CREATED data sets, files, macros, variables, etc.: ;
**   LibNames:  ;
**   SAS Data sets: ;
**   FileNames: ;
**   Macro variables: RequestID ProgramNam DataSource LogLstDir ThisRunsDate ThisRunsDateTime HTMLGPth PlotName ;
**   Macros: ;
**   ASCII files: ;
**   Tasks (SYSTASK): ;
** OUTPUTS: (datasets, files, macro variables, ...);
**   e-mail messages routed to the FILENAMEs:  ;

** Specify Log & Lst output directory (in which to store LOG & Output (.TXT) files);
%LET LogLstDir=S:\EPI\Data Requests\&FolderName.\; ** include the final "\";

** Create macro variables that store the current date and the current date and time. ;
%LET ThisRunsDate=%SYSFUNC(Date(),YYMMDD10.); ** Format: yyyy-mm-dd ;
%LET ThisRunsDateTime=%SYSFUNC(TRANSLATE(%SYSFUNC(Date(),YYMMDD10.) %SYSFUNC(Time(),TOD8.),.,:)); ** Format: yyyy-mm-dd HH.MM.SS (e.g. 2012-03-15 16.24.43). ;
** Save output to a file (rather than displaying it on the screen);

** If you want to append new logs & listings to old versions, rather than overwriting old verions, delete NEW from the end of the next line;
PROC PRINTTO
  LOG  ="&LogLstDir.&ProgramNam..log"
  PRINT="&LogLstDir.&ProgramNam..txt"
  NEW
; ** remove NEW append rather than overwrite the old log and lst;
** For programs that are periodically re-run, add the run-date to each LOG and PRINT file, like this: LOG="&LogLstDir.&ProgramNam._&ThisRunsDate..log" ;
RUN;

** Record date & time of run in output & log file, and in macro variable;
DATA _NULL_;
  FILE PRINT;
  PUT   "******************************************************************"
      / "********************* Run at &ThisRunsDateTime. *********************"
      / "******************************************************************";
  FILE LOG;
  PUT   "******************************************************************"
      / "********************* Run at &ThisRunsDateTime. *********************"
      / "******************************************************************";
RUN;

** 1) Select years of viral load analysis *******************
************************************************************;

%LET Year_1 = 2014;
%LET Year_2 = 2015;
%LET Year_3 = 2016;
%LET Year_4 = 2017;
%LET Year_5 = 2018;

** 2) Assign macro variables *******************
***********************************************;

%LET TheDate=%SYSFUNC(date(),YYMMDDN8.);
%LET ThisRunsDate=%SYSFUNC(Date(),YYMMDD10.); ** Format: yyyy-mm-dd ;

****	5. Define formats ;

PROC FORMAT ;
	VALUE USCrace
		1 = "White"
		2 = "Black"
		3 = "Hispanic/Latino"
		4 = "Asian/Pacific Islander"
		. = "Unk/Miss" 
	   Other = "Other" ;
	VALUE sextgcnd
		1    = 'Female'
		2    = 'Male'
		3    = 'Transgender'
       Other = 'Unk/Miss' ;
	VALUE age
		low-14	= "<15"
		15-19	= "15-19"
		20-24	= "20-24"
		25-34   = "25-34"
		35-44	= "35-44"
		45-54	= "45-54"
		55-64   = "55-64"
		65-high = "65+"
		Other   = "Unk/Miss" ;
	VALUE risk
		1 = "MSM"
   		2 = "IDU"
		3 = "Heterosexual"
		4 = "MSM and IDU"
		5 = "Perinatal"
		6 = "Other"
		7 = "Not Identified/Reported" ;
	VALUE YN
		0    = "No"
		1    = "Yes"
	   Other = "Unk/Miss" ;
RUN ;

** 3) Create eHARS data sets *******************
***********************************************;

** Create year one dataset;
%LET Year = &Year_1.;
%INCLUDE "&LogLstDir.&RequestID. SepYr_Sub-Prog.sas";
** Creates dataset &RequestID.Prev&Year_1.;

** Create year two dataset;
%LET Year = &Year_2.;
%INCLUDE "&LogLstDir.&RequestID. SepYr_Sub-Prog.sas";
** Creates dataset &RequestID.Prev&Year_2.;

** Create year three dataset;
%LET Year = &Year_3.;
%INCLUDE "&LogLstDir.&RequestID. SepYr_Sub-Prog.sas";
** Creates dataset &RequestID.Prev&Year_3.;

** Create year four dataset;
%LET Year = &Year_4.;
%INCLUDE "&LogLstDir.&RequestID. SepYr_Sub-Prog.sas";
** Creates dataset &RequestID.Prev&Year_4.;

** Create year five dataset;
%LET Year = &Year_5.;
%INCLUDE "&LogLstDir.&RequestID. SepYr_Sub-Prog.sas";
** Creates dataset &RequestID.Prev&Year_5.;

** 4) Sort and merge incidence data sets *************
*****************************************************;

DATA &RequestID.FinIncDat (KEEP=Year hiv_incidence linktocare) ;
	SET &RequestID.Yr&Year_1.Inc
		&RequestID.Yr&Year_2.Inc
		&RequestID.Yr&Year_3.Inc
		&RequestID.Yr&Year_4.Inc
		&RequestID.Yr&Year_5.Inc
		;
	WHERE hiv_incidence = 1 ;
RUN ;

** 5) Sort and merge estimated prevalence data sets *************
****************************************************************;

DATA &RequestID.FinEstUnDxDat (KEEP=Year NoObs EstUnDx TotPrev) ;
	SET &RequestID.Yr&Year_1.EstUnDx
		&RequestID.Yr&Year_2.EstUnDx
		&RequestID.Yr&Year_3.EstUnDx
		&RequestID.Yr&Year_4.EstUnDx
		&RequestID.Yr&Year_5.EstUnDx
		;
RUN ;

** 6) Create RISE data sets *******************
**********************************************;

** Create year one dataset;
%LET Year = &Year_1.;
%INCLUDE "&LogLstDir.&RequestID. RISE_sub-prog.sas";
** Creates dataset &RequestID.RWGCli&Year_1.;

** Create year two dataset;
%LET Year = &Year_2.;
%INCLUDE "&LogLstDir.&RequestID. RISE_sub-prog.sas";
** Creates dataset &RequestID.RWGCli&Year_2.;

** Create year three dataset;
%LET Year = &Year_3.;
%INCLUDE "&LogLstDir.&RequestID. RISE_sub-prog.sas";
** Creates dataset &RequestID.RWGCli&Year_3.;

** Create year four dataset;
%LET Year = &Year_4.;
%INCLUDE "&LogLstDir.&RequestID. RISE_sub-prog.sas";
** Creates dataset &RequestID.RWGCli&Year_4.;

** Create year five dataset;
%LET Year = &Year_5.;
%INCLUDE "&LogLstDir.&RequestID. RISE_sub-prog.sas";
** Creates dataset &RequestID.RWGCli&Year_5.;

** 7) Sort eHARS and RISE data sets pre-merge *************
**********************************************************;

PROC SORT DATA=&RequestID.Prev&Year_1.  ; BY ehars_uid_rwg; RUN;
PROC SORT DATA=&RequestID.RWGCli&Year_1.; BY ehars_uid_rwg; RUN;

PROC SORT DATA=&RequestID.Prev&Year_2.  ; BY ehars_uid_rwg; RUN; 
PROC SORT DATA=&RequestID.RWGCli&Year_2.; BY ehars_uid_rwg; RUN; 

PROC SORT DATA=&RequestID.Prev&Year_3.  ; BY ehars_uid_rwg; RUN; 
PROC SORT DATA=&RequestID.RWGCli&Year_3.; BY ehars_uid_rwg; RUN; 

PROC SORT DATA=&RequestID.Prev&Year_4.  ; BY ehars_uid_rwg; RUN; 
PROC SORT DATA=&RequestID.RWGCli&Year_4.; BY ehars_uid_rwg; RUN; 

PROC SORT DATA=&RequestID.Prev&Year_5.  ; BY ehars_uid_rwg; RUN;
PROC SORT DATA=&RequestID.RWGCli&Year_5.; BY ehars_uid_rwg; RUN;

** 8) Merge eHARS and RISE data sets by ehars_uid_rwg *****************************
**********************************************************************************;

** The merges include all PLWH/A found in eHARS which excludes some historic RWG clients who
   have dropped off our eHARS data sets due to moving out of the TGA and not having received
   a diagnosis within the TGA. This is a known issue and the HIV Epi is working to correct
   the data set. Note that this should not be much of an issue for current/past one year
   reporting, but will likely drop several clients when reporting historically. ;

DATA &RequestID.Merge&Year_1. ;
	MERGE &RequestID.Prev&Year_1.   (IN=In_E)
          &RequestID.RWGCli&Year_1. (IN=In_R)
	;
	BY ehars_uid_rwg ;
	IF In_E ; ** Keep only those records found in eHARS ;
	IF In_E AND In_R THEN RWGclient = 1 ;
                     ELSE RWGclient = 0 ; ** Creates Y/N indicator variable for RWG enrollment ;
	LABEL RWGclient = "Enrolled in RWSP" ;
	FORMAT RWGclient YN. ;
RUN ;

DATA &RequestID.Merge&Year_2. ;
	MERGE &RequestID.Prev&Year_2.   (IN=In_E)
          &RequestID.RWGCli&Year_2. (IN=In_R)
	;
	BY ehars_uid_rwg ;
	IF In_E ; ** Keep only those records found in eHARS ;
	IF In_E AND In_R THEN RWGclient = 1 ;
                     ELSE RWGclient = 0 ; ** Creates Y/N indicator variable for RWG enrollment ;
	LABEL RWGclient = "Enrolled in RWSP" ;
	FORMAT RWGclient YN. ;
RUN ;

DATA &RequestID.Merge&Year_3. ;
	MERGE &RequestID.Prev&Year_3.   (IN=In_E)
          &RequestID.RWGCli&Year_3. (IN=In_R)
	;
	BY ehars_uid_rwg ;
	IF In_E ; ** Keep only those records found in eHARS ;
	IF In_E AND In_R THEN RWGclient = 1 ;
                     ELSE RWGclient = 0 ; ** Creates Y/N indicator variable for RWG enrollment ;
	LABEL RWGclient = "Enrolled in RWSP" ;
	FORMAT RWGclient YN. ;
RUN ;

DATA &RequestID.Merge&Year_4. ;
	MERGE &RequestID.Prev&Year_4.   (IN=In_E)
          &RequestID.RWGCli&Year_4. (IN=In_R)
	;
	BY ehars_uid_rwg ;
	IF In_E ; ** Keep only those records found in eHARS ;
	IF In_E AND In_R THEN RWGclient = 1 ;
                     ELSE RWGclient = 0 ; ** Creates Y/N indicator variable for RWG enrollment ;
	LABEL RWGclient = "Enrolled in RWSP" ;
	FORMAT RWGclient YN. ;
RUN ;

DATA &RequestID.Merge&Year_5. ;
	MERGE &RequestID.Prev&Year_5.   (IN=In_E)
          &RequestID.RWGCli&Year_5. (IN=In_R)
	;
	BY ehars_uid_rwg ;
	IF In_E ; ** Keep only those records found in eHARS ;
	IF In_E AND In_R THEN RWGclient = 1 ;
                     ELSE RWGclient = 0 ; ** Creates Y/N indicator variable for RWG enrollment ;
	LABEL RWGclient = "Enrolled in RWSP" ;
	FORMAT RWGclient YN. ;
RUN ;

** 9) Sort and merge data sets by UID and year, retaining only those vars necessary for VL protocol analyses ***
***************************************************************************************************************;

PROC SORT DATA=&RequestID.Merge&Year_1.; BY ehars_uid_rwg year; RUN;
PROC SORT DATA=&RequestID.Merge&Year_2.; BY ehars_uid_rwg year; RUN;
PROC SORT DATA=&RequestID.Merge&Year_3.; BY ehars_uid_rwg year; RUN;
PROC SORT DATA=&RequestID.Merge&Year_4.; BY ehars_uid_rwg year; RUN;
PROC SORT DATA=&RequestID.Merge&Year_5.; BY ehars_uid_rwg year; RUN;

DATA &RequestID.Combined (KEEP=ehars_uid_rwg rwg_ClientNumber year eHARS_calc_VLend RWGclient SrvInYr MedVstGap
                                        Retained cur_county_name2 cur_zip_cd race_rec birth_sex sextgcnd cur_age_num
                                        risk rwg_FacilityID rwg_PctPoverty linktocare prevdx_status
                                        vl_first_det_valueN cd4_vl_first_hiv_dtdt) ;
	MERGE &RequestID.Merge&Year_1.
          &RequestID.Merge&Year_2.
		  &RequestID.Merge&Year_3.
		  &RequestID.Merge&Year_4.
		  &RequestID.Merge&Year_5.
	;
	BY ehars_uid_rwg year ;
RUN ;

***********************************************************************************
The CDC recommends the following for statistical analysis of community viral load:

"The rationale for this logarithmic transformation is that it helps to
normalize the distribution of viral load values and reduces the influence of
outlying measurements for persons having extreme viremia (due to acute infection,
advanced HIV disease, concurrent sexually transmitted infections, or random
variability). Since the interpretation of viral load measurements is often more
intuitive on a linear scale, we recommend calculation of geometric mean (GM) for
viral load. The GM is thus calculated through log transformation by averaging
the log transformed values and transforming the average back to the original
(linear) scale. The base used for the log transformation has no effect on the
final GM estimate. However, using log base 10 has an advantage by its
relationship to the value on the original scale - for example, a value of 2 on
the log10 scale is 100 on the original scale, 3 corresponding to 1000, 4
corresponding 10000, and so forth."

Centers for Disease Control and Prevention. (2011). Guidance on community viral
load: A family of measures, definitions, and method for calculation. Retrieved
from http://www.ct.gov/dph/lib/dph/aids_and_chronic/surveillance/statewide/
community_viralload_guidance.pdf

** CD4/VL values chosen because, "Data suggest that during a period of up to 2 years,
   patients with viral rebound to levels below 10,000 copies/mL may be able to
   maintain their CD4 counts despite a failing regimen."
   - http://www.hivguidelines.org/wp-content/uploads/viral-load-report.pdf ;

** Geometric mean is often used to evaluate data covering several orders of magnitude ;

***********************************************************************************;

** 10) Calculate Log10 of most recent individual viral load during &Year. **
**************************************************************************;

DATA &RequestID.FinDat ;
	SET &RequestID.Combined ;
	WHERE eHARS_calc_VLend ^= . ; ** Where last VL during period not missing ;
	IF eHARS_calc_VLend > 0 THEN LogVLend = LOG10(eHARS_calc_VLend) ;
	ELSE PUT "WAR""NING: Reported eHARS_calc_VLend should not be 0." ehars_uid_rwg= ;
	FORMAT eHARS_calc_VLend ; ** Use unformatted viral load results ;
RUN ;

** 11) Generate community geometric mean viral loads with 95% CI for variables known among all PLWH/A in the TGA **
*****************************************************************************************************************;

PROC MEANS DATA=&RequestID.FinDat NOPRINT ;
	CLASS year race_rec cur_age_num sextgcnd Retained RWGclient / missing ;
	VAR LogVLend ;
	OUTPUT OUT=_&RequestID.LogMeanYear         (WHERE=(_TYPE_ = INPUT("100000",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
	OUTPUT OUT=_&RequestID.LogMeanRace         (WHERE=(_TYPE_ = INPUT("110000",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
	OUTPUT OUT=_&RequestID.LogMeanRaceAge      (WHERE=(_TYPE_ = INPUT("111000",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
  	OUTPUT OUT=_&RequestID.LogMeanRaceGender   (WHERE=(_TYPE_ = INPUT("110100",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
  	OUTPUT OUT=_&RequestID.LogMeanRetained     (WHERE=(_TYPE_ = INPUT("100010",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
 	OUTPUT OUT=_&RequestID.LogMeanRaceRetained (WHERE=(_TYPE_ = INPUT("110010",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
	OUTPUT OUT=_&RequestID.LogMeanRWGStat      (WHERE=(_TYPE_ = INPUT("100001",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
	OUTPUT OUT=_&RequestID.LogMeanRaceRWGStat  (WHERE=(_TYPE_ = INPUT("110001",BINARY9.)) DROP=_FREQ_) N=NoObs MEAN=LogMean LCLM=LCLM UCLM=UCLM ;
RUN ;

** 12) Generate community geometric mean viral loads with 95% CI for variables known among only RWSP clients in the TGA **
*************************************************************************************************************************;

DATA &RequestID.LogMeanYear         (KEEP=Year NoObs LogMean LCLM UCLM);                      SET _&RequestID.LogMeanYear;         RUN;
DATA &RequestID.LogMeanRace         (KEEP=Year race_rec NoObs LogMean LCLM UCLM);             SET _&RequestID.LogMeanRace;         RUN;
DATA &RequestID.LogMeanRaceAge      (KEEP=Year race_rec cur_age_num NoObs LogMean LCLM UCLM); SET _&RequestID.LogMeanRaceAge;      RUN;
DATA &RequestID.LogMeanRaceGender   (KEEP=Year race_rec sextgcnd NoObs LogMean LCLM UCLM);    SET _&RequestID.LogMeanRaceGender;   RUN;
DATA &RequestID.LogMeanRetained     (KEEP=Year Retained NoObs LogMean LCLM UCLM);             SET _&RequestID.LogMeanRetained;     RUN;
DATA &RequestID.LogMeanRaceRetained (KEEP=Year race_rec Retained NoObs LogMean LCLM UCLM);    SET _&RequestID.LogMeanRaceRetained; RUN;
DATA &RequestID.LogMeanRWGStat      (KEEP=Year RWGclient NoObs LogMean LCLM UCLM);            SET _&RequestID.LogMeanRWGStat;      RUN;
DATA &RequestID.LogMeanRaceRWGStat  (KEEP=Year race_rec RWGclient NoObs LogMean LCLM UCLM);   SET _&RequestID.LogMeanRaceRWGStat;  RUN;

DATA &RequestID.GMYear (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanYear ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRace (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRace ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRaceAge (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRaceAge ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRaceGender (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRaceGender ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRetained (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRetained ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRaceRetained (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRaceRetained ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRWGStat (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRWGStat ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

DATA &RequestID.GMRaceRWGStat (DROP=LogMean LCLM UCLM) ;
	SET &RequestID.LogMeanRaceRWGStat ;
	IF LogMean ^=.                THEN GMean = 10**LogMean ;
	IF LCLM ^=. AND LCLM => 0     THEN LCLGM = 10**LCLM ;
	ELSE IF LCLM ^=. AND LCLM < 0 THEN LCLGM = 0 ;
	IF UCLM ^=. AND UCLM => 0     THEN UCLGM = 10**UCLM ;
	ELSE IF UCLM ^=. AND UCLM < 0 THEN UCLGM = 0 ;
RUN ;

** The above procedures created the following data sets of geometric mean viral loads and 95% CIs for:
		All PLWH/A:   &RequestID.GMYear &RequestID.GMRace &RequestID.GMRaceAge &RequestID.GMRaceGender 
                      &RequestID.GMRetained &RequestID.GMRaceRetained &RequestID.GMRWGStat &RequestID.GMRaceRWGStat ;

** 13) Generate descriptive statistics of community arithmetic mean viral load with 95% CI for variables known among all PLWH/A in the TGA **
********************************************************************************************************************************************;

PROC MEANS DATA=&RequestID.FinDat NOPRINT ;
	CLASS year race_rec cur_age_num sextgcnd Retained RWGclient / missing;
	VAR eHARS_calc_VLend ;
	OUTPUT OUT=_&RequestID.MeanYear         (WHERE=(_TYPE_ = INPUT("100000",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
	OUTPUT OUT=_&RequestID.MeanRace         (WHERE=(_TYPE_ = INPUT("110000",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
    OUTPUT OUT=_&RequestID.MeanRaceAge      (WHERE=(_TYPE_ = INPUT("111000",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
  	OUTPUT OUT=_&RequestID.MeanRaceGender   (WHERE=(_TYPE_ = INPUT("110100",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
  	OUTPUT OUT=_&RequestID.MeanRetained     (WHERE=(_TYPE_ = INPUT("100010",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
  	OUTPUT OUT=_&RequestID.MeanRaceRetained (WHERE=(_TYPE_ = INPUT("110010",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
	OUTPUT OUT=_&RequestID.MeanRWGStat      (WHERE=(_TYPE_ = INPUT("100001",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
	OUTPUT OUT=_&RequestID.MeanRaceRWGStat  (WHERE=(_TYPE_ = INPUT("110001",BINARY.)) DROP=_FREQ_) N=NoObs MEAN=AMEAN LCLM=LCLAM UCLM=UCLAM ;
RUN ;

** 14) Retain only needed variables and set LCL to 0 if it is a negative figure **
*********************************************************************************;

DATA &RequestID.AMYear (KEEP=Year NoObs AMean LCLAM UCLAM) ;
	SET _&RequestID.MeanYear ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRace (KEEP=Year NoObs AMean LCLAM UCLAM race_rec) ;
	SET _&RequestID.MeanRace ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRaceAge (KEEP=Year NoObs AMean LCLAM UCLAM race_rec cur_age_num) ;
	SET _&RequestID.MeanRaceAge ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRaceGender (KEEP=Year NoObs AMean LCLAM UCLAM race_rec sextgcnd) ;
	SET _&RequestID.MeanRaceGender ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRetained (KEEP=Year NoObs AMean LCLAM UCLAM Retained) ;
	SET _&RequestID.MeanRetained ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRaceRetained (KEEP=Year NoObs AMean LCLAM UCLAM race_rec Retained) ;
	SET _&RequestID.MeanRaceRetained ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRWGStat (KEEP=Year NoObs AMean LCLAM UCLAM RWGclient) ;
	SET _&RequestID.MeanRWGStat ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

DATA &RequestID.AMRaceRWGStat (KEEP=Year NoObs AMean LCLAM UCLAM race_rec RWGclient) ;
	SET _&RequestID.MeanRaceRWGStat ;
	IF LCLAM ^=. AND LCLAM => 0     THEN LCLAM = LCLAM ;
	ELSE IF LCLAM ^=. AND LCLAM < 0 THEN LCLAM = 0 ;
	IF UCLAM ^=. AND UCLAM => 0     THEN UCLAM = UCLAM ;
	ELSE IF UCLAM ^=. AND UCLAM < 0 THEN UCLAM = 0 ;
RUN;

** The above procedures created the following data sets of geometric mean viral loads and 95% CIs for:
		All PLWH/A:   &RequestID.AMYear &RequestID.AMRace &RequestID.AMRaceAge &RequestID.AMRaceGender
                      &RequestID.AMRetained &RequestID.AMRaceRetained &RequestID.AMRWGStat &RequestID.AMRaceRWGStat ;

** 15) Combine geometric and arithmetic mean data sets for reporting **
**********************************************************************;

DATA &RequestID.MeansYear;         MERGE &RequestID.GMYear &RequestID.AMYear;                 BY Year; RUN;
DATA &RequestID.MeansRace;         MERGE &RequestID.GMRace &RequestID.AMRace;                 BY Year; RUN;
DATA &RequestID.MeansRaceAge;      MERGE &RequestID.GMRaceAge &RequestID.AMRaceAge;           BY Year; RUN;
DATA &RequestID.MeansRaceGender;   MERGE &RequestID.GMRaceGender &RequestID.AMRaceGender;     BY Year; RUN;
DATA &RequestID.MeansRetained;     MERGE &RequestID.GMRetained &RequestID.AMRetained;         BY Year; RUN;
DATA &RequestID.MeansRaceRetained; MERGE &RequestID.GMRaceRetained &RequestID.AMRaceRetained; BY Year; RUN;
DATA &RequestID.MeansRWGStat;      MERGE &RequestID.GMRWGStat &RequestID.AMRWGStat;           BY Year; RUN;
DATA &RequestID.MeansRaceRWGStat;  MERGE &RequestID.GMRaceRWGStat &RequestID.AMRaceRWGStat;   BY Year; RUN;

** 16) Create indicator variable for suppressed vs. non-suppressed viral load **
*******************************************************************************;

DATA &RequestID.FinDat ;
	SET &RequestID.FinDat ;
	IF (0 < eHARS_calc_VLend < 200)  THEN suppVL = 1 ;
	ELSE IF (199 < eHARS_calc_VLend) THEN suppVL = 0 ;
	FORMAT suppVL yn. ;
	LABEL suppVL = "Suppressed Viral Load (<200 c/mL)" ;
RUN ;

** Create binary suppression var for logistic regression ;
DATA &RequestID.LogReg ;
	SET &RequestID.FinDat ;
	IF eHARS_calc_VLend ^= .
	AND (0 <= eHARS_calc_VLend <=199) THEN suppressed = 1 ;
	ELSE IF eHARS_calc_VLend ^= .
	AND (eHARS_calc_VLend > 199) THEN suppressed = 0 ;
RUN ;

** The above procedures created the following data sets of geometric and arithmetic mean viral loads with 95% CIs for:
		All PLWH/A:   &RequestID.MeansYear &RequestID.MeansRace &RequestID.MeansRaceAge &RequestID.MeansRaceGender
                      &RequestID.MeansRetained &RequestID.MeansRaceRetained &RequestID.MeansRWGStat &RequestID.MeansRaceRWGStat 
	These data sets can be used in conjunction with &RequestID.FinDat (by year and RWGclient (client status) for categorical eHARS_calc_VLend status ;

** 17) Create output files **
****************************;

ODS NORESULTS;     ** Keep ODS results window from appearing.;
ODS LISTING CLOSE; ** Turn off the default output (to screen or Printto file).;

ODS TAGSETS.EXCELXP
	file="&LogLstDir.&RequestID._VL Analysis Tables_&ThisRunsDate..xls"
    STYLE=meadow
    OPTIONS (Orientation='landscape'
        FitToPage='yes'
        Pages_FitWidth='1'
        borderwidth = '1'
        Pages_FitHeight='100'
        Contents_Workbook='yes'
        sheet_interval='proc'
        embedded_footnotes='yes'
        embedded_titles='yes'
        index='yes');

** Output detailed tables for VL protocol report ;

** Output individuals never linked to care, and not receiving care during 2017, by race/ethnicity ;
PROC FREQ DATA=&RequestID.Combined ;
	WHERE year = &Year_5. AND cd4_vl_first_hiv_dtdt =. ;
	TABLES race_rec / MISSING ;
	TITLE1 "&ThisRunsDate." ;
	TITLE2 "Viral Load Disparity Abstract" ;
	TITLE3 "Marion County Public Health Department, Epidemiology Request &RequestID." ;
	TITLE4 "Source: &DataSource." ;
	TITLE5 "Prepared by: Tammie L. Nelson, TNelson@MarionHealth.org" ;
	TITLE6 ;
	TITLE7 ;
	TITLE8 "Indiana TGA Residents Living with HIV/AIDS and Never Linked to Care (No Record of CD4 or Viral Load Test) as of 31DEC&Year_5., by Race/Ethnicity" ;
RUN ;

PROC FREQ DATA=&RequestID.Combined ;
	WHERE year = &Year_5. AND SrvInYr = 0 ;
	TABLES race_rec / MISSING ;
	TITLE8 "Indiana TGA Residents Living with HIV/AIDS with No Record of CD4 or Viral Load Test during &Year_5., by Race/Ethnicity" ;
RUN ;

** Overall viral load suppression in the Indianapolis TGA for report intro. ;
PROC FREQ DATA=&RequestID.FinDat ;
	TABLES year*suppVL / MISSING NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Overall HIV Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test: &Year_1.-&Year_5." ;
RUN ;

PROC PRINT DATA=&RequestID.MeansYear NOOBS ;
	TITLE "Overall Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test: &Year_1.-&Year_5." ;
RUN ;

** Viral load suppression by race/ethnicity in the Indianapolis TGA, regardless of retention in care status ;

PROC SORT DATA=&RequestID.FinDat; BY year; RUN;
PROC FREQ DATA=&RequestID.FinDat ;
	BY year ;
	TABLES race_rec*suppVL / CHISQ NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Chi-Square Evaluation of Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity: &Year_1.-&Year_5." ;
RUN ;  ** Report results as "Chi-square with four degrees of freedom = 41.86, p < .0001)" ;

PROC SORT DATA=&RequestID.FinDat; BY prevdx_status; RUN;
PROC FREQ DATA=&RequestID.FinDat ;
	WHERE year = &Year_5. ;
	TABLES prevdx_status*race_rec*suppVL / CHISQ NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Chi-Square Evaluation of Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and HIV/AIDS Status: &Year_5." ;
RUN ;  ** Report results as "Chi-square with four degrees of freedom = 41.86, p < .0001)" ;

PROC SORT DATA=&RequestID.MeansRace; BY year; RUN;
PROC PRINT DATA=&RequestID.MeansRace NOOBS ;
	BY year ;
	TITLE "Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity: &Year_1.-&Year_5." ;
RUN ;

** Viral load suppression by race and current age in the Indianapolis TGA ;

** Histogram and QQ plot of current age ;
PROC UNIVARIATE DATA=&RequestID.LogReg NOPRINT ;
	WHERE year = &Year_5. ;
	HISTOGRAM cur_age_num /
		MIDPOINTS=0 to 100 by 5
		NORMAL (mu=est sigma=est color=red w=2.5);
		INSET mean std max min / pos=NE;
		TITLE "Distribution of Current Age (Years)" ;
	FORMAT cur_age_num ;
RUN ;

ODS GRAPHICS ON ;
PROC TTEST DATA=&RequestID.LogReg h0=6.0 plotS=qq ;
	WHERE year = &Year_5. ;
     VAR cur_age_num ;
	 FORMAT cur_age_num ;
	 TITLE "Confidence Level and QQ Plot of Current Age" ;
RUN ;
ODS GRAPHICS OFF ;

PROC FREQ DATA=&RequestID.FinDat ;
	WHERE cur_age_num ^= . ;
	BY year ;
	TABLES race_rec*cur_age_num*suppVL / NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race and Current Age: &Year_1.-&Year_5." ;
RUN ;

PROC SORT DATA=&RequestID.MeansRaceAge; BY year; RUN;
PROC PRINT DATA=&RequestID.MeansRaceAge NOOBS ;
	BY year ;
	TITLE "Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicty and Current Age: &Year_1.-&Year_5." ;
RUN ;

** Viral load suppression by race/ethnicity and gender in the Indianapolis TGA, regardless of RWSP client status ;

PROC FREQ DATA=&RequestID.FinDat ;
	BY year ;
	TABLES race_rec*sextgcnd*suppVL / NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Gender: &Year_1.-&Year_5." ;
RUN ;

PROC SORT DATA=&RequestID.MeansRaceGender; BY year; RUN;
PROC PRINT DATA=&RequestID.MeansRaceGender NOOBS ;
	BY year ;
	TITLE "Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Gender: &Year_1.-&Year_5." ;
RUN ;

** Viral load suppression by race/ethnicity and retention in care status in the Indianapolis TGA ;

PROC FREQ DATA=&RequestID.FinDat ;
	BY year ;
	TABLES race_rec*retained*suppVL / NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Retention in Care Status: &Year_1.-&Year_5." ;
RUN ; 

PROC FREQ DATA=&RequestID.FinDat ;
	WHERE year = &Year_5. ;
	TABLES prevdx_status*race_rec*retained*suppVL / NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity, HIV/AIDS Status, and Retention in Care Status: &Year_5." ;
RUN ; 

PROC SORT DATA=&RequestID.MeansRaceRetained; BY year; RUN;
PROC PRINT DATA=&RequestID.MeansRaceRetained NOOBS ;
	BY year ;
	TITLE "Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Retention in Care Status: &Year_1.-&Year_5." ;
RUN ;

** Viral load suppression by race/ethnicity and Ryan White Part A/MAI/C enrollment status in care status in the Indianapolis TGA, regardless of RWSP client status ;

PROC FREQ DATA=&RequestID.FinDat ;
	BY year ;
	TABLES race_rec*RWGclient*suppVL / CHISQ NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Ryan White Part A/MAI/C Enrollment Status: &Year_1.-&Year_5." ;
RUN ;

PROC FREQ DATA=&RequestID.FinDat ;
	WHERE year = &Year_5. ;
	TABLES prevdx_status*race_rec*RWGclient*suppVL / CHISQ NOROW NOCOL NOPERCENT NOCUM ;
	TITLE "Viral Load Suppression among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity, HIV/AIDS Status, and Ryan White Part A/MAI/C Enrollment Status: &Year_5." ;
RUN ;

PROC SORT DATA=&RequestID.MeansRaceRWGStat; BY year; RUN;
PROC PRINT DATA=&RequestID.MeansRaceRWGStat NOOBS ;
	BY year ;
	TITLE "Geometric and Arithmetic Mean Viral Load among Indianapolis TGA Residents with at Least One Viral Load Test, by Race/Ethnicity and Ryan White Part A/MAI/C Enrollment Status: &Year_1.-&Year_5." ;
RUN ;

** Conduct stepwise logistic regression of all variables on Probility of Current HIV Viral Load Suppression ;

** Histogram and QQ plot of first detectable viral load ;
PROC UNIVARIATE DATA=&RequestID.LogReg NOPRINT ;
	WHERE year = &Year_5. ;
	HISTOGRAM vl_first_det_valueN /
		NORMAL (mu=est sigma=est color=red w=2.5);
		INSET mean std max min / pos=NE;
		TITLE "Distribution of First Detectable Viral Load (c/mL)" ;
	FORMAT vl_first_det_valueN ;
RUN ;

ODS GRAPHICS ON ;
PROC TTEST DATA=&RequestID.LogReg h0=6.0 plotS=qq ;
	WHERE year = &Year_5. ;
     VAR vl_first_det_valueN ;
	 FORMAT vl_first_det_valueN ;
	 TITLE "Confidence Level and QQ Plot of First Detectable Viral Load" ;
RUN ;
ODS GRAPHICS OFF ;

PROC LOGISTIC DATA=&RequestID.LogReg OUTEST=betas COVOUT ;
	WHERE year = &Year_5. ;
	CLASS vl_first_det_valueN race_rec cur_age_num sextgcnd retained RWGclient ;
	MODEL suppressed(EVENT='0') = vl_first_det_valueN race_rec cur_age_num sextgcnd retained RWGclient
		  / SELECTION=stepwise
            SLENTRY=0.3
            SLSTAY=0.35
            DETAILS
            LACKFIT;
	OUTPUT OUT=pred P=phat LOWER=lcl UPPER=ucl PREDPROB=(individual crossvalidate) ;
	TITLE "Stepwise Regression of First HIV Viral Load, Race/Ethnicity, Current Age, Gender, Retention in Care, and RWG Client Status on the Probility of Current HIV Viral Load Suppression" ;
RUN ;
** See https://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_logistic_sect059.htm
   and https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_logistic_sect053.htm
   and https://stats.idre.ucla.edu/sas/seminars/sas-logistic/proc-logistic-and-logistic-regression-models/ ;

** Output exposure category of African Americans found to be most at risk of unsuppressed viral load during &Year_5.
   (young adult (20-34 years of age) males). ;

PROC FREQ DATA=&RequestID.FinDat ;
	WHERE Year = &Year_5. AND eHARS_calc_VLend >199 AND race_rec =2 AND (20<= cur_age_num <=34) AND sextgcnd=2 ;
		TABLES risk*prevdx_status ;
	TITLE "Exposure Category of 20-34 Year Old African American Males with Unsuppressed Viral Load during &Year_5." ;
RUN ;

ODS tagsets.excelxP CLOSE ;
ODS LISTING;
ODS RESULTS ; ** Turn the ODS results window back on. ;

QUIT ;

** 16) Clean up. ;
** Delete the macro variables that were created. ;
%SYMDEL RequestID ProgramNam DataSource LogLstDir HTMLGPth PlotName / NOWARN ;
** Reset the output and log to the default (the screen). ;
PROC PRINTTO;
RUN;

