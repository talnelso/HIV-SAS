** Automated HIV Retention in Care Alerts v3 ;

** Marion County Public Health Department HIV Retention in Care Alerts Program;


*** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * * *  THIS PROGRAM CREATES OUTPUT CONTAINING V-E-R-Y   * * *  
* * * * SENSITIVE INFORMATION - PERSONALLY-IDENTIFIABLE * * * *
 * * *  HEALTH INFORMATION SUBJECT TO HIPAA REGULATIONS.  * * *  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * THIS PROGRAM REQUIRES INPUT BEFORE EACH RUN AT  * * * *
 * * *  LINE 121. IT ALSO REQUIRES REVISIONS BY MCPHD'S  * * *  
* * * * HIV EPI BEFORE THE FIRST RUN AT LINES 97, 130,  * * * *
 * * *  133, and 139. See notes below.					 * * *  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *;

** IMPORTANT: Pre-run instructions for MCPHD;

**	Revisions that MUST be made prior to running on your server the first time;
**		Line 97  - Change LogLstDir to that used by MCPHD's Epi Dept.
**		Line 130 - Change DataFolder to the secure MCPHD folder that contains raw eHARS lab and person datasets.
**		Line 133 - Change DataExport to file path where MCPHD's Epi Dept. stores secure/PHI data.
**		Line 139 - Revise to file path where MCPHD stored the included formats program.;

**  Revision that MUST be made prior to EVERY run;
**		Line 121 - Change run date to the current day. This needs to be revised before each run because I could not
				   find a way of automating it that will work within the macro that runs later in the program. The
				   date needs to be in the form of mmddyyyy within single quotes. The following macro variables
				   outputs it correctly, but the macro program chokes on its output. I don't know why...maybe you
				   can figure it out. If so, then the program can be fully automated and put into production.;
					/* 	%let TodayProgFrmt=%sysfunc(date(),mmddyyn8.); ** Formats date as mmddyyy.; */
					/*	%let DtCharQt=%str(%')&TodayProgFrmt.%str(%'); ** Adds single quotes to date.; */

** Revision that CAN be made to change look-back period;
**		Line 123 - This macro variable can be revised to the look-back period during which people are set to fall out of care.
				   It is currently set to 365 days only because of the large number of people in the list when looking back over
				   a longer period of time. I suggest starting with this list to get used to the process and start linking folks
				   back to care who have only recently become at risk. Once that list starts to thin, you can adjust the look-
				   back period to generate alerts for others. Note that some of those who have not been in care for an extended
				   period might have moved from the TGA and just not received any care, in which instance, no new address is known
				   for entry into eHARS. Some individuals may also be deceased and just not have a date of death in eHARS. The
				   MCPHD Epi can compare the lists generated to Marion County death certificates to clear some of these, but will
				   need to revise this program to do so. It could also be automated...;

** Created by: Constantin Yiannoutsos (IUFSPH) on 2024-03-21;
** Revisions: ** Constantin Yiannoutsos (IUFSPH), 2024-04-22, to go only to the first available previous test.;
			   ** Tammie Nelson (IUSM) revised the program on 2024-05-13 to eliminate errors, add comments/instructions for MCPHD,
				  refine viral load category cutoffs, change lab_test_cd codes to select only quantitative viral loads, add TGA diagnosis
				  variable, create date macros, add additional variables to the alerts to enable contact by RWHSP outreach staff, and
				  add code to output call list to an Excel file for use by RWHSP outreach staff.;

** Data Source: Raw eHARS person and labs data sets;

** Production schedule: Monthly (to be initiated by MCPHD);

** Purpose: To create proactive alerts of PLWH who are at
			risk of falling out of retained in care status.;

** Limitations and Wa-rnings: Will not eliminate individuals
			who have died unless their eHARS DOD is updated.
			Also will not eliminate individuals who moved out
			of the TGA unless they were HIV/AIDS diagnosed in
			the TGA and have received a lab or some other type
			of surveillance activity update in eHARS since
			their move.;

** Definitions: Retained in care (RIC) = 2+ labs 3+ months apart within 12 months.
			Program flags individuals as follows:
				If most recent viral load (VL) was:
					Detectable   (50+ c/mL), flag 10 months from most recent lab date.
					Undetectable (<50 c/mL), flag 11 months from most recent lab date.;

** Program Flow;
**  1) Assign run date and macro variables;
**  2) Call in formats.;
**  3) Bring in eHARS data and combine lab extracts into a single data set.;
**  4) Fix missing day in the sample_dt variable.;
**  5) Create variable identifying detectable vs. non-detectable VL.;
**  6) Sort and de-duplicate labs and demographic data.;
**  7) Create demographic dataset and repair current address and death dates.;
**  8) Exclude deceased and non-TGA records.;
**  9) Retain only most recent current address for each individual.;
** 10) Merge demographic and reverse-sorted lab data for those in the TGA;
** 11) Create and run the RIC flag macro.;
** 12) Sort calllist by sample_dt (oldest to newest) and ehars_uid.;
** 13) Export list of people at risk of falling out of care.;
** 14) Clean up;

** Specify Log & Lst output directory;
%let LogLstDir=E:\Users\Nelson\MCPHD RIC Alert Program\;
%let ProgramNam=MCPHD_RIC_Alert_Prog_v2;
%let TheDate=%sysfunc(date(),yymmdd10.);

proc printto
	log="&LogLstDir.&ProgramNam.&TheDate..log";
run;

** Record date & time of run in output & log file, and in macro variable;
data _null_;
	file print;
	put "******************************************************************"
      / "********************* Run at &sysdate9. &systime. ********************"
      / "******************************************************************";
  file log;
	put "******************************************************************"
      / "********************* Run at &sysdate. &systime. *********************"
      / "******************************************************************";
run;


****  1) Assign run date and macro variables;
*************************************************************************;

%let ManDate='01012023'; ** CHANGE TO TODAY'S DATE AS mmddyyyy.;

%let lookback=365; ** REVISE THIS NUMBER TO LOOK BACK FOR CARE
				      ATTRITION OVER A LONGER OR SHORTER PERIOD
					  OF TIME (i.e., 548 DAYS FOR 18 MONTHS,
					  730 DAYS FOR 24 MONTHS).;

%let Today=%sysfunc(date(),date9.); ** For program output. ;

%let DataFolder=&LogLstDir.eHARS Data 2022\	;
** Revise to MCPHD's file path to raw eHARS extract files ;

%let DataExport=&LogLstDir.;


****  2) Call in formats.;
*************************************************************************;

%INCLUDE "E:\Users\Nelson\MCPHD RIC Alert Program\subprog_eHARS_fmts.sas" ;

proc format;
	value detfmt
		1='Detectable'
		0='Undetectable';
run;


****  3) Bring in eHARS data and combine lab extracts into a single data set.;
*************************************************************************;

options nofmterr;

libname EHARSLBS "&DataFolder.";
** This sets the library to the subdirectory containing all the eHARS,
   person and lab datasets. The nofmterr option is included in case
   there's a missing format. The option allows the program to run if
   a format is missing.;

** Select only lab-specific data sets;
proc sql noprint;
     select
        case when (find(catx('.','EHARSLBS',memname),'LABS')>0)
             then catx('.','EHARSLBS',memname)
             else ''
        end
        into : dsnlabs separated by ' '
        from dictionary.tables
        where libname='EHARSLBS' ;
quit;

** The catx statements simply stitch together the data set by
   putting the libname EHARSLBS (must be in capital letters),
   followed by a ‘.’ and then the name of each of the files that
   have ‘LABS’ in the title. That’s what the find() condition
   accomplishes (i.e., only pulls files where the string ‘LABS’
   can be found (i.e., the index of where the string is in the
   file name is not zero because if it is zero the string is not
   found in the file name). Then it puts this into a string
   separated by a ‘ ‘. This generates the output found in
   Constantin's email, such as the examples below. ;
**
	EHARSLBS.EHARS_LABS_2022_01_04 EHARSLBS.EHARS_LABS_2022_01_18
	EHARSLBS.EHARS_LABS_2022_01_24 EHARSLBS.EHARS_LABS_2022_01_31
	...
	EHARSLBS.EHARS_LABS_2022_10_24 EHARSLBS.EHARS_LABS_2022_11_01
	EHARSLBS.EHARS_LABS_2022_11_07 EHARSLBS.EHARS_LABS_2022_11_14
;


****  4) Fix missing day in the sample_dt variable.;
*************************************************************************;

** The two digits for the day in the date string are taken up by ‘..’
   (two dots). If neither the month nor the day are known, the first
   four digits are taken up by ‘....’. We need to fill these in so that we
   can create viable date fields. The following macro accomplishes this;

** Macro to fix dates;
options mprint;
%macro fixdate(old_dt, new_dt);
 ** Some dates have a '..' or a '....' suffix that needs to befixed;
 ** If the suffix is '....' we take the middle of the year;
 ** If the suffix is '..', we take the middle of the month;
	if find(&old_dt,'....')>0
		then tempdate=strip(compress(&old_dt,'.')||'0615');
		else if find(&old_dt,'..')>0
		then tempdate=strip(compress(&old_dt,'.')||'15');
		else tempdate=&old_dt;
	&new_dt=input(tempdate,yymmdd10.);
%mend fixdate;
** Macro fixdate puts the number 15 in cases where the old date does
   not have the day and June 15 (about mid year) in cases where neither
   the day nor the month are known. The new date is then given a format
   yymmdd10. Now we can create the labs data set.;


****  5) Create variable identifying detectable vs. non-detectable VL.;
*************************************************************************;

data labs;
	format result $20.;
	set &dsnlabs;
	keep document_uid lab_seq result lab_test_cd newdate detectable;
	if lab_test_cd in ('EC-014','EC-015');
	** Fix sample dates;
	%fixdate(sample_dt, newdate);
	** Now we fix the lab results;
	if lab_test_cd = 'EC-014' then detectable = (input(result,8.)>49);
	if lab_test_cd = 'EC-015' then detectable = (input(result,8.)>49);
	format newdate mmddyy10. detectable yn.;
	label detectable = 'Last VL detectable (Y/N)'
		  newdate    = 'Lab date'
		  result     = 'VL result (c/mL)';
run;
** Notice that we retain only quantitative VL results (i.e., lab_test_cd in
   ('EC-014','EC-015')). We consider as “detectable” any test that is
   >49 copies/ml (this can be changed if the definition later changes).
   Also, the field newdate is now the new sample date, fixed.;

/*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
** The following is some code for housekeeping. I also check to see what
   the minimum and maximum dates are in the labs data set. You can use
   this code for quality checks, to look at the min/max dates of the labs
   data you've pulled in, and to see the overall number/percentage of
   detectable vs. non-detectable viral loads in the data you're using.
   You'll need to un-comment-out the code to run it though. ;

proc contents data=labs;
	title 'Contents of Labs Dataset';
run;

proc means data=labs min max noprint;
	var newdate;
	output out=minmax min=mindate max=maxdate;
run;

proc print data=minmax;
	title 'Minimum and Maximum Dates in Dataset';
	format mindate maxdate mmddyy10.;
run;

proc freq data=labs;
	title 'Frequency of Detectable vs. Non-Detectable Viral Loads';
	where lab_test_cd in ('EC-014','EC-015');
	table detectable;
run;
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*/


****  6) Sort and de-duplicate labs and demographic data.;
*************************************************************************;

** Labs;
proc sort data=labs (rename=(newdate=sample_dt)) out=downlabs nodupkey;
	by document_uid descending sample_dt descending lab_seq;
run;

** Demographics;
proc sql noprint;
	select
		case when (find(catx('.','EHARSLBS',memname),'PERSON')>0)
			 then catx('.','EHARSLBS',memname)
			 else ''
		end
		into : dsnperson separated by ' '
		from dictionary.tables
		where libname='EHARSLBS' ; /* libname must be capital letters */
quit ;

/*
** This creates the following string:

%put &dsnperson;

** And assembles the files as in the following example:

1189 %put &dsnperson;
EHARSLBS.EHARS_PERSON_2022_01_04 EHARSLBS.EHARS_PERSON_2022_01_18
EHARSLBS.EHARS_PERSON_2022_01_24 EHARSLBS.EHARS_PERSON_2022_01_31
...
EHARSLBS.EHARS_PERSON_2022_12_05 EHARSLBS.EHARS_PERSON_2022_12_12
EHARSLBS.EHARS_PERSON_2022_12_19 EHARSLBS.EHARS_PERSON_2022_12_27
*/


****  7) Create demographic dataset and repair current address and death dates.;
*************************************************************************;

data demo;
	set &dsnperson (where=(document_uid ne ''));
	** Fix dod's;
	%fixdate(dod,deathdt);
	** Fix current address dates;
	%fixdate(cur_address_dt, curraddressdt);
	keep first_name last_name document_uid deathdt cur_address_type_cd
		 cur_street_address1 cur_street_address2 cur_city_name
		 cur_county_name cur_state_cd cur_zip_cd cur_country_cd
		 curraddressdt ehars_uid cur_phone DxTGA;
	if rsh_state_cd    in ("IN") AND
	   rsh_county_name in ("*MARION", "BOONE CO.", "BROWN CO.", "HAMILTON CO.", "HANCOCK CO.", "HENDRICKS CO.", 
                           "JOHNSON CO.", "MARION CO.", "MORGAN CO.", "PUTNAM CO.", "SHELBY CO.")
		then rsh_TGA=1; ** HIV Dx in the TGA ;
		else rsh_TGA=0;
	if rsa_state_cd    in ("IN") AND
	   rsa_county_name in ("*MARION", "BOONE CO.", "BROWN CO.", "HAMILTON CO.", "HANCOCK CO.", "HENDRICKS CO.", 
                           "JOHNSON CO.", "MARION CO.", "MORGAN CO.", "PUTNAM CO.", "SHELBY CO.")
		then rsa_TGA=1; ** AIDS Dx in the TGA ; 
    	else rsa_TGA=0;
	if rsh_TGA=1 or rsa_TGA=1
		then DxTGA=1;
		else DxTGA=0;
	format deathdt curraddressdt mmddyy10. rsh_TGA rsa_TGA DxTGA yn.;
	label curraddressdt = 'Current address date'
		  deathdt       = 'Death date'
		  ehars_uid     = 'eHARS ID'
		  DxTGA 		= 'Should remain in data upon leaving TGA if in care';
run;

/*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
Examine how many observations have a current address in
Indiana and print a frequency table of the types of
addresses (informational and for quality checks). Un-
comment-out to run.

proc contents data=demo;
	title 'Contents of Dataset';
run;

Now we look at how many observations have a current
address in the TGA, where death date is missing, and
print a frequency table of the types of addresses.

proc freq data=demo (where=(cur_state_cd='IN'));
	where deathdt =.
	  and cur_county_name in ("*MARION", "BOONE CO.", "BROWN CO.",
								"HAMILTON CO.", "HANCOCK CO.",
								"HENDRICKS CO.", "JOHNSON CO.",
								"MARION CO.", "MORGAN CO.",
								"PUTNAM CO.", "SHELBY CO.");
	tables cur_address_type_cd cur_county_name;
	title 'Types of Addresses among PLWH in the TGA without a Death Date in eHARS';
run;

** Unsurprisingly, Marion county leads the pack. When we sort we
   also exclude anyone who does not have a missing death date
   (i.e., known deaths). We drop the death date field from the output
   dataset as we will not need it.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*/


****  8) Exclude deceased and non-TGA records.;
*************************************************************************;

proc sort data=demo (where=(deathdt = .
					   and cur_state_cd='IN' 
					   and cur_county_name in ("*MARION", "BOONE CO.",
							"BROWN CO.", "HAMILTON CO.", "HANCOCK CO.",
							"HENDRICKS CO.", "JOHNSON CO.", "MARION CO.",
							"MORGAN CO.", "PUTNAM CO.", "SHELBY CO.")))
	out=demoliveresaddress (drop=deathdt);
	by document_uid descending curraddressdt;
run;


****  9) Retain only most recent current address for each individual.;
*************************************************************************;

data demoTGA (drop=cur_country_cd);
	set demoliveresaddress;
	by document_uid descending curraddressdt;
	if first.document_uid;
run;


**** 10) Merge demographic and reverse-sorted lab data for those in the TGA;
*************************************************************************;

data flags;
	merge demoTGA (in=indemoTGA) downlabs (drop=lab_seq);
	by document_uid;
	if indemoTGA;
run;

/*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
Examine dataset contents.

proc contents data=flags;
	title 'Flags data set';
run;

proc print data=flags (obs=10);
	by document_uid;
	title 'Sample of flags data set';
run;

* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*/


**** 11) Create and run the RIC flag macro.;
*************************************************************************;

** We now read the macro which flags people. The arguments required
   are today’s date and a date window. They were set early in the
   program and, if you wish to change them, changes should be made
   there. The below code does not require changes. The date window
   represents how far back (in days) you wish to examine labs. We used
   a window of 730 days (two years) for testing but I've revised it to
   548 (18 months) to shorten the list sent to outreach staff. The
   lookback period can be lengthened at the start of the program if
   desired. ;

%macro todayflags(todaysdate, window);
	* Generate flags;
	data calllist (drop=result lab_test_cd today daysfromtoday);
	set flags (where=(result ne '' & input(&todaysdate,mmddyy8.)-sample_dt<&window));
	by document_uid descending sample_dt;
	today=input(&todaysdate,mmddyy8.);
	%put &todaysdate;
	daysfromtoday=today-sample_dt;
	if first.document_uid;
	if daysfromtoday>11/12*365.25 | (daysfromtoday>10/12*365.25 & detectable=1)
		then output calllist;
	label sample_dt='Most Recent VL Date';
run;
%mend
todayflags;

%todayflags(&ManDate.,&lookback.);


**** 12) Sort call list by sample_dt (oldest to newest) and ehars_uid.;
*************************************************************************;

proc sort data=calllist;
	by sample_dt ehars_uid;
run;

/*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
Examine dataset contents.

proc freq data=calllist;
	tables cur_state_cd cur_county_name DxTGA detectable cur_address_type_cd;
run;

proc print data=calllist (obs=5) label;
	var ehars_uid first_name last_name DxTGA curraddressdt
		cur_address_type_cd cur_street_address1
		cur_street_address2 cur_city_name cur_county_name
		cur_state_cd cur_zip_cd cur_phone sample_dt detectable;
run;

* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*/


**** 13) Export list of people at risk of falling out of care.;
*************************************************************************;

ods listing close;
ods excel file="&DataExport.TGA RIC Alert Call List_&TheDate..xlsx"
	options(sheet_interval="none"
			suppress_bylines="yes"
			embedded_titles="yes"
			embedded_footnotes="yes"
			sheet_label="none");

proc print data=calllist label;
	var ehars_uid first_name last_name DxTGA curraddressdt
		cur_address_type_cd cur_street_address1
		cur_street_address2 cur_city_name cur_county_name
		cur_state_cd cur_zip_cd cur_phone sample_dt detectable;
	title1 "People living with HIV in the TGA and at risk of non-retention in care: &Today.";
	title2 "Includes only individuals found in a look-back period of &lookback. days.";
	title3 "";
	title4 "It is possible that some of these individuals are deceased or have moved from the TGA.";
	title5 "If the field 'Should remain in data upon leaving TGA if in care' is No, then they might be in care outside of the TGA and we will not know."; 
	title6 "If that field is Yes, then they're likely living in the TGA and truly out of care."; 
run;

ods excel close;
ods listing;


**** 14) Clean up ;
*************************************************************************;

** Delete any macros and macro variables that were created.;
%SYMDEL LogLstDir ProgramNam TheDate ManDate lookback Today DataFolder DataExport / NOWARN;
**%SYSMACDELETE MyMacro / NOWARN;
PROC PRINTTO;
RUN;


