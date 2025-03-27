* * * * * * * * * * * * * * * * * * * * * * * * 
***********************************************
**** This is not a stand-alone program and ****
**** should be used only as called in by   ****
**** the &RequestID. main program.         ****
***********************************************
* * * * * * * * * * * * * * * * * * * * * * * ;

%LET  Year=&Year.; ** Edit for year incidence and prevalence should be obtained;
%LET  SMD=01JAN; ** Edit for month and year of Inc/Prev START date ;
%LET  EMD=31DEC; ** Edit for month and year of Inc/Prev END date ;
 
%LET StartDt=&SMD.&Year.;
%LET EndDt=&EMD.&Year.;

***************** Assign Libraries ***************************;

LIBNAME ORWG odbc NOPROMPT="DATABASE=RWG;Server=vssql14hhc01\Prod01;Driver={SQL Server};Trusted_Connection=Yes" SCHEMA=DBO; ** RISE Db ;

***************** Assign formats ****************************;

PROC FORMAT ;
	VALUE Housing
		1 = "Stable Permanent Housing"
		2 = "Temporary Housing"
		3 = "Unstable Housing"
	    4 = "Housing Status Missing/Unknown";

**  1) Define key constants, formats, etc. ;

%MACRO createFormat (DatasetName, FmtName, valuevar, valueLbl);
DATA &DatasetName.Fmt;
  SET ORWG.&DatasetName. (RENAME=(&valuevar.=Start &valueLbl.=Label));
  RETAIN fmtname &FmtName. TYPE 'n';
  RUN;
  
PROC FORMAT CNTLIN=&DatasetName.Fmt;
  RUN;

PROC DATASETS NOLIST LIBRARY=work;
  DELETE &DatasetName.Fmt;
  QUIT;
%MEND;

**  2) Call in rwg1cube data ;

%createFormat(RWGInsuranceType, "RWInsTyp", InsuranceTypeID, InsType);

** Create a dataset containing records only where insurance was valid during the time period of interest ;

DATA RWGClientInsuranceCUR (KEEP=ClientNumber InsuranceTypeID_txt) ;
	LENGTH InsuranceTypeID_txt $ 200 ;
	SET ORWG.RWGClientInsurance ;
	BY ClientNumber InsuranceTypeID ;
    ** Preserve the value of Ins_type_list between records. ;
	RETAIN InsuranceTypeID_txt ;
	FORMAT InsuranceTypeID RWInsTyp. ;
	** For the first record from each client, set the Ins_type_list variable as a blank. ;
    IF First.ClientNumber THEN InsuranceTypeID_txt = "" ;
	** Retains Ins_type_list and adds each occurance of insurance type to the Ins_type_list variable, separated by a comma.
	   This strips away leading blanks and converts pre-existing commas to periods. ;
	IF First.ClientNumber AND ((DATEPART(EffBegDt)<= "&EndDt."d)) AND ((DATEPART(EffEndDt) >= "&StartDt."d)) THEN InsuranceTypeID_txt = STRIP(PUT(VVALUE(InsuranceTypeID), $50.)) ;
		ELSE IF First.ClientNumber AND ((EffEndDt = .) OR (EffBegDt = .) OR (DATEPART(EffBegDt) > "&EndDt."d) OR (DATEPART(EffEndDt) < "&StartDt."d)) THEN InsuranceTypeID_txt = "" ;
		ELSE IF ((DATEPART(EffBegDt)<= "&EndDt."d)) AND ((DATEPART(EffEndDt) >= "&StartDt."d)) AND InsuranceTypeID_txt ^= "" THEN InsuranceTypeID_txt = STRIP(InsuranceTypeID_txt)||", "||STRIP(PUT(VVALUE(InsuranceTypeID), $50.)) ;
		ELSE IF ((DATEPART(EffBegDt)<= "&EndDt."d)) AND ((DATEPART(EffEndDt) >= "&StartDt."d)) AND InsuranceTypeID_txt  = "" THEN InsuranceTypeID_txt = STRIP(PUT(VVALUE(InsuranceTypeID), $50.)) ; 
	IF Last.ClientNumber THEN OUTPUT RWGClientInsuranceCUR ;
RUN ;

** Get client application history to determine how long clients have been in the program and whether
   they were newly enrolled or reenrolled during the grant period ;

DATA tmp_RWGClientAppHistory (KEEP=ClientNumber MostRecent1stAppDt FirstAppEverDt LastExpireDate);
  SET ORWG.RWGClientAppHistory;
  BY ClientNumber AppDt;
  RETAIN FirstAppEverDt MostRecent1stAppDt LastExpireDate;
  IF first.ClientNumber THEN DO; **Initialize variables;
    FirstAppEverDt = DATEPART(AppDt);
    MostRecent1stAppDt = DATEPART(AppDt);
    LastExpireDate = datepart(ExpireDate);
  Format FirstAppEverDt MostRecent1stAppDt LastExpireDate DATE9.;
  END; ** End of 'IF first.ClientNumber THEN DO';
  ELSE DO;
    IF INTCK('day', DATEPART(LastExpireDate), DATEPART(AppDt)) > 30 THEN MostRecent1stAppDt = DATEPART(AppDt);
	**If there was >30 day gap between when client's enrollment expired and new application submitted, applicant is new applicant (not recert);
    LastExpireDate = MAX(LastExpireDate, datepart(ExpireDate));
	**No matter what, always want the most recent Expiration Date;
  END; ** End of 'ELSE DO';
  IF last.ClientNumber THEN OUTPUT tmp_RWGClientAppHistory;
  RUN;

PROC SQL;
  CREATE TABLE tmp_Income AS 
  SELECT
      ClientNumber
    , SUM(Total) AS TotalIncome
  FROM 
    (SELECT
        ClientNumber
      , CASE 
          WHEN PayFreqID = 3 THEN ((Amount * 26)/12)
          WHEN PayFreqID = 2 THEN ((Amount * 52)/12)
          WHEN PayFreqID = 4 THEN ((Amount))
          ELSE Amount 
        END AS Total
    FROM ORWG.RWGClientIncome)
  GROUP BY ClientNumber;

  CREATE TABLE tmp_totalIncome AS
  SELECT
      clientfinancial.ClientNumber 
    , CASE
        WHEN clientfinancial.TotalNumberInHouseHold IS NULL THEN 1
        WHEN clientfinancial.TotalNumberInHouseHold = 0 THEN 1
        ELSE clientfinancial.TotalNumberInHouseHold
      END AS NumberInHousehold
    , tmp_income.TotalIncome 
  FROM 
  ORWG.RWGClientFinancial AS clientfinancial
  LEFT JOIN
  tmp_Income
  ON clientfinancial.ClientNumber=tmp_Income.ClientNumber
;
  
  CREATE TABLE tmp_CurrentCopay AS
  SELECT
      tmp_totalIncome.*
    , CASE
        WHEN tmp_totalIncome.TotalIncome <=0 THEN "$0"
        WHEN FeeScale.CoPay = "FULL CHARGE" THEN FeeScale.Copay
        ELSE ("$"||FeeScale.CoPay)
      END AS CurrentCoPay
    , FeeScale.PctPoverty
  FROM tmp_totalIncome
  LEFT JOIN 
  ORWG.RWGSlidingFeeScale AS FeeScale
  ON ((tmp_totalIncome.NumberInHousehold = FeeScale.FamilySize)
    AND (FeeScale.MonthlyIncomeFrom <= tmp_totalIncome.TotalIncome <= FeeScale.MonthlyIncomeTo)
    AND (DATEPART(ProjectYearStart) <= Date() <= DATEPART(ProjectYearEnd)));
QUIT;

%createFormat(RWGRecordStatus,   "RWStatus", RecordStatusID,   Description);
%createFormat(RWGEnrollmentType, "RWEnroll", EnrollmentTypeID, EnrollmentTypeDesc);

DATA tmp_CurrentCopay ;
	SET tmp_CurrentCopay ;
RUN ;

DATA tmp_RWGClientDemographics (KEEP=ClientNumber LastName FirstName MiddleName AliasLastName AliasFirstName DOBdt
                                     DateOfDeath SSN ReferralSource ehars_uid_rwg EnrollmentTypeID RecordStatusID Status
                                     rwg_HousingStatus StreetNumber Dir StreetName StreetType SuffDir City County State
                                     ZipCode MedicalRecordNumber RecordStatusID) ;
	SET ORWG.RWGClientDemographics ;
	BY ClientNumber ;
  ** Recode EHARSNumber pre-eHARS merge ;
	LENGTH ehars_uid_rwg $ 16 ;
	ehars_uid_rwg = EHARSNumber ;
  ** Create a SAS DOB date ;
	DOBdt = DATEPART(DOB) ;
  ** Apply format to HousingStatusID ;
	IF  HousingStatusID = 1   THEN rwg_HousingStatus = 1 ;
	IF  HousingStatusID = 2   THEN rwg_HousingStatus = 2 ;
	IF  HousingStatusID = 3   THEN rwg_HousingStatus = 3 ;
	IF (HousingStatusID ^= 1
   AND  HousingStatusID ^= 2
   AND  HousingStatusID ^= 3) THEN rwg_HousingStatus = 4 ;
	FORMAT RecordStatusID RWStatus. EnrollmentTypeID RWEnroll. DOBdt DATE9. rwg_HousingStatus Housing. ;
RUN;

%createFormat(RWGServiceProviderContract, "RWFacil", SPCID, ProviderName);

DATA tmp_RWGClientAddInfo (KEEP=ClientNumber FacilityID) ;
	SET ORWG.RWGClientAddInfo ;
	BY ClientNumber ;
	FORMAT FacilityID RWFacil. ;
RUN ;

** Close ORWG library ;

LIBNAME ORWG CLEAR ; ** Releases data file back to the system. ;

** ** ** ** ** ** Sort and merge all rwg1cube tables into one dataset ** ** ** ** ** ** ;

PROC SORT DATA=RWGClientInsuranceCUR NODUPKEY ;
BY ClientNumber ;
PROC SORT DATA=tmp_RWGClientAppHistory NODUPKEY ;
BY ClientNumber ;
PROC SORT DATA=tmp_CurrentCopay NODUPKEY ;
BY ClientNumber ;
PROC SORT DATA=tmp_RWGClientDemographics NODUPKEY ;
BY ClientNumber ;
PROC SORT DATA=tmp_RWGClientAddInfo NODUPKEY ;
BY ClientNumber ;
RUN ;

DATA rwg1data (KEEP=rwg_ClientNumber LastName FirstName MiddleName AliasLastName
                    AliasFirstName DOB ehars_uid_rwg rwg_DateOfDeath SSN StreetNumber Dir
                    StreetName StreetType SuffDir City County State ZipCode FirstAppEverDt
                    LastExpireDate MostRecent1stAppDt ReferralSource EnrollmentTypeID
                    RecordStatusID FacilityID InsuranceTypeID_txt PctPoverty
                    NumberInHousehold TotalIncome CurrentCoPay rwg_HousingStatus) ;
	MERGE RWGClientInsuranceCUR		(IN=In_I)
	      tmp_RWGClientAppHistory	(IN=In_A)
		  tmp_CurrentCopay			(IN=In_C)
		  tmp_RWGClientDemographics	(IN=In_D)
		  tmp_RWGClientAddInfo		(IN=In_P)
	;
	BY ClientNumber ;
	RENAME ClientNumber = rwg_ClientNumber ;
	RENAME DOBdt        = DOB ;
  ** Convert DATETIME vars to SAS dates ;
	rwg_DateOfDeath         = DATEPART(DateOfDeath)         ;
	FORMAT TotalIncome DOLLAR10.2 rwg_DateOfDeath DATE9. ;
	RUN ;
QUIT ;

**  Create final dataset of RISE data ;

DATA &RequestID.RISE_FinDat (KEEP=rwg_ClientNumber last_name first_name MiddleName rwg_AliasLastName
                                  rwg_AliasFirstName DOB ehars_uid_rwg rwg_DateOfDeath ssn_rev rwg_address
                                  rwg_city rwg_county rwg_State rwg_zip rwg_FirstAppEverDt rwg_LastExpireDate
                                  rwg_MostRecent1stAppDt rwg_ReferralSource rwg_EnrollmentTypeID rwg_RecordStatusID
                                  rwg_FacilityID rwg_InsuranceTypeID_txt rwg_NumberInHousehold rwg_PctPoverty
                                  rwg_TotalIncome rwg_CurrentCoPay rwg_HousingStatus) ;
	SET rwg1data ;
  ** Combine street address components into one variable ;
	LENGTH rwg_Address $ 200 ssn_rev $ 9;
	rwg_Address = StreetNumber||Dir||StreetName||StreetType||SuffDir ;
  ** SSN recode pre-eHARS merge ;
	ssn_rev = COMPRESS(SSN, "-", 'S');
  ** Rename variables so that its clear they are from the RWG/RISE data set ;
	RENAME AliasLastName       = rwg_AliasLastName ;
	RENAME AliasFirstName      = rwg_AliasFirstName ;
	RENAME FirstName           = first_name ;
	RENAME LastName            = last_name ;
	RENAME City                = rwg_city ;
	RENAME County              = rwg_county ;
	RENAME State               = rwg_State ;
	RENAME ZipCode             = rwg_zip ;
	RENAME FirstAppEverDt      = rwg_FirstAppEverDt ;
	RENAME LastExpireDate      = rwg_LastExpireDate ;
	RENAME MostRecent1stAppDt  = rwg_MostRecent1stAppDt ;
	RENAME ReferralSource      = rwg_ReferralSource ;
	RENAME EnrollmentTypeID    = rwg_EnrollmentTypeID ;
	RENAME RecordStatusID      = rwg_RecordStatusID ;
	RENAME FacilityID          = rwg_FacilityID ;
	RENAME InsuranceTypeID_txt = rwg_InsuranceTypeID_txt ;
	RENAME NumberInHousehold   = rwg_NumberInHousehold ;
	RENAME PctPoverty          = rwg_PctPoverty ;
	RENAME TotalIncome         = rwg_TotalIncome ;
	RENAME CurrentCoPay        = rwg_CurrentCoPay ;
RUN ;

DATA &RequestID.RISEdat ;
	SET &RequestID.RISE_FinDat ;
  ** Label variables ;
	LABEL last_name               = "rwg_Last Name"
          first_name              = "rwg_First Name"
          MiddleName              = "rwg_Middle Name"
          rwg_AliasLastName       = "rwg_Alias Last Name"
          rwg_AliasFirstName      = "rwg_Alias First Name"
		  DOB                     = "rwg_DOB"
          ehars_uid_rwg           = "rwg_eHARS unique identifier"
          rwg_DateOfDeath         = "rwg_Date of Death"
          ssn_rev                 = "rwg_Social Security Number (No hyphens)"
		  rwg_ClientNumber        = "rwg_Client Number"
          rwg_address             = "rwg_Address"
          rwg_city                = "rwg_City"
          rwg_county              = "rwg_County"
          rwg_State               = "rwg_State"
          rwg_zip                 = "rwg_Zip"
          rwg_FirstAppEverDt      = "rwg_First RWG App Ever Date"
          rwg_LastExpireDate      = "rwg_Last RWG Expire Date"
          rwg_MostRecent1stAppDt  = "rwg_Most Recent RWG App Date"
          rwg_ReferralSource      = "rwg_RWG Referral Source"
          rwg_EnrollmentTypeID    = "rwg_RWG Enrollment Type"
          rwg_RecordStatusID      = "rwg_RWG Client Status"
          rwg_FacilityID          = "rwg_RWG Facility"
          rwg_InsuranceTypeID_txt = "rwg_Insurance Type(s)"
          rwg_NumberInHousehold   = "rwg_Number Reported in Household"
          rwg_PctPoverty          = "rwg_Percent of Federal Poverty Level Reported"
          rwg_TotalIncome         = "rwg_Total Reported Income"
          rwg_CurrentCoPay        = "rwg_Current Co-Pay"
          rwg_HousingStatus       = "rwg_Housing Status"
	;
RUN ;

** Keep only those clients enrolled throughout &Year. and create a new variable indicating period of interest ;

DATA &RequestID.RWGCli&Year. ;
	SET &RequestID.RISEdat ;
	WHERE (rwg_FirstAppEverDt     < "&StartDt."d)                                        /* First enrolled before &StartDt.            */
      **AND (rwg_MostRecent1stAppDt < "&StartDt."d OR rwg_MostRecent1stAppDt > "&EndDt."d) /* Did not lapse for >30 days during &Year.   */;
      AND (rwg_LastExpireDate     > "&EndDt."d) ;                                        /* Coverage did not expire until after &Year. */
	Year = &Year. ;
	LABEL Year = "Year of Interest" ;
RUN ;

QUIT ;
