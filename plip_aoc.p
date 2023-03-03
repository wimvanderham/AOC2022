
/*------------------------------------------------------------------------
    File        : plip_aoc.p
    Purpose     : 

    Syntax      :

    Description : PLIP for Advent of Code

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Mar 03 22:17:23 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.*.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getTextInterval RETURNS CHARACTER 
(INPUT ipdtFromDatetime AS DATETIME,
 INPUT ipdtToDatetime   AS DATETIME) FORWARD.

/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE chkURL:
/*------------------------------------------------------------------------------
 Purpose: Check if the URL is available
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiYear     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDay      AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplOk       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

DEFINE VARIABLE dtNOW     AS DATETIME         NO-UNDO.
DEFINE VARIABLE dtStart   AS DATETIME         NO-UNDO.
DEFINE VARIABLE oRequest  AS IHttpRequest     NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse    NO-UNDO.
DEFINE VARIABLE cURL      AS CHARACTER        NO-UNDO.

   IF ipiYear LT 2015 OR ipiYear GT YEAR (TODAY) THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE("Year should be greater equal 2015 and less equal to &1. Year '&2' is invalid.", YEAR (TODAY), ipiYear)
      .
      RETURN.
   END.
   
   IF ipiDay LT 1 OR ipiDay GT 25 OR ipiDay EQ ? THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE ("Day number should be between 1 and 25. Day number '&1' is invalid.", ipiDay)
      .
      RETURN.
   END.

   /* Local start is at 6 o'clock in the morning */
   ASSIGN 
      dtNOW   = NOW
      dtStart = DATETIME (12, ipiDay, ipiYear, 6, 0)
   .
   IF dtNOW LT dtStart THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE ("This day is not available yet. You have to wait &1.",
                                  getTextInterval(dtNOW, dtStart))
      .
      RETURN.
   END.
   
   cURL = SUBSTITUTE ("https://adventofcode.com/&1/day/&2",
                      ipiYear,
                      ipiDay).
                         
   oRequest = RequestBuilder:Get(cURL):Request. 
   
   oResponse = ClientBuilder:Build():Client:Execute(oRequest).

   ASSIGN 
      oplOk = oResponse:StatusReason EQ "OK"
   .
   
   IF oplOk EQ FALSE THEN DO:
      ASSIGN 
         opcMessage = SUBSTITUTE ("HTTP Request failed with Status '&1' and Reason '&2'.",
                                  oResponse:StatusCode,
                                  oResponse:StatusReason)
      .
      RETURN.
   END.
                                     
/*   OUTPUT TO VALUE(ipcFileName).         */
/*   PUT UNFORMATTED                       */
/*   TRIM (STRING (oResponse:Entity)) SKIP.*/
/*   OUTPUT CLOSE.                         */
   
/*   FILE-INFO:FILE-NAME = ipcFileName.                                          */
/*                                                                               */
/*   ASSIGN                                                                      */
/*      opcMessage = SUBSTITUTE ("Input for Day &1 of Year &2 saved in file &3.",*/
/*                               ipiDay,                                         */
/*                               ipiYear,                                        */
/*                               FILE-INFO:FULL-PATHNAME)                        */
/*   .                                                                           */
                                     
   ASSIGN 
      opcMessage = SUBSTITUTE ("URL &1 OK", cURL)
   .   


END PROCEDURE.

PROCEDURE getInput:
/*------------------------------------------------------------------------------
 Purpose: Saves input for a particular day
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSession  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiYear     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDay      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOk       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

DEFINE VARIABLE dtNOW     AS DATETIME         NO-UNDO.
DEFINE VARIABLE dtStart   AS DATETIME         NO-UNDO.
DEFINE VARIABLE oRequest  AS IHttpRequest     NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse    NO-UNDO.
DEFINE VARIABLE oCookie   AS Cookie           NO-UNDO.
DEFINE VARIABLE cURL      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lcInput   AS LONGCHAR         NO-UNDO.

   IF ipiYear LT 2015 OR ipiYear GT YEAR (TODAY) THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE("Year should be greater equal 2015 and less equal to &1. Year '&2' is invalid.", YEAR (TODAY), ipiYear)
      .
      RETURN.
   END.
   
   IF ipiDay LT 1 OR ipiDay GT 25 OR ipiDay EQ ? THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE ("Day number should be between 1 and 25. Day number '&1' is invalid.", ipiDay)
      .
      RETURN.
   END.

   /* Local start is at 6 o'clock in the morning */
   ASSIGN 
      dtNOW   = NOW
      dtStart = DATETIME (12, ipiDay, ipiYear, 6, 0)
   .
   IF dtNOW LT dtStart THEN DO:
      ASSIGN 
         oplOk = FALSE 
         opcMessage = SUBSTITUTE ("This day is not available yet. You have to wait &1.",
                                  getTextInterval(dtNOW, dtStart))
      .
      RETURN.
   END.
   
   oCookie = Cookie:Parse(SUBSTITUTE ("session=&1", ipcSession)).

   cURL = SUBSTITUTE ("https://adventofcode.com/&1/day/&2/input",
                      ipiYear,
                      ipiDay).
                         
   oRequest = RequestBuilder:Get(cURL):AddCookie(oCookie):Request. 
   
   oResponse = ClientBuilder:Build():Client:Execute(oRequest).

   ASSIGN 
      oplOk = oResponse:StatusReason EQ "OK"
   .
   
   IF oplOk EQ FALSE THEN DO:
      ASSIGN 
         opcMessage = SUBSTITUTE ("HTTP Request failed with Status '&1' and Reason '&2'.",
                                  oResponse:StatusCode,
                                  oResponse:StatusReason)
      .
      RETURN.
   END.
                                     
   OUTPUT TO VALUE(ipcFileName).
   PUT UNFORMATTED 
   TRIM (STRING (oResponse:Entity)) SKIP.
   OUTPUT CLOSE.
   
   FILE-INFO:FILE-NAME = ipcFileName.
   
   ASSIGN 
      opcMessage = SUBSTITUTE ("Input for Day &1 of Year &2 saved in file &3.",
                               ipiDay,
                               ipiYear,
                               FILE-INFO:FULL-PATHNAME)
   .
                                     
   
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION getTextInterval RETURNS CHARACTER 
(INPUT ipdtFromDatetime AS DATETIME,
 INPUT ipdtToDatetime   AS DATETIME):
/*------------------------------------------------------------------------------
 Purpose: Return interval between two datetime values into text
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cTextInterval AS CHARACTER NO-UNDO.

DEFINE VARIABLE iYears   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMonths  AS INTEGER NO-UNDO.
DEFINE VARIABLE iDays    AS INTEGER NO-UNDO.
DEFINE VARIABLE iHours   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinutes AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeconds AS INTEGER NO-UNDO.

   ASSIGN 
      iYears = INTERVAL  (ipdtToDatetime, ipdtFromDatetime, "years")
   .
   IF iYears GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1 year&2",
                                  iYears,
                                  (IF iYears GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iYears, "years").
   END.
   iMonths = INTERVAL (ipdtToDatetime, ipdtFromDatetime, "months").
   IF iMonths GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1&2&3 month&4",
                                  cTextInterval,
                                  (IF cTextInterval NE "" THEN ", " ELSE ""),
                                  iMonths,
                                  (IF iMonths GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iMonths, "months").                                  
   END.                                                             
   iDays = INTERVAL (ipdtToDatetime, ipdtFromDatetime, "days").
   IF iDays GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1&2&3 day&4",
                                  cTextInterval,
                                  (IF cTextInterval NE "" THEN ", " ELSE ""),
                                  iDays,
                                  (IF iDays GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iDays, "days").         
   END.
   iHours = INTERVAL (ipdtToDatetime, ipdtFromDatetime, "hours").
   IF iHours GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1&2&3 hour&4",
                                  cTextInterval,
                                  (IF cTextInterval NE "" THEN ", " ELSE ""),
                                  iHours,
                                  (IF iHours GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iHours, "hours").
   END.
   iMinutes = INTERVAL (ipdtToDatetime, ipdtFromDatetime, "minutes").
   IF iMinutes GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1&2&3 minute&4",
                                  cTextInterval,
                                  (IF cTextInterval NE "" THEN ", " ELSE ""),
                                  iMinutes,
                                  (IF iMinutes GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iMinutes, "minutes").
   END.
   iSeconds = INTERVAL (ipdtToDatetime, ipdtFromDatetime, "seconds").
   IF iSeconds GT 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1&2&3 second&4",
                                  cTextInterval,
                                  (IF cTextInterval NE "" THEN ", " ELSE ""),
                                  iSeconds,
                                  (IF iSeconds GT 1 THEN "s" ELSE "")).
      ipdtFromDatetime = ADD-INTERVAL (ipdtFromDatetime, iSeconds, "seconds").
   END.
   
   IF INDEX (cTextInterval, ", ") NE 0 THEN DO:
      cTextInterval = SUBSTITUTE ("&1 and &2",
                                  SUBSTRING (cTextInterval, 1, R-INDEX (cTextInterval, ",") - 1),
                                  TRIM (SUBSTRING (cTextInterval, R-INDEX (cTextInterval, ",") + 1))).
   END.
   
   RETURN cTextInterval.

END FUNCTION.

