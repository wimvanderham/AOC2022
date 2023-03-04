USING Progress.Lang.*.
USING OpenEdge.Core.*.
USING OpenEdge.Net.HTTP.*.
USING Progress.Json.ObjectModel.*.
/* USING OpenEdge.Net.HTTP.IHttpRequest.   */
/* USING OpenEdge.Net.HTTP.IHttpResponse.  */
/* USING OpenEdge.Net.HTTP.ClientBuilder.  */
/* USING OpenEdge.Net.HTTP.RequestBuilder. */

DEFINE VARIABLE oRequest          AS IHttpRequest     NO-UNDO.
DEFINE VARIABLE oResponse         AS IHttpResponse    NO-UNDO.
DEFINE VARIABLE oCookie           AS Cookie           NO-UNDO.
DEFINE VARIABLE lcInput           AS LONGCHAR         NO-UNDO.
DEFINE VARIABLE iYear             AS INTEGER          NO-UNDO.
DEFINE VARIABLE iDay              AS INTEGER          NO-UNDO.
DEFINE VARIABLE cURL              AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cFileName         AS CHARACTER        NO-UNDO.

oCookie = Cookie:Parse("session=53616c7465645f5f0aa2b48889c4ecb0a71e7086a3ce378be60c9c62fff2ce2f0a803b3cf401a90e48d12df95cfd2383f2923a50c7378e392a1b5d4ce4438c7e").

ASSIGN 
   iYear = YEAR (TODAY)
   iDay  = DAY (TODAY)
.
IF MONTH (TODAY) LT 12 THEN DO:
   ASSIGN 
      iYear = iYear - 1
      iDay  = 1
   .
END.

REPEAT:
   UPDATE 
   iYear LABEL "Year" FORMAT "9999" SKIP 
   iDay  LABEL "Day " FORMAT "z9"   SKIP (1)
   WITH SIDE-LABELS 1 DOWN FRAME fr-Parameters TITLE " Select Year & Day " CENTERED ROW 3.
   
   cURL = SUBSTITUTE ("https://adventofcode.com/&1/day/&2/input", iYear, iDay).
   cFileName = SUBSTITUTE ("input\&1.txt", STRING (iDay, "99")).
      
   oRequest = RequestBuilder:Get(cURL):AddCookie(oCookie):Request. 

   oResponse = ClientBuilder:Build():Client:Execute(oRequest).

   DISPLAY
   oResponse:StatusCode   LABEL "Reason" SKIP 
   oResponse:StatusReason LABEL "OK?"    SKIP 
   WITH FRAME fr-Parameters.

   lcInput = CAST (oResponse:Entity, String):VALUE.
   
   COPY-LOB FROM lcInput TO FILE cFileName.
   
   /*
   OUTPUT TO VALUE(cFileName).
   PUT UNFORMATTED 
   RIGHT-TRIM (STRING (oResponse:Entity)) SKIP.
   OUTPUT CLOSE.
   */
   
   FILE-INFO:FILE-NAME = cFileName.
   IF FILE-INFO:FILE-TYPE EQ ? THEN DO:
      MESSAGE SUBSTITUTE ("Couldn't find '&1' file.", cFileName)
      VIEW-AS ALERT-BOX.    
      RETURN.
   END.
   
   RUN sy\win\show-file.w
      (INPUT FILE-INFO:FULL-PATHNAME).
      
END.
