
/*------------------------------------------------------------------------
    File        : day25.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day25 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 25 18:48:05 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/25".
DEFINE VARIABLE cOSCommand   AS CHARACTER NO-UNDO.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEntry       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEntry       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOpenURL     AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lPart        AS LOGICAL   NO-UNDO EXTENT 2.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INT64     NO-UNDO.
DEFINE VARIABLE cSolution    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPair        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPair        AS CHARACTER NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttFuel
   FIELD iLineNr  AS INTEGER 
   FIELD cSNAFU   AS CHARACTER 
   FIELD iDecimal AS INT64     FORMAT ">>>,>>>,>>>,>>>,>>>,>>9-"
INDEX indLineNr IS UNIQUE iLineNr.

   
/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION getDecimal RETURNS INT64 
   (INPUT ipcSNAFU AS CHARACTER) FORWARD.

FUNCTION getSNAFU RETURNS CHARACTER 
   (INPUT ipiDecimal AS INT64) FORWARD.

/* ***************************  Main Block  *************************** */
DISPLAY
   lOpenURL LABEL "Open URL?"     VIEW-AS TOGGLE-BOX SKIP 
   lPart[1] LABEL "Solve Part 1?" VIEW-AS TOGGLE-BOX SKIP
   lPart[2] LABEL "Solve Part 2?" VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug LABEL "Debug?"        VIEW-AS TOGGLE-BOX SKIP 
   lvlShow  LABEL "Show?"         VIEW-AS TOGGLE-BOX SKIP
WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".

UPDATE
   lOpenURL
   lPart
   lvlDebug
   lvlShow
WITH FRAME fr-Parameters.

IF lOpenURL THEN DO:
   cOSCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cOSCommand).
END.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\25.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\25_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   CREATE ttFuel.
   ASSIGN 
      ttFuel.iLineNr = iLine
      ttFuel.cSNAFU  = cLine
   .
      
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttFuel:HANDLE).   
END.                     

IF lPart[1] THEN DO:
   iSolution = 0.
   FOR EACH ttFuel:
      ASSIGN 
         ttFuel.iDecimal = getDecimal(ttFuel.cSNAFU)
      .
      iSolution = iSolution + ttFuel.iDecimal.
   END.
   
   cSolution = getSNAFU(iSolution).
   
END.

          
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttFuel:HANDLE).
END.
   
IF lPart[1] THEN DO: 
   OUTPUT TO "clipboard".
   PUT UNFORMATTED cSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         cSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 25 - Part One".
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 25 - Part Two".
END. /* Process Part Two */

CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   IF oError:CallStack NE ? THEN DO:
      cErrorMessage = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cErrorMessage, oError:CallStack).
   END.
   
   MESSAGE "Error!" SKIP (1)
   SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
   cErrorMessage SKIP(1) 
   VIEW-AS ALERT-BOX ERROR.

   IF lvlShow THEN DO:
   END.
   RETURN.      
END CATCH.

/* **********************  Internal Procedures  *********************** */



   
/* ************************  Function Implementations ***************** */

FUNCTION getDecimal RETURNS INT64 
(INPUT ipcSNAFU AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar  AS INT64     NO-UNDO.
DEFINE VARIABLE cChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValue AS INT64     NO-UNDO.
DEFINE VARIABLE iBase  AS INT64     NO-UNDO.
DEFINE VARIABLE iTotal AS INT64     NO-UNDO.

   ASSIGN 
      iTotal = 0
      iBase  = 1
   .
   DO iChar = LENGTH (ipcSNAFU) TO 1 BY -1:
      cChar = SUBSTRING (ipcSNAFU, iChar, 1).
      CASE cChar:
         WHEN "0" OR 
         WHEN "1" OR 
         WHEN "2" THEN 
            iValue = INTEGER (cChar).
         WHEN "-" THEN 
            iValue = -1.
         WHEN "=" THEN 
            iValue = -2.
         OTHERWISE
            MESSAGE "Unknown value:" cChar
            VIEW-AS ALERT-BOX.
      END CASE.
      iTotal = iTotal + (iBase * iValue).
      iBase = iBase * 5.
   END.
   
   RETURN iTotal.   
      
END FUNCTION.

FUNCTION getSNAFU RETURNS CHARACTER 
   (INPUT ipiDecimal AS INT64):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/   

DEFINE VARIABLE iBase     AS INT64     NO-UNDO.
DEFINE VARIABLE iValue    AS INT64     NO-UNDO.
DEFINE VARIABLE iTotal    AS INT64     NO-UNDO.
DEFINE VARIABLE cSNAFU    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLista    AS CHARACTER NO-UNDO INITIAL "=,-,0,1,2".
DEFINE VARIABLE iEntry    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cScelta   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDiff     AS INT64     NO-UNDO.

   iBase  = 1.
   iValue = 2.
   iTotal = iBase * iValue.
   cSNAFU = "2".
   
   DO WHILE iTotal LT ipiDecimal:
      iBase  = iBase * 5.
      cSNAFU = cSNAFU + "0".
      iTotal = iBase * iValue.
   END.
            
   IF lvlShow THEN                   
      MESSAGE cSNAFU SKIP 
      iTotal SKIP 
      ipiDecimal SKIP 
      iBase "*" iValue
      VIEW-AS ALERT-BOX.

   DO iChar = 2 TO LENGTH (cSNAFU):
      /* Assign subsequent characters */
      ASSIGN
         iDiff = ?
      .      
      DO iEntry = 1 TO NUM-ENTRIES (cLista):
         SUBSTRING (cSNAFU, iChar) = ENTRY (iEntry, cLista).

         IF lvlShow THEN 
            MESSAGE iChar iEntry SKIP 
            cSNAFU SKIP 
            getDecimal (cSNAFU) SKIP 
            ipiDecimal SKIP 
            "Diff:" ipiDecimal - getDecimal(cSNAFU) "Char:" ENTRY (iEntry, cLista) 
            VIEW-AS ALERT-BOX.
            
         IF iDiff EQ ? THEN 
            ASSIGN 
               iDiff = ipiDecimal - getDecimal(cSNAFU)
               cScelta = ENTRY (iEntry, cLista)
            .
         ELSE DO:
            IF ABSOLUTE (ipiDecimal - getDecimal(cSNAFU)) LT ABSOLUTE (iDiff) THEN DO:
               /* Found a value closer to input */
               ASSIGN 
                  iDiff   = ipiDecimal - getDecimal(cSNAFU).
                  cScelta = ENTRY (iEntry , cLista)
               .
            END.
         END.

      END.

      SUBSTRING (cSNAFU, iChar) = cScelta.
      IF ipiDecimal EQ getDecimal(cSNAFU) THEN
         RETURN cSNAFU.
   END. /* Assign subsequent characters */
               
   RETURN cSNAFU.
   
END FUNCTION.

