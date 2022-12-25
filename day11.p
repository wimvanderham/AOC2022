
/*------------------------------------------------------------------------
    File        : day11.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 11 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 23 22:52:08 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/11".
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
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPair        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPair        AS CHARACTER NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttMonkey
   FIELD iNr         AS INTEGER 
   FIELD cStartItems AS CHARACTER 
   FIELD cOperation  AS CHARACTER 
   FIELD iDivisible  AS INTEGER  
   FIELD iThrowTrue  AS INTEGER 
   FIELD iThrowFalse AS INTEGER 
   FIELD iInspected  AS INTEGER 
INDEX indNr     IS UNIQUE iNr
INDEX indInspected iInspected DESCENDING.

DEFINE BUFFER ttThrowMonkey FOR ttMonkey.

DEFINE VARIABLE iNewNr         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iItem          AS INT64     NO-UNDO.
DEFINE VARIABLE cCalculate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSituation     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lContinue      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLCM           AS INT64     NO-UNDO.
DEFINE VARIABLE lDivisibleTrue AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

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

COPY-LOB FROM FILE "input\11.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\11_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.


IF lvlShow THEN DO:
END. 
   
ASSIGN 
   iSolution = 0
.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      iNewNr = iNewNr + 1.
      NEXT ReadBlock.
   END.

   IF cLine BEGINS "Monkey" THEN DO:
      CREATE ttMonkey.
      ASSIGN 
         ttMonkey.iNr = iNewNr
      .
      NEXT.
   END.
   
   FIND  ttMonkey
   WHERE ttMonkey.iNr EQ iNewNr.
   
   IF TRIM (cLine) BEGINS "Starting items" THEN DO:
      ttMonkey.cStartItems = TRIM (ENTRY (2, cLine, ":")).
   END.
   
   IF TRIM (cLine) BEGINS "Operation" THEN DO:
      ttMonkey.cOperation = TRIM (ENTRY (2, cLine, ":")).
   END.
   
   IF TRIM (cLine) BEGINS "Test" THEN DO:
      ttMonkey.iDivisible = INTEGER (ENTRY (NUM-ENTRIES (cLine, " "), cLine, " ")).
   END.
   
   IF TRIM (cLine) BEGINS "If true" THEN DO:
      ttMonkey.iThrowTrue = INTEGER (ENTRY (NUM-ENTRIES (cLine, " "), cLine, " ")).
   END.
   
   IF TRIM (cLine) BEGINS "If false" THEN DO:
      ttMonkey.iThrowFalse = INTEGER (ENTRY (NUM-ENTRIES (cLine, " "), cLine, " ")).
   END.
   
END. /* ReadBlock: */

IF lPart[2] THEN DO:
   /* For Part Two we need to reduce the huge numbers by 
   ** only taking the remainder division by the LCM 
   ** (which is the product of all divisors
   */
   iLCM = 1.
   FOR EACH ttMonkey:
      iLCM = iLCM * ttMonkey.iDivisible.
   END.
END.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMonkey:HANDLE).   
END.                     

SolveBlock:
REPEAT:
   ACCUM "Round" (COUNT).
   FOR EACH ttMonkey:
      DO iEntry = 1 TO NUM-ENTRIES (ttMonkey.cStartItems):
         ttMonkey.iInspected = ttMonkey.iInspected + 1.
         iItem = INT64 (ENTRY (iEntry, ttMonkey.cStartItems)).
         cCalculate = REPLACE (TRIM (ENTRY (2, ttMonkey.cOperation, "=")), "old", STRING (iItem)).
         CASE ENTRY (2, cCalculate, " "):
            WHEN "*" THEN
               iItem = INT64 (ENTRY (1, cCalculate, " ")) * INT64 (ENTRY (3, cCalculate, " ")).
            WHEN "+" THEN 
               iItem = INT64 (ENTRY (1, cCalculate, " ")) + INT64 (ENTRY (3, cCalculate, " ")).
            OTHERWISE 
               MESSAGE "Unknown operation:"
               cCalculate
               VIEW-AS ALERT-BOX.
         END CASE.

         IF lPart[1] THEN 
            iItem = INT64 (TRUNCATE (iItem / 3, 0)).

         IF lPart[2] THEN
            /* Take the signficant and throw away the multiple(s) of LCM */
            iItem = iItem MOD iLCM.
            
         IF iItem MOD ttMonkey.iDivisible EQ 0 THEN
            FIND  ttThrowMonkey
            WHERE ttThrowMonkey.iNr EQ ttMonkey.iThrowTrue.
         ELSE 
            FIND  ttThrowMonkey
            WHERE ttThrowMonkey.iNr EQ ttMonkey.iThrowFalse.
         ASSIGN 
            ttThrowMonkey.cStartItems = SUBSTITUTE ("&1&2&3",
                                                    ttThrowMonkey.cStartItems,
                                                    (IF ttThrowMonkey.cStartItems NE "" THEN "," ELSE ""),
                                                    iItem)
         .
         ENTRY (iEntry, ttMonkey.cStartItems) = "".
      END.                                         
      ttMonkey.cStartItems = TRIM (ttMonkey.cStartItems, ",").
   END.

   IF lPart[1] THEN DO:
      /* Process Part One */
      IF (ACCUM COUNT "Round") EQ 20 THEN 
         LEAVE.
   END.
   IF lPart[2] THEN DO:
      IF lvlShow THEN DO:
      
         CASE (ACCUM COUNT "Round"):
            WHEN 1 OR 
            WHEN 20 OR 
            WHEN 1000 OR 
            WHEN 2000 OR 
            WHEN 3000 OR 
            WHEN 4000 OR 
            WHEN 5000 OR 
            WHEN 6000 OR 
            WHEN 7000 OR 
            WHEN 8000 OR 
            WHEN 9000 OR 
            WHEN 10000 THEN DO:
               cSituation = SUBSTITUTE ("== After round &1 ==", (ACCUM COUNT "Round")).
               
               FOR EACH ttMonkey:
                  cSituation = SUBSTITUTE ("&1&2&3",
                                           cSituation,
                                           (IF cSituation NE "" THEN "~n" ELSE ""),
                                           SUBSTITUTE ("Monkey &1 inspected items &2 times.",
                                                       ttMonkey.iNr,
                                                       ttMonkey.iInspected)).
               END.
               MESSAGE cSituation
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lContinue.
               IF lContinue EQ FALSE THEN 
                  LEAVE SolveBlock.
               IF lContinue EQ ? THEN 
                  RETURN.
            END.
         END CASE.
      END.
      IF (ACCUM COUNT "Round") EQ 10000 THEN
         LEAVE SolveBlock.
   END.              
END. /* SolveBlock: */
   
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMonkey:HANDLE).
END.
   
iSolution = 1.
FOR EACH ttMonkey
BY ttMonkey.iInspected DESCENDING:
   ACCUM "Top" (COUNT).
   iSolution = iSolution * ttMonkey.iInspected.
   IF (ACCUM COUNT "Top") EQ 2 THEN 
      LEAVE.               
END.

IF lPart[1] THEN DO: 
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 11 - Part One".
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 11 - Part Two".
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

