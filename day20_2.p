
/*------------------------------------------------------------------------
    File        : day20_2.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 20 with Linked List - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/20".
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
DEFINE VARIABLE lvcProgress  AS CHARACTER NO-UNDO EXTENT 2 FORMAT "X(60)".
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPair        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPair        AS CHARACTER NO-UNDO.
DEFINE STREAM   sOutput.

/* Specific */
DEFINE TEMP-TABLE ttNumber
   FIELD iOriginal     AS INTEGER 
   FIELD iNumber       AS INT64 
   FIELD iPrevOriginal AS INTEGER 
   FIELD iNextOriginal AS INTEGER
INDEX indOriginal IS UNIQUE iOriginal.

DEFINE BUFFER ttPrevNumber FOR ttNumber.
DEFINE BUFFER ttNextNumber FOR ttNumber.
DEFINE BUFFER ttMoveNumber FOR ttNumber.

DEFINE VARIABLE iMaxPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextOriginal  AS INTEGER NO-UNDO.
DEFINE VARIABLE iOldPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iStartPosition AS INTEGER NO-UNDO.
DEFINE VARIABLE iLeftNumber    AS INTEGER NO-UNDO.
DEFINE VARIABLE iRightNumber   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextNumber    AS INTEGER NO-UNDO.
DEFINE VARIABLE iMove          AS INTEGER NO-UNDO.
DEFINE VARIABLE iRuns          AS INTEGER NO-UNDO.
DEFINE VARIABLE iRun           AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */
DISPLAY
   lOpenURL          LABEL "Open URL?"     VIEW-AS TOGGLE-BOX SKIP 
   lPart[1]          LABEL "Solve Part 1?" VIEW-AS TOGGLE-BOX SKIP
   lPart[2]          LABEL "Solve Part 2?" VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug          LABEL "Debug?"        VIEW-AS TOGGLE-BOX SKIP 
   lvlShow           LABEL "Show?"         VIEW-AS TOGGLE-BOX SKIP(2)
   lvcProgress[1] NO-LABELS SKIP 
   lvcProgress[2] NO-LABELS SKIP 
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

COPY-LOB FROM FILE "input\20.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\20_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

IF lvlShow THEN DO:
   IF lvlDebug EQ FALSE THEN DO:
      OUTPUT STREAM sOutput TO "output\20.txt" UNBUFFERED.
      PUT STREAM sOutput UNFORMATTED 
         SUBSTITUTE ("Start run &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP.
   END.
END.
 
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   CREATE ttNumber.
   ASSIGN 
      /* Force a 0-start numbering to use MOD */
      ttNumber.iOriginal     = iLine - 1
      ttNumber.iNumber       = INT64 (cLine)
      ttNumber.iNumber       = ttNumber.iNumber * 811589153 WHEN lPart[2]
      ttNumber.iPrevOriginal = ttNumber.iOriginal - 1
      ttNumber.iNextOriginal = ttNumber.iOriginal + 1
   .

   /* Keep track of Maximum */
   iMaxPosition = ttNumber.iOriginal.
   
END. /* ReadBlock: */

/* Ajust first and last number */
FIND  ttNumber
WHERE ttNumber.iPrevOriginal LT 0.
ttNumber.iPrevOriginal = iMaxPosition.
FIND  ttNumber
WHERE ttNumber.iNextOriginal GT iMaxPosition.
ttNumber.iNextOriginal = 0.
 
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttNumber:HANDLE).
END.

ASSIGN 
   iRuns = 1
   iRuns = 10 WHEN lPart[2]
.

DO iRun = 1 TO iRuns:
   /* Number of Runs */
   iNextOriginal = 0.
   REPEAT:
      IF lvlShow THEN DO:
         lvcProgress[1] = SUBSTITUTE ("&1: &2/&3", STRING (TIME, "HH:MM:SS"), iNextOriginal + 1, iMaxPosition + 1).
         PAUSE 0 BEFORE-HIDE.
         DISPLAY
         lvcProgress
         WITH FRAME fr-Parameters.
         PROCESS EVENTS.
      END.
            
      FIND  ttNumber
      WHERE ttNumber.iOriginal EQ iNextOriginal.
      IF ttNumber.iNumber NE 0 THEN DO:
         /* We have a number to make the move */
         
         /* Remove the Number from the linked list */
         FIND  ttPrevNumber
         WHERE ttPrevNumber.iOriginal EQ ttNumber.iPrevOriginal.
         FIND  ttNextNumber
         WHERE ttNextNumber.iOriginal EQ ttNumber.iNextOriginal.
         ASSIGN 
            ttPrevNumber.iNextOriginal = ttNextNumber.iOriginal
            ttNextNumber.iPrevOriginal = ttPrevNumber.iOriginal
         .
         
         ASSIGN 
            iLeftNumber  = ttNumber.iPrevOriginal
            iRightNumber = ttNumber.iNextOriginal
         .
         DO iMove = 1 TO ABSOLUTE (ttNumber.iNumber) MOD iMaxPosition:
            IF ttNumber.iNumber LT 0 THEN
               /* Move Left */ 
               FIND  ttMoveNumber
               WHERE ttMoveNumber.iOriginal EQ iLeftNumber.
            ELSE 
               /* Move Right */
               FIND  ttMoveNumber
               WHERE ttMoveNumber.iOriginal EQ iRightNumber.
            
            ASSIGN 
               iLeftNumber  = ttMoveNumber.iPrevOriginal
               iRightNumber = ttMoveNumber.iNextOriginal
            .
         END.
         
         IF ttNumber.iNumber GT 0 THEN DO:
            /* Moved right, insert after */
            FIND  ttPrevNumber
            WHERE ttPrevNumber.iOriginal EQ ttMoveNumber.iOriginal.
            FIND  ttNextNumber
            WHERE ttNextNumber.iOriginal EQ ttMoveNumber.iNextOriginal.
         END.
         ELSE DO:
            /* Moved left, insert before */
            FIND  ttNextNumber
            WHERE ttNextNumber.iOriginal EQ ttMoveNumber.iOriginal.
            FIND  ttPrevNumber
            WHERE ttPrevNumber.iOriginal EQ ttMoveNumber.iPrevOriginal.
         END.
         
         /* Do the insertion */
         ASSIGN 
            ttPrevNumber.iNextOriginal = ttNumber.iOriginal
            ttNextNumber.iPrevOriginal = ttNumber.iOriginal
            ttNumber.iPrevOriginal     = ttPrevNumber.iOriginal
            ttNumber.iNextOriginal     = ttNextNumber.iOriginal
         .
      END. /* We have a number to make the move */
      
      iNextOriginal = iNextOriginal + 1.
      IF iNextOriginal GT iMaxPosition THEN
         LEAVE.
      
   END.
END. /* Number of Runs */

IF lvlShow THEN DO:
   IF lvlDebug THEN DO:
      OUTPUT STREAM sOutput CLOSE.
   END.
END.

/* Calculate Solution */
iSolution = 0.

FIND  ttNumber
WHERE ttNumber.iNumber EQ 0.
ASSIGN 
   iRightNumber = ttNumber.iNextOriginal
.

DO iNextNumber = 1 TO 3000:
   /* Move forward to the 1000th, 2000th and 3000th number */ 
   FIND  ttMoveNumber
   WHERE ttMoveNumber.iOriginal EQ iRightNumber.
   
   CASE iNextNumber:
      WHEN 1000 OR 
      WHEN 2000 OR 
      WHEN 3000 THEN
         iSolution = iSolution + ttMoveNumber.iNumber.
   END CASE.

   iRightNumber = ttMoveNumber.iNextOriginal.
   
END. /* Move forward to the 1000th, 2000th and 3000th number */
   
   
IF lPart[1] THEN DO:
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 20 - Part One".
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttNumber:HANDLE).
   END.
   
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 20 - Part Two".
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
      OUTPUT CLOSE.
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttNumber:HANDLE).
   END.
   RETURN.      
END CATCH.

/* **********************  Internal Procedures  *********************** */

   
/* ************************  Function Implementations ***************** */

