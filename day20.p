
/*------------------------------------------------------------------------
    File        : day20.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 20 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Jan 08 15:01:48 CET 2023
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
   FIELD iOriginal AS INTEGER 
   FIELD iNumber   AS INT64 
   FIELD iCurrent  AS INTEGER 
INDEX indOriginal IS UNIQUE iOriginal
INDEX indCurrent  IS UNIQUE iCurrent.

DEFINE BUFFER ttPrevNumber FOR ttNumber.
DEFINE BUFFER ttNextNumber FOR ttNumber.

DEFINE VARIABLE iMaxPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextOriginal  AS INTEGER NO-UNDO.
DEFINE VARIABLE iOldPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewPosition   AS INTEGER NO-UNDO.
DEFINE VARIABLE iStartPosition AS INTEGER NO-UNDO.
DEFINE VARIABLE iLeftNumber    AS INTEGER NO-UNDO.
DEFINE VARIABLE iRightNumber   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextNumber    AS INTEGER NO-UNDO.
DEFINE VARIABLE iMove          AS INTEGER NO-UNDO.
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
      ttNumber.iOriginal = iLine - 1
      ttNumber.iNumber   = INT64 (cLine)
      ttNumber.iCurrent  = ttNumber.iOriginal
   .
   iMaxPosition = ttNumber.iOriginal.
   
END. /* ReadBlock: */

IF lPart[1] THEN DO:
   iSolution = 0.

   IF lvlShow THEN DO:
      PUT STREAM sOutput UNFORMATTED 
         "Start: ".               
      iStartPosition = 0.
      DO iMove = 0 TO iMaxPosition:
         FIND  ttNumber
         WHERE ttNumber.iCurrent EQ iStartPosition.
         IF iMove EQ 0 THEN 
            PUT STREAM sOutput UNFORMATTED 
               "[".
         PUT STREAM sOutput UNFORMATTED 
            SUBSTITUTE ("&1", ttNumber.iNumber).
         IF iMove LT iMaxPosition THEN
            PUT STREAM sOutput UNFORMATTED 
               ", ".
         ELSE 
            PUT STREAM sOutput UNFORMATTED 
               "]" SKIP.
         iStartPosition = iStartPosition + 1.
         IF iStartPosition GT iMaxPosition THEN 
            iStartPosition = 0.   
      END.
   END.
   

   iNextOriginal = 0.
   REPEAT:
      lvcProgress[1] = SUBSTITUTE ("&1: &2/&3", STRING (TIME, "HH:MM:SS"), iNextOriginal + 1, iMaxPosition + 1).
      PAUSE 0 BEFORE-HIDE.
      DISPLAY
      lvcProgress
      WITH FRAME fr-Parameters.
      PROCESS EVENTS.
      
      FIND  ttNumber
      WHERE ttNumber.iOriginal EQ iNextOriginal.
      IF ttNumber.iNumber NE 0 THEN DO:
         /* We have a number to make the move */
         iOldPosition = ttNumber.iCurrent.
         iNewPosition = (ttNumber.iCurrent + ttNumber.iNumber) MOD iMaxPosition.
         
         IF iOldPosition GT iNewPosition THEN 
            iNewPosition = iNewPosition - 1.
         IF iNewPosition LT 0 THEN 
            iNewPosition = iMaxPosition.
             
         IF lvlShow THEN DO:
            IF lvlDebug THEN DO:
               MESSAGE SUBSTITUTE ("Move #&1 &2 from Old &3 to New &4", 
                           iNextOriginal, 
                           ttNumber.iNumber, 
                           iOldPosition, 
                           iNewPosition)
               VIEW-AS ALERT-BOX.
            END.
            ELSE DO:
               PUT STREAM sOutput UNFORMATTED 
                  SUBSTITUTE ("Move #&1 &2 from Old &3 to New &4", 
                              iNextOriginal, 
                              ttNumber.iNumber, 
                              iOldPosition, 
                              iNewPosition) SKIP.
            END.
         END.
                              
         iStartPosition = iOldPosition.
         
         SwitchBlock:
         REPEAT:
            /* SwitchBlock: */
            FIND  ttNextNumber
            WHERE ttNextNumber.iCurrent EQ iStartPosition NO-ERROR.
            IF NOT AVAILABLE ttNextNumber THEN DO:
               MESSAGE "No Next Number available for " iStartPosition
               VIEW-AS ALERT-BOX.
               IF lvlShow EQ FALSE THEN 
                  RUN sy\win\wbrowsett.w
                     (INPUT TEMP-TABLE ttNumber:HANDLE).
            END.
               
            IF iStartPosition EQ iOldPosition THEN 
               ttNextNumber.iCurrent = ?.
            ELSE DO: 
               ttNextNumber.iCurrent = ttNextNumber.iCurrent - 1.
               IF ttNextNumber.iCurrent LT 0 THEN 
                  ttNextNumber.iCurrent = iMaxPosition.
            END.
         
            IF iStartPosition EQ iNewPosition THEN
               LEAVE SwitchBlock.
                  
            iStartPosition = iStartPosition + 1.
            IF iStartPosition GT iMaxPosition THEN 
               iStartPosition = 0.
                  
         END. /* SwitchBlock: */
         
         ttNumber.iCurrent = iNewPosition.
         
      END.
      
      ASSIGN 
         iLeftNumber  = ttNumber.iCurrent - 1
         iRightNumber = ttNumber.iCurrent + 1
      .
      IF iLeftNumber LT 0 THEN 
         iLeftNumber = iMaxPosition.
      IF iRightNumber GT iMaxPosition THEN 
         iRightNumber = 0.
         
       
      IF lvlDebug THEN DO:
         FIND  ttPrevNumber
         WHERE ttPrevNumber.iCurrent EQ iLeftNumber NO-ERROR.
         IF AVAILABLE ttPrevNumber THEN 
            iLeftNumber = ttPrevNumber.iNumber.
         FIND  ttNextNumber
         WHERE ttNextNumber.iCurrent EQ iRightNumber NO-ERROR.
         IF AVAILABLE ttNextNumber THEN 
            iRightNumber = ttNextNumber.iNumber.

         MESSAGE SUBSTITUTE ("&1 moves between &2 (&3) and &4 (&5)",
                             ttNumber.iNumber,
                             iLeftNumber,
                             AVAILABLE ttPrevNumber,
                             iRightNumber,
                             AVAILABLE ttNextNumber)
         VIEW-AS ALERT-BOX.

      END.   

      IF lvlShow THEN DO:
         PUT STREAM sOutput UNFORMATTED
            SUBSTITUTE ("Moved number: #&1 &2", ttNumber.iOriginal, ttNumber.iNumber) SKIP.
         iStartPosition = iNewPosition + 1.
         IF iStartPosition GT iMaxPosition THEN 
            iStartPosition = 0.
         DO iMove = 0 TO iMaxPosition:
            FIND  ttNumber
            WHERE ttNumber.iCurrent EQ iStartPosition.
            IF iMove EQ 0 THEN 
               PUT STREAM sOutput UNFORMATTED 
                  "[".
            PUT STREAM sOutput UNFORMATTED 
               SUBSTITUTE ("&1", ttNumber.iNumber).
            IF iMove LT iMaxPosition THEN
               PUT STREAM sOutput UNFORMATTED 
                  ", ".
            ELSE 
               PUT STREAM sOutput UNFORMATTED 
                  "]" SKIP.
            iStartPosition = iStartPosition + 1.
            IF iStartPosition GT iMaxPosition THEN 
               iStartPosition = 0.   
         END.
      END.

      iNextOriginal = iNextOriginal + 1.
      IF iNextOriginal GT iMaxPosition THEN
         LEAVE.
      
      FOR EACH ttNumber
      WHERE ttNumber.iCurrent EQ ?:
         MESSAGE "NOK" SKIP 
         ttNumber.iOriginal SKIP 
         ttNumber.iNumber   SKIP 
         ttNumber.iCurrent  SKIP 
         VIEW-AS ALERT-BOX.
      END.
               
   END.
END.

IF lvlShow THEN DO:
   IF lvlDebug THEN DO:
      OUTPUT STREAM sOutput CLOSE.
   END.
END.

IF lPart[2] THEN DO:
   /* Part 2 */
   FOR EACH ttNumber:
      ASSIGN 
         ttNumber.iNumber  = ttNumber.iNumber * 811589153
         ttNumber.iCurrent = ttNumber.iOriginal
      .
   END.
   
   DO iRun = 1 TO 10:
      iNextOriginal = 0.
      REPEAT:
         FIND  ttNumber
         WHERE ttNumber.iOriginal EQ iNextOriginal.
         IF ttNumber.iNumber NE 0 THEN DO:
            /* We have a number to make the move */
            iOldPosition = ttNumber.iCurrent.
            iNewPosition = (ttNumber.iCurrent + ttNumber.iNumber) MOD iMaxPosition.
            
            IF iOldPosition GT iNewPosition THEN 
               iNewPosition = iNewPosition - 1.
            IF iNewPosition LT 0 THEN 
               iNewPosition = iMaxPosition.
                
            iStartPosition = iOldPosition.
            
            SwitchBlock:
            REPEAT:
               /* SwitchBlock: */
               FIND  ttNextNumber
               WHERE ttNextNumber.iCurrent EQ iStartPosition NO-ERROR.
               IF NOT AVAILABLE ttNextNumber THEN DO:
                  MESSAGE "No Next Number available for " iStartPosition
                  VIEW-AS ALERT-BOX.
                  IF lvlShow EQ FALSE THEN 
                     RUN sy\win\wbrowsett.w
                        (INPUT TEMP-TABLE ttNumber:HANDLE).
               END.
                  
               IF iStartPosition EQ iOldPosition THEN 
                  ttNextNumber.iCurrent = ?.
               ELSE DO: 
                  ttNextNumber.iCurrent = ttNextNumber.iCurrent - 1.
                  IF ttNextNumber.iCurrent LT 0 THEN 
                     ttNextNumber.iCurrent = iMaxPosition.
               END.
            
               IF iStartPosition EQ iNewPosition THEN
                  LEAVE SwitchBlock.
                     
               iStartPosition = iStartPosition + 1.
               IF iStartPosition GT iMaxPosition THEN 
                  iStartPosition = 0.
                     
            END. /* SwitchBlock: */
            
            ttNumber.iCurrent = iNewPosition.
            
         END.
         
   
         iNextOriginal = iNextOriginal + 1.
         IF iNextOriginal GT iMaxPosition THEN
            LEAVE.
         
      END.
      
   END. /* Mix 10 times */
   
END. /* Part 2 */

iSolution = 0.

FIND  ttNumber
WHERE ttNumber.iNumber EQ 0.
DO iNextNumber = 1 TO 3:
   FIND  ttNextNumber
   WHERE ttNextNumber.iCurrent EQ ((ttNumber.iCurrent + iNextNumber * 1000) MOD iMaxPosition).
   iSolution = iSolution + ttNextNumber.iNumber.
END.
   
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

