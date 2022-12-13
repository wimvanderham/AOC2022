
/*------------------------------------------------------------------------
    File        : day13.p
    Purpose     : 

    Syntax      :

    Description : Soliution to Day 13 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 13 07:40:36 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/13".
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

/* Specific */
DEFINE VARIABLE cLists   AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE iList    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOrdered AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttList
   FIELD lLeft  AS LOGICAL 
   FIELD iLevel AS INTEGER 
   FIELD iSub   AS INTEGER 
   FIELD lStart AS LOGICAL
   FIELD cList  AS CHARACTER FORMAT "X(50)"
   FIELD lEnd   AS LOGICAL
INDEX indSideLevel IS UNIQUE lLeft iLevel iSub.

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

COPY-LOB FROM FILE "input\13.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\13_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

ASSIGN 
   iList     = 0
   iSolution = 0
.
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      RUN checkLists
         (INPUT  cLists,
          OUTPUT lOrdered).
      IF lvlDebug THEN DO:
         MESSAGE cLists[1] SKIP 
         cLists[2] SKIP 
         "Ordered?" lOrdered
         VIEW-AS ALERT-BOX.
      END.
      IF lOrdered THEN 
         iSolution = iSolution + 1.
      iList = 0.          
      NEXT ReadBlock.
   END.

   iList = iList + 1.
   ASSIGN 
      cLists[iList] = cLine
   .

END. /* ReadBlock: */

         
IF lvlShow THEN DO:
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 13 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
          
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 13 - Part Two".
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

PROCEDURE checkLists:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcLists   AS CHARACTER NO-UNDO EXTENT 2.
DEFINE OUTPUT PARAMETER oplOrdered AS LOGICAL   NO-UNDO.

DEFINE VARIABLE iSide AS INTEGER   NO-UNDO.
DEFINE VARIABLE iChar AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

DEFINE VARIABLE iNewLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewSub   AS INTEGER NO-UNDO.
DEFINE VARIABLE lNewStart AS LOGICAL NO-UNDO.
DEFINE VARIABLE lNewEnd   AS LOGICAL NO-UNDO.

DEFINE BUFFER ttLeft  FOR ttList.
DEFINE BUFFER ttRight FOR ttList.
DEFINE BUFFER ttSub   FOR ttList.

   EMPTY TEMP-TABLE ttList.
   
   DO iSide = 1 TO 2:
      ASSIGN 
         iNewLevel = 1
         iNewSub   = 1
         lNewStart = FALSE 
         lNewEnd   = FALSE
      .
      CREATE ttList.
      ASSIGN 
         ttList.lLeft  = iSide EQ 1
         ttList.iLevel = iNewLevel
         ttList.iSub   = iNewSub
         ttList.cList  = ipcLists[iSide]
      .
      
      ParseList:
      DO iChar = 1 TO LENGTH (ttList.cList):
         cChar = SUBSTRING (ttList.cList, iChar, 1).
         IF cChar EQ "[" THEN DO:
            ASSIGN 
               iNewLevel = iNewLevel + 1
            .
            FIND LAST ttSub
            WHERE ttSub.lLeft  EQ ttList.lLeft
            AND   ttSub.iLevel EQ iNewLevel NO-ERROR.
            IF AVAILABLE ttSub THEN
            DO:
               iNewSub = ttSub.iSub + 1.
            END.
            ELSE DO:
               iNewSub = 1.
            END.
            lNewStart = TRUE.
            CREATE ttSub.
            ASSIGN 
               ttSub.lLeft  = ttList.lLeft
               ttSub.iLevel = iNewLevel
               ttSub.iSub   = iNewSub
               ttSub.lStart = lNewStart
            .
            lNewStart = FALSE.
            NEXT ParseList.
         END.
         IF cChar EQ "]" THEN DO:
            /* Close current list */
            ASSIGN 
               lNewEnd = TRUE
            .
            FIND LAST ttSub
            WHERE ttSub.lLeft  EQ ttList.lLeft
            AND   ttSub.iLevel EQ iNewLevel 
            AND   ttSub.lStart EQ TRUE NO-ERROR.
            IF AVAILABLE ttSub THEN
            DO:
               FIND  ttSub
               WHERE ttSub.lLeft  EQ ttList.lLeft
               AND   ttSub.iLevel EQ iNewLevel
               AND   ttSub.iSub   EQ iNewSub NO-ERROR.
               IF AVAILABLE ttSub THEN
               DO:
                  ASSIGN
                     ttSub.lEnd = lNewEnd
                     lNewEnd    = FALSE
                  .
               END.
            END.
            iNewLevel = iNewLevel - 1.
            
            FIND LAST ttSub
            WHERE ttSub.lLeft  EQ ttList.lLeft
            AND   ttSub.iLevel EQ iNewLevel NO-ERROR.
            IF AVAILABLE ttSub THEN
            DO:
               iNewSub = ttSub.iSub + 1.
            END.
            ELSE DO:
               iNewSub = 1.
            END.
            NEXT ParseList.
         END.
         IF cChar EQ "," THEN
         DO:
            iNewSub = iNewSub + 1.
            NEXT ParseList.
         END.
         FIND ttSub
         WHERE ttSub.lLeft  EQ ttList.lLeft
         AND   ttSub.iLevel EQ iNewLevel
         AND   ttSub.iSub   EQ iNewSub NO-ERROR.
         IF NOT AVAILABLE ttSub THEN DO:
            CREATE ttSub.
            ASSIGN 
               ttSub.lLeft  = ttList.lLeft
               ttSub.iLevel = iNewLevel
               ttSub.iSub   = iNewSub
               ttSub.lStart = lNewStart
            .
            lNewStart = FALSE.
         END.
         ASSIGN 
            ttSub.cList = ttSub.cList + cChar
            ttSub.lEnd  = lNewEnd
         .
         lNewEnd = FALSE.
         
      END.
   END.

   /* Fix mixed types by increasing level */
   /* First left side non-lists */
   FOR EACH ttLeft
   WHERE ttLeft.lLeft EQ TRUE
   AND   ttLeft.iLevel GT 1
   BREAK
   BY ttLeft.iLevel DESCENDING:
      FIND FIRST ttRight
      WHERE ttRight.lLeft  EQ FALSE
      AND   ttRight.iLevel EQ ttLeft.iLevel NO-ERROR.
      IF NOT AVAILABLE ttRight THEN
      DO:
         ttLeft.iLevel = ttLeft.iLevel + 1.
      END.
   END.
   
   /* Then right side non-lists */
   FOR EACH ttRight
   WHERE ttRight.lLeft  EQ FALSE 
   AND   ttRight.iLevel GT 1
   BREAK
   BY ttRight.iLevel DESCENDING:
      FIND FIRST ttLeft
      WHERE ttLeft.lLeft  EQ TRUE
      AND   ttLeft.iLevel EQ ttRight.iLevel NO-ERROR.
      IF NOT AVAILABLE ttLeft THEN
      DO:
         ttRight.iLevel = ttRight.iLevel + 1.
      END.
   END.
   
   oplOrdered = TRUE.
   CheckBlock:
   FOR EACH ttLeft
   WHERE ttLeft.lLeft  EQ TRUE
   AND   ttLeft.iLevel GT 1
   BREAK
   BY ttLeft.iLevel DESCENDING
   BY ttLeft.iSub:
   
      FIND  ttRight
      WHERE ttRight.lLeft  EQ FALSE 
      AND   ttRight.iLevel EQ ttLeft.iLevel
      AND   ttRight.iSub   EQ ttLeft.iSub NO-ERROR.
      IF NOT AVAILABLE ttRight THEN
      DO:
         oplOrdered = FALSE.
         LEAVE CheckBlock.
      END.
      IF AVAILABLE ttRight THEN
      DO:
         IF INTEGER(ttLeft.cList) NE INTEGER(ttRight.cList) THEN
         DO:
            IF INTEGER(ttLeft.cList) GT INTEGER(ttRight.cList) THEN
            DO:
               oplOrdered = FALSE.
            END.
            LEAVE CheckBlock.            
         END.
      END.
   END.
   
END PROCEDURE.
   
/* ************************  Function Implementations ***************** */
