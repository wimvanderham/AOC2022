
/*------------------------------------------------------------------------
    File        : day17.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day 17 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 27 23:27:41 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/17".
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
DEFINE VARIABLE cJetPattern   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentJet   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCurrentJet   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentShape AS INTEGER   NO-UNDO.
DEFINE VARIABLE lviHWM        AS INT64     NO-UNDO.
DEFINE VARIABLE iRound        AS INT64     NO-UNDO.
DEFINE VARIABLE iCurrentLine  AS INT64     NO-UNDO.
DEFINE VARIABLE iMaxRound     AS INT64     NO-UNDO.
DEFINE VARIABLE cTopRows      AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttGrid
   FIELD iLineNr AS INT64 
   FIELD cLine   AS CHARACTER FORMAT "X(9)" /* Two borders and 7 spaces */
INDEX indLine IS UNIQUE iLineNr.

DEFINE TEMP-TABLE ttShape
   FIELD iShapeNr AS INTEGER 
   FIELD cLine    AS CHARACTER FORMAT "X(4)" EXTENT 4
   FIELD iShapeX  AS INTEGER 
   FIELD iShapeY  AS INTEGER 
INDEX indShapeNr IS UNIQUE iShapeNr. 
      
DEFINE TEMP-TABLE ttVisited
   FIELD cTopRows    AS CHARACTER 
   FIELD iShapeNr    AS INTEGER 
   FIELD iCurrentJet AS INTEGER
   FIELD iRound      AS INT64
   FIELD iHWM        AS INT64 
INDEX indVisited IS UNIQUE cTopRows iShapeNr iCurrentJet.

DEFINE VARIABLE ideltaRound    AS INT64 NO-UNDO.
DEFINE VARIABLE ideltaHWM      AS INT64 NO-UNDO.
DEFINE VARIABLE ideltaIncrease AS INT64 NO-UNDO.
DEFINE VARIABLE iaddHWM        AS INT64 NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION canMoveShape RETURNS LOGICAL 
(INPUT ipiShapeNr AS INTEGER,
 INPUT ipiMoveX   AS INTEGER,
 INPUT ipiMoveY   AS INTEGER) FORWARD.

FUNCTION getRowValue RETURNS INTEGER 
(INPUT ipcRow AS CHARACTER) FORWARD.

FUNCTION getTopRows RETURNS CHARACTER 
(INPUT ipiHWM  AS INT64,
 INPUT ipiRows AS INTEGER) FORWARD.

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

COPY-LOB FROM FILE "input\17.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\17_test.txt" TO OBJECT lcInput.
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

   cJetPattern = cLine.
      
END. /* ReadBlock: */

/* Create the 5 Shapes in the ttShape temp-table */
RUN createShapes.

/* Create the Empty Grid */
RUN createEmptyGrid.

IF  lvlDebug
AND lvlShow THEN DO:
   OUTPUT TO "output\17.txt".
   PUT UNFORMATTED 
      SUBSTITUTE ("Start run &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP.
   OUTPUT CLOSE.
      
   RUN printGrid
      (INPUT "output\17.txt",
       INPUT SUBSTITUTE ("Empty Grid, HWM: &1", lviHWM),
       INPUT 0).
END.
   
ASSIGN 
   iCurrentShape = 1
   iCurrentJet   = 1
.

IF lPart[1] THEN 
   iMaxRound = 2022.
IF lPart[2] THEN 
   iMaxRound = 1000000000000.
   
iRound = 1.
RoundBlock:
DO WHILE iRound LE iMaxRound:
      
   IF iRound MOD 100000 EQ 0 THEN DO:
      PAUSE 0.
      DISPLAY 
      STRING (NOW, "99-99-9999 HH:MM:SS") FORMAT "X(20)" LABEL "Now.." SKIP 
      iRound    FORMAT "z,zzz,zzz,zzz,zz9"               LABEL "Round" SKIP 
      iMaxRound FORMAT "z,zzz,zzz,zzz,zz9"               LABEL "Max.." SKIP
      WITH WIDTH 76 SIDE-LABELS 1 DOWN.
   END.
      
   iCurrentLine = lviHWM + 4.
   
   FIND  ttShape 
   WHERE ttShape.iShapeNr EQ iCurrentShape.
   ASSIGN
      /* Start Position is CurrentLine (3 empty lines above lviHWM) */ 
      ttShape.iShapeY = iCurrentLine
      /* and two units away from the left wall */
      ttShape.iShapeX = 3
   .
   
   IF  lvlDebug
   AND lvlShow THEN DO:
      RUN printGrid
         (INPUT "output\17.txt",
          INPUT SUBSTITUTE ("Round &1, Shape &2", iRound, iCurrentShape),
          INPUT iCurrentShape).
   END.

   IF iRound GT 10 THEN DO:
      cTopRows = getTopRows(lviHWM, 20).
      FIND  ttVisited
      WHERE ttVisited.cTopRows    = cTopRows
      AND   ttVisited.iCurrentJet = iCurrentJet
      AND   ttVisited.iShapeNr    = iCurrentShape NO-ERROR.
      IF AVAILABLE ttVisited THEN DO:
         /* Bingo! */
         IF  lvlDebug
         AND lvlShow THEN DO:
            MESSAGE "Found an already visited situation:" SKIP 
            "Top Rows:" cTopRows SKIP 
            "Current Jet:" iCurrentJet SKIP 
            "Shape Nr:" iCurrentShape SKIP(2) 
            "HWM:" ttVisited.iHWM SKIP 
            "Round:" ttVisited.iRound SKIP(2)
            "Current Round:" iRound SKIP 
            "Current HWM:" lviHWM
            VIEW-AS ALERT-BOX.
         END.
         ASSIGN 
            /* Fast Forward */
            ideltaRound    = iRound - ttVisited.iRound 
            ideltaHWM      = (lviHWM - ttVisited.iHWM)
            ideltaIncrease = INT64 (TRUNCATE ((iMaxRound - iRound) / ideltaRound, 0))
            iaddHWM        = iaddHWM + (ideltaIncrease * ideltaHWM)
            iRound         = iRound + ideltaIncrease * ideltaRound
         .
      END.
      ELSE DO:
         /* First time this situation */
         CREATE ttVisited.
         ASSIGN 
            ttVisited.cTopRows    = cTopRows
            ttVisited.iCurrentJet = iCurrentJet
            ttVisited.iShapeNr    = iCurrentShape
            /* Save current situation */
            ttVisited.iHWM        = lviHWM
            ttVisited.iRound      = iRound
         .
      END.
   END.
   
   MoveBlock:
   REPEAT:
      /* Apply current jet */
      cCurrentJet = SUBSTRING (cJetPattern, iCurrentJet, 1).
      CASE cCurrentJet:
         WHEN ">" THEN DO:
            IF canMoveShape(ttShape.iShapeNr, +1, 0) THEN 
               ttShape.iShapeX = ttShape.iShapeX + 1.
         END.
         WHEN "<" THEN DO:
            IF canMoveShape(ttShape.iShapeNr, -1, 0) THEN 
               ttShape.iShapeX = ttShape.iShapeX - 1.
         END.
      END CASE.
      
      IF lvlDebug
      AND lvlShow THEN DO:
         RUN printGrid
            (INPUT "output\17.txt",
             INPUT SUBSTITUTE ("After Jet: &1", cCurrentJet),
             INPUT iCurrentShape).
      END.
                
      /* Move Jet Pointer ahead or back to the beginning */
      iCurrentJet = iCurrentJet + 1.
      IF iCurrentJet GT LENGTH (cJetPattern) THEN 
         iCurrentJet = 1.
         
      IF canMoveShape(ttShape.iShapeNr, 0, -1) THEN DO:
         ttShape.iShapeY = ttShape.iShapeY - 1.
         
         IF  lvlDebug
         AND lvlShow THEN DO:
            RUN printGrid
               (INPUT "output\17.txt",
                INPUT SUBSTITUTE ("After Shape Move Shape Y &1", ttShape.iShapeY),
                INPUT iCurrentShape).
         END.
         
      END.
      ELSE DO:
         RUN fixShapeOnGrid
            (INPUT  ttShape.iShapeNr,
             OUTPUT lviHWM).
         iCurrentShape = iCurrentShape + 1.
         IF iCurrentShape GT 5 THEN 
            iCurrentShape = 1.

         IF  lvlDebug
         AND lvlShow THEN DO:
            RUN printGrid
               (INPUT "output\17.txt",
                INPUT SUBSTITUTE ("After Fix Shape on Grid HWM: &1", lviHWM),
                INPUT 0).
         END.

         /* Done with this shape, move on to the next */
         LEAVE MoveBlock.
      END.
      
   END. /* MoveBlock: */
   
   iRound = iRound + 1.
   
END. /* RoundBlock: */
      
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE).
END.                     
   
IF lPart[1] THEN DO:
   FOR EACH ttGrid
   WHERE ttGrid.cLine MATCHES "*#*"
   BY ttGrid.iLineNr DESCENDING:
      iSolution = ttGrid.iLineNr.
      LEAVE.
   END. 
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 17 - Part One".
END.

IF lPart[2] THEN DO:
   
   iSolution = lviHWM + iaddHWM.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 17 - Part Two".
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

PROCEDURE createEmptyGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE iNewLineNr AS INTEGER NO-UNDO.

   DO iNewLineNr = 0 TO lviHWM + 8:
      CREATE ttGrid.
      ASSIGN 
         ttGrid.iLineNr = iNewLineNr
      .
      IF iLineNr GT 0 THEN 
         ttGrid.cLine = "|.......|".
      ELSE   
         ttGrid.cLine = "+-------+".
   END.

END PROCEDURE.

PROCEDURE createShapes:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   CREATE ttShape.
   ASSIGN 
      ttShape.iShapeNr = 1
      ttShape.cLine[1] = "@@@@"
   .
   
   CREATE ttShape.
   ASSIGN 
      ttShape.iShapeNr = 2
      ttShape.cLine[3] = " @  "
      ttShape.cLine[2] = "@@@ "
      ttShape.cLine[1] = " @  "
   .
   
   CREATE ttShape.
   ASSIGN 
      ttShape.iShapeNr = 3
      ttShape.cLine[3] = "  @ "
      ttShape.cLine[2] = "  @ "
      ttShape.cLine[1] = "@@@ "
   .
   
   CREATE ttShape.
   ASSIGN 
      ttShape.iShapeNr = 4
      ttShape.cLine[4] = "@   "
      ttShape.cLine[3] = "@   "
      ttShape.cLine[2] = "@   "
      ttShape.cLine[1] = "@   "
   .
   
   CREATE ttShape.
   ASSIGN 
      ttShape.iShapeNr = 5
      ttShape.cLine[2] = "@@  "
      ttShape.cLine[1] = "@@  "
   .

END PROCEDURE.

PROCEDURE fixShapeOnGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiShapeNr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiNewHWM  AS INT64   NO-UNDO.

DEFINE BUFFER ttShape FOR ttShape.

DEFINE VARIABLE iCheckX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheckY    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cShapeLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShapeChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE iHWM       AS INT64     NO-UNDO.

   FIND  ttShape
   WHERE ttShape.iShapeNr EQ ipiShapeNr.
   
   opiNewHWM = lviHWM.
   DO iCheckY = 1 TO 4:
      cShapeLine = ttShape.cLine[iCheckY].      
      DO iCheckX = 1 TO 4:      
         cShapeChar = SUBSTRING (cShapeLine, iCheckX, 1).
         IF cShapeChar EQ "@" THEN DO:
            /* Change moving Shape Char to solid rock on the grid */
            FIND  ttGrid
            WHERE ttGrid.iLineNr EQ ttShape.iShapeY + iCheckY - 1.
            IF SUBSTRING (ttGrid.cLine, ttShape.iShapeX + iCheckX, 1) EQ "." THEN DO:
               /* Replace . for # */
               SUBSTRING (ttGrid.cLine, ttShape.iShapeX + iCheckX, 1) = "#".
               IF ttGrid.iLineNr GT opiNewHWM THEN 
                  opiNewHWM = ttGrid.iLineNr.
            END.
            ELSE DO:
               MESSAGE 
               SUBSTITUTE ("Error in Round &1", iRound) SKIP 
               SUBSTITUTE ("Found '&1' where '.' was expected... Line: &2 Column: &3",
                           SUBSTRING (ttGrid.cLine, ttShape.iShapeX + iCheckX, 1),
                           ttGrid.iLineNr,
                           ttShape.iShapeX + iCheckX)
               VIEW-AS ALERT-BOX.
            END.
         END.
      END.
   END.

   DO iHWM = lviHWM TO opiNewHWM + 8:
      /* Expand Grid */
      FIND  ttGrid
      WHERE ttGrid.iLineNr EQ iHWM NO-ERROR.
      IF NOT AVAILABLE ttGrid THEN DO:
         CREATE ttGrid.
         ASSIGN 
            ttGrid.iLineNr = iHWM
            ttGrid.cLine   = "|.......|"
         .
      END.
   END. /* Expand Grid */
       
END PROCEDURE.

PROCEDURE printGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTitolo     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiShapeNr    AS INTEGER   NO-UNDO.

DEFINE BUFFER ttShape FOR ttShape.

DEFINE VARIABLE lInit AS LOGICAL NO-UNDO.

DEFINE VARIABLE iChar      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iShapeX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iShapeY    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cShapeLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShapeChar AS CHARACTER NO-UNDO.

   OUTPUT TO VALUE (ipcOutputFile) APPEND.
   
   lInit = FALSE.
   FIND  ttShape
   WHERE ttShape.iShapeNr EQ ipiShapeNr NO-ERROR.
      
   PUT UNFORMATTED 
      ipcTitolo SKIP.
            
   FOR EACH ttGrid
   BY ttGrid.iLineNr DESCENDING:
      IF ttGrid.cLine NE "|.......|" THEN 
         lInit = FALSE.
      IF NOT lInit THEN DO:
         PUT UNFORMATTED 
            SUBSTITUTE ("&1. ", STRING (ttGrid.iLineNr, "zzz,zzz,zz9")).
         DO iChar = 1 TO LENGTH (ttGrid.cLine):
            cChar = SUBSTRING (ttGrid.cLine, iChar, 1).
            iShapeX = iChar - 1.
            IF  AVAILABLE ttShape
            AND ttGrid.iLineNr LE ttShape.iShapeY + 3
            AND ttGrid.iLineNr GE ttShape.iShapeY 
            AND iShapeX GE ttShape.iShapeX 
            AND iShapeX LE ttShape.iShapeX + 3 THEN DO:
               /* This Grid position is within the Shape square */
               cShapeLine = ttShape.cLine[ttGrid.iLineNr - ttShape.iShapeY + 1].
               cShapeChar = SUBSTRING (cShapeLine, iChar - ttShape.iShapeX, 1).
               IF cShapeChar NE "" THEN 
                  PUT UNFORMATTED cShapeChar.
               ELSE 
                  PUT UNFORMATTED cChar.
            END.
            ELSE 
               PUT UNFORMATTED cChar.
         END.
         PUT UNFORMATTED SKIP.
      END.                            
   END.                     
            
   OUTPUT CLOSE.
         
END PROCEDURE.

   
/* ************************  Function Implementations ***************** */

FUNCTION canMoveShape RETURNS LOGICAL 
(INPUT ipiShapeNr AS INTEGER,
 INPUT ipiMoveX   AS INTEGER,
 INPUT ipiMoveY   AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/   

DEFINE BUFFER ttShape FOR ttShape.

DEFINE VARIABLE iNewStartX AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewStartY AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheckX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheckY    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cShapeLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShapeChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCanMove   AS LOGICAL   NO-UNDO.

   lCanMove = TRUE.
   
   FIND  ttShape
   WHERE ttShape.iShapeNr EQ ipiShapeNr.

   ASSIGN 
      iNewStartX = ttShape.iShapeX + ipiMoveX
      iNewStartY = ttShape.iShapeY + ipiMoveY
   .
   
   CheckBlock:
   DO iCheckY = 1 TO 4:
      cShapeLine = ttShape.cLine[iCheckY].
      DO iCheckX = 1 TO 4:
         cShapeChar = SUBSTRING (cShapeLine, iCheckX, 1).
         IF cShapeChar EQ "@" THEN DO:
            /* Check if the shape's character can move to this position */
            FIND  ttGrid 
            WHERE ttGrid.iLineNr EQ (iCheckY + iNewStartY - 1)  NO-ERROR.
            IF NOT AVAILABLE ttGrid THEN DO:
               MESSAGE "Didn't find the Grid line for line: " (iCheckY + iNewStartY - 1) 
               VIEW-AS ALERT-BOX.
               CREATE ttGrid.
               ASSIGN 
                  ttGrid.iLineNr = (iCheckY + iNewStartY - 1)
                  ttGrid.cLine   = "|.......|"
               .
            END.
            IF SUBSTRING (ttGrid.cLine, iCheckX + iNewStartX, 1) NE "." THEN DO:
               lCanMove = FALSE.
               LEAVE CheckBlock.
            END.
         END.
      END.
   END.
       
   RETURN lCanMove.
      
END FUNCTION.

FUNCTION getRowValue RETURNS INTEGER 
(INPUT ipcRow AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Transform input row (with . and #) into a number 
          considering # as 1 and . as 0 
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iValue AS INTEGER NO-UNDO.
DEFINE VARIABLE iBase  AS INTEGER NO-UNDO.
DEFINE VARIABLE iChar  AS INTEGER NO-UNDO.
   
   ipcRow = TRIM (ipcRow, "|").
   
   iBase = 1.
   DO iChar = LENGTH (ipcRow) TO 1 BY -1:
      IF SUBSTRING (ipcRow, iChar, 1) EQ "#" THEN 
         iValue = iValue + iBase.
      iBase = iBase * 2.
   END.
   
   RETURN iValue.
      
END FUNCTION.

FUNCTION getTopRows RETURNS CHARACTER 
(INPUT ipiHWM  AS INT64,
 INPUT ipiRows AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the n Top Rows in condensed HEX format
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttTopGrid FOR ttGrid.

DEFINE VARIABLE iRow     AS INTEGER NO-UNDO.
DEFINE VARIABLE cTopRows AS CHARACTER NO-UNDO.

   TopGridBlock:
   FOR EACH ttTopGrid
   WHERE ttTopGrid.iLineNr LE ipiHWM
   BY ttTopGrid.iLineNr DESCENDING:
      cTopRows = SUBSTITUTE ("&1&2&3",
                             cTopRows,
                             (IF cTopRows NE "" THEN "," ELSE ""),
                             getRowValue(ttTopGrid.cLine)).
      iRow = iRow + 1.
      IF iRow GE ipiRows THEN
         LEAVE TopGridBlock.                             
   END.                          
                           
   RETURN cTopRows.                                   

END FUNCTION.

