
/*------------------------------------------------------------------------
    File        : day22.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 22 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Jan 15 22:30:33 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/22".
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
DEFINE TEMP-TABLE ttGrid
   FIELD iRow  AS INTEGER 
   FIELD iCol  AS INTEGER 
   FIELD cType AS CHARACTER 
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indColRow IS UNIQUE iCol iRow.

DEFINE TEMP-TABLE ttStep
   FIELD iRow  AS INTEGER 
   FIELD iCol  AS INTEGER 
   FIELD cType AS CHARACTER 
INDEX indRowCol IS UNIQUE iRow iCol.

DEFINE TEMP-TABLE ttNextGrid
   FIELD iRow           AS INTEGER 
   FIELD iCol           AS INTEGER 
   FIELD cDirection     AS CHARACTER 
   FIELD iNextRow       AS INTEGER 
   FIELD iNextcol       AS INTEGER 
   FIELD cNextDirection AS CHARACTER 
INDEX indRowColDir IS UNIQUE iRow iCol cDirection.

DEFINE VARIABLE cSection        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lviRow          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lviCol          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lvcInstructions AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInstruction    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInstruction    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSteps          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStep           AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDirections     AS CHARACTER NO-UNDO INITIAL ">,v,<,^".
DEFINE VARIABLE cDirection      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDeltaRow       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDeltaCol       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNextGrid       AS LOGICAL   NO-UNDO.
   
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

COPY-LOB FROM FILE "input\22.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\22_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

cSection = "Grid".
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = ENTRY (iLine, lcInput, "~n").
   
   IF TRIM (cLine) EQ "" THEN DO:
      IF cSection = "Grid" THEN 
         cSection = "Instructions".
      NEXT ReadBlock.
   END.

   IF cSection = "Grid" THEN DO:
      DO iChar = 1 TO LENGTH (cLine):
         CREATE ttGrid.
         ASSIGN 
            ttGrid.iRow  = iLine
            ttGrid.iCol  = iChar
            ttGrid.cType = SUBSTRING (cLine, iChar, 1)
         .
      END.      
   END.
   IF cSection = "Instructions" THEN DO:
      lvcInstructions = cLine.
      lvcInstructions = REPLACE (lvcInstructions, "R", ",R,").
      lvcInstructions = REPLACE (lvcInstructions, "L", ",L,").
   END.
END. /* ReadBlock: */

IF lPart[2] THEN DO:
   RUN createCube.
END.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE).
   MESSAGE lvcInstructions
   VIEW-AS ALERT-BOX.
   IF lPart[2] THEN 
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttNextGrid:HANDLE).            
END.                     

ASSIGN 
   lviRow = 1
.
FIND FIRST ttGrid
WHERE ttGrid.iRow  EQ lviRow
AND   ttGrid.cType EQ ".".
lviCol = ttGrid.iCol.
cDirection = ">".
CREATE ttStep.
ASSIGN
   ttStep.iRow  = lviRow
   ttStep.iCol  = lviCol
   ttStep.cType = cDirection
.
InstructionBlock:
DO iInstruction = 1 TO NUM-ENTRIES (lvcInstructions):
   cInstruction = ENTRY (iInstruction, lvcInstructions).
   CASE cInstruction:
      WHEN "R" THEN
         IF LOOKUP (cDirection, cDirections) LT NUM-ENTRIES (cDirections) THEN 
            cDirection = ENTRY (LOOKUP (cDirection, cDirections) + 1, cDirections).
         ELSE 
            cDirection = ENTRY (1, cDirections).
      WHEN "L" THEN 
         IF LOOKUP (cDirection, cDirections) EQ 1 THEN 
            cDirection = ENTRY (NUM-ENTRIES (cDirections), cDirections).
         ELSE 
            cDirection = ENTRY (LOOKUP (cDirection, cDirections) - 1, cDirections).
      OTHERWISE DO:
         iSteps = INTEGER (cInstruction).
         StepBlock:
         DO iStep = 1 TO iSteps:
            lNextGrid = FALSE.
            CASE cDirection:
               /* Use direction to set deltaRow and deltaCol */
               WHEN ">" THEN 
                  ASSIGN
                     iDeltaRow = 0
                     iDeltaCol = +1
                  .
               WHEN "v" THEN 
                  ASSIGN 
                     iDeltaRow = +1
                     iDeltaCol = 0
                  .
               WHEN "<" THEN
                  ASSIGN 
                     iDeltaRow = 0
                     iDeltaCol = -1
                  .
               WHEN "^" THEN 
                  ASSIGN 
                     iDeltaRow = -1
                     iDeltaCol = 0
                  .
            END CASE. /* Use direction to set deltaRow and deltaCol */
            
            FIND  ttGrid
            WHERE ttGrid.iRow EQ lviRow + iDeltaRow
            AND   ttGrid.iCol EQ lviCol + iDeltaCol NO-ERROR.
            IF NOT AVAILABLE ttGrid 
            OR ttGrid.cType EQ " " THEN DO:
               IF lPart[1] THEN DO:
                  /* Part 1, rotate around to the other side */
                  IF iDeltaRow EQ 0 THEN 
                     IF iDeltaCol LT 0 THEN 
                        FIND LAST ttGrid 
                        WHERE ttGrid.iRow  EQ lviRow
                        AND   ttGrid.cType NE "".
                     ELSE 
                        FIND FIRST ttGrid
                        WHERE ttGrid.iRow  EQ lviRow
                        AND   ttGrid.cType NE "".
                  IF iDeltaCol EQ 0 THEN 
                     IF iDeltaRow LT 0 THEN 
                        FIND LAST ttGrid
                        WHERE ttGrid.iCol  EQ lviCol
                        AND   ttGrid.cType NE "".
                     ELSE 
                        FIND FIRST ttGrid
                        WHERE ttGrid.iCol  EQ lviCol
                        AND   ttGrid.cType NE "".
               END. /* Part 1, rotate around to the other side */
               IF lPart[2] THEN DO:
                  /* Part 2, treat the map as a cube */
                  FIND  ttNextGrid
                  WHERE ttNextGrid.iRow       EQ lviRow
                  AND   ttNextGrid.iCol       EQ lviCol
                  AND   ttNextGrid.cDirection EQ cDirection NO-ERROR.
                  IF AVAILABLE ttNextGrid THEN DO:
                     FIND  ttGrid
                     WHERE ttGrid.iRow EQ ttNextGrid.iNextRow
                     AND   ttGrid.iCol EQ ttNextGrid.iNextcol.
                     lNextGrid = TRUE.
                  END. 
               END. /* Part 2, treat the map as a cube */
            END.                        
            FIND  ttStep
            WHERE ttStep.iRow EQ lviRow
            AND   ttStep.iCol EQ lviCol NO-ERROR.
            IF NOT AVAILABLE ttStep THEN DO:
               CREATE ttStep.
               ASSIGN 
                  ttStep.iRow  = lviRow
                  ttStep.iCol  = lviCOl
               .
            END.
            ttStep.cType = cDirection.
            IF ttGrid.cType EQ "#" THEN DO:
               /* Wall, exit Steps */
               LEAVE StepBlock.
            END.
            ASSIGN 
               lviRow = ttGrid.iRow
               lviCol = ttGrid.iCol
            .
            IF lNextGrid THEN   
               cDirection = ttNextGrid.cNextDirection. 
         END. /* Stepblock */ 
      END. /* Otherwise */
   END CASE. /* Instruction */
   FIND  ttStep
   WHERE ttStep.iRow EQ lviRow
   AND   ttStep.iCol EQ lviCol NO-ERROR.
   IF NOT AVAILABLE ttStep THEN DO:
      CREATE ttStep.
      ASSIGN 
         ttStep.iRow  = lviRow
         ttStep.iCol  = lviCOl
      .
   END.
   ttStep.cType = cDirection.
            
   IF lvlShow THEN DO:
      RUN showGrid
         (INPUT SUBSTITUTE ("output\22_&1.txt", iInstruction),
          INPUT SUBSTITUTE ("&1 of (&2): &3 &4", iInstruction, lvcInstructions, cInstruction, cDirection)).
   END.
END. /* Instruction block */

iSolution = lviRow * 1000 + 4 * lviCol + LOOKUP (cDirection, cDirections) - 1.
             
IF lPart[1] THEN DO: 
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 22 - Part One".
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 22 - Part Two".
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

PROCEDURE showGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTitle    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRow  AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol  AS INTEGER NO-UNDO.

DEFINE VARIABLE iMaxRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxCol AS INTEGER NO-UNDO.

   FOR EACH ttGrid:
      ACCUM ttGrid.iRow (MAXIMUM).
      ACCUM ttGrid.iCol (MAXIMUM).
   END.
   ASSIGN 
      iMaxRow = (ACCUM MAXIMUM ttGrid.iRow)
      iMaxCol = (ACCUM MAXIMUM ttGrid.iCol)
   .

   OUTPUT TO VALUE (ipcFileName).
   PUT UNFORMATTED 
      ipcTitle SKIP (2).
      
   DO iRow = 1 TO iMaxRow:
      cLine = "".
      DO iCol = 1 TO iMaxCol:
         FIND  ttGrid 
         WHERE ttGrid.iRow EQ iRow
         AND   ttGrid.iCol EQ iCol NO-ERROR.
         FIND  ttStep
         WHERE ttStep.iRow EQ iRow
         AND   ttStep.iCol EQ iCol NO-ERROR.
         IF AVAILABLE ttStep THEN 
            cLine = cLine + ttStep.cType.
         ELSE 
            IF AVAILABLE ttGrid THEN 
               cLine = cLine + ttGrid.cType.
            ELSE 
               cLine = cLine + " ".
      END.
      PUT UNFORMATTED 
         cLine SKIP.          
   END.
   
   OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE createCube:
DEFINE VARIABLE iRowFrom      AS INTEGER NO-UNDO.
DEFINE VARIABLE iColFrom      AS INTEGER NO-UNDO.
DEFINE VARIABLE iRowTo        AS INTEGER NO-UNDO.
DEFINE VARIABLE iColTo        AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaRowFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaRowTo   AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaColFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaColTo   AS INTEGER NO-UNDO.
DEFINE VARIABLE iPoint        AS INTEGER NO-UNDO.

   IF lvlDebug THEN DO:
      RUN createSide
         (1, 9, 5, 4, 0, +1, 0, -1, "^", "v").
      RUN createSide
         (5, 1, 1, 12, 0, +1, 0, -1, "^", "v").
      RUN createSide
         (1, 9, 5, 5, +1, 0, 0, +1, "<", "v").
      RUN createSide
         (5, 5, 1, 9, 0, +1, +1, 0, "^", ">").
      RUN createSide
         (1, 12, 12, 16, +1, 0, -1, 0, ">", "<").
      RUN createSide
         (8, 5, 12, 9, 0, +1, -1, 0, "v", ">").
      RUN createSide
         (9, 9, 8, 8, +1, 0, 0, -1, "<", "^").
      RUN createSide
         (12, 9, 8, 4, 0, +1, 0, -1, "v", "^").
      RUN createSide
         (8, 1, 12, 12, 0, +1, 0, -1, "v", "^").
      RUN createSide
         (5, 1, 12, 16, +1, 0, 0, -1, "<", "^").
      RUN createSide
         (9, 13, 8, 12, 0, +1, +1, 0, "^", "<").
      RUN createSide
         (5, 12, 9, 16, +1, 0, 0, -1, ">", "v").
      RUN createSide
         (9, 16, 4, 12, +1, 0, -1, 0, ">", "<").
      RUN createSide
         (12, 13, 8, 1, 0, +1, -1, 0, "v", ">").
   END.
   ELSE DO:
      
      /* Row 1, Col 51 up to 100 map to row 151 up to 200 Col 1 ^ --> >*/
      RUN createSide
         (  1,  51, 151,   1,  0,  1,  1,  0, "^", ">").
      /* Row 151 up to 200, Col 1 map to Row 1, Col 51 up to 100, < --> v */
      RUN createSide
         (151,   1,   1,  51,  1,  0,  0,  1, "<", "v").
   
      /* Row 1, Col 101 up to 150 map to Row 200, Col 1 up to 50, ^ --> ^ */
      RUN createSide
         (  1, 101, 200,   1,  0, +1,  0, +1, "^", "^").
      /* Row 200, Col 1 up to 50 map to Row 1, Col 101 up to 150, v --> v */
      RUN createSide
         (200,   1,   1, 101,  0, +1,  0, +1, "v", "v").
   
      /* Row 1 up to 50, Col 150 map to Row 150 down to 101, Col 100 */
      RUN createSide
         (  1, 150, 150, 100,  1,  0, -1,  0, ">", "<").
      /* Row 51 up to 100, Col 100 map to Row 50, Col 101 up to 150, > --> ^ */
      RUN createSide
         ( 51, 100,  50, 101,  1,  0,  0,  1, ">", "^").
      RUN createSide
         ( 50, 101,  51, 100,  0, +1, +1,  0, "v", "<").
      
      /* Row 51 up to 100, Col 51 map to Row 101, Col 1 up to 50, < --> v */
      RUN createSide
         (  1,  51, 150,   1,  1,  0,  -1,  0, "<", ">").
      /* Row 101 up to 150, Col 1 map to Row 50 down to 1, Col 51, < --> > */
      RUN createSide
         (101,   1,  50,  51,  1,  0, -1,  0, "<", ">")
         .
      /* Row 101 up to 150, Col 100 map to Row 50 down to 1, Col 150, > --> < */
      RUN createSide
         (101, 100,  50, 150,  1,  0, -1,  0, ">", "<").
      /* Row 151 up to 200, Col 50 map to Row 150, Col 51 up to 100, > --> ^ */
      RUN createSide
         (151,  50, 150,  51,  1,  0,  0,  1, ">", "^").
      
      RUN createSide
         (150,  51, 151,  50,  0, +1, +1,  0, "v", "<").
         
      RUN createSide
         (101,   1,  51,  51,  0, +1, +1,  0, "^", ">").
         
      RUN createSide
         ( 51,  51, 101,   1, +1,  0,  0, +1, "<", "v").
             
   END.   
END. /* createCube */            

PROCEDURE createSide:
DEFINE INPUT  PARAMETER ipiRowFrom       AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiColFrom       AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiRowTo         AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiColTo         AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDeltaRowFrom  AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDeltaColFrom  AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDeltaRowTo    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiDeltaColTo    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcDirectionFrom AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDirectionTo   AS CHARACTER NO-UNDO.   

DEFINE VARIABLE iPoint AS INTEGER NO-UNDO.

DEFINE BUFFER ttNextGrid FOR ttNextGrid.

   DO iPoint = 1 TO (IF lvlDebug THEN 4 ELSE 50):
      CREATE ttNextGrid.
      ASSIGN 
         ttNextGrid.iRow           = ipiRowFrom
         ttNextGrid.iCol           = ipiColFrom
         ttNextGrid.cDirection     = ipcDirectionFrom
         ttNextGrid.iNextRow       = ipiRowTo
         ttNextGrid.iNextcol       = ipiColTo
         ttNextGrid.cNextDirection = ipcDirectionTo
      .
      ASSIGN 
         ipiRowFrom = ipiRowFrom + ipiDeltaRowFrom
         ipiRowTo   = ipiRowTo   + ipiDeltaRowTo
         ipiColFrom = ipiColFrom + ipiDeltaColFrom
         ipiColTo   = ipiColTo   + ipiDeltaColTo
      .
   END.
END. /* createSide */
         
/* ************************  Function Implementations ***************** */

