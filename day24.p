
/*------------------------------------------------------------------------
    File        : day24.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day 24 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Jan 22 22:39:00 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/24".
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
DEFINE TEMP-TABLE ttBlizzard 
   FIELD iPosX      AS INTEGER 
   FIELD iPosY      AS INTEGER 
   FIELD cDirection AS CHARACTER 
   FIELD iStartPosX AS INTEGER 
   FIELD iStartPosY AS INTEGER 
INDEX indPosXY iPosX iPosY.

DEFINE TEMP-TABLE ttStep
   FIELD iRound AS INTEGER 
   FIELD iStep  AS INTEGER 
   FIELD iStepX AS INTEGER 
   FIELD iStepY AS INTEGER 
   FIELD cPath  AS CHARACTER   
INDEX indRound IS UNIQUE iRound iStep
INDEX iStepXY  IS UNIQUE iStepX iStepY iRound.

DEFINE BUFFER ttNextStep FOR ttStep.

DEFINE TEMP-TABLE ttWall
   FIELD iWallX AS INTEGER 
   FIELD iWallY AS INTEGER 
INDEX indWallXY IS UNIQUE iWallX iWallY.

DEFINE TEMP-TABLE ttMove
   FIELD cDirection AS CHARACTER 
   FIELD iDeltaPosX AS INTEGER 
   FIELD iDeltaPosY AS INTEGER
INDEX indDirection IS UNIQUE cDirection.

DEFINE VARIABLE lviMaxX    AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxY    AS INTEGER NO-UNDO.
DEFINE VARIABLE lviStartX  AS INTEGER NO-UNDO.
DEFINE VARIABLE lviStartY  AS INTEGER NO-UNDO.
DEFINE VARIABLE lviEndX    AS INTEGER NO-UNDO.
DEFINE VARIABLE lviEndY    AS INTEGER NO-UNDO.   
DEFINE VARIABLE iNextRound AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextStep  AS INTEGER NO-UNDO.
   
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

COPY-LOB FROM FILE "input\24.txt" TO OBJECT lcInput.
/* Remove leading and trailing empty lines */ 
lcInput = TRIM (lcInput).

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\24_test.txt" TO OBJECT lcInput.
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

   IF  lviMaxX EQ 0
   AND lviMaxY EQ 0 THEN 
      ASSIGN 
         lviMaxX = LENGTH (cLine)
         lviMaxY = NUM-ENTRIES (lcInput, "~n")
      . 
   
   DO iChar = 1 TO LENGTH (cLine):
      cChar = SUBSTRING (cLine, iChar, 1).
      CASE cChar:
         WHEN ">" OR 
         WHEN "<" OR 
         WHEN "^" OR 
         WHEN "v" THEN DO:
            CREATE ttBlizzard.
            ASSIGN
               ttBlizzard.iPosX      = iChar
               ttBlizzard.iPosY      = iLine
               ttBlizzard.cDirection = cChar
               ttBlizzard.iStartPosX = ttBlizzard.iPosX
               ttBlizzard.iStartPosY = ttBlizzard.iPosY
            . 
         END.
         WHEN "." THEN DO:
            IF iLine EQ 1 THEN DO:
               ASSIGN 
                  lviStartX = iChar
                  lviStartY = iLine
               .
            END.
            IF iLine EQ lviMaxY THEN DO:
               ASSIGN 
                  lviEndX = iChar
                  lviEndY = iLine
               .
            END.
         END.
         WHEN "#" THEN DO:
            CREATE ttWall.
            ASSIGN 
               ttWall.iWallX = iChar
               ttWall.iWallY = iLine
            .
         END.
      END CASE.
   END.
   
END. /* ReadBlock: */

RUN fillMoves.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttBlizzard:HANDLE).   
END.                     

IF  lvlDebug 
AND lvlShow THEN DO:
   RUN showGrid
      (INPUT "output\24_0.txt",
       INPUT 0).
END.

CREATE ttStep.
ASSIGN 
   ttStep.iRound = 1
   ttStep.iStep  = 1
   ttStep.iStepX = lviStartX
   ttStep.iStepY = lviStartY
.

MainBlock:
REPEAT:
   ASSIGN 
      iNextRound = iNextRound + 1
      iNextStep  = 0
   .

   FOR EACH ttBlizzard,
   FIRST ttMove OF ttBlizzard
   BY ROWID (ttBlizzard):
       /* Move the Blizzards */
      ASSIGN 
         ttBlizzard.iPosX = ttBlizzard.iPosX + ttMove.iDeltaPosX
         ttBlizzard.iPosY = ttBlizzard.iPosY + ttMove.iDeltaPosY
      .
      IF ttBlizzard.iPosX EQ lviMaxX THEN 
         ttBlizzard.iPosX = 2.
      IF ttBlizzard.iPosX EQ 1 THEN 
         ttBlizzard.iPosX = lviMaxX - 1.
      IF ttBlizzard.iPosY EQ lviMaxY THEN
         ttBlizzard.iPosY = 2.
      IF ttBlizzard.iPosY EQ 1 THEN 
         ttBlizzard.iPosY = lviMaxY - 1.
   END.  /* Move the Blizzards */ 
   
   FOR EACH ttStep
   WHERE ttStep.iRound EQ iNextRound:
      /* All available Positions */
/*      IF  ttStep.iStepX NE lviStartX         */
/*      AND ttStep.iStepY NE lviStartY THEN DO:*/
         /* Check if we can wait */
         FIND  ttBlizzard
         WHERE ttBlizzard.iPosX EQ ttStep.iStepX
         AND   ttBlizzard.iPosY EQ ttStep.iStepY NO-ERROR.
         IF NOT AVAILABLE ttBlizzard THEN DO:
            /* Can choose to not move */
            CREATE ttNextStep.
            ASSIGN
               iNextStep         = iNextStep + 1 
               ttNextStep.iRound = ttStep.iRound + 1
               ttNextStep.iStep  = iNextStep
               ttNextStep.iStepX = ttStep.iStepX
               ttNextStep.iStepY = ttStep.iStepY
               ttNextStep.cPath  = SUBSTITUTE ("&1_", ttStep.cPath)
            .
/*         END. /* Can choose to not move */*/
      END.      
      MoveBlock:
      FOR EACH ttMove:
         /* All possible Moves */
         IF (ttStep.iStepX + ttMove.iDeltaPosX) GT lviMaxX
         OR (ttStep.iStepX + ttMove.iDeltaPosX) LT 1 THEN DO:
            /* X outside the basin */ 
            NEXT MoveBlock.
         END.
         IF (ttStep.iStepY + ttMove.iDeltaPosY) GT lviMaxY
         OR (ttStep.iStepY + ttMove.iDeltaPosY) LT 1 THEN DO:
            /* Y outside the basin */
            NEXT MoveBlock.
         END.
         FIND FIRST ttBlizzard 
         WHERE ttBlizzard.iPosX EQ (ttStep.iStepX + ttMove.iDeltaPosX)
         AND   ttBlizzard.iPosY EQ (ttStep.iStepY + ttMove.iDeltaPosY) NO-ERROR.
         FIND  ttWall
         WHERE ttWall.iWallX EQ (ttStep.iStepX + ttMove.iDeltaPosX)
         AND   ttWall.iWallY EQ (ttStep.iStepY + ttMove.iDeltaPosY) NO-ERROR.
         IF  NOT AVAILABLE ttBlizzard 
         AND NOT AVAILABLE ttWall THEN DO:
            FIND FIRST ttNextStep
            WHERE ttNextStep.iStepX EQ (ttStep.iStepX + ttMove.iDeltaPosX)
            AND   ttNextStep.iStepY EQ (ttStep.iStepY + ttMove.iDeltaPosY) NO-ERROR.
            IF NOT AVAILABLE ttNextStep THEN DO:
               CREATE ttNextStep.
               ASSIGN
                  iNextStep         = iNextStep + 1
                  ttNextStep.iRound = ttStep.iRound + 1
                  ttNextStep.iStep  = iNextStep
                  ttNextStep.iStepX = (ttStep.iStepX + ttMove.iDeltaPosX)
                  ttNextStep.iStepY = (ttStep.iStepY + ttMove.iDeltaPosY)
                  ttNextStep.cPath  = SUBSTITUTE ("&1&2", ttStep.cPath, ttMove.cDirection)
               .
               IF  ttNextStep.iStepX EQ lviEndX
               AND ttNextStep.iStepY EQ lviEndY THEN DO:
                  iSolution = ttStep.iRound.
                  RUN showGrid
                     (INPUT "output\24_Part1.txt",
                      INPUT iNextRound).
                  LEAVE MainBlock.
               END.                  
            END.
         END.
      END. /* All possible Moves */
      DELETE ttStep.
   END. /* All available Positions */
      
   IF  lvlDebug
   AND lvlShow THEN DO:
      RUN showGrid
         (INPUT SUBSTITUTE ("output\24_&1.txt", iNextRound),
          INPUT iNextRound).
   END.
         
   IF lPart[1] THEN DO:
      /*
      IF iNextRound EQ 20 THEN DO:
         LEAVE MainBlock.
      END.
      */
   END.      
   
END. /* MainBlock */   

          
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttBlizzard:HANDLE).
END.
   
IF lPart[1] THEN DO:
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 24 - Part One".
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 24 - Part Two".
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

PROCEDURE fillMoves:
/*------------------------------------------------------------------------------
 Purpose: Fill all the 4 possible Moves of the Blizzards
 Notes:
    
------------------------------------------------------------------------------*/

   CREATE ttMove.
   ASSIGN 
      ttMove.cDirection = ">"
      ttMove.iDeltaPosX = +1
      ttMove.iDeltaPosY = 0
   .
   
   CREATE ttMove.
   ASSIGN 
      ttMove.cDirection = "<"
      ttMove.iDeltaPosX = -1
      ttMove.iDeltaPosY = 0
   .

   CREATE ttMove.
   ASSIGN 
      ttMove.cDirection = "v"
      ttMove.iDeltaPosX = 0
      ttMove.iDeltaPosY = +1
   .

   CREATE ttMove.
   ASSIGN 
      ttMove.cDirection = "^"
      ttMove.iDeltaPosX = 0
      ttMove.iDeltaPosY = -1
   .
   
   /* Special Move to Wait */
   CREATE ttMove.
   ASSIGN 
      ttMove.cDirection = "_"
      ttMove.iDeltaPosX = 0
      ttMove.iDeltaPosY = 0
   .
   
   
END PROCEDURE.

PROCEDURE showGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiRound    AS INTEGER   NO-UNDO.

DEFINE VARIABLE iMinX   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxX   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinY   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxY   AS INTEGER NO-UNDO.
DEFINE VARIABLE iCheckX AS INTEGER NO-UNDO.
DEFINE VARIABLE iCheckY AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar   AS CHARACTER NO-UNDO.

DEFINE BUFFER ttStep FOR ttStep.
DEFINE BUFFER ttBlizzard FOR ttBlizzard.

   ASSIGN
      iMinX = 1
      iMinY = 1
      iMaxX = lviMaxX
      iMaxY = lviMaxY
   .
   
   OUTPUT TO VALUE (ipcFileName).
   IF ipiRound EQ 0 THEN
      PUT UNFORMATTED 
         "== Initial State ==" SKIP.
   ELSE
      PUT UNFORMATTED 
         SUBSTITUTE ("== End of Round &1 ==", ipiRound) SKIP.
   PUT UNFORMATTED 
      SUBSTITUTE ("MinX &1 MaxX &2, MinY &3 MaxY &4",
                  iMinX,
                  iMaxX,
                  iMinY,
                  iMaxY) SKIP
      SUBSTITUTE ("Start (&1,&2) End (&3, &4)",
                  lviStartX,
                  lviStartY,
                  lviEndX,
                  lviEndY) SKIP (1).
                  
   DO iCheckY = iMinY TO iMaxY:
      DO iCheckX = iMinX TO iMaxX:
         FOR EACH ttBlizzard
         WHERE ttBlizzard.iPosX EQ iCheckX
         AND   ttBlizzard.iPosY EQ iCheckY:
            ACCUM "" (COUNT).
            cChar = ttBlizzard.cDirection.
         END.
         IF (ACCUM COUNT "") GT 1 THEN 
            cChar = STRING ((ACCUM COUNT "") MOD 10).
         IF (ACCUM COUNT "") EQ 0 THEN DO: 
            cChar = ".".
            IF iCheckY EQ 1 OR iCheckY EQ iMaxY OR iCheckX EQ 1 OR iCheckX EQ iMaxX THEN
               cChar = "#".
            IF iCheckX EQ lviStartX AND iCheckY = lviStartY THEN 
               cChar = "S".
            IF iCheckX EQ lviEndX   AND iCheckY = lviEndY   THEN 
               cChar = "E".
         END.
         FIND  ttStep
         WHERE ttStep.iRound EQ ipiRound + 1
         AND   ttStep.iStepX EQ iCheckX
         AND   ttStep.iStepY EQ iCheckY NO-ERROR.
         IF AVAILABLE ttStep THEN
            cChar = "*".
         
         PUT UNFORMATTED cChar.
      END.
      PUT UNFORMATTED SKIP.
   END.
   PUT SKIP(1).
   
   PUT UNFORMATTED 
      SUBSTITUTE ("Locations of Blizzards.") SKIP.
   FOR EACH ttBlizzard
   BY ttBlizzard.iStartPosY
   BY ttBlizzard.iStartPosX:
      ACCUM "" (COUNT).
      PUT UNFORMATTED 
         SUBSTITUTE ("#&1 (&2, &3) --> (&4, &5)",
                     (ACCUM COUNT ""),
                     ttBlizzard.iStartPosX,
                     ttBlizzard.iStartPosY,
                     ttBlizzard.iPosX,
                     ttBlizzard.iPosY) SKIP.
   END.                     
   
   PUT UNFORMATTED SKIP (1)
      SUBSTITUTE ("Possible locations of Expedition.") SKIP.
   FOR EACH ttStep
   WHERE ttStep.iRound EQ ipiRound + 1:
      DISPLAY 
      ttStep.iRound LABEL "Round"
      ttStep.iStep  LABEL "Step"
      ttStep.iStepX LABEL "X"
      ttStep.iStepY LABEL "Y"
      WITH STREAM-IO WIDTH 255.
   END.      
   PUT SKIP (1).
   
   FIND  FIRST ttStep
   WHERE ttStep.iStepX EQ lviEndX
   AND   ttStep.iStepY EQ lviEndY NO-ERROR.
   IF AVAILABLE ttStep THEN DO:
      PUT UNFORMATTED 
         SUBSTITUTE ("Found shortest path to the exit: '&1'", ttStep.cPath) SKIP.
   END.

END PROCEDURE.


   
/* ************************  Function Implementations ***************** */

