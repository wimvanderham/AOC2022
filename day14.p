
/*------------------------------------------------------------------------
    File        : day14.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 14 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 16 18:37:55 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/14".
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
DEFINE TEMP-TABLE ttRock
   FIELD iNr         AS INTEGER 
   FIELD iFromX      AS INTEGER 
   FIELD iFromY      AS INTEGER 
   FIELD iToX        AS INTEGER 
   FIELD iToY        AS INTEGER 
INDEX indNr     IS UNIQUE iNr
INDEX indFromTo IS UNIQUE iFromX iFromY iToX iToY.

DEFINE TEMP-TABLE ttGrid
   FIELD iGridX AS INTEGER 
   FIELD iGridY AS INTEGER 
   FIELD cType  AS CHARACTER
INDEX indGridXY IS UNIQUE iGridX iGridY.

DEFINE BUFFER ttGridDown      FOR ttGrid.
DEFINE BUFFER ttGridDownLeft  FOR ttGrid.
DEFINE BUFFER ttGridDownRight FOR ttGrid.

DEFINE TEMP-TABLE ttSand 
   FIELD iNr    AS INTEGER 
   FIELD iSandX AS INTEGER 
   FIELD iSandY AS INTEGER 
   FIELD cType  AS CHARACTER
INDEX indNr IS UNIQUE iNr.
    
DEFINE VARIABLE iNewNr    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewFromX AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewFromY AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewToX   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewToY   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFrom     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iMinX     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMinY     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxX     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxY     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iX        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lChanges  AS LOGICAL   NO-UNDO.

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

COPY-LOB FROM FILE "input\14.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "498,4 -> 498,6 -> 496,6~n503,4 -> 502,4 -> 502,9 -> 494,9".
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
      NEXT ReadBlock.
   END.

   lFrom = TRUE.
   DO iPair = 1 TO NUM-ENTRIES (cLine, " "):
      cPair = ENTRY (iPair, cLine, " ").
      
      IF cPair EQ "->" THEN 
         NEXT.
         
      IF lFrom THEN DO:
         ASSIGN 
            iNewFromX = INTEGER (ENTRY (1, cPair))
            iNewFromY = INTEGER (ENTRY (2, cPair))
         .
         lFrom = FALSE.
         NEXT.
      END.
      
      IF lFrom EQ FALSE THEN DO:
         ASSIGN 
            iNewToX = INTEGER (ENTRY (1, cPair))
            iNewToY = INTEGER (ENTRY (2, cPair))
         .
         
         FIND  ttRock
         WHERE ttRock.iFromX EQ iNewFromX
         AND   ttRock.iFromY EQ iNewFromY
         AND   ttRock.iToX   EQ iNewToX
         AND   ttRock.iToY   EQ iNewToY NO-ERROR.
         IF NOT AVAILABLE ttRock THEN DO:
            iNewNr = iNewNr + 1.
               
            CREATE ttRock.
            ASSIGN
               ttRock.iNr    = iNewNr 
               ttRock.iFromX = iNewFromX
               ttRock.iFromY = iNewFromY
               ttRock.iToX   = iNewToX
               ttRock.iToY   = iNewToY
            .
         END.
         
         ASSIGN 
            iNewFromX = ttRock.iToX
            iNewFromY = ttRock.iToY
         .
      END.
             
   END. 
END. /* ReadBlock: */

/* Determine size of Grid */
FOR EACH ttRock:
   ACCUM ttRock.iFromX (MINIMUM MAXIMUM).
   ACCUM ttRock.iFromY (MINIMUM MAXIMUM).
   ACCUM ttRock.iToX   (MINIMUM MAXIMUM).
   ACCUM ttRock.iToY   (MINIMUM MAXIMUM).
END.

ASSIGN
   iMinX = MINIMUM ((ACCUM MINIMUM ttRock.iFromX), (ACCUM MINIMUM ttRock.iToX))
   iMinY = MINIMUM ((ACCUM MINIMUM ttRock.iFromY), (ACCUM MINIMUM ttRock.iToY), 0)
   iMaxX = MAXIMUM ((ACCUM MAXIMUM ttRock.iFromX), (ACCUM MAXIMUM ttRock.iToX), 500)
   iMaxY = MAXIMUM ((ACCUM MAXIMUM ttRock.iFromY), (ACCUM MAXIMUM ttRock.iToY))
.

IF lPart[2] THEN DO:
   iMaxY = iMaxY + 2.
END.
 
DO iY = iMinY TO iMaxY:
   DO iX = iMinX TO iMaxX:
      FIND  ttGrid
      WHERE ttGrid.iGridX EQ iX
      AND   ttGrid.iGridY EQ iY NO-ERROR.
      IF NOT AVAILABLE ttGrid THEN DO:
         CREATE ttGrid.
         ASSIGN 
            ttGrid.iGridX = iX
            ttGrid.iGridY = iY
         .
         IF iX EQ 500 AND iY EQ 0 THEN DO:
            /* Source of Sand */
            ttGrid.cType = "+".
         END.
         ELSE DO:
            ttGrid.cType = ".".
         END.            
      END.
   END.
END.

FOR EACH ttRock:
   DO iX = MINIMUM (ttRock.iFromX, ttRock.iToX) TO MAXIMUM (ttRock.iFromX, ttRock.iToX):
      DO iY = MINIMUM (ttRock.iFromY, ttRock.iToY) TO MAXIMUM (ttRock.iFromY, ttRock.iToY):
         FIND  ttGrid
         WHERE ttGrid.iGridX EQ iX
         AND   ttGrid.iGridY EQ iY.
         ttGrid.cType = "#".
       END.
   END.
END.

IF lPart[2] THEN DO:
   FOR EACH ttGrid
   WHERE ttGrid.iGridY EQ iMaxY:
      ttGrid.cType = "#".
   END.
END.
                
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRock:HANDLE).
   OUTPUT TO "output\14.txt".
   PUT UNFORMATTED 
   SUBSTITUTE ("Start: &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP(1).

   DO iY = iMinY TO iMaxY:
      DO iX = iMinX TO iMaxX:
         FIND  ttGrid
         WHERE ttGrid.iGridX EQ iX
         AND   ttGrid.iGridY EQ iY.
         PUT UNFORMATTED 
         ttGrid.cType.
      END.
      PUT UNFORMATTED SKIP.
   END.
   PUT UNFORMATTED SKIP.   
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   IF lvlDebug THEN DO:
   END.
   ELSE DO: 
   END.
      
   iNewNr = 1.
   
   CREATE ttSand.
   ASSIGN 
      ttSand.iNr    = iNewNr
      ttSand.iSandX = 500
      ttSand.iSandY = 0
      ttSand.cType  = "o"
   .
   
   SandBlock:
   REPEAT:
      lChanges = FALSE.
      ACCUM "" (COUNT).
      
      FIND  ttSand
      WHERE ttSand.iNr EQ iNewNr.
      FIND  ttGrid 
      WHERE ttGrid.iGridX EQ ttSand.iSandX
      AND   ttGrid.iGridY EQ ttSand.iSandY.
      FIND  ttGridDown
      WHERE ttGridDown.iGridX EQ ttSand.iSandX
      AND   ttGridDown.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      FIND  ttGridDownLeft
      WHERE ttGridDownLeft.iGridX EQ ttSand.iSandX - 1
      AND   ttGridDownLeft.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      FIND  ttGridDownRight
      WHERE ttGridDownRight.iGridX EQ ttSand.iSandX + 1
      AND   ttGridDownRight.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      
      IF NOT AVAILABLE ttGridDown 
      OR NOT AVAILABLE ttGridDownLeft 
      OR NOT AVAILABLE ttGridDownRight THEN
         LEAVE SandBlock.
         
      IF ttGridDown.cType EQ "." THEN DO:
         /* Sand can move down */
         ASSIGN
            ttSand.iSandY = ttGridDown.iGridY
            lChanges      = TRUE 
         .
      END. /* Sand can move down */
      ELSE DO:
         IF ttGridDown.cType EQ "#" OR ttGridDown.cType EQ "o" THEN DO:
            /* There's a solid part underneath */
            IF ttGridDownLeft.cType EQ "." THEN DO:
               /* Free space available down-left */
               ASSIGN 
                  ttSand.iSandX = ttGridDownLeft.iGridX
                  ttSand.iSandY = ttGridDownLeft.iGridY
               .
               lChanges = TRUE.
            END. /* Free space available down-left */
            ELSE DO:
               /* No free space available down-left */
               IF ttGridDownRight.cType EQ "." THEN DO:
                  /* Free space available down-right */
                  ASSIGN 
                     ttSand.iSandX = ttGridDownRight.iGridX
                     ttSand.iSandY = ttGridDownRight.iGridY
                  .
                  lChanges = TRUE.
               END.
               ELSE DO:
                  /* No free spaces available, rest here */
                  ASSIGN
                     ttGrid.cType = "o"
                  .
                  /* Create new Sand unit */
                  iNewNr = iNewNr + 1.
                  CREATE ttSand.
                  ASSIGN 
                     ttSand.iNr    = iNewNr
                     ttSand.iSandX = 500
                     ttSand.iSandY = 0
                     ttSand.cType  = "o"
                  .
                  lChanges = TRUE.
               END. /* No free spaces available, rest here */
            END. /* No free space available down-left */
         END. /* There's a solid part underneath */
      END.      

      IF lvlShow AND lvlDebug THEN DO:
         PUT UNFORMATTED 
         SUBSTITUTE ("Step: &1", (ACCUM COUNT "")) SKIP.
      
         DO iY = iMinY TO iMaxY:
            DO iX = iMinX TO iMaxX:
               FIND  ttSand
               WHERE ttSand.iSandX EQ iX
               AND   ttSand.iSandY EQ iY NO-ERROR.
               IF AVAILABLE ttSand THEN 
                  PUT UNFORMATTED 
                  ttSand.cType.
               ELSE DO:
                  FIND  ttGrid
                  WHERE ttGrid.iGridX EQ iX
                  AND   ttGrid.iGridY EQ iY.
                  PUT UNFORMATTED 
                  ttGrid.cType.
               END.
            END.
            PUT UNFORMATTED SKIP.
         END.
         PUT UNFORMATTED SKIP.   
      END.
            
      IF lChanges EQ FALSE THEN 
         LEAVE SandBlock.
         
   END. /* SandBlock */

   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("Fine") SKIP.
      DO iY = iMinY TO iMaxY:
         DO iX = iMinX TO iMaxX:
            FIND  ttGrid
            WHERE ttGrid.iGridX EQ iX
            AND   ttGrid.iGridY EQ iY.
            PUT UNFORMATTED 
            ttGrid.cType.
         END.
         PUT UNFORMATTED SKIP.
      END.
      PUT UNFORMATTED SKIP.
      OUTPUT CLOSE.
   END.      
         
   FOR EACH ttGrid
   WHERE ttGrid.cType EQ "o":
      ACCUM "" (COUNT).
   END.
   
   iSolution = (ACCUM COUNT "").
           
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 14 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   IF lvlDebug THEN DO:
   END.

   EMPTY TEMP-TABLE ttSand.
   FOR EACH ttGrid 
   WHERE ttGrid.cType = "o":
      ttGrid.cType = ".".
   END.
      
   iNewNr = 1.
   
   CREATE ttSand.
   ASSIGN 
      ttSand.iNr    = iNewNr
      ttSand.iSandX = 500
      ttSand.iSandY = 0
      ttSand.cType  = "o"
   .
   
   SandBlock:
   REPEAT:
      lChanges = FALSE.
      ACCUM "" (COUNT).
      
      FIND  ttSand
      WHERE ttSand.iNr EQ iNewNr.
      FIND  ttGrid 
      WHERE ttGrid.iGridX EQ ttSand.iSandX
      AND   ttGrid.iGridY EQ ttSand.iSandY NO-ERROR.
      IF NOT AVAILABLE ttGrid THEN DO:
         CREATE ttGrid.
         ASSIGN 
            ttGrid.iGridX = ttSand.iSandX
            ttGrid.iGridY = ttSand.iSandY
            ttGrid.cType  = "."
         .
      END.
      
      FIND  ttGridDown
      WHERE ttGridDown.iGridX EQ ttSand.iSandX
      AND   ttGridDown.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      IF NOT AVAILABLE ttGridDown THEN DO:
         CREATE ttGridDown.
         ASSIGN 
            ttGridDown.iGridX = ttSand.iSandX
            ttGridDown.iGridY = ttSand.iSandY + 1
            ttGridDown.cType  = ".".
         .
         IF ttGridDown.iGridY EQ iMaxY THEN 
            ttGridDown.cType  = "#".
      END.
      FIND  ttGridDownLeft
      WHERE ttGridDownLeft.iGridX EQ ttSand.iSandX - 1
      AND   ttGridDownLeft.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      IF NOT AVAILABLE ttGridDownLeft THEN DO:
         CREATE ttGridDownLeft.
         ASSIGN 
            ttGridDownLeft.iGridX = ttSand.iSandX - 1
            ttGridDownLeft.iGridY = ttSand.iSandY + 1
            ttGridDownLeft.cType  = "."
         .
         IF ttGridDownLeft.iGridY EQ iMaxY THEN  
            ttGridDownLeft.cType  = "#".
         iMinX = MINIMUM (iMinX, ttGridDownLeft.iGridX).
      END.
      FIND  ttGridDownRight
      WHERE ttGridDownRight.iGridX EQ ttSand.iSandX + 1
      AND   ttGridDownRight.iGridY EQ ttSand.iSandY + 1 NO-ERROR.
      IF NOT AVAILABLE ttGridDownRight THEN DO:
         CREATE ttGridDownRight.
         ASSIGN 
            ttGridDownRight.iGridX = ttSand.iSandX + 1
            ttGridDownRight.iGridY = ttSand.iSandY + 1
            ttGridDownRight.cType  = "."
         .
         IF ttGridDownRight.iGridY EQ iMaxY THEN 
            ttGridDownRight.cType  = "#".
         iMaxX = MAXIMUM (iMaxX, ttGridDownRight.iGridX).
      END.
      
      IF NOT AVAILABLE ttGridDown 
      OR NOT AVAILABLE ttGridDownLeft 
      OR NOT AVAILABLE ttGridDownRight THEN DO:
         MESSAGE SUBSTITUTE ("Why? (&1,&2) iMaxY=&3",
                             ttSand.iSandX,
                             ttSand.iSandY,
                             iMaxY)
         VIEW-AS ALERT-BOX.
         LEAVE SandBlock.
      END.
         
      IF ttGridDown.cType EQ "." THEN DO:
         /* Sand can move down */
         ASSIGN
            ttSand.iSandY = ttGridDown.iGridY
            lChanges      = TRUE 
         .
      END. /* Sand can move down */
      ELSE DO:
         IF ttGridDown.cType EQ "#" OR ttGridDown.cType EQ "o" THEN DO:
            /* There's a solid part underneath */
            IF ttGridDownLeft.cType EQ "." THEN DO:
               /* Free space available down-left */
               ASSIGN 
                  ttSand.iSandX = ttGridDownLeft.iGridX
                  ttSand.iSandY = ttGridDownLeft.iGridY
               .
               lChanges = TRUE.
            END. /* Free space available down-left */
            ELSE DO:
               /* No free space available down-left */
               IF ttGridDownRight.cType EQ "." THEN DO:
                  /* Free space available down-right */
                  ASSIGN 
                     ttSand.iSandX = ttGridDownRight.iGridX
                     ttSand.iSandY = ttGridDownRight.iGridY
                  .
                  lChanges = TRUE.
               END.
               ELSE DO:
                  /* No free spaces available, rest here */
                  ASSIGN
                     ttGrid.cType = "o"
                  .
                  IF  ttSand.iSandX EQ 500
                  AND ttSand.iSandY EQ 0 THEN 
                     LEAVE SandBlock.
                  /* Create new Sand unit */
                  iNewNr = iNewNr + 1.
                  CREATE ttSand.
                  ASSIGN 
                     ttSand.iNr    = iNewNr
                     ttSand.iSandX = 500
                     ttSand.iSandY = 0
                     ttSand.cType  = "o"
                  .
                  lChanges = TRUE.
               END. /* No free spaces available, rest here */
            END. /* No free space available down-left */
         END. /* There's a solid part underneath */
      END.      

      IF lvlShow AND lvlDebug THEN DO:
         PUT UNFORMATTED 
         SUBSTITUTE ("Step: &1", (ACCUM COUNT "")) SKIP.
      
         DO iY = iMinY TO iMaxY:
            DO iX = iMinX TO iMaxX:
               FIND  ttSand
               WHERE ttSand.iSandX EQ iX
               AND   ttSand.iSandY EQ iY NO-ERROR.
               IF AVAILABLE ttSand THEN 
                  PUT UNFORMATTED 
                  ttSand.cType.
               ELSE DO:
                  FIND  ttGrid
                  WHERE ttGrid.iGridX EQ iX
                  AND   ttGrid.iGridY EQ iY.
                  PUT UNFORMATTED 
                  ttGrid.cType.
               END.
            END.
            PUT UNFORMATTED SKIP.
         END.
         PUT UNFORMATTED SKIP.   
      END.
            
      IF lChanges EQ FALSE THEN 
         LEAVE SandBlock.
         
   END. /* SandBlock */

   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("Fine") SKIP.
      DO iY = iMinY TO iMaxY:
         DO iX = iMinX TO iMaxX:
            FIND  ttGrid
            WHERE ttGrid.iGridX EQ iX
            AND   ttGrid.iGridY EQ iY NO-ERROR.
            IF NOT AVAILABLE ttGrid THEN DO:
               IF iY EQ iMaxY THEN 
                  PUT UNFORMATTED "#".
               ELSE 
                  PUT UNFORMATTED ".".
            END.
            ELSE DO:
               PUT UNFORMATTED 
               ttGrid.cType.
            END.
         END.
         PUT UNFORMATTED SKIP.
      END.
      PUT UNFORMATTED SKIP.
      OUTPUT CLOSE.
   END.      
         
   FOR EACH ttGrid
   WHERE ttGrid.cType EQ "o":
      ACCUM "" (COUNT).
   END.
   
   iSolution = (ACCUM COUNT "").
    
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 14 - Part Two".
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

