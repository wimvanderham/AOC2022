
/*------------------------------------------------------------------------
    File        : day23.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 23 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Jan 21 14:45:53 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/23".
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
DEFINE TEMP-TABLE ttElf
   FIELD iPosX        AS INTEGER 
   FIELD iPosY        AS INTEGER 
   FIELD iNextPosX    AS INTEGER 
   FIELD iNextPosY    AS INTEGER 
INDEX indPosXY IS UNIQUE iPosX iPosY
INDEX indNextPosXY iNextPosX iNextPosY.

DEFINE VARIABLE cDirections AS CHARACTER NO-UNDO INITIAL "N,S,W,E".
DEFINE VARIABLE iDirection  AS INTEGER   NO-UNDO INITIAL 1.

DEFINE TEMP-TABLE ttDirection
   FIELD cDirection AS CHARACTER 
   FIELD iDeltaPosX AS INTEGER 
   FIELD iDeltaPosY AS INTEGER
INDEX indDirection IS UNIQUE cDirection.

DEFINE VARIABLE lDontMove AS LOGICAL NO-UNDO.
DEFINE VARIABLE iRound    AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxX   AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxY   AS INTEGER NO-UNDO.
   
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

COPY-LOB FROM FILE "input\23.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\23_test_2.txt" TO OBJECT lcInput.
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
      IF cChar EQ "#" THEN DO:
         /* Create ttElf */
         CREATE ttElf.
         ASSIGN 
            ttElf.iPosX        = iChar
            ttElf.iPosY        = iLine
            ttElf.iNextPosX    = ?
            ttElf.iNextPosY    = ?
         .
      END.
   END.
   
END. /* ReadBlock: */

RUN fillDirections.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttElf:HANDLE).   
END.                     

IF  lvlDebug 
AND lvlShow THEN DO:
   RUN showGrid
      (INPUT "output\23_0.txt",
       INPUT 0).
END.

MainBlock:
REPEAT:
   iRound = iRound + 1.
   
   FOR EACH ttElf:
      RUN checkDirections
         (INPUT  ttElf.iPosX,
          INPUT  ttElf.iPosY,
          INPUT  ENTRY (iDirection, cDirections),
          OUTPUT ttElf.iNextPosX,
          OUTPUT ttElf.iNextPosY).
   END.
   /* Set the next direction */
   IF iDirection LT NUM-ENTRIES (cDirections) THEN 
      iDirection = iDirection + 1.
   ELSE 
      iDirection = 1.
      
   CheckMoveBlock:
   FOR EACH ttElf
   WHERE ttElf.iNextPosX NE ?
   AND   ttElf.iNextPosY NE ?
   BREAK
   BY ttElf.iNextPosX
   BY ttElf.iNextPosY:
      /* If two or more Elves propose moving to the same position, none of those Elves move. */
      IF  FIRST-OF (ttElf.iNextPosY)
      AND NOT LAST-OF (ttElf.iNextPosY) THEN DO:
         lDontMove = TRUE.
      END.
      IF lDontMove THEN DO:
         ASSIGN 
            ttElf.iNextPosX = ?
            ttElf.iNextPosY = ?
         .
      END.
      IF LAST-OF (ttElf.iNextPosY) THEN 
         lDontMove = FALSE.
   END.
   
   MoveBlock:
   FOR EACH ttElf:
      IF  ttElf.iNextPosX NE ?
      AND ttElf.iNextPosY NE ? THEN DO:
         ACCUM "Moves" (COUNT).
      
         ASSIGN 
            ttElf.iPosX      = ttElf.iNextPosX
            ttElf.iPosY      = ttElf.iNextPosY
            ttElf.iNextPosX  = ?
            ttElf.iNextPosY  = ?
         .
      END.
   END.
   
   IF  lvlDebug
   AND lvlShow THEN DO:
      RUN showGrid
         (INPUT SUBSTITUTE ("output\23_&1.txt", iRound),
          INPUT iRound).
   END.
         
   IF (ACCUM COUNT "Moves") EQ 0 THEN DO:
      /* No Elves moved */
      iSolution = iRound. 
      LEAVE MainBlock.   
   END.             
   
   IF lPart[1] THEN DO:
      IF iRound EQ 10 THEN DO:
         RUN countEmpty
            (OUTPUT iSolution).
         LEAVE MainBlock.
      END.
   END.      
END. /* MainBlock */   

          
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttElf:HANDLE).
END.
   
IF lPart[1] THEN DO: 
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 23 - Part One".
END.

IF lPart[2] THEN DO:
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 23 - Part Two".
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

PROCEDURE checkDirections:
/*------------------------------------------------------------------------------
 Purpose: Check where Elf can go to
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiPosX           AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPosY           AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcStartDirection AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiNextPosX       AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiNextPosY       AS INTEGER   NO-UNDO.

DEFINE BUFFER ttNextElf FOR ttElf.

DEFINE VARIABLE iCheck          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheckX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheckY         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCheckDirection AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMoveDirection  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iDirection      AS INTEGER   NO-UNDO.

   /* Count Elves in the 8 directions around
   ** During the first half of each round, 
   ** each Elf considers the eight positions adjacent to themself. 
   ** If no other Elves are in one of those eight positions, 
   ** the Elf does not do anything during this round. */ 
   FOR EACH ttDirection,
   FIRST ttNextElf
   WHERE ttNExtElf.iPosX EQ ipiPosX + ttDirection.iDeltaPosX
   AND   ttNExtElf.iPosY EQ ipiPosY + ttDirection.iDeltaPosY:
      ACCUM "Adjacent" (COUNT).
   END.
   IF (ACCUM COUNT "Adjacent") EQ 0 THEN DO:
      ASSIGN 
         opiNextPosX = ?
         opiNextPosY = ?
      .
      RETURN.
   END.
   
   ASSIGN 
      cCheckDirection = ipcStartDirection
   .
   
   DO iCheck = 1 TO 4:
      /* Check the possible 4 move directions 
      ** Otherwise, the Elf looks in each of four directions in the following order 
      ** and proposes moving one step in the first valid direction
      */
      lMoveDirection = TRUE.
      CheckBlock:
      FOR EACH ttDirection
      WHERE INDEX (ttDirection.cDirection, cCheckDirection) NE 0:
         /* Check all 3 related "sub" directions.
         ** E.g. to check N direction, check NW, N and NE direction */
         ASSIGN 
            iCheckX = ipiPosX + ttDirection.iDeltaPosX
            iCheckY = ipiPosY + ttDirection.iDeltaPosY
         .
         /*
         If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
         If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
         If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
         If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
         */
         FIND  ttNextElf
         WHERE ttNextElf.iPosX EQ iCheckX
         AND   ttNExtElf.iPosY EQ iCheckY NO-ERROR.
         IF AVAILABLE ttNextElf THEN DO:
            lMoveDirection = FALSE.
            LEAVE CheckBlock.
         END.
      END. /* CheckBlock */
      IF lMoveDirection EQ TRUE THEN DO:
         FIND  ttDirection 
         WHERE ttDirection.cDirection EQ cCheckDirection.
         ASSIGN 
            opiNextPosX = ipiPosX + ttDirection.iDeltaPosX
            opiNextPosY = ipiPosY + ttDirection.iDeltaPosY
         .
         RETURN.
      END.
      
      iDirection = LOOKUP (cCheckDirection, cDirections). 
      IF iDirection LT NUM-ENTRIES (cDirections) THEN
         cCheckDirection = ENTRY (iDirection + 1, cDirections).
      ELSE 
         cCheckDirection = ENTRY (1, cDirections).
   END. /* Check 4 main directions */
   
   /* At this point there's no available direction */
   ASSIGN 
      opiNextPosX = ?
      opiNextPosY = ?
   .
   
END PROCEDURE.

PROCEDURE countEmpty:
/*------------------------------------------------------------------------------
 Purpose: Count the empty ground in the rectangulare containing all Elfes
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiEmpty AS INTEGER NO-UNDO.

DEFINE VARIABLE iMinX AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxX AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinY AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxY AS INTEGER NO-UNDO.

   FOR EACH ttElf:
      ACCUM "Elf" (COUNT).
      ACCUM ttElf.iPosX (MINIMUM MAXIMUM).
      ACCUM ttElf.iPosY (MINIMUM MAXIMUM).
   END.
   
   ASSIGN 
      iMinX = (ACCUM MINIMUM ttElf.iPosX)
      iMaxX = (ACCUM MAXIMUM ttElf.iPosX)
      iMinY = (ACCUM MINIMUM ttElf.iPosY)
      iMaxY = (ACCUM MAXIMUM ttElf.iPosY)
   .

   /* Number of empty ground is the area of the smalles including rectangle - the number of Elves */   
   opiEmpty = (iMaxX - iMinX + 1) * (iMaxY - iMinY + 1) - (ACCUM COUNT "Elf").

END PROCEDURE.

PROCEDURE fillDirections:
/*------------------------------------------------------------------------------
 Purpose: Fill all the 8 possible Directions with the Delta X and Y values
 Notes:
    
X: 123456789
Y: 1
   2   N
   3  W#E 
   4   S
   5   

Dir dX  dY 
-----------       
N = (0, -1)
S = (0, +1)
W = (-1, 0)
E = (+1, 0)         
------------------------------------------------------------------------------*/

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "N"
      ttDirection.iDeltaPosX = 0
      ttDirection.iDeltaPosY = -1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "NW"
      ttDirection.iDeltaPosX = -1
      ttDirection.iDeltaPosY = -1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "NE"
      ttDirection.iDeltaPosX = +1
      ttDirection.iDeltaPosY = -1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "S"
      ttDirection.iDeltaPosX = 0
      ttDirection.iDeltaPosY = +1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "SW"
      ttDirection.iDeltaPosX = -1
      ttDirection.iDeltaPosY = +1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "SE"
      ttDirection.iDeltaPosX = +1
      ttDirection.iDeltaPosY = +1
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "W"
      ttDirection.iDeltaPosX = -1
      ttDirection.iDeltaPosY = 0
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "E"
      ttDirection.iDeltaPosX = +1
      ttDirection.iDeltaPosY = 0
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
DEFINE VARIABLE iEmpty  AS INTEGER NO-UNDO.

   FOR EACH ttElf:
      ACCUM "Elf" (COUNT).
      ACCUM ttElf.iPosX (MINIMUM MAXIMUM).
      ACCUM ttElf.iPosY (MINIMUM MAXIMUM).
   END.
   
   ASSIGN 
      iMinX = MINIMUM (1,       (ACCUM MINIMUM ttElf.iPosX))
      iMinX = (ACCUM MINIMUM ttElf.iPosX)
      iMaxX = MAXIMUM (lviMaxX, (ACCUM MAXIMUM ttElf.iPosX))
      iMaxX = (ACCUM MAXIMUM ttElf.iPosX)
      iMinY = MINIMUM (1,       (ACCUM MINIMUM ttElf.iPosY))
      iMinY = (ACCUM MINIMUM ttElf.iPosY)
      iMaxY = MAXIMUM (lviMaxY, (ACCUM MAXIMUM ttElf.iPosY))
      iMaxY = (ACCUM MAXIMUM ttElf.iPosY)
   .
   
   OUTPUT TO VALUE (ipcFileName).
   IF ipiRound EQ 0 THEN
      PUT UNFORMATTED 
         "== Initial State ==" SKIP.
   ELSE
      PUT UNFORMATTED 
         SUBSTITUTE ("== End of Round &1 ==", ipiRound) SKIP.
   DO iCheckY = iMinY TO iMaxY:
      DO iCheckX = iMinX TO iMaxX:
         FIND  ttElf
         WHERE ttElf.iPosX EQ iCheckX
         AND   ttElf.iPosY EQ iCheckY NO-ERROR.
         IF AVAILABLE ttElf THEN 
            PUT UNFORMATTED "#".
         ELSE DO:
            PUT UNFORMATTED ".".
            iEmpty = iEmpty + 1.
         END.
      END.
      PUT UNFORMATTED SKIP.
   END.
   PUT SKIP(1).

   PUT UNFORMATTED 
      SUBSTITUTE ("Number of Empty Ground: &1", iEmpty) SKIP.
END PROCEDURE.


   
/* ************************  Function Implementations ***************** */

