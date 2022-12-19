
/*------------------------------------------------------------------------
    File        : day18.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day 18 of Advent of Code

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 18 13:57:10 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/18".
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
DEFINE TEMP-TABLE ttPoint
   FIELD iPointX  AS INTEGER 
   FIELD iPointY  AS INTEGER 
   FIELD iPointZ  AS INTEGER 
   FIELD iExposed AS INTEGER 
   FIELD iBorders AS INTEGER 
   FIELD iStep    AS INTEGER 
INDEX indXYZ  IS UNIQUE iPointX iPointY iPointZ
INDEX indStep iStep iPointX iPointY iPointZ.

DEFINE BUFFER ttOtherPoint FOR ttPoint.

DEFINE TEMP-TABLE ttDelta
   FIELD iDeltaX AS INTEGER 
   FIELD iDeltaY AS INTEGER 
   FIELD iDeltaZ AS INTEGER 
INDEX indXYZ IS UNIQUE iDeltaX iDeltaY iDeltaZ.

DEFINE VARIABLE lviMinX        AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxX        AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMinY        AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxY        AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMinZ        AS INTEGER NO-UNDO.
DEFINE VARIABLE lviMaxZ        AS INTEGER NO-UNDO.
DEFINE VARIABLE lNewExternal AS LOGICAL NO-UNDO.
   
DEFINE TEMP-TABLE ttAir
   FIELD iStep     AS INTEGER
   FIELD iAirX     AS INTEGER
   FIELD iAirY     AS INTEGER 
   FIELD iAirZ     AS INTEGER 
   FIELD lExternal AS LOGICAL 
INDEX indXYZ iAirX iAirY iAirZ.
   
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

COPY-LOB FROM FILE "input\18.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "2,2,2~n1,2,2~n3,2,2~n2,1,2~n2,3,2~n2,2,1~n2,2,3~n2,2,4~n2,2,6~n1,2,5~n3,2,5~n2,1,5~n2,3,5~n".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

RUN createDelta.

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

   CREATE ttPoint.
   ASSIGN 
      ttPoint.iPointX  = INTEGER (ENTRY(1, cLine))
      ttPoint.iPointY  = INTEGER (ENTRY(2, cLine))
      ttPoint.iPointZ  = INTEGER (ENTRY(3, cLine))
      ttPoint.iExposed = 6
   .
   IF iLine EQ 1 THEN DO:
      ASSIGN 
         lviMinX = ttPoint.iPointX
         lviMaxX = ttPoint.iPointX
         lviMinY = ttPoint.iPointY
         lviMaxY = ttPoint.iPointY
         lviMinZ = ttPoint.iPointZ
         lviMaxZ = ttPoint.iPointZ
      .
   END.
   ELSE DO:
      ASSIGN 
         lviMinX = MINIMUM (lviMinX, ttPoint.iPointX)
         lviMaxX = MAXIMUM (lviMaxX, ttPoint.iPointX)
         lviMinY = MINIMUM (lviMinY, ttPoint.iPointY)
         lviMaxY = MAXIMUM (lviMaxY, ttPoint.iPointY)
         lviMinZ = MINIMUM (lviMinZ, ttPoint.iPointZ)
         lviMaxZ = MAXIMUM (lviMaxZ, ttPoint.iPointZ)
      .
   END.
      
END. /* ReadBlock: */

IF lPart[1] THEN DO:
   /* Process Part One */
   IF lvlDebug THEN DO:
   END.
   ELSE DO: 
   END.

   iSolution = 0.
   
   FOR EACH ttPoint:
      FOR EACH ttDelta:
         FIND  ttOtherPoint
         WHERE ttOtherPoint.iPointX EQ (ttPoint.iPointX + ttDelta.iDeltaX)
         AND   ttOtherPoint.iPointY EQ (ttPoint.iPointY + ttDelta.iDeltaY)
         AND   ttOtherPoint.iPointZ EQ (ttPoint.iPointZ + ttDelta.iDeltaZ) NO-ERROR.
         IF AVAILABLE ttOtherPoint THEN DO:
            ttPoint.iExposed = ttPoint.iExposed - 1.
         END.
      END.
      iSolution = iSolution + ttPoint.iExposed.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 18 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   IF lvlDebug THEN DO:
   END.

   iSolution = 0.
   FOR EACH ttPoint:
      ttPoint.iExposed = 0.
      IF lvlDebug AND lvlShow THEN DO:
         IF ttPoint.iPointZ EQ 5 THEN DO:
            MESSAGE SUBSTITUTE ("Checking Point (&1, &2, &3)", 
                                ttPoint.iPointX,
                                ttPoint.iPointY,
                                ttPoint.iPointZ) 
            VIEW-AS ALERT-BOX.
         END.
      END.

      FOR EACH ttDelta:
         FIND  ttOtherPoint
         WHERE ttOtherPoint.iPointX EQ (ttPoint.iPointX + ttDelta.iDeltaX)
         AND   ttOtherPoint.iPointY EQ (ttPoint.iPointY + ttDelta.iDeltaY)
         AND   ttOtherPoint.iPointZ EQ (ttPoint.iPointZ + ttDelta.iDeltaZ) NO-ERROR.
         IF NOT AVAILABLE ttOtherPoint THEN DO:
            RUN checkAir
               (INPUT  (ttPoint.iPointX + ttDelta.iDeltaX),
                INPUT  (ttPoint.iPointY + ttDelta.iDeltaY),
                INPUT  (ttPoint.iPointZ + ttDelta.iDeltaZ),
                OUTPUT lNewExternal).
             IF lNewExternal THEN 
               ttPoint.iExposed = ttPoint.iExposed + 1.
         END.
      END.
      iSolution = iSolution + ttPoint.iExposed.
   END.

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 18 - Part Two".

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttAir:HANDLE).
   END.      
   
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

PROCEDURE checkAir:
/*------------------------------------------------------------------------------
 Purpose: Checks if Air cube is Internal or External to other Cubes
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiAirX     AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiAirY     AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiAirZ     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oplExternal AS LOGICAL NO-UNDO.

DEFINE BUFFER ttAir      FOR ttAir.
DEFINE BUFFER ttOtherAir FOR ttAir.
DEFINE BUFFER ttPoint    FOR ttPoint.
DEFINE BUFFER ttDelta    FOR ttDelta.
   
DEFINE VARIABLE iNewStep  AS INTEGER NO-UNDO.
DEFINE VARIABLE lChanges  AS LOGICAL NO-UNDO.
   
   IF lvlDebug AND lvlShow THEN DO:
      IF  ipiAirX = 1
      AND ipiAirY = 2
      AND ipiAirZ = 4 THEN DO:
         MESSAGE "Check" ipiAirX ipiAirY ipiAirZ
         VIEW-AS ALERT-BOX.
      END.
   END.
   
   iNewStep = 1.
   
   FIND  ttAir
   WHERE ttAir.iAirX EQ ipiAirX
   AND   ttAir.iAirY EQ ipiAirY
   AND   ttAir.iAirZ EQ ipiAirZ NO-ERROR.
   IF AVAILABLE ttAir THEN DO:
      /* Already checked */
      oplExternal = ttAir.lExternal.
      RETURN.
   END.
   
   /* First time this Air cube */
   CREATE ttAir.
   ASSIGN
      ttAir.iStep     = iNewStep 
      ttAir.iAirX     = ipiAirX
      ttAir.iAirY     = ipiAirY
      ttAir.iAirZ     = ipiAirZ
      ttAir.lExternal = ?
   .
   oplExternal = ?.
   
   SearchBlock:
   REPEAT:
      lChanges = FALSE.
      
      AirBlock:
      FOR EACH ttAir
      WHERE ttAir.iStep EQ iNewStep:
         /* All reachable Air with this Step */
         FOR EACH ttDelta:
            FIND  ttPoint
            WHERE ttPoint.iPointX EQ (ttAir.iAirX + ttDelta.iDeltaX)
            AND   ttPoint.iPointY EQ (ttAir.iAirY + ttDelta.iDeltaY)
            AND   ttPoint.iPointZ EQ (ttAir.iAirZ + ttDelta.iDeltaZ) NO-ERROR.
            IF AVAILABLE ttPoint THEN
               /* A Point blocks, continue searching Air */ 
               NEXT.
            FIND  ttOtherAir
            WHERE ttOtherAir.iAirX EQ (ttAir.iAirX + ttDelta.iDeltaX)
            AND   ttOtherAir.iAirY EQ (ttAir.iAirY + ttDelta.iDeltaY)
            AND   ttOtherAir.iAirZ EQ (ttAir.iAirZ + ttDelta.iDeltaZ) NO-ERROR.
            IF AVAILABLE ttOtherAir 
            AND ttOtherAir.lExternal NE ? THEN DO:
               /* Already checked */
               oplExternal = ttOtherAir.lExternal.
               LEAVE AirBlock.
            END.
            IF NOT AVAILABLE ttOtherAir THEN DO:
               /* First time this Air, check on next step */
               CREATE ttOtherAir.
               ASSIGN 
                  ttOtherAir.iAirX     = (ttAir.iAirX + ttDelta.iDeltaX)
                  ttOtherAir.iAirY     = (ttAir.iAirY + ttDelta.iDeltaY)
                  ttOtherAir.iAirZ     = (ttAir.iAirZ + ttDelta.iDeltaZ)
                  ttOtherAir.iStep     = ttAir.iStep + 1
                  ttOtherAir.lExternal = ?
               .
               lChanges = TRUE.
            END.                        
         END. /* FOR EACH ttDelta: */
      END. /* All reachable Air with this Step */
      
      IF oplExternal NE ? THEN DO:
         /* Found Air already checked */
         FOR EACH ttOtherAir
         WHERE ttOtherAir.lExternal EQ ?:
            ASSIGN 
               ttOtherAir.iStep     = ?
               ttOtherAir.lExternal = oplExternal
            .
         END.
         LEAVE SearchBlock.
      END. /* Found Air already checked */
                        
      FIND FIRST ttOtherAir
      WHERE ttOtherAir.iStep EQ iNewStep + 1
      AND  (ttOtherAir.iAirX LT (lviMinX - 1) OR ttOtherAir.iAirX GT (lviMaxX + 1)
         OR ttOtherAir.iAirY LT (lviMinY - 1) OR ttOtherAir.iAirY GT (lviMaxY + 1)
         OR ttOtherAir.iAirZ LT (lviMinZ - 1) OR ttOtherAir.iAirZ GT (lviMaxZ + 1)) NO-ERROR.
      IF AVAILABLE ttOtherAir THEN DO:
         /* Found ttOtherAir outside borders of Cubes */
         /* Flag all "unknown" air cubes as External */
         FOR EACH ttOtherAir
         WHERE ttOtherAir.lExternal EQ ?:
            ASSIGN 
               ttOtherAir.iStep     = ?
               ttOtherAir.lExternal = TRUE
            .
         END.
         oplExternal = TRUE.
         LEAVE SearchBlock.
      END. 
      IF lChanges EQ FALSE THEN DO:
         /* No more changes, Air is Internal */
         FOR EACH ttOtherAir
         WHERE ttOtherAir.lExternal EQ ?:
            ASSIGN 
               ttOtherAir.iStep     = ?
               ttOtherAir.lExternal = FALSE
            .
         END.
         oplExternal = FALSE.
         LEAVE SearchBlock.
      END. /* No more changes, Air is Internal */
            
      ASSIGN 
         iNewStep = iNewStep + 1
      .
   END. /* SearchBlock */
END.   

PROCEDURE createDelta:
/*------------------------------------------------------------------------------
 Purpose: Create a ttDelta record for every direction (-1, +1) per Axis
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLista AS CHARACTER NO-UNDO INITIAL "-1,0,0~n1,0,0~n0,-1,0~n0,1,0~n0,0,-1~n0,0,1".

DEFINE VARIABLE iDelta AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDelta AS CHARACTER NO-UNDO.

   DO iDelta = 1 TO NUM-ENTRIES (cLista, "~n"):
      cDelta = ENTRY (iDelta, cLista, "~n").
      CREATE ttDelta.
      ASSIGN 
         ttDelta.iDeltaX = INTEGER (ENTRY (1, cDelta))
         ttDelta.iDeltaY = INTEGER (ENTRY (2, cDelta))
         ttDelta.iDeltaZ = INTEGER (ENTRY (3, cDelta))
      .
   END.

END PROCEDURE.

   
/* ************************  Function Implementations ***************** */

