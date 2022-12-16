
/*------------------------------------------------------------------------
    File        : day15.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 15 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Dec 15 22:33:44 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/15".
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

/* Specific */
DEFINE VARIABLE iNewNr        AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewManhattan AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewY         AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaX       AS INTEGER NO-UNDO.
DEFINE VARIABLE iMovePoint    AS INTEGER NO-UNDO.
DEFINE VARIABLE lConcat       AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttSensor
   FIELD iNr        AS INTEGER 
   FIELD iX         AS INTEGER 
   FIELD iY         AS INTEGER 
   FIELD iBeaconX   AS INTEGER 
   FIELD iBeaconY   AS INTEGER 
   FIELD iManhattan AS INTEGER 
INDEX indNr IS UNIQUE iNr.

DEFINE TEMP-TABLE ttRange
   FIELD iNr    AS INTEGER 
   FIELD iFromX AS INTEGER 
   FIELD iToX   AS INTEGER 
INDEX indNr IS UNIQUE iNr.

DEFINE BUFFER ttNextRange FOR ttRange.

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

COPY-LOB FROM FILE "input\15.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15~nSensor at x=9, y=16: closest beacon is at x=10, y=16~nSensor at x=13, y=2: closest beacon is at x=15, y=3~nSensor at x=12, y=14: closest beacon is at x=10, y=16~nSensor at x=10, y=20: closest beacon is at x=10, y=16~nSensor at x=14, y=17: closest beacon is at x=10, y=16~nSensor at x=8, y=7: closest beacon is at x=2, y=10~nSensor at x=2, y=0: closest beacon is at x=2, y=10~nSensor at x=0, y=11: closest beacon is at x=2, y=10~nSensor at x=20, y=14: closest beacon is at x=25, y=17~nSensor at x=17, y=20: closest beacon is at x=21, y=22~nSensor at x=16, y=7: closest beacon is at x=15, y=3~nSensor at x=14, y=3: closest beacon is at x=15, y=3~nSensor at x=20, y=1: closest beacon is at x=15, y=3".
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

   iNewNr = iNewNr + 1.
   CREATE ttSensor.
   ASSIGN 
      ttSensor.iNr        = iNewNr
      ttSensor.iX         = INTEGER (ENTRY (1, ENTRY (2, cLine, "=")))
      ttSensor.iY         = INTEGER (ENTRY (1, ENTRY (3, cLine, "="), ":"))
      ttSensor.iBeaconX   = INTEGER (ENTRY (1, ENTRY (4, cLine, "=")))
      ttSensor.iBeaconY   = INTEGER (ENTRY (5, cLine, "="))
      ttSensor.iManhattan = ABSOLUTE (ttSensor.iX - ttSensor.iBeaconX) + 
                            ABSOLUTE (ttSensor.iY - ttSensor.iBeaconY)
   .
END. /* ReadBlock: */
         
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttSensor:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   IF lvlDebug THEN 
      iNewY = 10.
   ELSE 
      iNewY = 2000000.
      
   iSolution = 0.
   FOR EACH ttSensor:
      iNewManhattan = ABSOLUTE (ttSensor.iY - iNewY).
      IF iNewManhattan LE ttSensor.iManhattan THEN DO:
         /* Line is within sensor range */
         CREATE ttRange.
         ASSIGN 
            ttRange.iNr    = ttSensor.iNr
            ttRange.iFromX = ttSensor.iX - (ttSensor.iManhattan - iNewManhattan)
            ttRange.iToX   = ttSensor.iX + (ttSensor.iManhattan - iNewManhattan)
         .
      END.
   END.   
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttRange:HANDLE).
   END.
   
   FOR EACH ttRange:
      FOR EACH ttNextRange
      WHERE ttNextRange.iNr    NE ttRange.iNr
      AND   ttNextRange.iToX   GE ttRange.iFromX
      AND   ttNextRange.iFromX LE ttRange.iToX:
         /* Overlapping ranges */
         IF  ttNextRange.iFromX GE ttRange.iFromX
         AND ttNextRange.iToX   LE ttRange.iToX THEN DO:
            /* Completely overlap, delete NextRange */
            DELETE ttNextRange.
         END. /* Completely overlap, delete NextRange */
         ELSE DO:
            IF  ttNextRange.iFromX LT ttRange.iFromX 
            AND ttNextRange.iToX   GE ttRange.iFromX
            AND ttNextRange.iToX   LE ttRange.iToX THEN DO:
               /* Partial overlapping right part, cut off */
               ttNextRange.iToX = ttRange.iFromX - 1.
            END.
            ELSE DO:
               IF  ttNextRange.iFromX GE ttRange.iFromX
               AND ttNextRange.iToX   LE ttRange.iToX 
               AND ttNextRange.iToX   GT ttRange.iToX THEN DO:
                  /* Partial overlapping left part, cut off */
                  ttNextRange.iFromX = ttRange.iToX + 1.
               END.
            END.
         END.
      END. /* FOR EACH ttNextRange */
   END. /* FOR EACH ttRange: */

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttRange:HANDLE).
   END.

   ConcatBlock:
   REPEAT:
      lConcat = FALSE.
      FOR EACH ttRange:
         FIND  ttNextRange
         WHERE ttNextRange.iFromX EQ ttRange.iToX + 1 NO-ERROR.
         IF AVAILABLE ttNextRange THEN DO:
            ttRange.iToX = ttNextRange.iToX.
            DELETE ttNextRange.
            lConcat = TRUE.
            NEXT ConcatBlock.
         END.
      END.
      IF lConcat EQ FALSE THEN 
         LEAVE ConcatBlock.
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttRange:HANDLE).
   END.
      
   iSolution = 0.
   FOR EACH ttRange:
      iSolution = iSolution + (ttRange.iToX - ttRange.iFromX) + 1.
   END.
   
   /* Don't count the Beacons */
   FOR EACH ttSensor
   WHERE ttSensor.iBeaconY EQ iNewY
   BREAK 
   BY ttSensor.iBeaconY
   BY ttSensor.iBeaconX:
      /* All Beacons on the Y-line */
      IF FIRST-OF (ttSensor.iBeaconX) THEN DO:
         /* Only first occurances counts */
         iSolution = iSolution - 1.
      END.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 15 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 15 - Part Two".
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
