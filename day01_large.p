
/*------------------------------------------------------------------------
    File        : day01_large.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day01 of ACO2022 - Optimized for large file

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Dec 01 06:28:35 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/1".
DEFINE VARIABLE cCommand     AS CHARACTER NO-UNDO.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
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
DEFINE VARIABLE iNr AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttElf
   FIELD iNr AS INTEGER 
   FIELD iTotCal AS INT64
INDEX indNR IS UNIQUE iNr.
/* Variables for Large input file */
DEFINE VARIABLE iCal   AS INT64   NO-UNDO.
DEFINE VARIABLE iTotal AS INT64   NO-UNDO.
DEFINE VARIABLE iMax   AS INT64   NO-UNDO EXTENT 3.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


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
   cCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cCommand).
END.

/* Start Processing */
ETIME (YES).

INPUT FROM "C:\Users\wim\Downloads\aoc_2022_day01_large_input.txt".
REPEAT:
   IMPORT UNFORMATTED iCal.
   IF iCal EQ 0 THEN DO:
      IF iTotal GT iMax[1] THEN DO:
         ASSIGN 
            iMax[3] = iMax[2]
            iMax[2] = iMax[1]
            iMax[1] = iTotal
         .
      END.
      ELSE DO:
         IF iTotal GT iMax[2] THEN DO:
            ASSIGN 
               iMax[3] = iMax[2]
               iMax[2] = iTotal
            .
         END.
         ELSE DO:
            IF iTotal GT iMax[3] THEN DO:
               ASSIGN 
                  iMax[3] = iTotal
               .
            END.
         END.
      END.
      ASSIGN 
         iTotal = 0
      .
   END.
   ELSE DO:
      iTotal = iTotal + iCal.
   END. 
END.
INPUT CLOSE.


/* Part 1 */
iSolution = iMax[1].
MESSAGE 
   SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   STRING (INTEGER (ETIME / 1000), "HH:MM:SS")
VIEW-AS ALERT-BOX TITLE " 2022 - Day 01 - Part One".

/* Part 2 */
iSolution = iMax[1] + iMax[2] + iMax[3].

MESSAGE 
   SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
VIEW-AS ALERT-BOX TITLE " 2022 - Day 01 - Part Two".

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

