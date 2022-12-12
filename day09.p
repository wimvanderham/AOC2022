
/*------------------------------------------------------------------------
    File        : day09.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 09 of Advent of Code

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 10 00:37:20 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/9".
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
DEFINE TEMP-TABLE ttGrid
   FIELD iRow         AS INTEGER 
   FIELD iCol         AS INTEGER 
   FIELD lTailTouched AS LOGICAL INITIAL ?
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indColRow IS UNIQUE iCol iRow.

DEFINE TEMP-TABLE ttKnot
   FIELD iNr  AS INTEGER 
   FIELD iRow AS INTEGER 
   FIELD iCol AS INTEGER 
INDEX indNr IS UNIQUE iNr.

DEFINE VARIABLE iHeadRow     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iHeadCol     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTailRow     AS INTEGER   NO-UNDO EXTENT 9.
DEFINE VARIABLE iTailCol     AS INTEGER   NO-UNDO EXTENT 9.
DEFINE VARIABLE cDirection   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMoves       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMove        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iKnot        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPrevKnotRow AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPrevKnotCol AS INTEGER   NO-UNDO.

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

COPY-LOB FROM FILE "input\09.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "R 4~nU 4~nL 3~nD 1~nR 4~nD 1~nL 5~nR 2".
   IF lPart[2] THEN
      lcInput = "R 5~nU 8~nL 8~nD 3~nR 17~nD 10~nL 25~nU 20".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
   OUTPUT TO "output\09.txt".
   PUT UNFORMATTED
   SUBSTITUTE ("Start &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP  
   SUBSTITUTE ("Debug run with input:") SKIP 
   STRING(lcInput) SKIP(2).
   OUTPUT CLOSE.
END.

/* Start position */
IF lPart[1] THEN DO:
   CREATE ttKnot.
   ASSIGN 
      ttKnot.iNr  = 0
      ttKnot.iRow = 0
      ttKnot.iCol = 0
   .
END.
IF lPart[2] THEN DO:
   DO iKnot = 1 TO 9:
      CREATE ttKnot.
      ASSIGN 
         ttKnot.iNr  = iKnot
         ttKnot.iRow = 0
         ttKnot.iCol = 0
      .
   END.
END.
 
ASSIGN 
   iHeadRow     = 0
   iHeadCol     = 0
.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      IF lvlDebug THEN DO:
         MESSAGE "Found empty line:" iLine
         VIEW-AS ALERT-BOX.
      END.
      NEXT ReadBlock.
   END.

   ASSIGN 
      cDirection = ENTRY (1, cLine, " ")
      iMoves     = INTEGER (ENTRY (2, cLine, " "))
   .
   
   DO iMove = 1 TO iMoves:
      
      CASE cDirection:
         WHEN "U" THEN iHeadRow = iHeadRow + 1.
         WHEN "D" THEN iHeadRow = iHeadRow - 1.
         WHEN "R" THEN iHeadCol = iHeadCol + 1.
         WHEN "L" THEN iHeadCol = iHeadCol - 1.
      END CASE.
      
      IF lvlDebug AND lvlShow THEN DO:
         MESSAGE
         SUBSTITUTE ("Head Row &1 Col &2, Tail Row &3 Col &4",
                     iHeadRow,
                     iHeadCol,
                     iTailRow[1],
                     iTailCol[1]) 
         VIEW-AS ALERT-BOX TITLE "Before".
      END.
      
      ASSIGN
         iPrevKnotRow = iHeadRow
         iPrevKnotCol = iHeadCol
      . 
         
      FOR EACH ttKnot:
         RUN getTailPosition
            (INPUT        iPrevKnotRow,
             INPUT        iPrevKnotCol,
             INPUT-OUTPUT ttKnot.iRow,
             INPUT-OUTPUT ttKnot.iCol).
          ASSIGN 
            iPrevKnotRow = ttKnot.iRow
            iPrevKnotCol = ttKnot.iCol
         .
         FIND  ttGrid
         WHERE ttGrid.iRow EQ ttKnot.iRow
         AND   ttGrid.iCol EQ ttKnot.iCol NO-ERROR.
         IF NOT AVAILABLE ttGrid THEN DO:
            CREATE ttGrid.
            ASSIGN 
               ttGrid.iRow = ttKnot.iRow
               ttGrid.iCol = ttKnot.iCol
            .
         END.
         IF ttKnot.iNr EQ 9 THEN DO:
            ASSIGN 
               ttGrid.lTailTouched = TRUE 
            .
         END.
      END.

      IF lvlDebug AND lvlShow THEN DO:
         MESSAGE
         SUBSTITUTE ("Tail Row &3 Col &4",
                     iHeadRow,
                     iHeadCol,
                     iTailRow[1],
                     iTailCol[1]) 
         VIEW-AS ALERT-BOX TITLE "After".
      END.

      IF lvlDebug THEN DO:
         ASSIGN 
            iTailRow = 0
            iTailCol = 0
         .
         IF lPart[1] THEN DO:
            FIND  ttKnot
            WHERE ttKnot.iNr EQ 0.
            ASSIGN 
               iTailRow[1] = ttKnot.iRow
               iTailCol[1] = ttKnot.iCol
            .
         END.
         IF lPart[2] THEN DO:
            FOR EACH ttKnot:
               ASSIGN 
                  iTailRow[ttKnot.iNr] = ttKnot.iRow
                  iTailCol[ttKnot.iNr] = ttKnot.iCol
               .
            END.
         END.
         RUN printGrid
            (INPUT "output\09.txt",
             INPUT SUBSTITUTE ("Input: &4~nDirection: &1, &2/&3", cDirection, iMove, iMoves, cLine),
             INPUT 5,
             INPUT 6,
             INPUT iHeadRow,
             INPUT iHeadCol,
             INPUT iTailRow,
             INPUT iTailCol,
             INPUT FALSE).
       END.
   END.
              
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttGrid:HANDLE).
END.

iSolution = 0.
FOR EACH ttGrid
WHERE ttGrid.lTailTouched:
   iSolution = iSolution + 1.
END.

IF lvlDebug THEN DO:
   ASSIGN 
      iTailRow = 0
      iTailCol = 0
   .
   IF lPart[1] THEN DO:
      FIND ttKnot 
      WHERE ttKnot.iNr EQ 0.
      ASSIGN 
         iTailRow [1] = ttKnot.iRow
         iTailCol [1] = ttKnot.iCol
      .
   END.
   ELSE DO:
      FOR EACH ttKnot:
         ASSIGN 
            iTailRow[ttKnot.iNr] = ttKnot.iRow
            iTailCol[ttKnot.iNr] = ttKnot.iCol
         .
      END.
   END.
   RUN printGrid
      (INPUT "output\09.txt",
       INPUT SUBSTITUTE ("Fine"),
       INPUT 5,
       INPUT 6,
       INPUT iHeadRow,
       INPUT iHeadCol,
       INPUT iTailRow,
       INPUT iTailCol,
       INPUT TRUE).

   cOSCommand = SUBSTITUTE ("start &1", "output\09.txt").
   OS-COMMAND SILENT VALUE (cOSCommand).
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
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 09 - Part One".
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
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 09 - Part Two".
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
PROCEDURE getTailPosition:
   DEFINE INPUT        PARAMETER ipiHeadRow     AS INTEGER NO-UNDO.
   DEFINE INPUT        PARAMETER ipiHeadCol     AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ppiTailRow     AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ppiTailCol     AS INTEGER NO-UNDO.
   
   IF (ABSOLUTE (ipiHeadCol - ppiTailCol) EQ 1 AND ABSOLUTE (ipiHeadRow - ppiTailRow) EQ 1) 
   OR (ipiHeadCol EQ ppiTailCol AND ABSOLUTE (ipiHeadRow - ppiTailRow) EQ 1)
   OR (ipiHeadRow EQ ppiTailRow AND ABSOLUTE (ipiHeadCol - ppiTailCol) EQ 1) 
   THEN
      /* No move necessary */
      RETURN.
      
   IF ipiHeadRow EQ ppiTailRow AND ABSOLUTE (ipiHeadCol - ppiTailCol) GT 1 THEN
      ppiTailCol = ppiTailCol + IF ipiHeadCol GT ppiTailCol THEN 1 ELSE -1.
   ELSE DO:
      IF ipiHeadCol EQ ppiTailCol AND ABSOLUTE (ipiHeadRow - ppiTailRow) GT 1 THEN 
         ppiTailRow = ppiTailRow + IF ipiHeadRow GT ppiTailRow THEN 1 ELSE -1.
      ELSE DO:
         IF ABSOLUTE (ipiHeadCol - ppiTailCol) + ABSOLUTE (ipiHeadRow - ppiTailRow) GT 1 THEN
            ASSIGN
               ppiTailRow = ppiTailRow + IF ipiHeadRow GT ppiTailRow THEN 1 ELSE -1 WHEN ABSOLUTE (ipiHeadRow - ppiTailRow) GE 1
               ppiTailCol = ppiTailCol + IF ipiHeadCol GT ppiTailCol THEN 1 ELSE -1 WHEN ABSOLUTE (ipiHeadCol - ppiTailCol) GE 1
            .
      END.
   END.
   
END PROCEDURE. /* getTailPosition */
   
PROCEDURE printGrid:
   DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ipcTitolo   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiRowMax   AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipiColMax   AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipiHeadRow  AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipiHeadCol  AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipiTailRow  AS INTEGER   NO-UNDO EXTENT 9.
   DEFINE INPUT  PARAMETER ipiTailCol  AS INTEGER   NO-UNDO EXTENT 9.
   DEFINE INPUT  PARAMETER iplFinal    AS LOGICAL   NO-UNDO.
   
   DEFINE VARIABLE iRow    AS INTEGER NO-UNDO.
   DEFINE VARIABLE iCol    AS INTEGER NO-UNDO.
   DEFINE VARIABLE iKnot   AS INTEGER NO-UNDO.
   DEFINE VARIABLE lShown  AS LOGICAL NO-UNDO.
   
   FOR EACH ttGrid:
      ACCUM ttGrid.iRow (MAXIMUM).
      ACCUM ttGrid.iCol (MAXIMUM).
   END.
   
   ASSIGN 
      ipiRowMax = MAXIMUM (ipiRowMax, (ACCUM MAXIMUM ttGrid.iRow))
      ipiColMax = MAXIMUM (ipiColMax, (ACCUM MAXIMUM ttGrid.iCol))
   .
   
   OUTPUT TO VALUE (ipcFileName) APPEND.
   PUT UNFORMATTED 
   ipcTitolo SKIP.
   DO iRow = ipiRowMax - 1 TO 0 BY -1:
      DO iCol = 0 TO ipiColMax - 1:
         lShown = FALSE.
         IF iRow EQ 0 AND iCol = 0 THEN DO:
            lShown = TRUE.
            PUT UNFORMATTED 
            "s".
         END.
         ELSE DO:
            FIND  ttGrid 
            WHERE ttGrid.iRow EQ iRow
            AND   ttGrid.iCol EQ iCol NO-ERROR.
            IF iplFinal EQ FALSE THEN DO:
               /* Show intermediate Grid with Head and Tail */
               IF iRow EQ ipiHeadRow AND iCol EQ ipiHeadCol THEN DO:
                  lShown = TRUE.
                  PUT UNFORMATTED
                  "H".
               END.
               ELSE DO:
                  DO iKnot = 1 TO 9:
                     IF iRow EQ ipiTailRow[iKnot] AND iCol EQ ipiTailCol[iKnot] THEN DO:
                        IF NOT lShown THEN DO: 
                           lShown = TRUE. 
                           PUT UNFORMATTED 
                           STRING (iKnot).
                        END.
                     END.
                     IF NOT lShown THEN DO:
                        IF AVAILABLE ttGrid THEN
                           PUT UNFORMATTED 
                           (IF ttGrid.lTailTouched THEN "#" ELSE ".").
                        ELSE
                           PUT UNFORMATTED 
                           ".".
                        lShown = TRUE.
                     END.                     
                  END.
               END.
            END.
            ELSE DO:
               IF AVAILABLE ttGrid THEN DO:
                  PUT UNFORMATTED 
                  (IF ttGrid.lTailTouched THEN "#" ELSE ".").
               END.
               ELSE DO:
                  PUT UNFORMATTED 
                  ".".
               END.
            END.
         END.
      END.
      PUT UNFORMATTED 
      SKIP.
   END.
   PUT UNFORMATTED
   SKIP (2).
   OUTPUT CLOSE.
   
END PROCEDURE.
   
/* ************************  Function Implementations ***************** */
