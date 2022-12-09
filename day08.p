
/*------------------------------------------------------------------------
    File        : day08.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 8 Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 09 02:19:04 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/8".
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
DEFINE VARIABLE iTrees       AS INTEGER   NO-UNDO EXTENT 4.

/* Specific */
DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttGrid
   FIELD iRow         AS INTEGER 
   FIELD iCol         AS INTEGER 
   FIELD iHeight      AS INTEGER
   FIELD lVisible     AS LOGICAL INITIAL ?
   FIELD iScenicScore AS INTEGER 
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indColRow IS UNIQUE iCol iRow.

DEFINE BUFFER ttTree FOR ttGrid.

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

COPY-LOB FROM FILE "input\08.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "30373~n25512~n65332~n33549~n35390~n".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

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

   DO iCol = 1 TO LENGTH (cLine):
      iRow = iLine.
      CREATE ttGrid.
      ASSIGN 
         ttGrid.iRow    = iRow
         ttGrid.iCol    = iCol
         ttGrid.iHeight = INTEGER (SUBSTRING (cLine, iCol, 1))
      .
      IF iLine EQ 1 THEN 
         ttGrid.lVisible = TRUE.
      IF iCol EQ 1 THEN 
         ttGrid.lVisible = TRUE.
      IF iCol EQ LENGTH (cLine) THEN 
         ttGrid.lVisible = TRUE.
   END.
END. /* ReadBlock: */

FOR EACH ttGrid
WHERE ttGrid.iRow EQ iRow:
   ttGrid.lVisible = TRUE.
END.
         
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttGrid:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */

   FOR EACH ttGrid
   WHERE ttGrid.lVisible EQ ?:
      FIND FIRST ttTree
      WHERE ttTree.iRow    EQ ttGrid.iRow
      AND   ttTree.iCol    LT ttGrid.iCol
      AND   ttTree.iHeight GE ttGrid.iHeight NO-ERROR.
      IF NOT AVAILABLE ttTree THEN DO:
         ttGrid.lVisible = TRUE.
      END.
      ELSE DO:
         FIND FIRST ttTree
         WHERE ttTree.iRow    EQ ttGrid.iRow
         AND   ttTree.iCol    GT ttGrid.iCol
         AND   ttTree.iHeight GE ttGrid.iHeight NO-ERROR.
         IF NOT AVAILABLE ttTree THEN DO:
            ttGrid.lVisible = TRUE.
         END.
         ELSE DO:
            FIND FIRST ttTree
            WHERE ttTree.iCol    EQ ttGrid.iCol
            AND   ttTree.iRow    GT ttGrid.iRow
            AND   ttTree.iHeight GE ttGrid.iHeight NO-ERROR.
            IF NOT AVAILABLE ttTree THEN DO:
               ttGrid.lVisible = TRUE.
            END.
            ELSE DO:
               FIND FIRST ttTree
               WHERE ttTree.iCol    EQ ttGrid.iCol
               AND   ttTree.iRow    LT ttGrid.iRow
               AND   ttTree.iHeight GE ttGrid.iHeight NO-ERROR.
               IF NOT AVAILABLE ttTree THEN DO:
                  ttGrid.lVisible = TRUE.
               END.
               ELSE DO:
                  ttGrid.lVisible = FALSE.
               END.
            END.
         END.
      END.
   END.

   FOR EACH ttGrid 
   WHERE ttGrid.lVisible EQ TRUE:
      ACCUM "" (COUNT).
   END.
      
   ASSIGN 
      iSolution = (ACCUM COUNT "")
   .
     
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 08 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   FOR EACH ttGrid:
      iTrees = 0.
      FOR EACH ttTree 
      WHERE ttTree.iRow EQ ttGrid.iRow
      AND   ttTree.iCol LT ttGrid.iCol
      BY ttTree.iCol DESCENDING:
         /* Look Left */
         iTrees[1] = iTrees[1] + 1.
         IF ttTree.iHeight GE ttGrid.iHeight THEN 
            LEAVE.
      END.
      FOR EACH ttTree 
      WHERE ttTree.iRow EQ ttGrid.iRow
      AND   ttTree.iCol GT ttGrid.iCol:
         /* Look Right */
         iTrees[2] = iTrees[2] + 1.
         IF ttTree.iHeight GE ttGrid.iHeight THEN 
            LEAVE.
      END.
      IF lvlDebug THEN DO:
         IF ttGrid.iRow = 3 AND ttGrid.iCol = 2 THEN 
            MESSAGE ttGrid.iRow ttGrid.iCol ttGrid.iHeight
            VIEW-AS ALERT-BOX.
      END.
      
      FOR EACH ttTree 
      WHERE ttTree.iCol EQ ttGrid.iCol
      AND   ttTree.iRow LT ttGrid.iRow
      BY ttTree.iRow DESCENDING:
         /* Look Up */
         iTrees[3] = iTrees[3] + 1.
         IF ttTree.iHeight GE ttGrid.iHeight THEN 
            LEAVE.
      END.
      FOR EACH ttTree 
      WHERE ttTree.iCol EQ ttGrid.iCol
      AND   ttTree.iRow GT ttGrid.iRow:
         /* Look Down */
         iTrees[4] = iTrees[4] + 1.
         IF ttTree.iHeight GE ttGrid.iHeight THEN 
            LEAVE.
      END.
      
      ASSIGN 
         ttGrid.iScenicScore = iTrees[1] * iTrees[2] * iTrees[3] * iTrees[4]
      .
      IF ttGrid.iScenicScore GT iSolution THEN DO:
         IF lvlDebug THEN DO:
            MESSAGE SUBSTITUTE ("New best scenic score: Row &1 Col &2 Height &3 Score Left &4 * Right &5 * Up &6 * Down &7 = &8",
                                ttGrid.iRow,
                                ttGrid.iCol,
                                ttGrid.iHeight,
                                iTrees[1],
                                iTrees[2],
                                iTrees[3],
                                iTrees[4],
                                ttGrid.iScenicScore)
            VIEW-AS ALERT-BOX.
         END.
         iSolution = ttGrid.iScenicScore.
      END.
   END.
          
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 08 - Part Two".
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
