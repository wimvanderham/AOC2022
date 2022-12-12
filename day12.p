
/*------------------------------------------------------------------------
    File        : day12.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 12 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 12 12:41:38 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/12".
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
DEFINE VARIABLE iRow         AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol         AS INTEGER NO-UNDO.
DEFINE VARIABLE iStartRow    AS INTEGER NO-UNDO.
DEFINE VARIABLE iStartCol    AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndRow      AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndCol      AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrentStep AS INTEGER NO-UNDO.
DEFINE VARIABLE iFromRow     AS INTEGER NO-UNDO.
DEFINE VARIABLE iFromCol     AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxRow      AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxCol      AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinSolution AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttNode
   FIELD iRow         AS INTEGER 
   FIELD iCol         AS INTEGER
   FIELD cChar        AS CHARACTER  
   FIELD iHeight      AS INTEGER
   FIELD iDistance    AS INTEGER
   FIELD iStep        AS INTEGER 
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indColRow IS UNIQUE iCol iRow
INDEX indStep   iStep iRow iCol.

DEFINE BUFFER ttNeighbour FOR ttNode.
DEFINE BUFFER ttStartNode FOR ttNode.

DEFINE TEMP-TABLE ttPath
   FIELD iRowFrom  AS INTEGER 
   FIELD iColFrom  AS INTEGER 
   FIELD iRowTo    AS INTEGER 
   FIELD iColTo    AS INTEGER 
   FIELD iDistance AS INTEGER
INDEX indPath IS UNIQUE iRowFrom iColFrom iRowTo iColTo.

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

COPY-LOB FROM FILE "input\12.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "Sabqponm~nabcryxxl~naccszExk~nacctuvwj~nabdefghi".
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

   iMaxCol = LENGTH (cLine).
   DO iCol = 1 TO LENGTH (cLine):
      iRow = iLine.
      cChar = SUBSTRING (cLine, iCol, 1).
      CREATE ttNode.
      ASSIGN 
         ttNode.iRow    = iRow
         ttNode.iCol    = iCol
         ttNode.cChar   = cChar
         ttNode.iStep   = -1
      .
      IF ASC (cChar) EQ ASC ("S") THEN DO:
         /* Found Start Position */
         ASSIGN 
            iStartRow = iRow
            iStartCol = iCol
         .
      END.
      ELSE DO: 
         IF ASC (cChar) EQ ASC ("E") THEN DO:
            /* Found End Position */
            ASSIGN
               iEndRow = iRow
               iEndCol = iCol
               ttNode.iHeight = ASC ("z") - ASC ("a")
            .
         END.
         ELSE DO:
            ASSIGN
               ttNode.iHeight = ASC (cChar) - ASC ("a")
            .
         END.
      END.
   END.
   iMaxRow = iLine.
END. /* ReadBlock: */

/* Create Paths between Nodes (To can be maximum 1 higher than From) */
FOR EACH ttNode:
   DO iRow = -1 TO 1:
      DO iCol = -1 TO 1:
         IF ABSOLUTE (iRow) EQ ABSOLUTE (iCol) THEN
            /* Exclude diagonals */
            NEXT.
         FIND  ttNeighbour 
         WHERE ttNeighbour.iRow = ttNode.iRow + iRow 
         AND   ttNeighbour.iCol = ttNode.iCol + iCol NO-ERROR.
         IF  AVAILABLE ttNeighbour
         AND ttNeighbour.iHeight LE ttNode.iHeight + 1 THEN DO:
            /* There's a path from ttNode to ttNeighbour */
            CREATE ttPath.
            ASSIGN 
               ttPath.iRowFrom  = ttNode.iRow
               ttPath.iColFrom  = ttNode.iCol
               ttPath.iRowTo    = ttNeighbour.iRow
               ttPath.iColTo    = ttNeighbour.iCol
               ttPath.iDistance = 1
            .
         END.
      END.
   END.
END.
IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttNode:HANDLE).
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttPath:HANDLE).
END.


IF lPart[1] THEN DO:
   /* Process Part One */
   iCurrentStep = 0.
   FIND  ttNode
   WHERE ttNode.iRow = iStartRow
   AND   ttNode.iCol = iStartCol.
   ttNode.iStep = iCurrentStep.

   IF lvlDebug THEN DO:
      OUTPUT TO "output\12.txt".
      PUT UNFORMATTED 
      SUBSTITUTE ("Debug run. Start: &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP.
      DO iRow = 1 TO iMaxRow:
         DO iCol = 1 TO iMaxCol:
            FIND  ttNode
            WHERE ttNode.iRow EQ iRow
            AND   ttNode.iCol EQ iCol.
            PUT UNFORMATTED
            SUBSTITUTE ("  &1", ttNode.cChar).
         END.
         PUT UNFORMATTED SKIP.
      END.
   END.
         
   SearchBlock:   
   REPEAT:
      IF lvlDebug THEN DO:
         PUT UNFORMATTED 
         SUBSTITUTE ("Current Step: &1", iCurrentStep) SKIP.
      END.
      
      FOR EACH ttNode
      WHERE ttNode.iStep EQ iCurrentStep:
         IF lvlDebug THEN DO:
            IF  ttNode.iRow EQ 5
            AND ttNode.iCol EQ 2 THEN DO:
               MESSAGE "Found ttNode (5,2)"
               VIEW-AS ALERT-BOX.
            END.
            IF iCurrentStep EQ 5 THEN DO:
               MESSAGE "Current Step 5"
               VIEW-AS ALERT-BOX.
            END.
         END.
         
         FOR EACH ttPath
         WHERE ttPath.iRowFrom EQ ttNode.iRow
         AND   ttPath.iColFrom EQ ttNode.iCol,
         FIRST ttNeighbour 
         WHERE ttNeighbour.iRow EQ ttPath.iRowTo
         AND   ttNeighbour.iCol EQ ttPath.iColTo:
            IF ttNeighbour.iStep     EQ -1
            OR ttNeighbour.iDistance GT ttNode.iDistance + 1 THEN DO:
               ASSIGN 
                  ttNeighbour.iDistance = ttNode.iDistance + 1
                  ttNeighbour.iStep     = iCurrentStep + 1
               .
               IF  ttNeighbour.iRow EQ iEndRow
               AND ttNeighbour.iCol EQ iEndCol THEN DO:
                  iSolution = ttNeighbour.iDistance.
                  LEAVE SearchBlock.
               END. 
            END.
         END.
      END.
      
      IF lvlDebug THEN DO:
         DO iRow = 1 TO iMaxRow:
            DO iCol = 1 TO iMaxCol:
               FIND  ttNode
               WHERE ttNode.iRow EQ iRow
               AND   ttNode.iCol EQ iCol.
               PUT UNFORMATTED 
               STRING (ttNode.iDistance, "-z9").
            END.
            PUT UNFORMATTED SKIP.
         END.
      END.
         
      iCurrentStep = iCurrentStep + 1.
   END.

   IF lvlDebug THEN DO:
      OUTPUT CLOSE.
   END.
            
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 12 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   
   /* Initialize Nodes */
   FOR EACH ttNode:
      ASSIGN 
         ttNode.iDistance = 0
         ttNode.iStep     = -1
      .
   END.

   iCurrentStep = 0.

   FIND  ttNode
   WHERE ttNode.iRow EQ iEndRow
   AND   ttNode.iCol EQ iEndCol.
   ttNode.iStep = iCurrentStep.
      
   SearchBlock:   
   REPEAT:
      /* SearchBlock: */
      FOR EACH ttNode
      WHERE ttNode.iStep EQ iCurrentStep:
         
         FOR EACH ttPath
         WHERE ttPath.iRowTo EQ ttNode.iRow
         AND   ttPath.iColTo EQ ttNode.iCol,
         FIRST ttNeighbour 
         WHERE ttNeighbour.iRow EQ ttPath.iRowFrom
         AND   ttNeighbour.iCol EQ ttPath.iColFrom:
            /* Search backwards */
            IF ttNeighbour.iStep     EQ -1
            OR ttNeighbour.iDistance GT ttNode.iDistance + 1 THEN DO:
               ASSIGN 
                  ttNeighbour.iDistance = ttNode.iDistance + 1
                  ttNeighbour.iStep     = iCurrentStep + 1
               .
               IF  ttNeighbour.iHeight EQ 0 THEN DO:
                  iSolution = ttNeighbour.iDistance.
                  LEAVE SearchBlock.
               END. 
            END.
         END. /* Search backwards */
      END.
         
      iCurrentStep = iCurrentStep + 1.
   END. /* SearchBlock: */

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 12 - Part Two".
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
