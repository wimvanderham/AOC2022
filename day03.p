
/*------------------------------------------------------------------------
    File        : day03.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 03 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 03 11:46:47 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/3".
DEFINE VARIABLE cCommand     AS CHARACTER NO-UNDO.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar        AS CHARACTER NO-UNDO.
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
DEFINE TEMP-TABLE ttRuckSackContents
   FIELD iNr          AS INTEGER 
   FIELD lCompartment AS LOGICAL FORMAT "Left/Right"
   FIELD iPosition    AS INTEGER 
   FIELD cItem        AS CHARACTER CASE-SENSITIVE 
   FIELD iPriority    AS INTEGER 
INDEX indItem IS UNIQUE iNr lCompartment iPosition
INDEX indSearch iNr lCompartment iPriority.

DEFINE TEMP-TABLE ttDouble
   FIELD iNr AS INTEGER 
   FIELD iPriority AS INTEGER 
INDEX indPriority IS UNIQUE iNr iPriority.

DEFINE BUFFER ttOtherRuckSackContents FOR ttRuckSackContents.
/* Specific variables */ 
DEFINE VARIABLE lCompartment AS LOGICAL NO-UNDO.   
   
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

COPY-LOB FROM FILE "input\03.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "vJrwpWtwJgWrhcsFMMfFFhFp~njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL~nPmmdzqPrVvPwwTWBwg~nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn~nttgJtRGJQctTZtZT~nCrZsJsPPZsGzwwsLwLmpwMDw".
   MESSAGE "Debug Input:" SKIP 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   lCompartment = FALSE.
   DO iChar = 1 TO LENGTH (cLine):
      cChar = SUBSTRING (cLine, iChar, 1).
      
      CREATE ttRuckSackContents.
      ASSIGN 
         ttRuckSackContents.iNr          = iLine
         ttRuckSackContents.lCompartment = lCompartment
         ttRuckSackContents.iPosition    = iChar
         ttRuckSackContents.cItem        = cChar
         ttRuckSackContents.iPriority    = ASC (cChar) - ASC ("a") + 1
      .
      IF ttRuckSackContents.iPriority LT 0 OR ttRuckSackContents.iPriority GT 26 THEN 
         ttRuckSackContents.iPriority = ASC (cChar) - ASC ("A") + 27.

      IF iChar EQ INTEGER (LENGTH (cLine) / 2) THEN DO:
         /* Second part in different compartment */
         lCompartment = NOT lCompartment.
      END.

   END.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttRuckSackContents:HANDLE).
END.
   
IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.
   
   FOR EACH ttRuckSackContents
   WHERE ttRuckSackContents.lCompartment EQ TRUE 
   BREAK 
   BY ttRuckSackContents.iNr
   BY ttRuckSackContents.lCompartment
   BY ttRuckSackContents.iPriority:
      FIND  FIRST ttOtherRuckSackContents
      WHERE ttOtherRuckSackContents.iNr          EQ ttRuckSackContents.iNr
      AND   ttOtherRuckSackContents.lCompartment EQ (NOT ttRuckSackContents.lCompartment)
      AND   ttOtherRuckSackContents.iPriority    EQ ttRuckSackContents.iPriority NO-ERROR.
      IF AVAILABLE ttOtherRuckSackContents THEN DO:
         FIND  ttDouble
         WHERE ttDouble.iNr       EQ ttRuckSackContents.iNr
         AND   ttDouble.iPriority EQ ttRuckSackContents.iPriority NO-ERROR.
         IF NOT AVAILABLE ttDouble THEN DO:
            /* First time this double found */
            CREATE ttDouble.
            ASSIGN 
               ttDouble.iNr       = ttRuckSackContents.iNr
               ttDouble.iPriority = ttRuckSackContents.iPriority
            .
            
            iSolution = iSolution + ttRuckSackContents.iPriority.
            IF lvlShow THEN DO:
               MESSAGE 
               ttRuckSackContents.cItem ttOtherRuckSackContents.cItem SKIP (2)
               ttRuckSackContents.iNr          "EQ" ttOtherRuckSackContents.iNr          SKIP 
               ttRuckSackContents.lCompartment "NE" ttOtherRuckSackContents.lCompartment SKIP 
               ttRuckSackContents.iPriority    "EQ" ttOtherRuckSackContents.iPriority     SKIP
               iSolution 
               VIEW-AS ALERT-BOX.
            END.
         END. /* First time this double found */
      END.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 03 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   iSolution = 0.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 03 - Part Two".
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


