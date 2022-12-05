
/*------------------------------------------------------------------------
    File        : day05.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 5 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 05 09:04:34 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/5".
DEFINE VARIABLE cCommand     AS CHARACTER NO-UNDO.

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

/* Specific */
DEFINE TEMP-TABLE ttStack
   FIELD iStackNr       AS INTEGER 
   FIELD iCratePosition AS INTEGER 
   FIELD cTopCrate      AS CHARACTER 
INDEX indStackNr IS UNIQUE iStackNr.

DEFINE TEMP-TABLE ttCrate
   FIELD iCrateNr  AS INTEGER 
   FIELD iStackNr  AS INTEGER 
   FIELD iPosition AS INTEGER 
   FIELD cLetter   AS CHARACTER 
INDEX indCrateNr IS UNIQUE iCrateNr
INDEX indStack   iStackNr iPosition DESCENDING.

DEFINE VARIABLE iLastCrateNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLastStackNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iCrates      AS INTEGER NO-UNDO.
DEFINE VARIABLE iFromStack   AS INTEGER NO-UNDO.
DEFINE VARIABLE iToStack     AS INTEGER NO-UNDO.

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
   cCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cCommand).
END.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\05.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   // lcInput = "~n".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

/* Read Input into Temp-table */
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

   IF cLine BEGINS "[" THEN DO:
      /* Found a crate */
      DO iChar = 1 TO LENGTH (cLine):
         /* Crates are in stacks, input is like this:
            
            [B]                     [N]     [H]
            [V]         [P] [T]     [V]     [P]
            [W]     [C] [T] [S]     [H]     [N]
            [T]     [J] [Z] [M] [N] [F]     [L]
            [Q]     [W] [N] [J] [T] [Q] [R] [B]
            [N] [B] [Q] [R] [V] [F] [D] [F] [M]
            [H] [W] [S] [J] [P] [W] [L] [P] [S]
            [D] [D] [T] [F] [G] [B] [B] [H] [Z]
             1   2   3   4   5   6   7   8   9
         */     
         cChar = SUBSTRING (cLine, iChar, 1).
         IF cChar EQ "[" THEN DO:        
            FIND  ttStack 
            WHERE ttStack.iCratePosition EQ iChar + 1 NO-ERROR.
            IF NOT AVAILABLE ttStack THEN DO:
               CREATE ttStack.
               ASSIGN
                  ttStack.iCratePosition = iChar  + 1
               .
               ASSIGN
                  /* Every 4 characters there's a new stack, 
                  ** first stack starts at position 2 */
                  ttStack.iStackNr = INTEGER ((ttStack.iCratePosition + 2) / 4)
               .
            END.
            iLastCrateNr = iLastCrateNr + 1.
            CREATE ttCrate.
            ASSIGN
               ttCrate.iCrateNr  = iLastCrateNr
               ttCrate.iStackNr  = ttStack.iStackNr
               ttCrate.iPosition = 9 /* Max line */ - iLine
               ttCrate.cLetter   = SUBSTRING (cLine, iChar + 1, 1)
            .
            IF ttStack.cTopCrate = "" THEN 
               ttStack.cTopCrate = ttCrate.cLetter.
            /* Handled this crate, move to (possible) next stack */
            iChar = iChar + 3.
         END.
      END.
   END. /* Found a crate */
   
   IF cLine BEGINS "1" THEN DO:
      /* Line with the Stack number */
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttStack:HANDLE).
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttCrate:HANDLE).
      END.
   END.
      
   IF cLine BEGINS "move" THEN DO:
      /* Move instruction. */ 
      /* Instructions have this format:
         move 2 from 8 to 1
         move 4 from 9 to 8
      */
      ASSIGN 
         iCrates    = INTEGER (ENTRY (2, cLine, " "))
         iFromStack = INTEGER (ENTRY (4, cLine, " "))
         iToStack   = INTEGER (ENTRY (6, cLine, " "))
      .
         
      IF lPart[1] THEN DO:
         RUN moveCrates
            (INPUT iCrates,
             INPUT iFromStack,
             INPUT iToStack).
      END.
      ELSE DO:
         RUN moveCrates_2
            (INPUT iCrates,
             INPUT iFromStack,
             INPUT iToStack).
      END.
   END. /* Move instruction. */
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttStack:HANDLE).
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttCrate:HANDLE).
END.

cSolution = "".

FOR EACH ttStack
BY ttStack.iCratePosition:
   cSolution = cSolution + ttStack.cTopCrate.
END.
   
IF lPart[1] THEN DO:
   /* Process Part One */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED cSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         cSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 04 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED cSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         cSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 05 - Part Two".
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
PROCEDURE moveCrates:
   DEFINE INPUT  PARAMETER ipiNrCrates  AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiFromStack AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiToStack   AS INTEGER NO-UNDO.
   
   DEFINE VARIABLE iCrate AS INTEGER NO-UNDO.
   DEFINE VARIABLE iDone  AS INTEGER NO-UNDO.
   
   DEFINE BUFFER ttFromStack   FOR ttStack.
   DEFINE BUFFER ttToStack     FOR ttStack.
   DEFINE BUFFER ttTopCrate    FOR ttCrate.
   DEFINE BUFFER ttNewTopCrate FOR ttCrate.
   DEFINE BUFFER ttOldTopCrate FOR ttCrate.
   
   FIND  ttFromStack
   WHERE ttFromStack.iStackNr EQ ipiFromStack.
   FIND  ttToStack
   WHERE ttToStack.iStackNr   EQ ipiToStack.
   
   MoveBlock:
   FOR EACH ttTopCrate
   WHERE ttTopCrate.iStackNr EQ ttFromStack.iStackNr
   BY ttTopCrate.iStackNr
   BY ttTopCrate.iPosition DESCENDING:
      /* Move Crates from Stack "From" to Stack "To" */
      
      /* Pop crate from FromStack */
      ASSIGN
         ttTopCrate.iStackNr = ttToStack.iStackNr
      .
      FIND  ttNewTopCrate
      WHERE ttNewTopCrate.iStackNr  EQ ttFromStack.iStackNr
      AND   ttNewTopCrate.iPosition EQ (ttTopCrate.iPosition - 1) NO-ERROR.
      IF AVAILABLE ttNewTopCrate THEN DO:
         ASSIGN 
            ttFromStack.cTopCrate = ttNewTopCrate.cLetter
         .
      END.
      ELSE DO:
         ASSIGN 
            ttFromStack.cTopCrate = ""
         .
      END.

      /* Push to ToStack */
      IF ttToStack.cTopCrate NE "" THEN DO:
         ASSIGN 
            ttTopCrate.iPosition = 0
         .
         /* ToStack has Crates */
         FOR EACH ttOldTopCrate
         WHERE ttOldTopCrate.iStackNr = ttToStack.iStackNr
         BY ttOldTopCrate.iStackNr
         BY ttOldTopCrate.iPosition DESCENDING:
            ASSIGN 
               ttTopCrate.iPosition = ttOldTopCrate.iPosition + 1
            .
            LEAVE.
         END.
      END.
      ELSE DO:
         ASSIGN 
            ttTopCrate.iPosition = 1
         .
      END.
      ASSIGN 
         ttToStack.cTopCrate   = ttTopCrate.cLetter
      .
      
      iDone = iDone + 1.
      IF iDone EQ ipiNrCrates THEN
         LEAVE MoveBlock.
   END.
   
END PROCEDURE. /* moveCrates */

PROCEDURE moveCrates_2:
   DEFINE INPUT  PARAMETER ipiNrCrates  AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiFromStack AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiToStack   AS INTEGER NO-UNDO.
   
   DEFINE BUFFER ttFromStack FOR ttStack.
   DEFINE BUFFER ttToStack   FOR ttStack.
   DEFINE BUFFER ttFromCrate FOR ttCrate.
   DEFINE BUFFER ttToCrate   FOR ttCrate.
   
   DEFINE VARIABLE iStartPositionFromStack AS INTEGER NO-UNDO.
   DEFINE VARIABLE iToPositionToStack      AS INTEGER NO-UNDO.
   
   FIND ttFromStack WHERE ttFromStack.iStackNr EQ ipiFromStack.
   FIND ttToStack   WHERE ttToStack.iStack     EQ ipiToStack.
   
   /* Search Top Crate of to Stack */
   ASSIGN 
      iToPositionToStack = 1
   .
   FOR EACH ttToCrate
   WHERE ttToCrate.iStackNr EQ ttToStack.iStackNr
   BY ttToCrate.iStackNr
   BY ttToCrate.iPosition DESCENDING:
      ASSIGN 
         iToPositionToStack = ttToCrate.iPosition + 1
      .
      LEAVE.
   END.
   
   /* Search Start Crate of the From Stack and establish new Top Crate */
   ASSIGN 
      ttFromStack.cTopCrate = ""
   .
   FOR EACH ttFromCrate
   WHERE ttFromCrate.iStackNr EQ ttFromStack.iStackNr
   BY ttFromCrate.iStackNr
   BY ttFromCrate.iPosition DESCENDING:
      ACCUM "" (COUNT).
      IF (ACCUM COUNT "") EQ ipiNrCrates THEN DO:
         /* Found the last Crate to move */
         iStartPositionFromStack = ttFromCrate.iPosition.
      END.
      IF (ACCUM COUNT "") EQ (ipiNrCrates + 1) THEN DO:
         ttFromStack.cTopCrate = ttFromCrate.cLetter.
         LEAVE.
      END. 
   END.
   
   /* Now do the moves */
   FOR EACH ttFromCrate
   WHERE ttFromCrate.iStackNr  EQ ttFromStack.iStackNr
   AND   ttFromCrate.iPosition GE iStartPositionFromStack
   BY ttFromCrate.iStackNr
   BY ttFromCrate.iPosition:
      ACCUM "" (COUNT).
      ASSIGN 
         ttFromCrate.iStackNr  = ttToStack.iStackNr
         ttFromCrate.iPosition = iToPositionToStack
      .
      iToPositionToStack = iToPositionToStack + 1.
      IF (ACCUM COUNT "") EQ ipiNrCrates THEN DO:
         /* Reached the new Top Crate */
         ttToStack.cTopCrate = ttFromCrate.cLetter.
         LEAVE.
      END.
   END.
END PROCEDURE. /* moveCrates_2 */
   
/* ************************  Function Implementations ***************** */
