
/*------------------------------------------------------------------------
    File        : day02.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day02 of Advent of Code 2022 - Rock, Scissors and Paper

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 02 08:42:23 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/2".
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
DEFINE VARIABLE cTheyPlay AS CHARACTER NO-UNDO INITIAL "A,B,C".
DEFINE VARIABLE cYouPlay  AS CHARACTER NO-UNDO INITIAL "X,Y,Z".
DEFINE VARIABLE cObject   AS CHARACTER NO-UNDO INITIAL "Rock,Paper,Scissors".

DEFINE VARIABLE cGamePlay    AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE iGamePoints  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalPoints AS INTEGER   NO-UNDO.

/*
Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win. 
"The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.

*/


/* ************************  Function Prototypes ********************** */

FUNCTION getResult RETURNS INTEGER 
   (INPUT ipcTheyPlay AS CHARACTER, INPUT ipcYouPlay AS CHARACTER) FORWARD.

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

COPY-LOB FROM FILE "input\02.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "A Y~nB X~nC Z".

   MESSAGE STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   ASSIGN 
      cGamePlay[1] = ENTRY (1, cLine, " ") /* They Play */
      cGamePlay[2] = ENTRY (2, cLine, " ") /* You Play or Result */
   .
   
   /* Decrypt input, transform A, B, C, X, Y, Z in Standard Objects */
   ASSIGN 
      cGamePlay[1] = ENTRY (LOOKUP (cGamePlay[1], cTheyPlay),  cObject).
   .
   
   IF lPart[1] THEN DO:
      cGamePlay[2] = ENTRY (LOOKUP (cGamePlay[2], cYouPlay), cObject).
   END.
         
   IF lPart[2] EQ TRUE THEN DO:
      CASE cGamePlay[2]:
         WHEN "X" THEN DO:
            /* Need to loose */
            CASE cGamePlay[1]:
               WHEN "Rock" THEN
                  cGamePlay[2] = "Scissors".
               WHEN "Paper" THEN 
                  cGamePlay[2] = "Rock".
               WHEN "Scissors" THEN 
                  cGamePlay[2] = "Paper".
            END CASE.
         END. /* Need to loose */
         WHEN "Y" THEN DO:
            /* Need to draw */
            cGamePlay[2] = cGamePlay[1].
         END. /* Need to draw */
         WHEN "Z" THEN DO:
            /* Need to win */
            CASE cGamePlay[1]:
               WHEN "Rock" THEN
                  cGamePlay[2] = "Paper".
               WHEN "Paper" THEN 
                  cGamePlay[2] = "Scissors".
               WHEN "Scissors" THEN 
                  cGamePlay[2] = "Rock".
            END CASE.
         END. /* Need to win */
      END.
   END.
   
   iGamePoints = getResult(cGamePlay[1], cGamePlay[2]).
   
   iTotalPoints = iTotalPoints + iGamePoints.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
END.
   
IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = iTotalPoints.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 02 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */

   iSolution = iTotalPoints.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 02 - Part Two".
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


FUNCTION getResult RETURNS INTEGER 
   (INPUT ipcTheyPlay AS CHARACTER,
    INPUT ipcYouPlay  AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:   shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus 
 the score for the outcome of the round (0 if you lost, 3 if the round was a draw, 
 and 6 if you won).
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iGamePoints AS INTEGER NO-UNDO.

   CASE ipcYouPlay:
      WHEN "Rock" THEN DO:
         iGamePoints = 1.
         CASE ipcTheyPlay:
            WHEN "Rock" THEN
               iGamePoints = iGamePoints + 3.
            WHEN "Scissors" THEN 
               iGamePoints = iGamePoints + 6.
         END CASE.
      END.
      WHEN "Paper" THEN DO:
         iGamePoints = 2.
         CASE ipcTheyPlay:
            WHEN "Paper" THEN 
               iGamePoints = iGamePoints + 3.
            WHEN "Rock" THEN 
               iGamePoints = iGamePoints + 6.
         END CASE.
      END.
      WHEN "Scissors" THEN DO:
         iGamePoints = 3.
         CASE ipcTheyPlay:
            WHEN "Scissors" THEN 
               iGamePoints = iGamePoints + 3.
            WHEN "Paper" THEN
               iGamePoints = iGamePoints + 6.
         END CASE.
      END.
   END CASE.
   
   RETURN iGamePoints.
      
END FUNCTION.
