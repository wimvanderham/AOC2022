
/*------------------------------------------------------------------------
    File        : day10.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day 10 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 19 23:15:21 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/10".
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
DEFINE TEMP-TABLE ttInstruction
   FIELD iNr              AS INTEGER 
   FIELD cInstruction     AS CHARACTER
   FIELD iValue           AS INTEGER  
   FIELD iStartCycle      AS INTEGER 
   FIELD iEndCycle        AS INTEGER 
   FIELD iRegisterXStart  AS INTEGER 
   FIELD iRegisterXEnd    AS INTEGER 
INDEX indNr IS UNIQUE iNr.

DEFINE VARIABLE iNewNr         AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewCycle      AS INTEGER NO-UNDO.
DEFINE VARIABLE iPrevRegisterX AS INTEGER NO-UNDO.

DEFINE VARIABLE cCRTLine AS CHARACTER NO-UNDO FORMAT "X(40)" EXTENT 6.
DEFINE VARIABLE iCRTLine AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCRTPos  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLetter  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRow     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPixels  AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION getLetter RETURNS CHARACTER 
   (INPUT ipcLetter AS CHARACTER) FORWARD.

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

COPY-LOB FROM FILE "input\10.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\10_test.txt" TO OBJECT lcInput.
END.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   ASSIGN
      iNewNr    = iNewNr + 1
      iNewCycle = iNewCycle + 1
   .
   CREATE ttInstruction.
   ASSIGN 
      ttInstruction.iNr          = iNewNr
      ttInstruction.iStartCycle  = iNewCycle
      ttInstruction.cInstruction = ENTRY (1, cLine, " ")
   .
   CASE ttInstruction.cInstruction:
      WHEN "addx" THEN DO:
         ASSIGN 
            ttInstruction.iValue    = INTEGER (ENTRY (2, cLine, " "))
            iNewCycle               = iNewCycle + 1
            ttInstruction.iEndCycle = iNewCycle
         .
      END.
      WHEN "noop" THEN DO:
         ASSIGN 
            ttInstruction.iEndCycle = ttInstruction.iStartCycle
         .
      END.
   END CASE.    
END. /* ReadBlock: */

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.
   
   FOR EACH ttInstruction:
      IF ttInstruction.iNr EQ 1 THEN DO:
         /* Initial value */
         ttInstruction.iRegisterXStart = 1.
         iNewCycle = 1. 
      END.
      ELSE DO:
         ttInstruction.iRegisterXStart = iPrevRegisterX.
      END.
      
      DO WHILE iNewCycle LE ttInstruction.iEndCycle:
         
         CASE ttInstruction.cInstruction:
            WHEN "addx" THEN DO:
               IF iNewCycle EQ ttInstruction.iEndCycle THEN DO:
                  ttInstruction.iRegisterXEnd = ttInstruction.iRegisterXStart + 
                     ttInstruction.iValue.
               END.
            END.
            WHEN "noop" THEN DO:
               ttInstruction.iRegisterXEnd = ttInstruction.iRegisterXStart.
            END.
         END CASE.

         CASE iNewCycle:
            WHEN  20 OR 
            WHEN  60 OR 
            WHEN 100 OR 
            WHEN 140 OR 
            WHEN 180 OR 
            WHEN 220 THEN DO:
               iSolution = iSolution + (iNewCycle * ttInstruction.iRegisterXStart).
            END.
         END CASE.
         
         iNewCycle = iNewCycle + 1.
         
      END.
      
      iPrevRegisterX = ttInstruction.iRegisterXEnd.
   END.
           
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttInstruction:HANDLE).
   END.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 10 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */

   FOR EACH ttInstruction:
      IF ttInstruction.iNr EQ 1 THEN DO:
         /* Initial value */
         ttInstruction.iRegisterXStart = 1.
         iNewCycle = 1.
         iCRTLine  = 1.
         iCRTPos   = 0.
      END.
      ELSE DO:
         ttInstruction.iRegisterXStart = iPrevRegisterX.
      END.
      
      DO WHILE iNewCycle LE ttInstruction.iEndCycle:
         
         IF  ttInstruction.iRegisterXStart GE (iCRTPos - 1)
         AND ttInstruction.iRegisterXStart LE (iCRTPos + 1) THEN DO:
            /* Draw */
            SUBSTRING (cCRTLine[iCRTLine], iCRTPos + 1, 1) = "#".
         END.
         ELSE DO:
            SUBSTRING (cCRTLine[iCRTLine], iCRTPos + 1, 1) = ".".
         END.
         
         CASE ttInstruction.cInstruction:
            WHEN "addx" THEN DO:
               IF iNewCycle EQ ttInstruction.iEndCycle THEN DO:
                  ttInstruction.iRegisterXEnd = ttInstruction.iRegisterXStart + 
                     ttInstruction.iValue.
               END.
            END.
            WHEN "noop" THEN DO:
               ttInstruction.iRegisterXEnd = ttInstruction.iRegisterXStart.
            END.
         END CASE.

         iNewCycle = iNewCycle + 1.
         
         iCRTPos = iCRTPos + 1.
         IF iCRTPos GE 40 THEN DO:
            iCRTLine = iCRTLine + 1.
            iCRTPos  = 0.
         END.
            
      END.
      
      iPrevRegisterX = ttInstruction.iRegisterXEnd.
   END.
           
    
   OUTPUT TO "output\10.txt".
   PUT UNFORMATTED 
   cCRTLine[1] SKIP
   cCRTLine[2] SKIP 
   cCRTLine[3] SKIP 
   cCRTLine[4] SKIP 
   cCRTLine[5] SKIP 
   cCRTLine[6] SKIP.
   OUTPUT CLOSE.
   
   DO iLetter = 1 TO 8:
      cPixels = "".
      DO iPart = 1 TO 6:
         cPixels = SUBSTITUTE ("&1&2&3",
            cPixels,
            (IF cPixels NE "" THEN "," ELSE ""),
            SUBSTRING (cCRTLine[iPart], (iLetter - 1) * 5 + 1, 4)).
      END.
      cSolution = SUBSTITUTE ("&1&2",
                     cSolution,
                     getLetter(cPixels)).
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED 
   cSolution SKIP.
   OUTPUT CLOSE.
          
   MESSAGE 
      SUBSTITUTE ("Solution: &1.",
         cSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 10 - Part Two".
   
   IF lvlShow THEN DO:
      RUN sy\win\show-file.w
         (INPUT "output\10.txt").
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

   
/* ************************  Function Implementations ***************** */

FUNCTION getLetter RETURNS CHARACTER 
   (INPUT ipcPixels AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Translates Pixels into a letter
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cLetter      AS CHARACTER NO-UNDO EXTENT 26.
DEFINE VARIABLE iLetter      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFoundLetter AS LOGICAL   NO-UNDO.

   ASSIGN 
      cLetter[3] = ".##.,#..#,#...,#...,#..#,.##."
      cLetter[5] = "####,#...,###.,#...,#...,####"
      cLetter[6] = "####,#...,###.,#...,#...,#..."
      cLetter[8] = "#..#,#..#,####,#..#,#..#,#..#"
      cLetter[26] = "####,...#,..#.,.#..,#...,####"
   .
   
   DO iLetter = 1 TO 26:
      
      IF cLetter[iLetter] EQ "" THEN 
         NEXT.
         
      lFoundLetter = TRUE.
      DO iLine = 1 TO NUM-ENTRIES (cLetter[iLetter]):
         IF ENTRY (iLine, ipcPixels) NE ENTRY (iLine, cLetter[iLetter]) THEN DO: 
            lFoundLetter = FALSE.
            LEAVE.
         END.
      END.
      
      IF lFoundLetter THEN 
         RETURN CHR (ASC ("A") + iLetter - 1).
   END.
   
   RETURN "?".
      
END FUNCTION.

