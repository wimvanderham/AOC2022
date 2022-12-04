
/*------------------------------------------------------------------------
    File        : day04_min.p
    Purpose     : 

    Syntax      :

    Description : Minimal version to solve Day 04 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 04 20:42:40 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEntry    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFromTo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFrom     AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iTo       AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iSolution AS INTEGER   NO-UNDO EXTENT 2.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ETIME (YES).

INPUT FROM "input\04.txt".

REPEAT:
   IMPORT UNFORMATTED cLine.
   
   DO iEntry = 1 TO 2:
      ASSIGN
         cFromTo         = ENTRY (iEntry, cLine)
         iFrom[iEntry]   = INTEGER (ENTRY (1, cFromTo, "-"))
         iTo[iEntry]     = INTEGER (ENTRY (2, cFromTo, "-"))
      .
   END.
   
   IF (iFrom[1] GE iFrom[2] AND iTo[1] LE iTo[2])
   OR (iFrom[2] GE iFrom[1] AND iTo[2] LE iTo[1]) THEN DO:
      /* Fully Contains */
      iSolution[1] = iSolution[1] + 1.
   END. /* Fully Contains */ 
   
   IF ((iFrom[1] GE iFrom[2] AND iFrom[1] LE iTo[2]) OR 
       (iTo[1]   GE iFrom[2] AND iTo[1]   LE iTo[2]))
   OR ((iFrom[2] GE iFrom[1] AND iFrom[2] LE iTo[1]) OR 
       (iTo[2]   GE iFrom[1] AND iTo[2]   LE iTo[1])) 
    THEN DO:
       /* Overlap */
      iSolution[2] = iSolution[2] + 1.
   END. /* Overlap */
END.
INPUT CLOSE.

MESSAGE
SUBSTITUTE ("Solution 1: &1.", iSolution[1]) SKIP
SUBSTITUTE ("Solution 2: &1.", iSolution[2]) SKIP (1)
SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2022 - Day 04 - Part One & Two". 

