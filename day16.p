
/*------------------------------------------------------------------------
    File        : day16.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 16 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 26 18:42:13 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/16".
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
DEFINE TEMP-TABLE ttValve
   FIELD cValve     AS CHARACTER 
   FIELD iFlowRate  AS INTEGER
   FIELD cValveList AS CHARACTER FORMAT "X(20)"
   FIELD iStep      AS INTEGER 
INDEX indValve IS UNIQUE cValve.

DEFINE BUFFER ttStartValve FOR ttValve.
DEFINE BUFFER ttNewValve   FOR ttValve.

DEFINE TEMP-TABLE ttPath
   FIELD cFromValve AS CHARACTER 
   FIELD cToValve   AS CHARACTER
   FIELD iMinutes   AS INTEGER  
INDEX indPath IS UNIQUE cFromValve cToValve.

DEFINE BUFFER ttNewPath FOR ttPath.

DEFINE TEMP-TABLE ttVisited
   FIELD cValve        AS CHARACTER 
   FIELD iTimeLeft     AS INTEGER 
   FIELD cOpenSwitches AS CHARACTER 
   FIELD iMaxValue     AS INTEGER 
INDEX indValveTimeOpen IS UNIQUE cValve iTimeLeft cOpenSwitches.

DEFINE VARIABLE iValve          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValve          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNewStep        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFoundNew       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLeftMinutes    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOpenList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNextValve      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTotalFlow      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPathLength     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxValveStates AS INT64     NO-UNDO.
DEFINE VARIABLE iValveState     AS INT64     NO-UNDO.
DEFINE VARIABLE cLeftOpenList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRightOpenList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSwitches       AS CHARACTER NO-UNDO.    
DEFINE VARIABLE iOpenValves     AS INT64     NO-UNDO.
DEFINE VARIABLE cLeftSwitches   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRightSwitches  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iFunctionCalls  AS INT64     NO-UNDO.
DEFINE VARIABLE iAlreadyVisited AS INT64     NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION getBinary RETURNS CHARACTER 
(INPUT ipiInt64  AS INT64, 
 INPUT ipiLength AS INTEGER) FORWARD.

FUNCTION getMaxFlow RETURNS INTEGER 
(INPUT ipcValve    AS CHARACTER,
 INPUT ipiTimeLeft AS INTEGER,
 INPUT ipcOpenList AS CHARACTER) FORWARD.

FUNCTION getOpenList RETURNS CHARACTER 
(INPUT ipicSwitches AS CHARACTER) FORWARD.

FUNCTION getSwitches RETURNS CHARACTER 
(INPUT ipcOpenList AS CHARACTER) FORWARD.

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

COPY-LOB FROM FILE "input\16.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\16_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   CREATE ttValve.
   ASSIGN 
      ttValve.cValve     = ENTRY (2, cLine, " ")
      ttValve.iFlowRate  = INTEGER (ENTRY (2, TRIM (ENTRY (5, cLine, " "), ";"), "="))
      ttValve.cValveList = TRIM (SUBSTRING (cLine, R-INDEX (cLine, "valve") + 6))
      ttValve.iStep      = ?
   .

   DO iValve = 1 TO NUM-ENTRIES (ttValve.cValveList):
      cValve = TRIM (ENTRY (iValve, ttValve.cValveList)).
      CREATE ttPath.
      ASSIGN 
         ttPath.cFromValve = ttValve.cValve
         ttPath.cToValve   = cValve
         ttPath.iMinutes   = 1
      .
   END.
         
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttValve:HANDLE).
END.                     

/* Create all possible paths */
FOR EACH ttStartValve:
   /* Reset distances */
   FOR EACH ttValve:
      IF ttValve.cValve EQ ttStartValve.cValve THEN
         ttValve.iStep = 0.
      ELSE 
         ttValve.iStep = ?.
   END.
   
   iNewStep = 0.
   SearchBlock:
   REPEAT:
      lFoundNew = FALSE.
      
      FOR EACH ttValve 
      WHERE ttValve.iStep EQ iNewStep,
      EACH  ttPath
      WHERE ttPath.cFromValve EQ ttValve.cValve
      /* Proceed on paths 1 minute at a time */
      AND   ttPath.iMinute    EQ 1,
      FIRST ttNewValve
      WHERE ttNewValve.cValve EQ ttPath.cToValve:
         IF ttNewValve.iStep EQ ? THEN
            ASSIGN 
               ttNewValve.iStep = ttValve.iStep + 1
               lFoundNew        = TRUE
            .
      END.
      IF lFoundNew EQ FALSE THEN
         /* No more new paths found */ 
         LEAVE.
      
      iNewStep = iNewStep + 1.
   END. /* SearchBlock: */
   
   FOR EACH ttValve
   WHERE ttValve.cValve    NE ttStartValve.cValve
   /* For all reachable destinations from this start valve ... */
   AND   ttValve.iStep     NE ?:
      /* ... create a path */ 
      FIND  ttPath
      WHERE ttPath.cFromValve EQ ttStartValve.cValve
      AND   ttPath.cToValve   EQ ttValve.cValve NO-ERROR.
      IF NOT AVAILABLE ttPath THEN DO:
         CREATE ttPath.
         ASSIGN 
            ttPath.cFromValve = ttStartValve.cValve
            ttPath.cToValve   = ttValve.cValve
            ttPath.iMinutes   = ttValve.iStep
         .
      END.
   END.
END.

/* Clean up useless paths (leading to 0 flow rate valves) */
FOR EACH ttPath,
FIRST ttValve
WHERE ttValve.cValve    EQ ttPath.cToValve
AND   ttValve.iFlowRate EQ 0:
   DELETE ttPath.
END.
/* Or starting at 0 flow rate valves (except for the start valve "AA") */
FOR EACH ttPath
WHERE ttPath.cFromValve NE "AA",
FIRST ttValve
WHERE ttValve.cValve    EQ ttPath.cFromValve
AND   ttValve.iFlowRate EQ 0:
   DELETE ttPath.
END.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPath:HANDLE).
END.   

IF lPart[1] THEN DO:

   IF lvlShow THEN DO:
      OUTPUT TO "output\16.txt".
   END.
   
   /* Search for the best path from start valve AA 
   ** with 30 minutes of time left and 
   ** no open valves */
   iSolution = getMaxFlow("AA", 30, "").

   IF lvlDebug THEN DO:
      MESSAGE 
      "Total Function Calls:" iFunctionCalls SKIP 
      "Already Visited:" iAlreadyVisited SKIP 
      VIEW-AS ALERT-BOX.
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("Solution: &1", iSolution) SKIP.
      OUTPUT CLOSE.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 16 - Part One".

END.


IF lPart[2] THEN DO:
   
   /* Calculate maximum different states of the valves */
   iMaxValveStates = 1.
   FOR EACH ttValve
   WHERE ttValve.iFlowRate GT 0:
      /* A valve can be open or closed: 2 states per (significant) valve */
      ACCUM "Valve" (COUNT).
      iMaxValveStates = iMaxValveStates * 2.
   END.

   IF lvlShow THEN DO:
      OUTPUT TO "output\16_2.txt".
      PUT UNFORMATTED 
      SUBSTITUTE ("#Valves: &1, Maximum Valve States: &2", (ACCUM COUNT "Valve"), iMaxValveStates) SKIP.
   END.
   
   iSolution = 0.
   DO iOpenValves = 0 TO INTEGER (iMaxValveStates / 2) - 1:
      cLeftSwitches  = getBinary(iOpenValves, (ACCUM COUNT "Valve")).
      cRightSwitches = getBinary(iMaxValveStates - iOpenValves - 1, (ACCUM COUNT "Valve")).
      IF lvlShow THEN DO:
         PUT UNFORMATTED 
         SUBSTITUTE ("#&3. Checking combination: &1 + &2 -->", 
                     getOpenList(cLeftSwitches), 
                     getOpenList(cRightSwitches),
                     iOpenValves).
      END.
      
      iSolution = MAXIMUM (iSolution, 
                           getMaxFlow("AA", 26, getOpenList(cLeftSwitches)) + 
                           getMaxFlow("AA", 26, getOpenList(cRightSwitches))).
      IF lvlShow THEN DO:
         PUT UNFORMATTED 
         SUBSTITUTE (" Solution = &1", iSolution) SKIP.
      END.                            
   END.
                     
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 16 - Part Two".
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

FUNCTION getBinary RETURNS CHARACTER 
(INPUT ipiInt64 AS INT64, INPUT ipiLength AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Converts an int64 into binary 01010101
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iBit    AS INTEGER NO-UNDO.
DEFINE VARIABLE iValue  AS INTEGER NO-UNDO.
DEFINE VARIABLE cBinary AS CHARACTER NO-UNDO.

   iValue = 1.
   DO iBit = 1 TO ipiLength:
      IF ipiInt64 MOD (iValue * 2) NE 0 THEN 
         cBinary = "1" + cBinary.
      ELSE 
         cBinary = "0" + cBinary.
      iValue = iValue * 2.
      ipiInt64 = ipiInt64 - (ipiInt64 MOD iValue).
   END.
   
   RETURN cBinary.
                     
END FUNCTION.

FUNCTION getMaxFlow RETURNS INTEGER 
(INPUT ipcValve    AS CHARACTER,
 INPUT ipiTimeLeft AS INTEGER,
 INPUT ipcOpenList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Establish Maximum Flow from valve ipcValve with ipiTimeLeft time left
          and ipcOpenList
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttValve     FOR ttValve.
DEFINE BUFFER ttPath      FOR ttPath.
DEFINE BUFFER ttNextValve FOR ttValve.
DEFINE BUFFER ttVisited   FOR ttVisited.

DEFINE VARIABLE iMaxValue    AS INTEGER NO-UNDO.
DEFINE VARIABLE iTimeLeft    AS INTEGER NO-UNDO.
DEFINE VARIABLE cNewOpenList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSwitches    AS CHARACTER NO-UNDO.

   IF lvlDebug THEN DO:
      ASSIGN 
         iFunctionCalls = iFunctionCalls + 1
      .
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("From: &1, Left Minutes: &2, Open Valves: &3 ...",
                  ipcValve,
                  ipiTimeLeft,
                  ipcOpenList) SKIP.
                  
   END.

   cSwitches = getSwitches(ipcOpenList).   
   FIND  ttVisited
   WHERE ttVisited.cValve        EQ ipcValve
   AND   ttVisited.iTimeLeft     EQ ipiTimeLeft
   AND   ttVisited.cOpenSwitches EQ cSwitches NO-ERROR.
   IF AVAILABLE ttVisited THEN DO:
      IF lvlShow THEN
         PUT UNFORMATTED 
         SUBSTITUTE ("Already visited (&1): &2", ttVisited.cOpenSwitches, ttVisited.iMaxValue) SKIP.
      IF lvlDebug THEN 
         iAlreadyVisited = iAlreadyVisited + 1. 
      RETURN ttVisited.iMaxValue.
   END.
      
   iMaxValue = 0.
   FIND ttValve
   WHERE ttValve.cValve EQ ipcValve.
   
   FOR EACH ttPath
   WHERE ttPath.cFromValve EQ ttValve.cValve,
   FIRST ttNextValve
   WHERE ttNextValve.cValve EQ ttPath.cToValve:
      IF lvlShow THEN
         PUT UNFORMATTED 
         SUBSTITUTE ("to Valve: &1, Path: &2, Flow rate: &3.",
                     ttNextValve.cValve,
                     ttPath.iMinutes,
                     ttNextValve.cValve) SKIP.
                     
      IF LOOKUP (ttNextValve.cValve, ipcOpenList) NE 0 THEN 
         /* Next Valve already open */
         NEXT.
      
      iTimeLeft    = ipiTimeLeft - ttPath.iMinutes - 1.
      IF iTimeLeft LE 0 THEN
         /* No more time left going down this path */
         NEXT.
         
      cNewOpenList = SUBSTITUTE ("&1&2&3",
                                ipcOpenList,
                                (IF ipcOpenList NE "" THEN "," ELSE ""),
                                ttNextValve.cValve).

      iMaxValue = MAXIMUM (iMaxValue, getMaxFlow(ttNextValve.cValve, iTimeLeft, cNewOpenList) + (iTimeLeft * ttNextValve.iFlowRate)).
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("From: &1, Left Minutes: &2, Open Valves: &3 Max Value: &4",
                  ipcValve,
                  ipiTimeLeft,
                  ipcOpenList,
                  iMaxValue) SKIP.
   END.
   
   CREATE ttVisited.
   ASSIGN 
      ttVisited.cValve        = ipcValve
      ttVisited.iTimeLeft     = ipiTimeLeft
      ttVisited.cOpenSwitches = cSwitches
      ttVisited.iMaxValue     = iMaxValue
   .
   
   RETURN iMaxValue.
      
END FUNCTION.

FUNCTION getOpenList RETURNS CHARACTER 
(INPUT ipcSwitches AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the Open Valves list based on the switches 
 Notes:   The switches are 0 (closed) or 1 (open) for the valves in alfabetic order
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttValve FOR ttValve.

DEFINE VARIABLE cOpenList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPos      AS INTEGER NO-UNDO.

   iPos = 0.
   FOR EACH ttValve
   WHERE ttValve.iFlowRate GT 0
   BY ttValve.cValve:
      iPos = iPos + 1.
      IF SUBSTRING (ipcSwitches, iPos, 1) EQ "1" THEN 
         cOpenList = SUBSTITUTE ("&1&2&3",
                                 cOpenList,
                                 (IF cOpenList NE "" THEN "," ELSE ""),
                                 ttValve.cValve).
   END.
   
   RETURN cOpenList.                                 
      
END FUNCTION.

FUNCTION getSwitches RETURNS CHARACTER 
   (INPUT ipcOpenList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns a positional string of the open valves
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttValve FOR ttValve.

DEFINE VARIABLE cSwitches AS CHARACTER NO-UNDO.
   
   FOR EACH ttValve
   WHERE ttValve.iFlowRate GT 0
   BY ttValve.cValve:
      cSwitches = SUBSTITUTE ("&1&2",
                              cSwitches,
                              (IF (LOOKUP (ttValve.cValve, ipcOpenList) EQ 0) THEN "0" ELSE "1")).
   END.                              
       
   RETURN cSwitches.                              
      
END FUNCTION.

