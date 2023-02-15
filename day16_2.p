
/*------------------------------------------------------------------------
    File        : day16.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 16 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 26 18:42:13 CET 2022
    Notes       :
       
    Update      : This version calculates Part Two with a new version of 
                  the getMaxFlow function: getMaxFlowMultiplayer.
                  It has an extra parameter with respect of the getMaxFlow
                  that indicates the number of other players (0 for Part One).
                  When there is no more time left for exploration of a path,
                  and there's another player available, we restart the 
                  getMaxFlowMultiplayer with AA as start valve, 26 as minutes left,
                  *the open valves list that we have reached at that point*
                  and other players - 1
                  
    Update 2    : Implemented alternative getMaxFlowSimul that tries to find the maximum
                  value with both players simultanous.
                                
                             
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
   FIELD cValve     AS CHARACTER FORMAT "X(2)"  LABEL "Valve"
   FIELD iFlowRate  AS INTEGER   FORMAT "z9"    LABEL "Flow Rate"
   FIELD cValveList AS CHARACTER FORMAT "X(20)" LABEL "Neighbors List"
   FIELD iStep      AS INTEGER                  LABEL "Temporary Step Distance"
INDEX indValve IS UNIQUE cValve.

DEFINE BUFFER ttStartValve FOR ttValve.
DEFINE BUFFER ttNewValve   FOR ttValve.

DEFINE TEMP-TABLE ttPath
   FIELD cFromValve AS CHARACTER FORMAT "X(2)" LABEL "From"
   FIELD cToValve   AS CHARACTER FORMAT "X(2)" LABEL "To"
   FIELD iMinutes   AS INTEGER   FORMAT "zz9"  LABEL "Minutes"
INDEX indPath IS UNIQUE cFromValve cToValve.

DEFINE BUFFER ttNewPath FOR ttPath.

DEFINE TEMP-TABLE ttVisited
   FIELD cValve        AS CHARACTER FORMAT "X(2)"  LABEL "Valve"
   FIELD iTimeLeft     AS INTEGER   FORMAT "z9"    LABEL "Minutes Left"
   FIELD cOpenSwitches AS CHARACTER FORMAT "X(55)" LABEL "Open Valves (Switches)"
   /* Add number of other players */
   FIELD iOtherPlayers AS INTEGER   FORMAT "9"     LABEL "Other Players"
   FIELD iMaxValue     AS INTEGER   FORMAT "z,zz9" LABEL "Maximum Value"
INDEX indValveTimeOpen IS UNIQUE cValve iTimeLeft cOpenSwitches iOtherPlayers.

DEFINE TEMP-TABLE ttFullPath
   FIELD cPath      AS CHARACTER FORMAT "X(45)" LABEL "Full Path"
   FIELD cSwitches  AS CHARACTER FORMAT "X(20)" LABEL "Switches"
   FIELD iPathTime  AS INTEGER   FORMAT "z9"    LABEL "Path Time"
   FIELD iValue     AS INTEGER   FORMAT "zzz9"  LABEL "Value"
   FIELD cOtherPath AS CHARACTER FORMAT "X(45)" LABEL "Other Path"
   FIELD iBestValue AS INTEGER                  LABEL "Best Value" 
INDEX ind_Path IS UNIQUE cPath.
 
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
DEFINE VARIABLE lvcOpenList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvcMethodTwo    AS CHARACTER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION getBinary RETURNS CHARACTER 
(INPUT ipiInt64  AS INT64, 
 INPUT ipiLength AS INTEGER) FORWARD.

FUNCTION getMaxFlowMultiplayer RETURNS INTEGER 
(INPUT ipcValve        AS CHARACTER,
 INPUT ipiTimeLeft     AS INTEGER,
 INPUT ipcOpenList     AS CHARACTER,
 INPUT ipiOtherPlayers AS INTEGER) FORWARD.

FUNCTION getMaxFlowPaths RETURNS INTEGER 
(INPUT ipiTimeLeft AS INTEGER) FORWARD.

FUNCTION getOpenList RETURNS CHARACTER 
(INPUT ipicSwitches AS CHARACTER) FORWARD.

FUNCTION getSwitches RETURNS CHARACTER 
(INPUT ipcOpenList AS CHARACTER) FORWARD.

FUNCTION getValueofPath RETURNS INTEGER  
(INPUT ipcPath     AS CHARACTER,
 INPUT ipiTimeLeft AS INTEGER) FORWARD.

FUNCTION IsXOR RETURNS LOGICAL 
(INPUT ipcPathOne AS CHARACTER,
 INPUT ipcPathTwo AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
DISPLAY
   lOpenURL     LABEL "Open URL?"     VIEW-AS TOGGLE-BOX AT 10 SKIP 
   lPart[1]     LABEL "Solve Part 1?" VIEW-AS TOGGLE-BOX AT 10 SKIP
   lPart[2]     LABEL "Solve Part 2?" VIEW-AS TOGGLE-BOX AT 10 SKIP
   /*
   lvcMethodTwo LABEL "Method"        VIEW-AS RADIO-SET HORIZONTAL RADIO-BUTTONS "Multiplayer", "M", "Paths", "P" SKIP
   */ 
   lvlDebug     LABEL "Debug?"        VIEW-AS TOGGLE-BOX AT 10 SKIP 
   lvlShow      LABEL "Show?"         VIEW-AS TOGGLE-BOX AT 10 SKIP
WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".

UPDATE
   lOpenURL
   lPart
   lvlDebug
   lvlShow
WITH FRAME fr-Parameters.

lvcMethodTwo = "M".

IF lOpenURL THEN DO:
   cOSCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cOSCommand).
END.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\16.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\16_test.txt" TO OBJECT lcInput.
   IF lvlShow THEN DO:
      MESSAGE "Debug Input:" SKIP(1) 
      STRING (lcInput)
      VIEW-AS ALERT-BOX.
   END.
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
   ** with 30 minutes of time left 
   ** no open valves and
   ** 0 other players */
   iSolution = getMaxFlowMultiplayer("AA", 30, "", 0).
   
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
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME) SKIP (1)
      lvcOpenList
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 16 - Part One".

   IF lvlDebug
   OR lvlShow THEN DO:
      MESSAGE 
      "Total Function Calls:" iFunctionCalls SKIP 
      "Already Visited:" iAlreadyVisited SKIP 
      VIEW-AS ALERT-BOX.
   END.      
END.


IF lPart[2] THEN DO:
   
   IF lvcMethodTwo EQ "M" THEN       
      iSolution = getMaxFlowMultiplayer("AA", 26, "", 1).
   ELSE DO:
      RUN createPaths
         (INPUT "AA",
          INPUT 26,
          INPUT "AA",
          INPUT 0).
    
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttFullPath:HANDLE).
      END.          
      
      iSolution = getMaxFlowPaths(26).
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME) SKIP (1)
   lvcOpenList
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
   SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP(3)
   cErrorMessage SKIP(1) 
   VIEW-AS ALERT-BOX ERROR.

   IF lvlShow THEN DO:
   END.
   RETURN.      
END CATCH.

/* **********************  Internal Procedures  *********************** */
PROCEDURE createPaths:
   DEFINE INPUT  PARAMETER ipcStartValve AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiTimeLeft   AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipcStartPath  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiPathTime   AS INTEGER NO-UNDO.

DEFINE BUFFER ttPath     FOR ttPath.
DEFINE BUFFER ttValve    FOR ttValve.
DEFINE BUFFER ttFullPath FOR ttFullPath.

DEFINE VARIABLE cNewPath     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNewTimeLeft AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewPathTime AS INTEGER   NO-UNDO.

   IF ipcStartPath NE "" THEN DO:
      /* We have a path, save it */
      FIND  ttFullPath
      WHERE ttFullPath.cPath EQ ipcStartPath NO-ERROR.
      IF NOT AVAILABLE ttFullPath THEN DO:
         CREATE ttFullPath.
         ASSIGN 
            ttFullPath.cPath     = ipcStartPath
            ttFullPath.cSwitches = getSwitches(ttFullPath.cPath)
            ttFullPath.iPathTime = ipiPathTime
            ttFullPath.iValue    = getValueofPath(ttFullPath.cPath, 26)
         .
      END.
   END.
   
   FOR EACH ttPath
   WHERE ttPath.cFromValve EQ ipcStartValve,
   FIRST ttValve
   WHERE ttValve.cValve EQ ttPath.cToValve:
      /* All reachable paths from start valve */
      IF LOOKUP (ttPath.cToValve, ipcStartPath) NE 0 THEN 
         /* Next Valve already in Full Path */
         NEXT.
         
      iNewTimeLeft = ipiTimeLeft - ttPath.iMinutes - 1.
      IF iNewTimeLeft LE 0 THEN
         /* No Time Left to reach this Valve and open it */
         NEXT.
         
      ASSIGN 
         cNewPath = SUBSTITUTE ("&1&2&3", 
                                ipcStartPath, 
                                (IF ipcStartPath NE "" THEN "," ELSE ""),
                                ttPath.cToValve)
         iNewPathTime = ipiPathTime + ttPath.iMinutes + 1
      .
                                      
      RUN createPaths   
         (INPUT ttPath.cToValve,
          INPUT iNewTimeLeft,
          INPUT cNewPath,
          INPUT iNewPathTime).
   END.
                      
END. /* create Paths */   
   
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

FUNCTION getMaxFlowMultiplayer RETURNS INTEGER 
(INPUT ipcValve        AS CHARACTER,
 INPUT ipiTimeLeft     AS INTEGER,
 INPUT ipcOpenList     AS CHARACTER,
 INPUT ipiOtherPlayers AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Establish Maximum Flow from valve ipcValve with ipiTimeLeft time left
          ipcOpenList open valves and ipiOtherPlayers 
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttValve     FOR ttValve.
DEFINE BUFFER ttPath      FOR ttPath.
DEFINE BUFFER ttNextValve FOR ttValve.
DEFINE BUFFER ttVisited   FOR ttVisited.

DEFINE VARIABLE iMaxValue     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTimeLeft     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNewOpenList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSwitches     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPrevMax      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNextValve    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNextFlowRate AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOtherPlayers AS INTEGER   NO-UNDO.
DEFINE VARIABLE iExplored     AS INTEGER   NO-UNDO.

   IF lvlDebug 
   OR lvlShow THEN DO:
      ASSIGN 
         iFunctionCalls = iFunctionCalls + 1
      .
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("From: &1, Left Minutes: &2, Open Valves: &3 Other Players: &4 ...",
                  ipcValve,
                  ipiTimeLeft,
                  ipcOpenList,
                  ipiOtherPlayers) SKIP.
                  
   END.

   cSwitches = getSwitches(ipcOpenList).   
   FIND  ttVisited
   WHERE ttVisited.cValve        EQ ipcValve
   AND   ttVisited.iTimeLeft     EQ ipiTimeLeft
   AND   ttVisited.cOpenSwitches EQ cSwitches 
   AND   ttVisited.iOtherPlayers EQ ipiOtherPlayers NO-ERROR.
   IF AVAILABLE ttVisited THEN DO:
      IF lvlShow THEN
         PUT UNFORMATTED 
         SUBSTITUTE ("Already visited (&1): &2", ttVisited.cOpenSwitches, ttVisited.iMaxValue) SKIP.
      IF lvlDebug 
      OR lvlShow THEN 
         iAlreadyVisited = iAlreadyVisited + 1. 
      RETURN ttVisited.iMaxValue.
   END.
      
   iMaxValue = 0.
   FIND ttValve
   WHERE ttValve.cValve EQ ipcValve.
   
   iExplored = 0.
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
      
      IF iTimeLeft LE 0 THEN DO:
         /* No more time left following this path */
         IF ipiOtherPlayers GT 0 THEN 
            /* But we have another player available, continue with this player, starting from AA
            ** with original 26 minutes left and the *currently* found open list
            ** and one less other player */
            iMaxValue = MAXIMUM (iMaxValue, getMaxFlowMultiplayer("AA", 26, ipcOpenList, ipiOtherPlayers - 1)).
         NEXT.
      END.
          
      ASSIGN 
         cNextValve    = ttNextValve.cValve
         iNextFlowRate = ttNextValve.iFlowRate
         cNewOpenList  = SUBSTITUTE ("&1&2&3",
                                     ipcOpenList,
                                     (IF ipcOpenList NE "" THEN "," ELSE ""),
                                     cNextValve)
         iOtherPlayers = ipiOtherPlayers                                        
      .
         
      iExplored = iExplored + 1.         
      iMaxValue = MAXIMUM (iMaxValue, getMaxFlowMultiplayer(cNextValve, iTimeLeft, cNewOpenList, iOtherPlayers) + (iTimeLeft * iNextFlowRate)).
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("From: &1, Left Minutes: &2, Open Valves: &3 Max Value: &4. Open List: &5. Other Players: &6.",
                  ipcValve,
                  ipiTimeLeft,
                  ipcOpenList,
                  iMaxValue,
                  lvcOpenList,
                  ipiOtherPlayers) SKIP.
   END.
   
   CREATE ttVisited.
   ASSIGN 
      ttVisited.cValve        = ipcValve
      ttVisited.iTimeLeft     = ipiTimeLeft
      ttVisited.cOpenSwitches = cSwitches
      ttVisited.iOtherPlayers = ipiOtherPlayers
      ttVisited.iMaxValue     = iMaxValue
   .
   
   RETURN iMaxValue.
      
END FUNCTION.

FUNCTION getMaxFlowPaths RETURNS INTEGER 
(INPUT ipiTimeLeft AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttFullPathOne FOR ttFullPath.
DEFINE BUFFER ttFullPathTwo FOR ttFullPath.

DEFINE VARIABLE iCheckValve AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCheckValve AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValueOne   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iValueTwo   AS INTEGER   NO-UNDO.

DEFINE VARIABLE iPrevBest  AS INTEGER NO-UNDO.
DEFINE VARIABLE iValueBest AS INTEGER NO-UNDO.

   PathOneBlock:
   FOR EACH ttFullPathOne:
      iValueOne = ttFullPathOne.iValue.
      
      PathTwoBlock:
      FOR EACH ttFullPathTwo
      WHERE ttFullPathTwo.cPath NE ttFullPathOne.cPath:
         
         IF isXOR(ttFullPathOne.cSwitches, ttFullPathTwo.cSwitches) THEN DO:
            /* Path One switches are XOR Path Two switches, no overlapping */
            
/*         DO iCheckValve = 2 TO NUM-ENTRIES (ttFullPathTwo.cPath):      */
/*            cCheckValve = ENTRY (iCheckValve, ttFullPathTwo.cPath).    */
/*            IF LOOKUP  (cCheckValve, ttFullPathOne.cPath) NE 0 THEN DO:*/
/*               /* Valve already covered by path one */                 */
/*               NEXT PathTwoBlock.                                      */
/*            END.                                                       */
/*         END.                                                          */
            iValueTwo = ttFullPathTwo.iValue.
            
            iPrevBest = iValueBest.
            iValueBest = MAXIMUM (iValueBest, iValueOne + iValueTwo).
            IF iValueBest NE iPrevBest THEN DO:
               ASSIGN 
                  ttFullPathOne.cOtherPath = ttFullPathTwo.cPath
                  ttFullPathOne.iBestValue = iValueBest
               .
            END.
         END. /* IsXOR */         
      END. /* PathTwoBlock: */
   END. /* PathOneBlock: */
   
   RETURN iValueBest.
      
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

FUNCTION getValueofPath RETURNS INTEGER 
(INPUT ipcPath     AS CHARACTER,
 INPUT ipiTimeLeft AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the value of a path by going to all the valves and open them
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iValve     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValve     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValue     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPrevValve AS CHARACTER NO-UNDO.

DEFINE BUFFER ttPath  FOR ttPath.
DEFINE BUFFER ttValve FOR ttValve.

   DO iValve = 1 TO NUM-ENTRIES (ipcPath):
      cValve = ENTRY (iValve, ipcPath).
      FIND ttValve
      WHERE ttValve.cValve EQ cValve.
      IF iValve GT 1 THEN DO:
         FIND  ttPath
         WHERE ttPath.cFromValve EQ cPrevValve
         AND   ttPath.cToValve   EQ cValve.
         ipiTimeLeft = ipiTimeLeft - ttPath.iMinutes - 1.
         iValue = iValue + ttValve.iFlowRate * ipiTimeLeft.
      END.         
      
      cPrevValve = cValve.
   END.
   
   RETURN iValue.

END FUNCTION.

FUNCTION IsXOR RETURNS LOGICAL 
(INPUT ipcBitString1 AS CHARACTER,
 INPUT ipcBitString2 AS CHARACTER):
    
/*------------------------------------------------------------------------------
 Purpose: Calculates an XOR (only *different values* per position allowed)
          and returns the result
 Notes:   Takes BitString in input, a binary value with "0" and "1" characters
------------------------------------------------------------------------------*/
DEFINE VARIABLE iBit AS INTEGER NO-UNDO.

   IF LENGTH (ipcBitString1) NE LENGTH (ipcBitString2) THEN 
      RETURN FALSE.
   
   DO iBit = 1 TO LENGTH (ipcBitString1):
      IF SUBSTRING (ipcBitString1, iBit, 1) EQ SUBSTRING (ipcBitString2, iBit, 1) THEN
         /* On first occurance of XOR = FALSE, return FALSE */ 
         RETURN FALSE.   
   END.
   
   RETURN TRUE.
      
END FUNCTION. /* IsXOR */

