
/*------------------------------------------------------------------------
    File        : day19.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 19 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Wed Jan 04 22:57:04 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/19".
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
/*
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
*/  
DEFINE TEMP-TABLE ttBlueprint
   FIELD iBlueprint          AS INTEGER 
   FIELD cLine               AS CHARACTER 
   FIELD iOreRobotOre        AS INTEGER 
   FIELD iClayRobotOre       AS INTEGER
   FIELD iObsidianRobotOre   AS INTEGER 
   FIELD iObsidianRobotClay  AS INTEGER
   FIELD iGeodeRobotOre      AS INTEGER
   FIELD iGeodeRobotObsidian AS INTEGER
   FIELD iMaxOre             AS INTEGER   
   FIELD iMaxClay            AS INTEGER 
   FIELD iMaxObsidian        AS INTEGER
   FIELD iMaxGeode           AS INTEGER  
   INDEX indBlueprint IS UNIQUE iBlueprint.

DEFINE TEMP-TABLE ttState
   FIELD iNrState          AS INT64 
   FIELD iBlueprint        AS INTEGER 
   FIELD iMinutesLeft      AS INTEGER 
   FIELD iOreRobot         AS INTEGER
   FIELD iOreRobotAdd      AS INTEGER  
   FIELD iOre              AS INTEGER 
   FIELD iClayRobot        AS INTEGER
   FIELD iClayRobotAdd     AS INTEGER 
   FIELD iClay             AS INTEGER  
   FIELD iObsidianRobot    AS INTEGER
   FIELD iObsidianRobotAdd AS INTEGER  
   FIELD iObsidian         AS INTEGER
   FIELD iGeodeRobot       AS INTEGER
   FIELD iGeodeRobotAdd    AS INTEGER  
   FIELD iGeode            AS INTEGER 
INDEX indNrState IS UNIQUE PRIMARY iNrState.

DEFINE BUFFER ttNewState FOR ttState.

DEFINE TEMP-TABLE ttVisited
   FIELD iOreRobot      AS INTEGER 
   FIELD iOre           AS INTEGER 
   FIELD iClayRobot     AS INTEGER 
   FIELD iClay          AS INTEGER 
   FIELD iObsidianRobot AS INTEGER 
   FIELD iObsidian      AS INTEGER
   FIELD iGeodeRobot    AS INTEGER  
   FIELD iGeode         AS INTEGER
   FIELD iMinutesLeft   AS INTEGER 
INDEX indCombo IS UNIQUE iOreRobot iOre iClayRobot iClay iObsidianRobot iObsidian iGeodeRobot iGeode iMinutesLeft.
 
DEFINE VARIABLE iNewNrState AS INT64   NO-UNDO.
DEFINE VARIABLE iVisited    AS INT64   NO-UNDO.
DEFINE VARIABLE iStates     AS INT64   NO-UNDO.

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

COPY-LOB FROM FILE "input\19.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\19_test.txt" TO OBJECT lcInput.
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

   /*
   1         2  3    4   5     6     7 8    9    10   11    12   13 14   15   16       17    18   19 20  21 22 23    24   25    26    27   28 29  30  31 32
   Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 5 clay. Each geode robot costs 3 ore and 15 obsidian.
   Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 17 clay. Each geode robot costs 2 ore and 13 obsidian.
   */
   CREATE ttBlueprint.
   ASSIGN 
      ttBlueprint.iBlueprint          = INTEGER (TRIM (ENTRY (2, cLine, " "), ":"))
      ttBlueprint.cLine               = cLine
      ttBlueprint.iOreRobotOre        = INTEGER (ENTRY (7, cLine, " "))
      ttBlueprint.iClayRobotOre       = INTEGER (ENTRY (13, cLine, " "))
      ttBlueprint.iObsidianRobotOre   = INTEGER (ENTRY (19, cLine, " "))
      ttBlueprint.iObsidianRobotClay  = INTEGER (ENTRY (22, cLine, " "))
      ttBlueprint.iGeodeRobotOre      = INTEGER (ENTRY (28, cLine, " "))
      ttBlueprint.iGeodeRobotObsidian = INTEGER (ENTRY (31, cLine, " "))
      ttBlueprint.iMaxOre             = MAXIMUM (ttBlueprint.iOreRobotOre, ttBlueprint.iClayRobotOre, ttBlueprint.iObsidianRobotOre, ttBlueprint.iGeodeRobotOre)
      ttBlueprint.iMaxClay            = ttBlueprint.iObsidianRobotClay
      ttBlueprint.iMaxObsidian        = ttBlueprint.iGeodeRobotObsidian
   .
      
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttBlueprint:HANDLE).   
END.                     
   
IF lvlShow THEN DO:
   OUTPUT TO "output\19.txt" UNBUFFERED.
   PUT UNFORMATTED 
      SUBSTITUTE ("Start run &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP
   .
   FOR EACH ttBlueprint:
      DISPLAY 
      ttBlueprint
      WITH STREAM-IO WIDTH 255 TITLE " Blueprints ".
   END.
END.

BlueprintBlock:
FOR EACH ttBlueprint:
   
   ACCUM "" (COUNT).
      
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
         SUBSTITUTE ("#&1. Start: &2", ttBlueprint.iBlueprint, ttBlueprint.cLine) SKIP.
   END.
   
   /*
   iMaxGeode = getMaxGeode(ttBlueprint.iBlueprint, 24, 1, 0, 0, 0, 0, 0, 0, 0).
   */
   
   EMPTY TEMP-TABLE ttState.
   EMPTY TEMP-TABLE ttVisited.
   
   iNewNrState = 1.
   CREATE ttState.
   iStates = iStates + 1.
   ASSIGN 
      ttState.iNrState     = iNewNrState
      ttState.iBlueprint   = ttBlueprint.iBlueprint
      ttState.iMinutesLeft = 24
      ttState.iOreRobot    = 1
   .

   IF lPart[2] THEN DO:
      ttState.iMinutesLeft = 32.
   END.
   
   SearchBlock:
   REPEAT
   WITH FRAME fr-progress STREAM-IO WIDTH 255:
      /* Search maximum */
      FIND FIRST ttState NO-ERROR.
      IF NOT AVAILABLE ttState THEN
         LEAVE.

      ttBlueprint.iMaxGeode = MAXIMUM(ttBlueprint.iMaxGeode, ttState.iGeode).
          
      IF ttState.iMinutesLeft EQ 0 THEN DO:
         DELETE ttState.
         NEXT.
      END.

      /* Don't create more robots than required */
      IF ttState.iOreRobot GT ttBlueprint.iMaxOre THEN 
         ttState.iOreRobot      = ttBlueprint.iMaxOre.
      IF ttState.iClayRobot GT ttBlueprint.iMaxClay THEN 
         ttState.iClayRobot     = ttBlueprint.iMaxClay.
      IF ttState.iObsidianRobot GT ttBlueprint.iMaxObsidian THEN 
         ttState.iObsidianRobot = ttBlueprint.iMaxObsidian.
                  
      /* Reduce required material to what can be maximum consumed to reduce the ttVisited state */                     
      IF ttState.iOre GT (ttState.iMinutesLeft * ttBlueprint.iMaxOre - (ttState.iOreRobot * (ttState.iMinutesLeft - 1))) THEN 
         ttState.iOre = (ttState.iMinutesLeft * ttBlueprint.iMaxOre - (ttState.iOreRobot * (ttState.iMinutesLeft - 1))).
      IF ttState.iClay GT (ttState.iMinutesLeft * ttBlueprint.iMaxClay - (ttState.iClayRobot * (ttState.iMinutesLeft - 1))) THEN
         ttState.iClay = (ttState.iMinutesLeft * ttBlueprint.iMaxClay - (ttState.iClayRobot * (ttState.iMinutesLeft - 1))).
      IF ttState.iObsidian GT (ttState.iMinutesLeft * ttBlueprint.iMaxObsidian - (ttState.iObsidianRobot * (ttState.iMinutesLeft - 1))) THEN 
         ttState.iObsidian = (ttState.iMinutesLeft * ttBlueprint.iMaxObsidian - (ttState.iObsidianRobot * (ttState.iMinutesLeft - 1))).
                      
      FIND  ttVisited
      WHERE ttVisited.iOreRobot      EQ ttState.iOreRobot
      AND   ttVisited.iOre           EQ ttState.iOre
      AND   ttVisited.iClayRobot     EQ ttState.iClayRobot
      AND   ttVisited.iClay          EQ ttState.iClay
      AND   ttVisited.iObsidianRobot EQ ttState.iObsidianRobot
      AND   ttVisited.iObsidian      EQ ttState.iObsidian
      AND   ttVisited.iGeodeRobot    EQ ttState.iGeodeRobot
      AND   ttVisited.iGeode         EQ ttState.iGeode
      AND   ttVisited.iMinutesLeft   EQ ttState.iMinutesLeft NO-ERROR.
              
      IF AVAILABLE ttVisited THEN DO:
         /* Already visited this state, skip */
         DELETE ttState.
         iStates = iStates - 1. 
         NEXT.
      END.
      
      CREATE ttVisited.
      ASSIGN
         ttVisited.iOreRobot      = ttState.iOreRobot
         ttVisited.iOre           = ttState.iOre
         ttVisited.iClayRobot     = ttState.iClayRobot
         ttVisited.iClay          = ttState.iClay
         ttVisited.iObsidianRobot = ttState.iObsidianRobot
         ttVisited.iObsidian      = ttState.iObsidian
         ttVisited.iGeodeRobot    = ttState.iGeodeRobot
         ttVisited.iGeode         = ttState.iGeode
         ttVisited.iMinutesLeft   = ttState.iMinutesLeft
      .
      iVisited = iVisited + 1.

      IF lvlShow THEN DO:
         /* PAUSE 0 BEFORE-HIDE. */
         DISPLAY 
         ttState.iMinutesLeft   COLUMN-LABEL "Min."         FORMAT "z9"
         ttState.iOreRobot      COLUMN-LABEL "Ore Robot"    FORMAT "zz9"
         ttState.iOre           COLUMN-LABEL "Ore"          FORMAT "z,zz9"
         ttState.iClayRobot     COLUMN-LABEL "Clay Robot"   FORMAT "zz9"
         ttState.iClay          COLUMN-LABEL "Clay"         FORMAT "z,zz9"
         ttState.iObsidianRobot COLUMN-LABEL "Obs Robot"    FORMAT "zz9"
         ttState.iObsidian      COLUMN-LABEL "Obs"          FORMAT "z,zz9"
         ttState.iGeodeRobot    COLUMN-LABEL "Geode Robot"  FORMAT "zz9"
         ttState.iGeode         COLUMN-LABEL "Geode"        FORMAT "z,zz9"
         ttBlueprint.iMaxGeode  COLUMN-LABEL "Max Geode"    FORMAT "z,zz9"
         iStates                COLUMN-LABEL "States"       FORMAT "z,zzz,zzz,zz9"      
         iVisited               COLUMN-LABEL "Visited"      FORMAT "z,zzz,zzz,zz9"
         .
         PROCESS EVENTS.
      END.
                  
      IF  ttState.iOre      GE ttBlueprint.iOreRobotOre
      AND ttState.iOreRobot LT ttBlueprint.iMaxOre THEN DO:
         /* Can build an ore robot and it's needed */
         iNewNrState = iNewNrState + 1.
         CREATE ttNewState.
         iStates = iStates + 1.
         ASSIGN 
            ttNewState.iNrState = iNewNrState
         .
         BUFFER-COPY ttState EXCEPT iNrState TO ttNewState
            ASSIGN
               ttNewState.iOre         = ttState.iOre       + ttState.iOreRobot
               ttNewState.iClay        = ttState.iClay     + ttState.iClayRobot
               ttNewState.iObsidian    = ttState.iObsidian + ttState.iObsidianRobot
               ttNewState.iGeode       = ttState.iGeode    + ttState.iGeodeRobot
               ttNewState.iMinutesLeft = ttState.iMinutesLeft - 1
            . 
         ASSIGN 
            ttNewState.iOreRobot = ttNewState.iOreRobot + 1
            ttNewState.iOre      = ttNewState.iOre - ttBlueprint.iOreRobotOre
         .                       
      END. /* Can build an ore robot and it's needed */
         
      IF  ttState.iOre       GE ttBlueprint.iClayRobotOre
      AND ttState.iClayRobot LT ttBlueprint.iMaxClay THEN DO:
         /* Can build a clay robot and it's needed */
         iNewNrState = iNewNrState + 1.
         CREATE ttNewState.
         iStates = iStates + 1.
         ASSIGN 
            ttNewState.iNrState = iNewNrState
         .
         BUFFER-COPY ttState EXCEPT iNrState TO ttNewState
            ASSIGN
               ttNewState.iOre         = ttState.iOre      + ttState.iOreRobot
               ttNewState.iClay        = ttState.iClay     + ttState.iClayRobot
               ttNewState.iObsidian    = ttState.iObsidian + ttState.iObsidianRobot
               ttNewState.iGeode       = ttState.iGeode    + ttState.iGeodeRobot
               ttNewState.iMinutesLeft = ttState.iMinutesLeft - 1
            . 
         ASSIGN 
            ttNewState.iClayRobot = ttNewState.iClayRobot + 1
            ttNewState.iOre       = ttNewState.iOre - ttBlueprint.iClayRobotOre
         .                       
      END. /* Can build a clay robot and it's needed */
   
      IF  ttState.iOre           GE ttBlueprint.iObsidianRobotOre 
      AND ttState.iClay          GE ttBlueprint.iObsidianRobotClay 
      AND ttState.iObsidianRobot LT ttBlueprint.iMaxObsidian THEN DO:
         /* Can make an Obsidian Robot and it's needed */
         iNewNrState = iNewNrState + 1.
         CREATE ttNewState.
         iStates = iStates + 1.
         ASSIGN 
            ttNewState.iNrState = iNewNrState
         .
         BUFFER-COPY ttState EXCEPT iNrState TO ttNewState
            ASSIGN
               ttNewState.iOre         = ttState.iOre      + ttState.iOreRobot
               ttNewState.iClay        = ttState.iClay     + ttState.iClayRobot
               ttNewState.iObsidian    = ttState.iObsidian + ttState.iObsidianRobot
               ttNewState.iGeode       = ttState.iGeode    + ttState.iGeodeRobot
               ttNewState.iMinutesLeft = ttState.iMinutesLeft - 1
            . 
         ASSIGN 
            ttNewState.iObsidianRobot = ttNewState.iObsidianRobot + 1
            ttNewState.iOre           = ttNewState.iOre           - ttBlueprint.iObsidianRobotOre
            ttNewState.iClay          = ttNewState.iClay          - ttBlueprint.iObsidianRobotClay
         .                       
         
      END. /* Can make an Obsidian Robot and it's needed */
           
      IF  ttState.iOre      GE ttBlueprint.iGeodeRobotOre 
      AND ttState.iObsidian GE ttBlueprint.iGeodeRobotObsidian THEN DO:
         /* Can make a Geode cracking robot */
         iNewNrState = iNewNrState + 1.
         CREATE ttNewState.
         iStates = iStates + 1.
         ASSIGN 
            ttNewState.iNrState = iNewNrState
         .
         BUFFER-COPY ttState EXCEPT iNrState TO ttNewState
            ASSIGN
               ttNewState.iOre         = ttState.iOre      + ttState.iOreRobot
               ttNewState.iClay        = ttState.iClay     + ttState.iClayRobot
               ttNewState.iObsidian    = ttState.iObsidian + ttState.iObsidianRobot
               ttNewState.iGeode       = ttState.iGeode    + ttState.iGeodeRobot
               ttNewState.iMinutesLeft = ttState.iMinutesLeft - 1
            . 
         ASSIGN 
            ttNewState.iGeodeRobot = ttNewState.iGeodeRobot + 1
            ttNewState.iOre        = ttNewState.iOre        - ttBlueprint.iGeodeRobotOre
            ttNewState.iObsidian   = ttNewState.iObsidian   - ttBlueprint.iGeodeRobotObsidian
         .                       
      END. /* Can make a Geode cracking robot */

      /* Always add a collecting-only state */         
      iNewNrState = iNewNrState + 1.
      CREATE ttNewState.
      iStates = iStates + 1.
      ASSIGN 
         ttNewState.iNrState = iNewNrState
      .
      BUFFER-COPY ttState EXCEPT iNrState TO ttNewState
         ASSIGN
            ttNewState.iOre         = ttState.iOre      + ttState.iOreRobot
            ttNewState.iClay        = ttState.iClay     + ttState.iClayRobot
            ttNewState.iObsidian    = ttState.iObsidian + ttState.iObsidianRobot
            ttNewState.iGeode       = ttState.iGeode    + ttState.iGeodeRobot
            ttNewState.iMinutesLeft = ttState.iMinutesLeft - 1
         .
         
      /* This state is done, delete it from "the stack" */
      DELETE ttState.
      iStates = iStates - 1.
       
   END. /* SearchBlock: Search maximum */
                

   IF lvlShow
   AND lvlDebug THEN DO:
      MESSAGE ttBlueprint.iBlueprint SKIP
      ttBlueprint.iMaxGeode SKIP 
      ttBlueprint.iBlueprint * ttBlueprint.iMaxGeode SKIP 
      iSolution
      VIEW-AS ALERT-BOX.                   
   END.
         
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
         SUBSTITUTE ("#&1. Maximum Geode: &2, Solution: &3", ttBlueprint.iBlueprint, ttBlueprint.iMaxGeode, iSolution) SKIP 
      .            
   END.
   
   IF (ACCUM COUNT "") EQ 3 THEN DO:
      IF lPart[2] THEN
         LEAVE BlueprintBlock.
   END.
   
END. /* FOR EACH ttBlueprint: */

IF lvlShow THEN DO:
   OUTPUT CLOSE.
END.
 
IF lPart[1] THEN DO:
   iSolution = 0.
   
   FOR EACH ttBlueprint:
      iSolution = iSolution + ttBlueprint.iBlueprint * ttBlueprint.iMaxGeode.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2022 - Day 19 - Part One".
END.

IF lPart[2] THEN DO:
   iSolution = 1.
   
   FOR EACH ttBlueprint:
      ACCUM "" (COUNT).
      iSolution = iSolution * ttBlueprint.iMaxGeode.
      
      IF (ACCUM COUNT "") EQ 3 THEN 
         LEAVE.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2022 - Day 19 - Part Two".
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

