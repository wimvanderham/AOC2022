
/*------------------------------------------------------------------------
    File        : day21.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 21 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Dec 22 17:37:46 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/21".
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
DEFINE VARIABLE iPair        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPair        AS CHARACTER NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttMonkey
   FIELD iNr         AS INTEGER 
   FIELD cMonkey     AS CHARACTER 
   FIELD iValue      AS DECIMAL   FORMAT ">>>,>>>,>>>,>>>,>>9-"
   FIELD cMonkey1    AS CHARACTER
   FIELD iValue1     AS DECIMAL   FORMAT ">>>,>>>,>>>,>>>,>>9-" 
   FIELD cOperation  AS CHARACTER 
   FIELD cMonkey2    AS CHARACTER
   FIELD iValue2     AS DECIMAL   FORMAT ">>>,>>>,>>>,>>>,>>9-"
   FIELD lCalculated AS LOGICAL 
INDEX indNr     IS UNIQUE iNr
INDEX indMonkey IS UNIQUE cMonkey.
 
DEFINE BUFFER ttMonkey1 FOR ttMonkey.
DEFINE BUFFER ttMonkey2 FOR ttMonkey.
DEFINE BUFFER ttYou     FOR ttMonkey.

DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.

DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOpen       AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttExpression
   FIELD iOpen       AS INTEGER 
   FIELD cExpression AS CHARACTER 
INDEX indOpen IS UNIQUE iOpen.


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

COPY-LOB FROM FILE "input\21.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "root: pppw + sjmn~ndbpl: 5~ncczh: sllz + lgvd~nzczc: 2~nptdq: humn - dvpt~ndvpt: 3~nlfqf: 4~nhumn: 5~nljgn: 2~nsjmn: drzm * dbpl~nsllz: 4~npppw: cczh / lfqf~nlgvd: ljgn * ptdq~ndrzm: hmdt - zczc~nhmdt: 32~n".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.


IF lvlShow THEN DO:
END. 
   
ASSIGN 
   iSolution = 0
.

ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   CREATE ttMonkey.
   ASSIGN 
      ttMonkey.iNr = iLine
      ttMonkey.cMonkey = ENTRY (1, cLine, ":")
   .
   cLine = TRIM (ENTRY (2, cLine, ":")).
   IF NUM-ENTRIES (cLine, " ") GT 1 THEN DO:
      ASSIGN 
         ttMonkey.cMonkey1   = ENTRY (1, cLine, " ")
         ttMonkey.cOperation = ENTRY (2, cLine, " ")
         ttMonkey.cMonkey2   = ENTRY (3, cLine, " ")
      .
   END.
   ELSE DO:
      ASSIGN 
         ttMonkey.iValue = INTEGER (cLine)
      .
   END.
    
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMonkey:HANDLE).   
END.                     

IF lPart[1] THEN DO:
   /* Process Part One */

   iSolution = 0.
   
   SolveBlock:
   REPEAT:
      
      FOR EACH ttMonkey 
      WHERE ttMonkey.iValue EQ 0:
         ACCUM "" (COUNT).
         FIND  ttMonkey1 
         WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1
         AND   ttMonkey1.iValue  NE 0 NO-ERROR.
         FIND  ttMonkey2
         WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2
         AND   ttMonkey2.iValue  NE 0 NO-ERROR.
         IF  AVAILABLE ttMonkey1
         AND AVAILABLE ttMonkey2 THEN DO:
            CASE ttMonkey.cOperation:
               WHEN "+" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue + ttMonkey2.iValue.
               WHEN "*" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue * ttMonkey2.iValue.
               WHEN "-" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue - ttMonkey2.iValue.
               WHEN "/" THEN 
                  ttMonkey.iValue = DECIMAL (ttMonkey1.iValue / ttMonkey2.iValue).
            END CASE.
         END.
      END.
      
      IF (ACCUM COUNT "") EQ 0 THEN 
         LEAVE.
   END.         
       
   FIND  ttMonkey
   WHERE ttMonkey.cMonkey EQ "root".
   iSolution = ttMonkey.iValue.
                   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 21 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   
   /* Change data for Part Two */
   FIND ttMonkey WHERE ttMonkey.cMonkey EQ "root".
   ttMonkey.cOperation = "=".
   
   FIND ttYou WHERE ttYou.cMonkey EQ "humn".
   ttYou.iValue = 0.
   
   SolveBlock:
   REPEAT:
      FOR EACH ttMonkey:
         ttMonkey.lCalculated = FALSE.
      END.
       
      FOR EACH ttMonkey 
      WHERE ttMonkey.iValue  EQ 0
      AND  (ttMonkey.iValue1 EQ 0 OR ttMonkey.iValue2 EQ 0)
      /* Exclude "humn" (in ttYou) from solution */
      AND   ttMonkey.iNr        NE ttYou.iNr
      /* Exclude "root" (with operation =) from solution */
      AND   ttMonkey.cOperation NE "=":
         FIND  ttMonkey1 
         WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1
         AND   ttMonkey1.iValue  NE 0 NO-ERROR.
         IF  AVAILABLE ttMonkey1 
         AND ttMonkey.iValue1 EQ 0 THEN DO: 
            ttMonkey.iValue1 = ttMonkey1.iValue.
            ttMonkey.lCalculated = TRUE.
         END.
            
         FIND  ttMonkey2
         WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2
         AND   ttMonkey2.iValue  NE 0 NO-ERROR.
         IF  AVAILABLE ttMonkey2 
         AND ttMonkey.iValue2 EQ 0 THEN DO:
            ttMonkey.iValue2 = ttMonkey2.iValue.
            ttMonkey.lCalculated = TRUE.
         END.
         
         IF  AVAILABLE ttMonkey1
         AND AVAILABLE ttMonkey2 THEN DO:
            /* We can calculate the value */
            CASE ttMonkey.cOperation:
               WHEN "+" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue + ttMonkey2.iValue.
               WHEN "*" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue * ttMonkey2.iValue.
               WHEN "-" THEN 
                  ttMonkey.iValue = ttMonkey1.iValue - ttMonkey2.iValue.
               WHEN "/" THEN 
                  ttMonkey.iValue = DECIMAL  (ttMonkey1.iValue / ttMonkey2.iValue).
            END CASE.
         END. /* We can calculate the value */
      END.
      
      FOR EACH ttMonkey
      WHERE ttMonkey.lCalculated EQ TRUE:
         ACCUM "" (COUNT).
      END.
      
      IF (ACCUM COUNT "") EQ 0 THEN
         /* No new calculations done */
         LEAVE.
   END.         
   
   IF lvlShow THEN DO:
      OUTPUT TO "output\21.txt".
   END.
   
   /* Assign possible values for "root" Monkey */
   FOR EACH ttMonkey
   WHERE ttMonkey.cMonkey EQ "root":
      FIND  ttMonkey1
      WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1.
      ASSIGN 
         ttMonkey.iValue1 = ttMonkey1.iValue.
      FIND  ttMonkey2
      WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2.
      ASSIGN 
         ttMonkey.iValue2 = ttMonkey2.iValue.
   END.
   
   FIND  ttMonkey 
   WHERE ttMonkey.cMonkey EQ "root".
   IF lvlShow THEN DO:
      PUT UNFORMATTED 
      SUBSTITUTE ("&1: &2 &3 &4 &5 &6", 
                  ttMonkey.cMonkey, 
                  ttMonkey.cMonkey1,
                  ttMonkey.iValue1, 
                  ttMonkey.cOperation, 
                  ttMonkey.cMonkey2,
                  ttMonkey.iValue2) SKIP.
   END.                     

   FIND  ttMonkey1
   WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1.
   FIND  ttMonkey2
   WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2.
   IF ttMonkey1.iValue NE 0 THEN DO:
      iSolution = ttMonkey1.iValue.
      FIND  ttMonkey
      WHERE ttMonkey.cMonkey EQ ttMonkey2.cMonkey.
   END.
   ELSE DO:
      iSolution = ttMonkey2.iValue.
      FIND  ttMonkey
      WHERE ttMonkey.cMonkey EQ ttMonkey1.cMonkey.
   END.
   
   IF lvlShow THEN DO:
      PUT UNFORMATTED
      SUBSTITUTE ("root --> &1 &2 = &3 &4",
                  ttMonkey1.cMonkey,
                  TRIM (STRING (ttMonkey1.iValue, ">>>,>>>,>>>,>>>,>>9-")),
                  ttMonkey2.cMonkey,
                  TRIM (STRING (ttMonkey2.iValue, ">>>,>>>,>>>,>>>,>>9-"))) SKIP.
   END.
   
   cExpression = SUBSTITUTE ("= &1", iSolution).
   
   InvertBlock:
   REPEAT:
      FIND  ttMonkey1
      WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1.
      FIND  ttMonkey2
      WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2.
      /* Invert the Operation */
      IF lvlShow THEN DO:
        PUT UNFORMATTED
         SUBSTITUTE ("Solution: &7 Monkey: &1, &2 &3 &4 &5 &6",
                     ttMonkey.cMonkey,
                     ttMonkey1.cMonkey,
                     TRIM (STRING (ttMonkey1.iValue, ">>>,>>>,>>>,>>>,>>9-")),
                     ttMonkey.cOperation,
                     ttMonkey2.cMonkey,
                     TRIM (STRING (ttMonkey2.iValue, ">>>,>>>,>>>,>>>,>>9-")),
                     TRIM (STRING (iSolution, ">>>,>>>,>>>,>>>,>>9-"))) SKIP.
      END.                     
      iOpen = iOpen + 1.
      
      CASE ttMonkey.cOperation:
         WHEN "+" THEN DO:
            IF ttMonkey1.iValue NE 0 THEN
               ASSIGN 
                  iSolution = iSolution - ttMonkey1.iValue
                  cExpression = SUBSTITUTE (" + &1) &2", ttMonkey1.iValue, cExpression)
               .
            ELSE
               ASSIGN 
                  iSolution = iSolution - ttMonkey2.iValue
                  cExpression = SUBSTITUTE (" + &1) &2", ttMonkey2.iValue, cExpression)
               .
         END.               
         WHEN "*" THEN DO:
            IF ttMonkey1.iValue NE 0 THEN
               ASSIGN 
                  iSolution = DECIMAL  (iSolution / ttMonkey1.iValue)
                  cExpression = SUBSTITUTE (" * &1) &2", ttMonkey1.iValue, cExpression)
               .
            ELSE
               ASSIGN 
                  iSolution = DECIMAL  (iSolution / ttMonkey2.iValue)
                  cExpression = SUBSTITUTE (" * &1) &2", ttMonkey2.iValue, cExpression)
               .
         END.
         WHEN "-" THEN DO:
            IF ttMonkey1.iValue NE 0 THEN
               ASSIGN 
                  iSolution = ttMonkey1.iValue - iSolution
                  cExpression = SUBSTITUTE ("&1 - &2)", ttMonkey1.iValue, cExpression)
               .
            ELSE
               ASSIGN 
                  iSolution = iSolution + ttMonkey2.iValue
                  cExpression = SUBSTITUTE (" - &1) &2", ttMonkey2.iValue, cExpression)
               .
         END.               
         WHEN "/" THEN DO:
            IF ttMonkey1.iValue NE 0 THEN
               ASSIGN 
                  iSolution = iSolution * ttMonkey1.iValue
                  cExpression = SUBSTITUTE (" / &1) &2", ttMonkey1.iValue, cExpression)
               .
            ELSE
               ASSIGN 
                  iSolution = iSolution * ttMonkey2.iValue
                  cExpression = SUBSTITUTE (" / &1) &2", ttMonkey2.iValue, cExpression)
               .
         END.                
      END CASE.
      CREATE ttExpression.
      ASSIGN 
         ttExpression.iOpen = iOpen
         ttExpression.cExpression = SUBSTITUTE ("&1&2 &3", FILL ("(", iOpen), iSolution, cExpression)
      .
      IF ttMonkey1.iValue EQ 0 THEN
         /* Monkey1 value not available, resolve it on next iteration */ 
         FIND  ttMonkey
         WHERE ttMonkey.cMonkey EQ ttMonkey1.cMonkey.
      ELSE
         /* Monkey2 value not available, resolve it on next iteration */
         FIND  ttMonkey
         WHERE ttMonkey.cMonkey EQ ttMonkey2.cMonkey.
      
      IF lvlShow THEN DO:
         PUT UNFORMATTED
         SUBSTITUTE ("Solution: &1, next Monkey: &2: &3 &4 &5",
                     TRIM (STRING (iSolution, ">>>,>>>,>>>,>>>,>>9-")),
                     ttMonkey.cMonkey,
                     TRIM (STRING (ttMonkey.iValue1, ">>>,>>>,>>>,>>>,>>9-")),
                     ttMonkey.cOperation,
                     TRIM (STRING (ttMonkey.iValue2, ">>>,>>>,>>>,>>>,>>9-"))) SKIP. 
      END.
      
      IF ttMonkey.cMonkey EQ ttYou.cMonkey THEN DO:
         IF lvlShow THEN DO:
            PUT UNFORMATTED 
            SUBSTITUTE ("Found Solution: &1",
                        TRIM (STRING (iSolution, ">>>,>>>,>>>,>>>,>>9-"))) SKIP(2)
            SUBSTITUTE ("&1&2 &3", FILL ("(", iOpen), iSolution, cExpression) SKIP.
            
            FOR EACH ttExpression:
               PUT UNFORMATTED
                  SUBSTITUTE ("PUT UNFORMATTED &1 &2 ': ' &3 SKIP.", 
                              ttExpression.iOpen, 
                              QUOTER (ENTRY (1, ttExpression.cExpression, "=")),
                              ENTRY(1, ttExpression.cExpression, "=")) SKIP. 
            END.
            
            OUTPUT CLOSE.
         END.
         
         ttMonkey.iValue = iSolution.

         SolveBlock:
         REPEAT:
            FOR EACH ttMonkey
            WHERE ttMonkey.cMonkey1 NE ""
            AND   ttMonkey.iValue1  EQ 0:
               FIND  ttMonkey1
               WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1.
               ttMonkey.iValue1 = ttMonkey1.iValue.
            END.
            FOR EACH ttMonkey
            WHERE ttMonkey.cMonkey2 NE ""
            AND   ttMonkey.iValue2  EQ 0:
               FIND  ttMonkey2
               WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2.
               ttMonkey.iValue2 = ttMonkey2.iValue.
            END.

            FOR EACH ttMonkey
            WHERE ttMonkey.iValue EQ 0
            AND   ttMonkey.cMonkey NE "root":
               ACCUM "" (COUNT).
               FIND  ttMonkey1
               WHERE ttMonkey1.cMonkey EQ ttMonkey.cMonkey1
               AND   ttMonkey1.iValue  NE 0 NO-ERROR.
               FIND  ttMonkey2
               WHERE ttMonkey2.cMonkey EQ ttMonkey.cMonkey2
               AND   ttMonkey2.iValue  NE 0 NO-ERROR.
               IF  AVAILABLE ttMonkey1
               AND AVAILABLE ttMonkey2 THEN DO:
                  CASE ttMonkey.cOperation:
                     WHEN "+" THEN
                        ttMonkey.iValue = ttMonkey1.iValue + ttMonkey2.iValue.
                     WHEN "*" THEN
                        ttMonkey.iValue = ttMonkey1.iValue * ttMonkey2.iValue.
                     WHEN "-" THEN
                        ttMonkey.iValue = ttMonkey1.iValue - ttMonkey2.iValue.
                     WHEN "/" THEN
                        ttMonkey.iValue = DECIMAL  (ttMonkey1.iValue / ttMonkey2.iValue).
                  END CASE.
               END.
            END.

            IF (ACCUM COUNT "") EQ 0 THEN
               LEAVE.
         END.
         IF lvlShow THEN DO:
            RUN sy\win\wbrowsett.w
               (INPUT TEMP-TABLE ttMonkey:HANDLE).
         END.
         LEAVE.
      END.
                           
   END. /* InvertBlock */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 21 - Part Two".


   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttMonkey:HANDLE).
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

