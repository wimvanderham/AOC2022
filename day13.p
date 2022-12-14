
/*------------------------------------------------------------------------
    File        : day13.p
    Purpose     : 

    Syntax      :

    Description : Soliution to Day 13 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 13 07:40:36 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/13".
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

/* Specific */
DEFINE VARIABLE cLists      AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE iList       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOrdered    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iNewOrder   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSwitch     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iStartOrder AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttPacket
   FIELD iNr         AS INTEGER 
   FIELD cPacket     AS CHARACTER
   FIELD iOrder      AS INTEGER  
INDEX indNr    IS UNIQUE iNr
INDEX indOrder iOrder.
 
DEFINE BUFFER ttNextPacket FOR ttPacket.

DEFINE TEMP-TABLE ttCompare
   FIELD cLeft  AS CHARACTER 
   FIELD cRight AS CHARACTER 
   FIELD isOrdered AS LOGICAL
INDEX indLeftRight IS UNIQUE cLeft cRight.
 
/* ********************  Preprocessor Definitions  ******************** */
   
/* ************************  Function Prototypes ********************** */

FUNCTION getNestedListElement RETURNS CHARACTER 
   (INPUT ipiElement AS INTEGER,
    INPUT ipcList    AS CHARACTER) FORWARD.

FUNCTION getNestedListNumEntries RETURNS INTEGER 
   (INPUT ipcList AS CHARACTER) FORWARD.

FUNCTION isInteger RETURNS LOGICAL 
   (INPUT ipcString AS CHARACTER) FORWARD.

FUNCTION isOrdered RETURNS LOGICAL 
   (INPUT ipcLeft  AS CHARACTER,
    INPUT ipcRight AS CHARACTER) FORWARD.

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

COPY-LOB FROM FILE "input\13.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   COPY-LOB FROM FILE "input\13_test.txt" TO OBJECT lcInput.
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

IF lvlShow THEN DO:
   OUTPUT TO "output\13.txt".
   PUT UNFORMATTED SKIP.
   OUTPUT CLOSE.
END. 
   
ASSIGN 
   iList     = 0
   iSolution = 0
   iPair     = 0
.
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      iPair = iPair + 1.
      FIND ttCompare
      WHERE ttCompare.cLeft  EQ cLists[1]
      AND   ttCompare.cRight EQ cLists[2] NO-ERROR.
      IF NOT AVAILABLE ttCompare THEN DO:
         CREATE ttCompare.
         ASSIGN 
            ttCompare.cLeft     = cLists[1]
            ttCompare.cRight    = cLists[2]
            ttCompare.isOrdered = isOrdered(ttCompare.cLeft, ttCompare.cRight)
         .
      END.
      lOrdered = ttCompare.isOrdered.
      /*
      RUN checkLists
         (INPUT  cLists,
          OUTPUT lOrdered).
      */
      IF lvlDebug THEN DO:
      END.
      IF lvlShow THEN DO:
         IF lOrdered THEN DO:
            OUTPUT TO "output\13.txt" APPEND.
            PUT UNFORMATTED 
            SUBSTITUTE ("&1 &2", iPair, cLists[1]) SKIP
            SUBSTITUTE ("&1 &2", iPair, cLists[2]) SKIP.
            OUTPUT CLOSE.
         END.
      END.
      IF lOrdered THEN 
         iSolution = iSolution + iPair.
      iList = 0.          
      NEXT ReadBlock.
   END.

   CREATE ttPacket.
   ASSIGN 
      ttPacket.iNr     = iLine
      ttPacket.cPacket = cLine
   .

   iList = iList + 1.
   ASSIGN 
      cLists[iList] = cLine
   .

END. /* ReadBlock: */

         
IF lvlShow THEN DO:
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 13 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   CREATE ttPacket.
   ASSIGN 
      ttPacket.iNr = iLine + 1
      ttPacket.cPacket = "[[2]]"
   .
   CREATE ttPacket.
   ASSIGN 
      ttPacket.iNr = iLine + 2
      ttPacket.cPacket = "[[6]]"
   .
   
   IF lvlShow THEN DO:
      OUTPUT TO "output\13.txt" APPEND UNBUFFERED.
   END.
         
   /* Initialize Ordering */
   FOR EACH ttPacket
   BY TRIM (ttPacket.cPacket, "["):
      ACCUM "" (COUNT).
      ttPacket.iOrder = (ACCUM COUNT "").
      IF lvlShow THEN DO:
         PUT UNFORMATTED
         SUBSTITUTE ("&1. &2", ttPacket.iOrder, ttPacket.cPacket) SKIP.
      END.
   END.
      
   iStartOrder = 1.
   OrderBlock:
   REPEAT:
      /* OrderBlock */
      lSwitch = FALSE.
      PacketBlock:
      REPEAT:
         FIND FIRST ttPacket
         WHERE ttPacket.iOrder EQ iStartOrder NO-ERROR.
         IF NOT AVAILABLE ttPacket THEN DO:
            LEAVE OrderBlock.
         END.
         FIND FIRST ttNextPacket
         WHERE ttNextPacket.iOrder GT ttPacket.iOrder NO-ERROR.
         DO WHILE AVAILABLE ttNextPacket:
            FIND ttCompare
            WHERE ttCompare.cLeft  EQ ttPacket.cPacket
            AND   ttCompare.cRight EQ ttNextPacket.cPacket NO-ERROR.
            IF NOT AVAILABLE ttCompare THEN DO:
               CREATE ttCompare.
               ASSIGN 
                  ttCompare.cLeft     = ttPacket.cPacket
                  ttCompare.cRight    = ttNextPacket.cPacket
                  ttCompare.isOrdered = isOrdered(ttCompare.cLeft, ttCompare.cRight)
               .
                           
            END.
            lOrdered = ttCompare.isOrdered.
            IF lvlShow THEN DO:
               PUT UNFORMATTED 
               SUBSTITUTE ("&1. &2 vs &3. &4 ==> &5",
                           ttPacket.iOrder,
                           ttPacket.cPacket,
                           ttNextPacket.iOrder,
                           ttNextPacket.cPacket,
                           STRING(lOrdered, "Ordered/NOT Ordered")) SKIP.
            END.                         
            IF lOrdered EQ FALSE THEN DO:
               ASSIGN 
                  lSwitch             = TRUE 
                  iNewOrder           = ttNextPacket.iOrder
                  ttNextPacket.iOrder = ttPacket.iOrder
                  ttPacket.iOrder     = iNewOrder
               .
               NEXT OrderBlock.
            END.
            FIND  ttPacket 
            WHERE ttPacket.iNr EQ ttNextPacket.iNr.
            FIND  FIRST ttNextPacket
            WHERE ttNextPacket.iOrder GT ttPacket.iOrder NO-ERROR.
         END.
         IF lSwitch EQ FALSE THEN 
            LEAVE PacketBlock.    
      END. /* PacketBlock */
      IF lSwitch = FALSE THEN DO:
         iStartOrder = iStartOrder + 1.
      END.
   END. /* OrderBlock */
   
   IF lvlShow THEN DO:
      OUTPUT CLOSE.
   END.
   
   IF lvlDebug THEN DO:
      MESSAGE "Final Order"
      VIEW-AS ALERT-BOX.
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPacket:HANDLE).     
   END.
      
   iSolution = 1.
   FOR EACH ttPacket
   WHERE ttPacket.cPacket EQ "[[2]]"
   OR    ttPacket.cPacket EQ "[[6]]":
      iSolution = iSolution * ttPacket.iOrder.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 13 - Part Two".
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

FUNCTION getNestedListElement RETURNS CHARACTER 
   (INPUT ipiElement    AS INTEGER,
    INPUT ipcNestedList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the nth element of a nested list
 Notes:   A nested list is a list [] that can contain sub lists like e.g. [[1,2],[3,4]]
          The function call expects in input the INTERNAL of a NestedList,
          iow, strip off the beginning [ and ending ] before calling this function
 Based on: ENTRY ( iElement , cList[ , character ] )
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iNumEntries AS INTEGER   NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOpen       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cElement    AS CHARACTER NO-UNDO.

   IF ipcNestedList EQ "" THEN 
      RETURN "".
   ELSE 
      iNumEntries = 1.
      
   CharBlock:
   DO iChar = 1 TO LENGTH (ipcNestedList):
      cChar = SUBSTRING (ipcNestedList, iChar, 1).
      IF cChar EQ "[" THEN 
         iOpen = iOpen + 1.
      IF cChar EQ "]" THEN
         iOpen = iOpen - 1.

      IF cChar EQ "," THEN DO:
         IF iOpen EQ 0 THEN DO:
            /* Only count if not a sub list*/
            iNumEntries = iNumEntries + 1.
            NEXT CharBlock.
         END.
      END.

      IF ipiElement EQ iNumEntries THEN DO:
         cElement = SUBSTITUTE ("&1&2", cElement, cChar).
      END.
   END.
      
   RETURN cElement.      

      
END FUNCTION.

FUNCTION getNestedListNumEntries RETURNS INTEGER 
   (INPUT ipcNestedList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the number of elements in a nested list
 Notes:   A nested list is a list [] with sub lists like e.g. [[],[]]
          This function expects the stripped Nested list in input,
          iow words without the leading [ and trailing ]
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iNumEntries AS INTEGER   NO-UNDO.
DEFINE VARIABLE iChar       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOpen       AS INTEGER   NO-UNDO.

   IF ipcNestedList EQ "" THEN
      RETURN 0.
   ELSE
      iNumEntries = 1.
      
   DO iChar = 1 TO LENGTH (ipcNestedList):
      cChar = SUBSTRING (ipcNestedList, iChar, 1).
      IF cChar EQ "[" THEN 
         /* Keep track of nested lists */
         iOpen = iOpen + 1.
      IF cChar EQ "]" THEN
         /* Keep track of nested lists */
         iOpen = iOpen - 1.
      IF cChar EQ "," THEN DO:
         IF iOpen EQ 0 THEN
            /* Only count if not a sub list*/
            iNumEntries = iNumEntries + 1.
      END.
   END.
   
   RETURN iNumEntries.   
END FUNCTION.

FUNCTION isInteger RETURNS LOGICAL 
   (INPUT ipcString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Determines if the string in input is an integer
 Notes:   Should only contain numbers 0-9
------------------------------------------------------------------------------*/   
DEFINE VARIABLE lInteger AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iChar    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar    AS CHARACTER NO-UNDO.

   lInteger = TRUE.
   
   CheckBlock:
   DO iChar = 1 TO LENGTH (ipcString):
      cChar = SUBSTRING (ipcString, iChar, 1).
      IF cChar LT "0" OR cChar GT "9" THEN DO: 
         lInteger = FALSE.
         LEAVE CheckBlock.
      END.
   END.
   
   RETURN lInteger.
      
END FUNCTION.

FUNCTION isOrdered RETURNS LOGICAL 
   (INPUT ipcLeft  AS CHARACTER,
    INPUT ipcRight AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Determine if two inputs (Left & Right) are ordered
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iLeft         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRight        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLeftElement  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRightElement AS CHARACTER NO-UNDO.

DEFINE VARIABLE iLeftChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRightChar       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLeftChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRightChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLeftInteger     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRightInteger    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNumEntriesLeft  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNumEntriesRight AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOrdered         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iElement         AS INTEGER   NO-UNDO.

   IF ipcLeft BEGINS "[" AND ipcRight BEGINS "[" THEN DO:
      /* Two lists in Input */
      ASSIGN
         /* Strip off heading [ and trailing ] */ 
         ipcLeft  = SUBSTRING (ipcLeft,  2, LENGTH (ipcLeft)  - 2)
         ipcRight = SUBSTRING (ipcRight, 2, LENGTH (ipcRight) - 2)
      .
      ASSIGN 
         iNumEntriesLeft  = getNestedListNumEntries(ipcLeft)
         iNumEntriesRight = getNestedListNumEntries(ipcRight)
      .
      iElement = 1.
      DO WHILE iElement LE iNumEntriesLeft
      AND iElement LE iNumEntriesRight:
         ASSIGN 
            cLeftElement  = getNestedListElement(iElement, ipcLeft)
            cRightElement = getNestedListElement(iElement, ipcRight)
         .
         lOrdered = isOrdered(cLeftElement, cRightElement).
         IF lOrdered EQ TRUE THEN DO:
            RETURN TRUE.
         END.
         ELSE IF lOrdered EQ FALSE THEN DO:
            RETURN FALSE.
         END.
         ELSE DO:
            iElement = iElement + 1.
         END.
      END. 
         
      IF iNumEntriesLeft LT iNumEntriesRight THEN DO:
         RETURN TRUE.
      END.
      ELSE DO: 
         IF iNumEntriesLeft GT iNumEntriesRight THEN DO:
            RETURN FALSE.
         END.
      END.
      
      /* No differences found, continue checking rest of input */
      RETURN ?.
   END.
   ELSE DO:
      IF isInteger(ipcLeft) AND isInteger(ipcRight) THEN DO:
         IF INTEGER (ipcLeft) LT INTEGER (ipcRight) THEN DO:
            RETURN TRUE.
         END.
         ELSE IF INTEGER (ipcLeft) GT INTEGER (ipcRight) THEN DO:
            RETURN FALSE.
         END.
         ELSE DO:
            RETURN ?.
         END.
      END.
      ELSE DO:
         IF isInteger(ipcLeft) AND ipcRight BEGINS "[" THEN DO:
            RETURN isOrdered (SUBSTITUTE ("[&1]", ipcLeft), ipcRight).
         END.
         ELSE DO:
            IF ipcLeft BEGINS "[" AND isInteger(ipcRight) THEN DO:
               RETURN isOrdered (ipcLeft, SUBSTITUTE ("[&1]", ipcRight)).
            END.
         END.
      END.
   END.
            
END FUNCTION.
