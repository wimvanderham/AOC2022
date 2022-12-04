
/*------------------------------------------------------------------------
    File        : day04.p
    Purpose     : 

    Syntax      :

    Description : Solution of Day 04 - Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 04 18:18:10 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/4".
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
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttClean
   FIELD iPair        AS INTEGER
   FIELD iElf         AS INTEGER  
   FIELD iFromSection AS INTEGER 
   FIELD iToSection   AS INTEGER 
INDEX indPair IS UNIQUE iPair iElf
INDEX indSection iFromSection iToSection.

DEFINE BUFFER ttOtherClean FOR ttClean.

DEFINE TEMP-TABLE ttOverlap
   FIELD iPair     AS INTEGER 
   FIELD cSection1 AS CHARACTER 
   FIELD cSection2 AS CHARACTER 
INDEX indPair IS UNIQUE iPair.

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

COPY-LOB FROM FILE "input\04.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "2-4,6-8~n2-3,4-5~n5-7,7-9~n2-8,3-7~n6-6,4-6~n2-6,4-8".
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

   DO iEntry = 1 TO NUM-ENTRIES (cLine):
      cEntry = ENTRY (iEntry, cLine).
      CREATE ttClean.
      ASSIGN
         ttClean.iPair        = iLine
         ttClean.iElf         = iEntry 
         ttClean.iFromSection = INTEGER (ENTRY (1, cEntry, "-"))
         ttClean.iToSection   = INTEGER (ENTRY (2, cEntry, "-"))
      .
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttClean:HANDLE).
END.
   
IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   FOR EACH ttClean:
      FIND FIRST ttOtherClean
      WHERE ttOtherClean.iPair        EQ ttClean.iPair
      AND   ttOtherClean.iElf         NE ttClean.iElf
      AND   ttOtherClean.iFromSection GE ttClean.iFromSection
      AND   ttOtherClean.iToSection   LE ttClean.iToSection 
      NO-ERROR.
      IF AVAILABLE ttOtherClean THEN DO:
         FIND  ttOverlap 
         WHERE ttOverlap.iPair EQ ttClean.iPair NO-ERROR.
         IF NOT AVAILABLE ttOverlap THEN DO:
            /* Found an overlap in this pair */
            iSolution = iSolution + 1.
            CREATE ttOverlap.
            ASSIGN 
               ttOverlap.iPair = ttClean.iPair
               ttOverlap.cSection1 = SUBSTITUTE ("&1-&2", ttClean.iFromSection, ttClean.iToSection)
               ttOverlap.cSection2 = SUBSTITUTE ("&1-&2", ttOtherClean.iFromSection, ttOtherClean.iToSection)
            .
            
            IF lvlDebug THEN DO:
               MESSAGE "Found overlap:" SKIP 
               SUBSTITUTE ("#&1 &2-&3 has overlap &5-&6 ==> &7", 
                           ttClean.iPair, 
                           ttClean.iFromSection, 
                           ttClean.iToSection, 
                           ttOtherClean.iPair, 
                           ttOtherClean.iFromSection, 
                           ttOtherClean.iToSection,
                           iSolution) 
               VIEW-AS ALERT-BOX.
            END.
         END.
      END.
   END.
      
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttOverlap:HANDLE).
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 04 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   EMPTY TEMP-TABLE ttOverlap.
   
   iSolution = 0.

   FOR EACH ttClean:
      FIND FIRST ttOtherClean
      WHERE ttOtherClean.iPair        EQ ttClean.iPair
      AND   ttOtherClean.iElf         NE ttClean.iElf
      AND ((ttOtherClean.iFromSection GE ttClean.iFromSection AND
            ttOtherClean.iFromSection LE ttClean.iToSection) 
           OR
           (ttOtherClean.iToSection GE ttClean.iFromSection AND 
            ttOtherClean.iToSection LE ttClean.iToSection))
      NO-ERROR.
      IF AVAILABLE ttOtherClean THEN DO:
         FIND  ttOverlap 
         WHERE ttOverlap.iPair EQ ttClean.iPair NO-ERROR.
         IF NOT AVAILABLE ttOverlap THEN DO:
            /* Found an overlap in this pair */
            iSolution = iSolution + 1.
            CREATE ttOverlap.
            ASSIGN 
               ttOverlap.iPair = ttClean.iPair
               ttOverlap.cSection1 = SUBSTITUTE ("&1-&2", ttClean.iFromSection, ttClean.iToSection)
               ttOverlap.cSection2 = SUBSTITUTE ("&1-&2", ttOtherClean.iFromSection, ttOtherClean.iToSection)
            .
            
            IF lvlDebug THEN DO:
               MESSAGE "Found overlap:" SKIP 
               SUBSTITUTE ("#&1 &2-&3 has overlap &5-&6 ==> &7", 
                           ttClean.iPair, 
                           ttClean.iFromSection, 
                           ttClean.iToSection, 
                           ttOtherClean.iPair, 
                           ttOtherClean.iFromSection, 
                           ttOtherClean.iToSection,
                           iSolution) 
               VIEW-AS ALERT-BOX.
            END.
         END.
      END.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 04 - Part Two".
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
