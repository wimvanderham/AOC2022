
/*------------------------------------------------------------------------
    File        : day07.p
    Purpose     : 

    Syntax      :

    Description : Solution to Day 7 of Advent of Code 2022

    Author(s)   : Wim van der Ham (WITS)
    Created     : Wed Dec 07 15:49:36 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/2022/day/7".
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

/* Specific */
DEFINE VARIABLE cPWD       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFullPath  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDirList   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFileSize  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFreeSpace AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttDirectory
   FIELD cFullPath    AS CHARACTER 
   FIELD iTotalSize   AS INTEGER  
   FIELD cParentDir   AS CHARACTER
   FIELD cRelativeDir AS CHARACTER  
INDEX indFull IS UNIQUE cFullPath.

DEFINE BUFFER ttParentDirectory FOR ttDirectory.

DEFINE TEMP-TABLE ttFile
   FIELD cFullPath AS CHARACTER 
   FIELD cFileName AS CHARACTER 
   FIELD iFileSize AS INTEGER 
INDEX indFullName IS UNIQUE cFullPath cFileName.

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

COPY-LOB FROM FILE "input\07.txt" TO OBJECT lcInput.

IF lvlDebug THEN DO:
   lcInput = "$ cd /~n$ ls~ndir a~n14848514 b.txt~n8504156 c.dat~ndir d~n$ cd a~n$ ls~ndir e~n29116 f~n2557 g~n62596 h.lst~n$ cd e~n$ ls~n584 i~n$ cd ..~n$ cd ..~n$ cd d~n$ ls~n4060174 j~n8033020 d.log~n5626152 d.ext~n7214296 k".
   MESSAGE "Debug Input:" SKIP(1) 
   STRING (lcInput)
   VIEW-AS ALERT-BOX.
END.

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
      
   IF cLine BEGINS "$" THEN DO:
      lDirList = FALSE.
      cCommand = TRIM (SUBSTRING (cLine, 2)).
      CASE ENTRY (1, cCommand, " "):
         WHEN "cd" THEN DO:
            cDirectory = TRIM (SUBSTRING (cCommand, 3)).
            IF cDirectory BEGINS "/" THEN DO:
               cPWD = cDirectory.
               FIND  ttDirectory
               WHERE ttDirectory.cFullPath EQ cPWD NO-ERROR.
               IF NOT AVAILABLE ttDirectory THEN DO:
                  CREATE ttDirectory.
                  ASSIGN 
                     ttDirectory.cFullPath = cPWD
                  .
               END.
            END.
            ELSE DO:
               IF cDirectory = ".." THEN DO:
                  ENTRY (NUM-ENTRIES (cPWD, "/"), cPWD, "/") = "".
                  cPWD = RIGHT-TRIM (cPWD, "/").
                  IF cPWD = "" THEN 
                     cPWD = "/".
               END.
               ELSE DO:
                  cFullpath = SUBSTITUTE ("&1/&2", (IF cPWD NE "/" THEN cPWD ELSE ""), cDirectory).
           
                  FIND  ttDirectory 
                  WHERE ttDirectory.cFullPath EQ cFullpath NO-ERROR.
                  IF NOT AVAILABLE ttDirectory THEN DO:
                     CREATE ttDirectory.
                     ASSIGN 
                        ttDirectory.cFullPath  = cFullPath
                        ttDirectory.cParentDir = cPWD
                     .
                  END.
                  cPWD = cFullPath.
               END.
            END.
         END.
         WHEN "ls" THEN DO:
            lDirList = TRUE.
         END.
      END CASE.
      
      IF lvlDebug THEN DO:
         MESSAGE 
         iLine cLine SKIP 
         cPWD
         VIEW-AS ALERT-BOX.
      END.
      
      NEXT ReadBlock.
   END. /* IF cLine BEGINS "$" THEN DO: */

   IF lDirList THEN DO:
      IF ENTRY (1, cLine, " ") EQ "dir" THEN DO:
         cFullPath = SUBSTITUTE ("&1/&2", (IF cPWD NE "/" THEN cPWD ELSE ""), ENTRY (2, cLine, " ")).
         FIND  ttDirectory
         WHERE ttDirectory.cFullPath EQ cFullPath NO-ERROR.
         IF NOT AVAILABLE ttDirectory THEN DO:
            CREATE ttDirectory.
            ASSIGN
               ttDirectory.cFullPath  = cFullPath
               ttDirectory.cParentDir = cPWD
            .
         END.
      END.
      ELSE DO:
         ASSIGN 
            cFileName = ENTRY (2, cLine, " ")
            iFileSize = INTEGER (ENTRY (1, cLine, " "))
         .
         
         FIND  ttFile
         WHERE ttFile.cFullPath = cPWD
         AND   ttFile.cFileName = cFileName NO-ERROR.
         IF NOT AVAILABLE ttFile THEN DO:
            CREATE ttFile.
            ASSIGN 
               ttFile.cFullPath = cPWD
               ttFile.cFileName = cFileName
               ttFile.iFileSize = iFileSize
            .
         END.
      END.
   END.  
       
END. /* ReadBlock: */

FOR EACH ttFile,
EACH ttDirectory OF ttFile:
   ttDirectory.iTotalSize = ttDirectory.iTotalSize + ttFile.iFileSize.
END.

FOR EACH ttDirectory
BY NUM-ENTRIES (ttDirectory.cFullPath, "/") DESCENDING:
   /* Process all Directories, from deepest level til root */
   FIND  ttParentDirectory
   WHERE ttParentDirectory.cFullPath = ttDirectory.cParentDir NO-ERROR.
   IF AVAILABLE ttParentDirectory THEN 
      ttParentDirectory.iTotalSize = ttParentDirectory.iTotalSize + ttDirectory.iTotalSize.
END. /* Process all Directories, from deepest level til root */


IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttDirectory:HANDLE).
   RUN sy\win\wbrowsett.w (INPUT TEMP-TABLE ttFile:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   
   FOR EACH ttDirectory
   WHERE ttDirectory.iTotalSize LE 100000:
      ACCUM ttDirectory.iTotalSize (TOTAL).
   END.
   
   ASSIGN 
      iSolution = (ACCUM TOTAL ttDirectory.iTotalSize)
   .
     
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 07 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */

   FIND FIRST ttDirectory WHERE ttDirectory.cParentDir EQ "".
   IF lvlDebug THEN DO:
      MESSAGE ttDirectory.cFullPath SKIP 
      ttDirectory.iTotalSize
      VIEW-AS ALERT-BOX.
   END.
   
   iFreeSpace = 70000000 - ttDirectory.iTotalSize.
   
   FOR EACH ttDirectory
   WHERE ttDirectory.iTotalSize GE (30000000 - iFreeSpace)
   BY ttDirectory.iTotalSize:
      iSolution = ttDirectory.iTotalSize.
      LEAVE.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2022 - Day 07 - Part Two".
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
