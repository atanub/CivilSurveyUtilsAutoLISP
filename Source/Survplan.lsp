;;SurvPlan.LSP 
;;Last modified : Dec-30'1998 >> Added flexibility of drawing spot levels either in 2D or in 3D

;;
;;(setq gstrSrvPlanPath "C:\\\SURVEY\\DIST\\")
;;
(defun PrintLoadMsg(iPerCent) (if (= iPerCent 0) (princ "\nPlease wait...Loading neccessary files\n") (princ (strcat "\r" (itoa iPerCent) "% Loaded"))) (if (= iPerCent 100) (princ "\nLoading Successful\n"))(princ))
(PrintLoadMsg 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SURVUTL1.LSP Start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;From a Line Data extracts the First Word of a Line &
;;Returns the Word
;;Status OK
;;
(defun GetFirstWord (strLineData / StrReturn i iLoopLim char bWordRecState)
  (setq StrReturn "" i 1 iLoopLim (+ (strlen strLineData) 1))
  (setq bWordRecState 0); 1= start; 2 = end
  (while (< i iLoopLim)
     (setq char (substr strLineData i 1))
     (if (or (= char " ") (= char "\t") (= char "\n"))
        (progn
           (if (= bWordRecState 1)
              ;; break the Loop
              (setq bWordRecState 2 i (+ iLoopLim 100))
           )
        )
        (progn
           (if (= bWordRecState 0) (setq bWordRecState 1))
           (if (= bWordRecState 1) (setq StrReturn (strcat StrReturn char)))
        )
     )
     (setq i (+ i 1))
  );while
  (setq StrReturn StrReturn);; Return Statement
)
;;
;;From a Line Data extracts All Words of a Line &
;;Returns the Words in a list
;;Status OK
;;
(defun XtractAllWords (strLineData / retListOfStr i iLoopLim char bRecordChar StrWord )
  (if (= strLineData nil)
    (progn
       (princ "\nError Occured in Func. (GetFirstWord)\nParameter Received is nil\n")
       (princ "\nTrace in Func. GetFirstWord Arg = ")(princ "\n") (princ strLineData) (princ "\n")
     )
  )
  (setq retListOfStr (list ()))
  (setq i 1 iLoopLim (strlen strLineData) bRecordChar 0)
  (while (< i (+ iLoopLim 1))
     (setq char (substr strLineData i 1))
     (if (or (= char " ") (= char "\n") (= char "\t"))
        (progn
           (if (= bRecordChar 1)
             (progn
              (setq retListOfStr (append retListOfStr (list StrWord)))
              (setq StrWord "" bRecordChar 0)
             )
           )
        )
        (progn
           (if (= bRecordChar 0) (setq bRecordChar 1 StrWord ""))
           (setq StrWord (strcat StrWord char))
        )
     )
     (setq i (+ i 1))
  );while
  (if (= bRecordChar 1) (setq retListOfStr (append retListOfStr (list StrWord))))
  (setq retListOfStr (cdr retListOfStr));; Return
)
;;
;;Converts a List of strings to a list of real
;;Returns the real Nos in a list
;;Status OK
;;
(defun WordListToRealList (listOfWords / retListOfReals i iLoopLim)
  (setq retListOfReals (list ()))
  (setq i 0 iLoopLim (length listOfWords))
  (while (< i iLoopLim)
     (setq retListOfReals (append retListOfReals (list (atof (nth i listOfWords)))))
     (setq i (+ i 1))
  );while
  (setq retListOfReals (cdr retListOfReals));; Return
)
(defun MakeOffsetElevList (OffElevDataStr / StrOfOffElev i iLoopLim listOfOffElev )
  (setq listTemp (XtractAllWords OffElevDataStr))
  ;; Remove First two columns having only 0's & 3rd having Chainage
  (setq listTemp (cdr (cdr (cdr listTemp))))
  (setq listTemp (WordListToRealList listTemp))
)

(defun DebugPrintList (strCaption strList / i iLoopLim)
	(setq i 0 iLoopLim (length strList))
	(while (< i iLoopLim)
		(princ (strcat "\n" strCaption "Start=============\n"))
		(princ (nth i strList))
        (princ (strcat "\n" strCaption "End============\n"))
		(if (= DEBUG T) (getstring "\nPaused :"))
		(setq i (+ i 1))
	)
)
(PrintLoadMsg 10)
(defun IsStrNumeric (strArg / i iLoopLim chChar bContinue bDotFound)
	(setq i 1  iLoopLim (strlen strArg) bContinue 1 bDotFound 0)
	(while (and (<= i iLoopLim) (= bContinue 1))
		(setq chChar (ascii (substr strArg i 1)))
		(if (or (and (<= chChar 57) (>= chChar 48)) (= chChar 46))
			(if (= chChar 46)
			    (if (= bDotFound 1)
					(setq bDotFound 2 bContinue 0)
					(setq bDotFound 1 bContinue 1)
			    )
			)
			(setq bContinue 0)
		)
		(setq i (+ i 1))
	)
    (if (= bContinue 0) (setq ret nil) (setq ret T)) ; return
    (setq ret ret) ; return
)
;; Verifies wheather a string is like "10.00+12" or not
;;
(defun IsStrStation (strArg / i iLoopLim strPre chChar bGotThePlusSign ret)
	(setq i 1  iLoopLim (strlen strArg) strPre "" strPost "")
	(setq bGotThePlusSign 0)
	(while (<= i iLoopLim)
		(setq chChar (substr strArg i 1))
		(if (= chChar "+") (setq bGotThePlusSign 1))
		(if (= bGotThePlusSign 0)
		   (setq strPre (strcat strPre chChar))
		   (if (not (= chChar "+")) (setq strPost (strcat strPost chChar)))
		)
		(setq i (+ i 1))
	)
    ;(princ strPost) (princ "<<<<<<<<strPost\n") (princ strPre) (princ "<<<<<<<<strPre\n")
	(if (= bGotThePlusSign 1)
		(progn
			(if (and (= (IsStrNumeric  strPre) T) (= (IsStrNumeric  strPost) T))
				(setq ret (list (atof strPre) (atof strPost)))
				(setq ret nil)
			)
		)
		(progn
			(setq ret nil)
		)
	)
	(setq ret ret); Return
)
(defun ConvStnToChainage (strArg fInterval / i iLoopLim strPre chChar bGotThePlusSign ret)
	(setq retList (IsStrStation strArg))
	(princ retList)(princ " <<<<<<< retList\n")

	(setq retChainage nil)
	(if (= retList nil)
	   (alert "ERROR: Bad Param In SubR FindChainage")
	   (setq retChainage (+ (* (car retList) fInterval) (cadr retList)))
	)
	(setq retChainage retChainage)
)
(defun FindMinFrList (listParam / i iLoopLim fMin)
	(setq i 0 iLoopLim (length listParam) fMin (nth 0 listParam))
	(while (< i iLoopLim)
		(if (> fMin (nth i listParam)) (setq fMin (nth i listParam)))
		(setq i (+ i 1))
	)
	(setq fMin fMin)
)
(defun FindMaxFrList (listParam / i iLoopLim fMax)
	(setq i 0 iLoopLim (length listParam) fMax (nth 0 listParam))
	(while (< i iLoopLim)
		(if (< fMax (nth i listParam)) (setq fMax (nth i listParam)))
		(setq i (+ i 1))
	)
	(setq fMax fMax)
)
(defun SubstValInList (listParam iIndex voidVal / i iLoopLim retList)
    (setq i 0 iLoopLim (length listParam) retList (list ()))
    (while (< i iLoopLim)
        (if (= i iIndex)
            (setq retList (append retList (list voidVal)))
            (setq retList (append retList (list (nth i listParam))))
        )
        (setq i (+ i 1))
    )
    (setq retList (cdr retList));Return
)
;Makes a list having iNoOfItems of items and all the items will be
;initialized as voidItem
(defun InitializeList (iNoOfItems voidItem / i retList)
    (setq i 0 retList (list ()))
    (while (< i iNoOfItems) (setq retList (append retList (list voidItem)) i (+ i 1)))
    (setq retList (cdr retList));Return
)
(defun IsCommentLine (strLine chComment / bRet)
    (setq strTemp (GetFirstWord strLine))
    (if (or (= strTemp "") (= (substr strTemp 1 1) chComment)) (setq bRet T) (setq bRet nil))
    (setq bRet bRet)
)
(PrintLoadMsg 20)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General utility file for Drawing
;;
(defun D2R (fAng) (* fAng (/ pi 180.0)))
(defun R2D (fAng) (* (/ 180.0  pi) fAng))
(defun SIMPLEHATCH (entNameArg fScale fAngleDeg strLyr / iCMDECHO strCurlyr)
    (setq strCurlyr (ChangeCurrentLayer strLyr) iCMDECHO (getvar "cmdecho")) (setvar "cmdecho" 0)
    (command "hatch" "U,I" fAngleDeg fScale "N" entNameArg "")
    (ChangeCurrentLayer strCurlyr)
    (setvar "cmdecho" iCMDECHO)
    (entlast) ;Return
)
(defun POLYLINE (listPts  bIsClose strLyr / iCMDECHO i iLoopLim strCurlyr)
    (setq strCurlyr (ChangeCurrentLayer strLyr) iCMDECHO (getvar "cmdecho")) (setvar "cmdecho" 0)
    (setq i 0 iLoopLim (length listPts))
    (command "pline")
    (while (< i iLoopLim) (command (nth  i listPts)) (setq i (+ i 1)))
    (if (= bIsClose T) (command "C") (command "")) (setvar "cmdecho" iCMDECHO)
	(ChangeCurrentLayer strCurlyr)
    (entlast) ;Return
)
(defun LINE (ptStart ptEnd strLyr / listTmpA listTmpB)
	(if (< (length ptStart) 3)
		(setq listTmpA (list (car ptStart) (cadr ptStart) 0.0))
		(setq listTmpA ptStart)
	)
	(if (< (length ptEnd) 3)
		(setq listTmpB (list (car ptEnd) (cadr ptEnd) 0.0))
		(setq listTmpB ptEnd)
	)
	(entmake (list (cons 0 "LINE") (cons 10 listTmpA) (cons 11 listTmpB) (cons 8 strLyr)))
)
(defun CIRCLE (ptCen fRad strLyr) (entmake (list (cons 0 "CIRCLE") (cons 10 (list (car ptCen) (cadr ptCen) 0.0)) (cons 40 fRad) (cons 8 strLyr))))
(defun DONUT (ptCen fInRad fOutRad strLyr)
    (setq strCurlyr (ChangeCurrentLayer strLyr) iCMDECHO (getvar "cmdecho")) (setvar "cmdecho" 0)
    (command "donut" fInRad fOutRad ptCen "")
    (ChangeCurrentLayer strCurlyr)
    (entlast) ;Return
)
(defun TEXTLEFT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str)  (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun TEXTMID (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str) (cons 71 0) (cons 72 4) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun TEXTRIGHT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle"))  (cons 1 str) (cons 71 0) (cons 72 2) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun BLOCK (strBlkName ptIns fX fY fRot strLyr / strPreLyr bReturn)
  (setq bReturn 0)
  (if (not (= (tblsearch "BLOCK" strBlkName) nil))
    (progn
      ; Insert Built-in Block
      (entmake (list (cons 0 "INSERT") (cons 8 strLyr) (cons 2 strBlkName) (cons 10 ptIns) (cons 50 (D2R fRot)) (cons 41 fX) (cons 42 fY) (cons 43 1)))
      (setq bReturn 1)
    )
    (progn ; Try to Insert DWG File
      (setq strBlkName (findfile strBlkName))
      (if (= strBlkName nil)
	(setq bReturn 0)
	(progn
	  ;;Insert Dwg File
	  (setq strPreLyr (ChangeCurrentLayer strLyr))
      
	  (command "insert" strBlkName ptIns fX fY fRot)

      (ChangeCurrentLayer strPreLyr)
	  (setq bReturn 1)
	)
      )
    )
  );if
  (if (= bReturn 0) (setq bReturn nil) (setq bReturn 1));; Return Statement
)
(PrintLoadMsg 25)
;; Draws members 
;; bIsClosed = 1 means closed others means open 
;; bDraw = T Draws lines other wise if nil then doesn't draw
(defun GenerateParaLine (listOfCenPts fOffset bIsClosed bDraw strLYR / DRAW_MEMB GEN_MEMB GEN_PAR_PTS GEN_VER_PTS GENMEMBERLIST aListOfParPts aListOfVertexes)
      (setq bIsClosed 0) ;;;Always open;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;SubR Starts here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Draws members 
      (defun DRAW_MEMB (listOfVertexes strLYR) (foreach N listOfVertexes (LINE (car n) (cadr n) strLYR)))
      ;; Generates list of members from list of vertexs
      (defun GEN_MEMB (CPLIST IS_CLOSED / count len ret_list memb)
        (setq count 1 ret_list '() len (length cplist))
        (while (< count len)
          (setq memb (list (nth (- count 1) cplist) (nth count cplist)) ret_list (append (list memb) ret_list) count (1+ count))
          (if (and (= count len) (= is_closed 1)) ;; For last member
            (progn (setq memb (list (nth (- count 1) cplist) (nth 0 cplist)) ret_list (append (list memb) ret_list)))
          )
        )
        (setq ret_list (reverse ret_list))
      )
      ;; Generates Parallel member nodes
      (defun GEN_PAR_PTS (CPLIST DIST / count len ret_list memb new_memb start_p end_p new_ang start_n end_n)
        (setq count 0 len (length cplist) ret_list '())
        (while (< count len)
          (setq memb (nth count cplist) start_p (car memb) end_p (cadr memb))
          (setq new_ang (+ (angle start_p end_p) (* pi 0.5))
                start_n (polar start_p new_ang dist) end_n (polar end_p new_ang dist)
                new_memb (list start_n end_n) ret_list (append (list new_memb) ret_list)
                count (1+ count)
          )
        );while
        (setq ret_list (reverse ret_list))
      )
      ;; Generates Verices of a boundary
      (defun GEN_VER_PTS (CPLIST IS_CLOSED / ret_list len count f_memb l_memb start memb_1 memb_2 start_1 end_1 start_2 end_2 vert_end)
        ;;finds start point
        (setq f_memb (nth 0 cplist) l_memb (last cplist))
        (if (= is_closed 1)
         (setq start (inters (cadr f_memb) (car f_memb) (car l_memb) (cadr l_memb) nil))
         (setq start (car f_memb))
        )
        (setq count 1 len (length cplist) ret_list (list start))
        (while (< count len)
          (setq memb_1 (nth (- count 1) cplist) memb_2 (nth count cplist)
                start_1 (car memb_1) end_1 (cadr memb_1) start_2 (car memb_2) end_2 (cadr memb_2)
          )
          (setq vert_end (inters start_1 end_1 start_2 end_2 nil) count (1+ count))
          (if vert_end (setq ret_list (append (list vert_end) ret_list))) ;; eliminate consecutive points on same angle
        );while
        (if (= is_closed 0)
          (setq ret_list (append (list (cadr (last cplist))) ret_list)) 
        )
        (setq ret_list (reverse ret_list))
      )
      ;; For Conversion from (list (list 0.0 0.0) (list 10.0 0.0) (list 10.0 10.0))
      ;; to (list (list (list 0.0 0.0) (list 0.0 10.0)) (list  (list 0.0 10.0) (list 10.0 10.0)))
      (defun GENMEMBERLIST (listPts / i iLoopLim retList listTemp)
          (setq i 1 iLoopLim (length listPts) retList (list ()))
          (while (< i iLoopLim)
              (setq listTemp (list (nth (- i 1) listPts) (nth i listPts))
                    retList (append retList (list listTemp))
                    i (+ i 1)
              )
          )
          (setq retList (cdr retList)) ;Return
      )
      ;;;;;;;SubR Ends here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq listOfCenPts (GENMEMBERLIST listOfCenPts))
      (setq aListOfParPts (GEN_PAR_PTS listOfCenPts fOffset)
            aListOfVertexes (GEN_VER_PTS aListOfParPts bIsClosed)
            aListOfParPts aListOfVertexes
            aListOfParPts (GEN_MEMB aListOfParPts bIsClosed)
      )
      (if (= bDraw T) (DRAW_MEMB aListOfParPts strLYR))
      ;;(DRAW_MEMB listOfCenPts strLYR);; Draws Centre line Debug
      (setq aListOfVertexes aListOfVertexes);; Return Statement
);GenerateParaLine 
(defun CreateLayer (listOfLyrNameStr / strPrevLyr i iLoopLim)
	(setq i 0 iLoopLim (length listOfLyrNameStr) strPrevLyr (getvar "clayer"))
	(while (< i iLoopLim)
		(if (= (tblsearch "LAYER" (nth i listOfLyrNameStr)) nil)
			(command "layer" "m" (nth i listOfLyrNameStr) "c" (+ i 1) "" "")
		)
		(setq i (+ i 1))
	)
	(if (> i 0) (command "layer" "s" strPrevLyr ""))
)
(defun DrawSpotLevel (dLevel ptOrigin dTextHt dTextAngleRad strMarkBlkName dMarkSize strLyr / ptCen ptLeft ptRight strLeft strRt ENameLast GetLeftNRiteStr)
	(defun GetLeftNRiteStr(dLevel / ParseLeftNRiteStrs listStrs)
		;;From a Line Data extracts All Words of a Line &
		;;Returns the Words in a list
		(defun ParseLeftNRiteStrs(strLineData chSeparator / retListOfStr i iLoopLim char bRecordChar StrWord )
		  (if (= strLineData nil)
			(progn
			   (princ "\nError Occured in Func. (XtractAllWords)\nParameter Received is nil\n")
			   (princ "\nTrace in Func. XtractAllWords Arg = ")(princ "\n") (princ strLineData) (princ "\n")
			 )
		  )
		  (setq retListOfStr (list ()))
		  (setq i 1 iLoopLim (strlen strLineData) bRecordChar 0)
		  (while (< i (+ iLoopLim 1))
			 (setq char (substr strLineData i 1))
			 (if (or (= char chSeparator) (= char "\n") (= char "\t"))
				(progn
				   (if (= bRecordChar 1)
					 (progn
					  (setq retListOfStr (append retListOfStr (list StrWord)))
					  (setq StrWord "" bRecordChar 0)
					 )
				   )
				)
				(progn
				   (if (= bRecordChar 0) (setq bRecordChar 1 StrWord ""))
				   (setq StrWord (strcat StrWord char))
				)
			 )
			 (setq i (+ i 1))
		  );while
		  (if (= bRecordChar 1) (setq retListOfStr (append retListOfStr (list StrWord))))
		  (setq retListOfStr (cdr retListOfStr));; Return
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq listStrs (ParseLeftNRiteStrs (rtos dLevel 2) "."))
		(if(= (length listStrs) 1)
			(progn
				(setq listStrs (append listStrs (list "000")))
			)
			(progn
			)
		)
		(setq listStrs listStrs)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (= (tblsearch "BLOCK" strMarkBlkName) nil)
		(progn
			(alert (strcat "Block \"" strMarkBlkName "\" not present in the current drawing\nDrawing not possible !"))
			(exit)
		)
	)
	(setq ptCen (polar ptOrigin (- dTextAngleRad (/ pi 2.0)) (/ dTextHt 2.0))
		  ptLeft (polar ptCen (+ dTextAngleRad pi) (/ dMarkSize 2.0))
		  ptRight (polar ptCen dTextAngleRad (/ dMarkSize 2.0))
	)
	(setq listStrs (GetLeftNRiteStr dLevel))
	(setq strLeft (nth 0 listStrs) strRt (nth 1 listStrs))

	(setq ENameLast (TEXTLEFT ptRight strRt (R2D dTextAngleRad) dTextHt strLyr))
	;REM (if(/= ENameLast nil) (AttatchXData ENameLast strDataMain (rtos dLevel 2 4)))

	(setq ENameLast (TEXTRIGHT ptLeft strLeft (R2D dTextAngleRad) dTextHt strLyr))
	;REM (if(/= ENameLast nil) (AttatchXData ENameLast strDataMain (rtos dLevel 2 4)))

	(setq ENameLast (BLOCK strMarkBlkName ptOrigin dMarkSize dMarkSize (R2D dTextAngleRad) strLyr))
	;REM (if(/= ENameLast nil) (AttatchXData ENameLast strDataMain (rtos dLevel 2 4)))
)
(PrintLoadMsg 33)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SURVOBJS.LSP Start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Configuration Related Funcs. <OBJECT>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ReadObjectConfig(strSrcFileName / strLine listObjConfigs listTemp FILEptr)
    (if (= (findfile strSrcFileName) nil) (alert (strcat "ERROR: File '" strSrcFileName "' not found!")))
    (if (= (setq FILEptr (open strSrcFileName "r")) nil) (alert (strcat "ERROR: Opening File '" strSrcFileName "'")))
    (setq strLine T listObjConfigs (list ()))
    (while (not (= strLine nil))
        (setq strLine (read-line FILEptr))
        (if (not (= strLine nil))
            (progn
                (if (and (/= strLine "") (= (IsCommentLine strLine "*") nil))
                    (progn
                        (setq listTemp (XtractAllWords strLine))
                        ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                        ;;Xtract Object configuration settings here
                        (setq listTemp (XtractAllWords strLine)
                              listObjConfigs (append listObjConfigs (list listTemp))
                        )
                        ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    );progn
                )
            )
        )
    )
    (close FILEptr)
    (setq listObjConfigs (cdr listObjConfigs));; Return
)
(defun DrawAllObjects(strSrcFileName strObjCfgFile listConfigVals strLyr / strLine listObjConfigs listTemp FILEptr)
    (if (= (findfile strSrcFileName) nil) (alert (strcat "ERROR: File '" strSrcFileName "' not found!")))
    (if (= (setq FILEptr (open strSrcFileName "r")) nil) (alert (strcat "ERROR: Opening File '" strSrcFileName "'")))
    ;;Read Object configuration file
    (princ "\nPlease wait...Drawing Surveyed Objects\n")
    (setq listObjConfigs (ReadObjectConfig strObjCfgFile))

    (setq strLine T)
    (while (not (= strLine nil))
        (setq strLine (read-line FILEptr))
        (if (not (= strLine nil))
            (progn
                (if (and (/= strLine "") (= (IsCommentLine strLine "*") nil))
                    (progn
                        (setq listTemp (XtractAllWords strLine))
                        (if (/= (CheckForValidLabel (car listTemp) listObjConfigs) nil)
                            (progn
                                ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                ;;Call Drawing Routines Here
                                (princ (strcat "\rDrawing Object '" (car listTemp) "'                   "))
                                (DrawObjects listTemp FILEptr listObjConfigs listConfigVals strLyr)
                                ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                            )
                            (progn
                                (alert (strcat "ERROR: Invalid line\n'" strLine "'\nFound in Objects file!"))
                            )
                        );if
                    );progn
                )
            )
        )
    )
    (close FILEptr)
)
(PrintLoadMsg 41)

;;Checks wheater an Object is valid or not
;;If Valid then returns the associate Label (Block/LineType)
;;Otherwise returns nil
(defun CheckForValidLabel(strLabel listObjConfigs / i iLoopLim strAssocLabel)
    (setq i 0 iLoopLim (length listObjConfigs) strAssocLabeL nil)
    (while (and (< i iLoopLim) (= strAssocLabel nil))
        (if (= (strcase (car (nth i listObjConfigs))) (strcase strLabel))
            (setq strAssocLabel (cadr (nth i listObjConfigs)))
        )
        (setq i (+ i 1))
    )
    (setq strAssocLabeL strAssocLabel);Return
)

(defun DrawObjects(listLabelObject FILEptr listObjConfigs listConfigVals strLyr / strLabel )
    (setq strLabel (strcase (car listLabelObject)) fBlkSizeMFwrtTxtSize 1.25)
    (if (or (= strLabel "SURVEYED_TREE") (= strLabel "NOT_IN_SITU_TREE") (= strLabel "PALM_TREE") (= strLabel "TUBE_WELL") (= strLabel "KANCHA_WELL") (= strLabel "PUCCA_WELL"))
        (DrawBlockObjects listLabelObject listObjConfigs listConfigVals strLyr)
    )
    (if (or (= strLabel "KANCHA_ROAD_SIDE") (= strLabel "HOUSE_BOUNDARY")
            (= strLabel "VILLEGE_BOUNDARY") (= strLabel "JUNGLE_LIMIT")
            (= strLabel "MOORUM_ROAD_SIDE") (= strLabel "FOOTPATH_SIDE")
            (= strLabel "NALLAH_SIDE") (= strLabel "PROPOSED_PLANT_SITE")
        )
        (DrawSingleLineObjects listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (or (= strLabel "TELEPHONE_POST") (= strLabel "ELEC_LIGHT_POST"))
        (DrawElecOrTelphonePost listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (or (= strLabel "KANCHA_ROAD_CEN") (= strLabel "FOOTPATH_CEN")
            (= strLabel "NALLAH_CEN") (= strLabel "MOORUM_ROAD_CEN")
        )
        (DrawDoubleLineObjects listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (or (= strLabel "POND") (= strLabel "HUTMENT") (= strLabel "PUCCA_BUILDING"))
        (DrawPondOrHutmentOrPBuilding listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (= strLabel "EMBANKMENT")
        (DrawEmbankment listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (= strLabel "OPEN_ROCK")
        (DrawOpenRock listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    (if (or (= strLabel "CULVERT01") (= strLabel "CULVERT02"))
        (DrawCulvert listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
    ;;;;;;;;;;;;;;Not implemented ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (= strLabel "RAIN_CUT")
        (DrawRainCut listLabelObject FILEptr listObjConfigs listConfigVals strLyr)
    )
)
(PrintLoadMsg 46)
;;
;;Reads 'iNoOfPoints' nos of vaild line and returns the Point list
;;
(defun ReadPointData (iNoOfPoints  FILEptr / i strLine listOfPointsOnLine listTemp)
   (setq i 0 strLine T listOfPointsOnLine (list ()))
   (while (and (< i iNoOfPoints) (/= strLine nil))
        (setq strLine (read-line FILEptr))
        (if (not (= strLine nil))
            (progn
                (if (and (/= strLine "") (= (IsCommentLine strLine "*") nil))
                    (progn
                        (setq listTemp (XtractAllWords strLine)
                              i (+ i 1)
                              ;;Old listTemp (list (* 1000.0 (atof (cadr listTemp))) (* 1000.0 (atof (car listTemp))))
                              listTemp (list (atof (cadr listTemp)) (atof (car listTemp)))
                              listOfPointsOnLine (append listOfPointsOnLine (list listTemp))
                        )
                    );progn
                )
            )
            (progn
                (alert "ERROR: Can't continue any more\n, Please check the Survey OBjects input file [[Func.: 'ReadPointData 01']]")
            );progn
        );if
   )
   (setq listOfPointsOnLine (cdr listOfPointsOnLine)) ;Return
)
(defun DrawCulvert (listLabelObject FILEptr listObjConfigs listConfigVals strLyr / CULVERT01 CULVERT02 strLabel strCurLType strObjLType iNoOfPoints listOfPointsOnLine fRoadWidth fCulvLength fAngDeg)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun CULVERT01 (listPtCen fWidth fLength fAngleDeg strLyr / listPtCenTop listPtCenBot listPtTopLeft listPtTopRt listPtBotLeft listPtBotRt listPtTopLeftExt listPtTopRtExt listPtBotLeftExt listPtBotRtExt )
        (setq listPtCenTop (polar listPtCen (+ fAngleDeg (/ PI 2.0)) (/ fWidth 2.0))
              listPtCenBot (polar listPtCen (- fAngleDeg (/ PI 2.0)) (/ fWidth 2.0))
        )
        (setq listPtTopLeft (polar listPtCenTop (D2R (- fAngleDeg 180.0)) (/ fLength 2.0))
              listPtTopRt (polar listPtCenTop (D2R fAngleDeg) (/ fLength 2.0))
              listPtBotLeft (polar listPtCenBot (D2R (- fAngleDeg 180.0)) (/ fLength 2.0))
              listPtBotRt (polar listPtCenBot (D2R fAngleDeg) (/ fLength 2.0))
        )
        (setq listPtTopLeftExt (polar listPtTopLeft (D2R (+ fAngleDeg 150.0)) (* fWidth 0.4))
              listPtTopRtExt (polar listPtTopRt (D2R (+ fAngleDeg 30.0)) (* fWidth 0.4))
              listPtBotLeftExt (polar listPtBotLeft (D2R (- fAngleDeg 150.0)) (* fWidth 0.4))
              listPtBotRtExt (polar listPtBotRt (D2R (- fAngleDeg 30.0)) (* fWidth 0.4))
        )
        (POLYLINE (list listPtTopLeftExt listPtTopLeft listPtTopRt listPtTopRtExt) nil strLyr)
        (POLYLINE (list listPtBotLeftExt listPtBotLeft listPtBotRt listPtBotRtExt) nil strLyr)
    )
    (defun CULVERT02 (listOfPoints strLyr / listPtTopLeft listPtTopRt listPtBotLeft listPtBotRt fAngleDeg fWidth listPtTopLeftExt listPtTopRtExt listPtBotLeftExt listPtBotRtExt)
        (setq listPtTopLeft (nth 0 listOfPoints) listPtTopRt (nth 1 listOfPoints)
              listPtBotLeft (nth 2 listOfPoints) listPtBotRt (nth 3 listOfPoints)
        )
        (setq fAngleDeg (angle listPtTopLeft listPtTopRt) fWidth (distance listPtTopLeft listPtBotLeft))
        (setq listPtTopLeftExt (polar listPtTopLeft (D2R (+ fAngleDeg 150.0)) (* fWidth 0.4))
              listPtTopRtExt (polar listPtTopRt (D2R (+ fAngleDeg 30.0)) (* fWidth 0.4))
              listPtBotLeftExt (polar listPtBotLeft (D2R (- fAngleDeg 150.0)) (* fWidth 0.4))
              listPtBotRtExt (polar listPtBotRt (D2R (- fAngleDeg 30.0)) (* fWidth 0.4))
        )
        (POLYLINE (list listPtTopLeftExt listPtTopLeft listPtTopRt listPtTopRtExt) nil strLyr)
        (POLYLINE (list listPtBotLeftExt listPtBotLeft listPtBotRt listPtBotRtExt) nil strLyr)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq strLabel (strcase (car listLabelObject))
          strCurLType (getvar "celtype")
          strObjLType (CheckForValidLabel strLabel listObjConfigs)
    )
    (command "linetype" "s" strObjLType "")
    (if (= strLabel "CULVERT01")
        (progn
            (setq iNoOfPoints 1
                  listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr) ;;Read all the points
                  fRoadWidth (atof (nth 1 listLabelObject))
                  fCulvLength (atof (nth 2 listLabelObject))
                  fAngDeg (atof (nth 3 listLabelObject))
            )
            (CULVERT01 (car listOfPointsOnLine) fRoadWidth fCulvLength fAngDeg strLyr)
        );progn
        (progn
            (setq iNoOfPoints (atoi (cadr listLabelObject)))
            (if (/= iNoOfPoints 4) (alert (strcat "\nERROR: No. of points for CULVERT02 should be 4\nBut found '" (itoa iNoOfPoints) "'")))
            ;;Read all the points
            (setq listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr))
            (CULVERT02 listOfPointsOnLine strLyr)
        );progn
    )
    (command "linetype" "s" strCurLType "")
)
(defun DrawOpenRock (listLabelObject FILEptr listObjConfigs listConfigVals strLyr / OpenRockAnnotationOnLine strLabel iNoOfPoints strCurLType  strObjLType listOfPointsOnLine fRayHtMFwrtTxtSize fDivnLenMFwrtTxtSize fMaxRayHt fMinRayHt fDivnLen)
    (defun OpenRockAnnotationOnLine (listOfPointsOnLine fSegLength fMaxRayHt iNoOfDivn strLyr / GenOpenRockSymbol iLoopLim i listPtStart listPtEnd)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (defun GenOpenRockSymbol (listPtStart listPtEnd fSegLength fMaxRayHt iNoOfDivn strLyr / i fLength fAngle fMinRayHt fRayHtChange fCurRayHt listPtTemp listPtVert fDistTemp)
            (setq fLength (distance listPtStart listPtEnd)
                  fAngle (angle listPtStart listPtEnd)
                  fMinRayHt (* fMaxRayHt 0.25)
                  fRayHtChange (/ (- fMaxRayHt fMinRayHt) (/ iNoOfDivn 2.0))
            )
            (if (< fLength fSegLength) (princ "\nERROR: Length is too short to generate Rays in Open Rock Symbol"))
            
            (setq fCurRayHt fMaxRayHt i 1 fCumLength 0.0)
            (while (< fCumLength fLength)
                (setq listPtTemp (polar listPtStart fAngle fCumLength)
                      listPtVert (polar listPtTemp (+ fAngle (/ PI 2.0)) fCurRayHt)
                )
                (LINE listPtTemp listPtVert strLyr)
                (if (< i (/ iNoOfDivn 2))
                    (setq fCurRayHt (- fCurRayHt fRayHtChange))
                    (setq fCurRayHt (+ fCurRayHt fRayHtChange))
                )
                (setq fCumLength (+  fCumLength (/ fSegLength iNoOfDivn))
                      i (+ i 1)
                )
                (if (= i iNoOfDivn) (setq i 0))
            )
        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (setq i 1 iLoopLim (length listOfPointsOnLine))
        (while (< i iLoopLim)
            (setq listPtStart (nth (- i 1) listOfPointsOnLine)
                  listPtEnd (nth i listOfPointsOnLine)
                  i (+ i 1)
            )
            (GenOpenRockSymbol listPtStart listPtEnd fSegLength fMaxRayHt iNoOfDivn strLyr)
        )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq strLabel (strcase (car listLabelObject))
          iNoOfPoints (atoi (cadr listLabelObject))
          strCurLType (getvar "celtype")
          strObjLType (CheckForValidLabel strLabel listObjConfigs)
    )
    ;;Read all the points
    (setq listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
          ;;Make Close boundary
          listOfPointsOnLine (append listOfPointsOnLine (list (nth 0 listOfPointsOnLine)))
    )
    ;;Call The Draw routine below this>>>>>>>>>>>>>>>>>>>>>>>
    (command "linetype" "s" strObjLType "")
    (setq fRayHtMFwrtTxtSize 1.75 fSegLenMFwrtTxtSize 4.0
          fMaxRayHt (* fRayHtMFwrtTxtSize (nth 1 listConfigVals)) 
          fSegLength (* fSegLenMFwrtTxtSize (nth 1 listConfigVals))
          iNoOfDivn 6
    )
    (POLYLINE listOfPointsOnLine nil strLyr)
    (OpenRockAnnotationOnLine listOfPointsOnLine fSegLength fMaxRayHt iNoOfDivn strLyr)
    (command "linetype" "s" strCurLType "")
    ;;Call The Draw routine above this >>>>>>>>>>>>>>>>>>>>>>>
)
(defun DrawRainCut (listLabelObject FILEptr listObjConfigs listConfigVals strLyr / RainCutAnnotationOnLine strLabel iNoOfPoints strCurLType strObjLType listOfPointsOnLine fChWidMFwrtTxtSize fChSpacingMFwrtTxtSize fChHeightMFwrtTxtSize fChWid fChSpa fChHt fPathWid listRightVerts listLeftVerts bLeft)
    (defun RainCutAnnotationOnLine (bLeftSideChannel listOfPointsOnLine fChWid fChHt fChSpa strLyr / GenRainCutSymbol i iLoopLim listPtStart listPtEnd)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (defun GenRainCutSymbol (bLeftSideChannel listPtStart listPtEnd fChWid fChHt fChSpa strLyr / fLength fAngle fCumLength listPtChBaseStart listPtChBaseEnd listPtChBaseStartVert listPtChBaseEndVert)
            (setq fLength (distance listPtStart listPtEnd)
                  fAngle (angle listPtStart listPtEnd)
                  fCumLength 0.0
            )
            (if (= bLeftSideChannel T) (setq fAngRadTemp (+ fAngle (/ PI 2.0))) (setq fAngRadTemp (- fAngle (/ PI 2.0))))
            (if (< fLength fChWid) (princ "\nERROR: Length is too short to generate RAIN_CUT Symbol"))
            (setq fCumLength 0.0 listPtChBaseStart listPtStart listPtChBaseEnd (polar listPtChBaseStart fAngle fChWid))
            (while (< fCumLength fLength)
                (setq listPtChBaseStartVert (polar listPtChBaseStart fAngRadTemp fChHt)
                      listPtChBaseEndVert (polar listPtChBaseEnd fAngRadTemp fChHt)
                )
                (LINE listPtChBaseStartVert listPtChBaseStart strLyr)
                (LINE listPtChBaseStart listPtChBaseEnd strLyr)
                (LINE listPtChBaseEnd listPtChBaseEndVert strLyr)
                (setq
                      listPtChBaseStart (polar listPtChBaseEnd fAngle fChSpa)
                      listPtChBaseEnd (polar listPtChBaseStart fAngle fChWid)
                )
                (setq fCumLength (+ fCumLength fChWid fChSpa))
            )
        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (setq i 1 iLoopLim (length listOfPointsOnLine))
        (while (< i iLoopLim)
            (setq listPtStart (nth (- i 1) listOfPointsOnLine)
                  listPtEnd (nth i listOfPointsOnLine)
                  i (+ i 1)
            )
            (GenRainCutSymbol bLeftSideChannel listPtStart listPtEnd fChWid fChHt fChSpa strLyr)
        )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq strLabel (strcase (car listLabelObject))
          iNoOfPoints (atoi (cadr listLabelObject))
          strCurLType (getvar "celtype")
          strObjLType (CheckForValidLabel strLabel listObjConfigs)
          ;;Read all the points
          listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
    )
    ;;Call The Draw routine below this>>>>>>>>>>>>>>>>>>>>>>>
    (command "linetype" "s" strObjLType "")
    (setq fChWidMFwrtTxtSize 2.25 fChSpacingMFwrtTxtSize 1.0 fChHeightMFwrtTxtSize 1.0
          fChWid (* fChWidMFwrtTxtSize (nth 1 listConfigVals)) 
          fChSpa  (* fChSpacingMFwrtTxtSize (nth 1 listConfigVals))
          fChHt (* fChHeightMFwrtTxtSize (nth 1 listConfigVals))
          fPathWid (* 1.0 (nth 1 listConfigVals))
    )
    (setq listRightVerts (GenerateParaLine listOfPointsOnLine (- 0.0 (/ fPathWid 2.0)) 0 nil strLyr)
          listLeftVerts (GenerateParaLine listOfPointsOnLine (/ fPathWid 2.0) 0 nil strLyr)
    )
    (setq bLeft T)
    (RainCutAnnotationOnLine bLeft listLeftVerts fChWid fChHt fChSpa strLyr)
    (setq bLeft nil)
    (RainCutAnnotationOnLine bLeft listRightVerts fChWid fChHt fChSpa strLyr)
    (command "linetype" "s" strCurLType "")
    (princ)
)
(defun DrawEmbankment (listLabelObject FILEptr listObjConfigs listConfigVals strLyr / EmbankmentOnLine strLabel iNoOfPoints strCurLType  strObjLType listOfPointsOnLine fTriangleSizeMFwrtTxtSize fLineWidMFwrtTxtSize fTriangleDim fLineWidth)
    (defun EmbankmentOnLine (listOfPointsOnLine fTriangleDim fLineWidth strLyr / GenEmbankmentSymbol i iLoopLim listPtStart listPtEnd)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (defun GenEmbankmentSymbol (listPtStart listPtEnd fTriangleDim bStartFrZero bLeftOrRt strLyr / fLength fAngle fLengthCum listTglStart listTglVert listTglEnd entTemp)
            (setq fLength (distance listPtStart listPtEnd)
                  fAngle (angle listPtStart listPtEnd)
                  fLengthCum 0.0
            )
            (while (< (+ fLengthCum fTriangleDim) fLength)
                (if (and (= fLengthCum 0.0) (= bStartFrZero nil))
                    (setq fLengthCum (/ fTriangleDim 2.0) listTglStart (polar listPtStart fAngle (/ fTriangleDim 2.0)))
                    (setq listTglStart (polar listPtStart fAngle fLengthCum))
                )
                (setq listTglEnd (polar listTglStart fAngle fTriangleDim))
                (setq listTempPt (polar listTglStart fAngle (/ fTriangleDim 2.0)))
                (if (= bLeftOrRt T)
                    (setq listTglVert (polar listTempPt (+ fAngle (/ PI 2)) (/ fTriangleDim 2.0)))
                    (setq listTglVert (polar listTempPt (- fAngle (/ PI 2)) (/ fTriangleDim 2.0)))
                )
                ;(LINE listTglStart listTglVert strLyr) (LINE listTglEnd listTglVert strLyr)
                (setq entTemp (POLYLINE (list listTglStart listTglEnd  listTglVert) T strLyr))
                (SIMPLEHATCH entTemp (* fTriangleDim 0.3) (R2D (+ fAngle (/ PI 4.0))) strLyr)
            
                (setq fLengthCum (+ fLengthCum fTriangleDim))
            )
        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (setq listLeftVerts (GenerateParaLine listOfPointsOnLine (- 0.0 (/ fLineWidth 2.0)) 0 T strLyr)
              listRightVerts (GenerateParaLine listOfPointsOnLine (/ fLineWidth 2.0) 0 T strLyr)
        )
        ;;For Left Side ;;;;;;;;;;;;;;;;;;
        (setq i 1 iLoopLim (length listLeftVerts)  bLeftOrRt nil bStartFrZero nil)
        (while (< i iLoopLim)
            (setq listPtStart (nth (- i 1) listLeftVerts)
                  listPtEnd (nth i listLeftVerts)
                  i (+ i 1)
            )
            (GenEmbankmentSymbol listPtStart listPtEnd fTriangleDim bStartFrZero bLeftOrRt strLyr)
        )
        ;;For Rt Side ;;;;;;;;;;;;;;;;;;
        (setq i 1 iLoopLim (length listRightVerts) bLeftOrRt T bStartFrZero T)
        (while (< i iLoopLim)
            (setq listPtStart (nth (- i 1) listRightVerts)
                  listPtEnd (nth i listRightVerts)
                  i (+ i 1)
            )
            (GenEmbankmentSymbol listPtStart listPtEnd fTriangleDim bStartFrZero bLeftOrRt strLyr)
        )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq strLabel (strcase (car listLabelObject))
          iNoOfPoints (atoi (cadr listLabelObject))
          strCurLType (getvar "celtype")
          strObjLType (CheckForValidLabel strLabel listObjConfigs)
    )
    ;;Read all the points
    (setq listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr))

    ;;Call The Draw routine below this>>>>>>>>>>>>>>>>>>>>>>>
    (command "linetype" "s" strObjLType "")
    (setq fTriangleSizeMFwrtTxtSize 2.0 fLineWidMFwrtTxtSize 0.5
          fTriangleDim (* fTriangleSizeMFwrtTxtSize (nth 1 listConfigVals)) 
          fLineWidth (* fLineWidMFwrtTxtSize (nth 1 listConfigVals))
    )
    (EmbankmentOnLine listOfPointsOnLine fTriangleDim fLineWidth strLyr)
    (command "linetype" "s" strCurLType "")
    ;;Call The Draw routine above this >>>>>>>>>>>>>>>>>>>>>>>
)

;; Draws block only objects
(defun DrawBlockObjects(listLabelObject listObjConfigs listConfigVals strLyr / strLabel fBlkSizeMFwrtTxtSize strBlkName listInsPoint fScale)
    ;;for Block insertion Objs only
    (setq strLabel (strcase (car listLabelObject)) fBlkSizeMFwrtTxtSize 1.25)
    (if (or (= strLabel "SURVEYED_TREE") (= strLabel "NOT_IN_SITU_TREE") (= strLabel "PALM_TREE") (= strLabel "TUBE_WELL"))
        (progn
            (if (< (length listLabelObject) 3) (progn (alert (strcat "ERROR: Invalid data for '" strLabel "'!!!")) (EXIT)))
            (setq strBlkName (CheckForValidLabel strLabel listObjConfigs)
                  listInsPoint (list (atof (nth 2 listLabelObject)) (atof (nth 1 listLabelObject)))
            )
            (if (/= (BLOCK strBlkName listInsPoint (* fBlkSizeMFwrtTxtSize (nth 1 listConfigVals)) (* fBlkSizeMFwrtTxtSize (nth 1 listConfigVals)) 0.0 strLyr) 1)
                (alert (strcat "ERROR: Block '" strBlkName "' not found"))
            )
        )
        (progn
            (if (or (= strLabel "KANCHA_WELL") (= strLabel "PUCCA_WELL"))
                (progn
                    (if (< (length listLabelObject) 4) (progn (alert (strcat "ERROR: Invalid data for '" strLabel "'!!!")) (EXIT)))
                    (setq strBlkName (CheckForValidLabel strLabel listObjConfigs)
                          listInsPoint (list (atof (nth 2 listLabelObject)) (atof (nth 1 listLabelObject)))
                          fScale (atof (nth 1 listLabelObject))
                    )
                    (if (/= (BLOCK strBlkName listInsPoint fScale fScale 0.0 strLyr) 1)
                        (alert (strcat "ERROR: Block '" strBlkName "' not found"))
                    )
                );progn
                (alert "ERROR: In Abnormal Path Func.: 'DrawBlockObjects'")
            );if
        )
    )
)
(defun DrawSingleLineObjects(listLabelObject FILEptr listObjConfigs listConfigVals strLyr / strLabel strCurLType strObjLType iNoOfPoints listOfPointsOnLine)
    (setq strLabel (strcase (car listLabelObject)))
    (if (or (= strLabel "KANCHA_ROAD_SIDE") (= strLabel "HOUSE_BOUNDARY")
            (= strLabel "VILLEGE_BOUNDARY") (= strLabel "JUNGLE_LIMIT")
            (= strLabel "MOORUM_ROAD_SIDE") (= strLabel "FOOTPATH_SIDE")
            (= strLabel "NALLAH_SIDE")
        )
        (progn
            (setq strCurLType (getvar "celtype")
                  strObjLType (CheckForValidLabel strLabel listObjConfigs)
                  iNoOfPoints (atoi (cadr listLabelObject))
                  listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
            )
            (command "linetype" "s" strObjLType "")
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            (POLYLINE listOfPointsOnLine  nil strLyr)
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            (command "linetype" "s" strCurLType "")
        )
        (progn
            (alert "ERROR: In Abnormal Path Func.: 'DrawSingleLineObjects 01'")
        )
    )
)
(PrintLoadMsg 53)
(defun DrawElecOrTelphonePost (listLabelObject FILEptr listObjConfigs listConfigVals strLyr / strLabel strCurLType strObjLType fPostSizeMFwrtTxtSize iNoOfPoints i listOfPointsOnLine)
    (setq strLabel (strcase (car listLabelObject)))
    (if (or (= strLabel "TELEPHONE_POST") (= strLabel "ELEC_LIGHT_POST"))
        (progn
            (setq strCurLType (getvar "celtype")
                  strObjLType (CheckForValidLabel strLabel listObjConfigs)
                  iNoOfPoints (atoi (cadr listLabelObject))
                  listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
                  fPostSizeMFwrtTxtSize 0.5
            )
            (setq i 0)
            (while (< i iNoOfPoints)
                ;;Draw Posts >>>>>>>>>>>>>>>>>>>>>>>>>>
                (if (= strLabel "TELEPHONE_POST")
                    (progn
                        (CIRCLE (nth i listOfPointsOnLine) (* fPostSizeMFwrtTxtSize (nth 1 listConfigVals)) strLyr)
                        (TEXTMID (list (car (nth i listOfPointsOnLine)) (+ (cadr (nth i listOfPointsOnLine)) (/ (nth 1 listConfigVals) 2.0) (* fPostSizeMFwrtTxtSize (nth 1 listConfigVals)))) "T.P" 0.0 (nth 1 listConfigVals) strLyr)
                    )
                    (progn
                        (DONUT (nth i listOfPointsOnLine) 0.0 (* 2.0 fPostSizeMFwrtTxtSize (nth 1 listConfigVals)) strLyr)
                        (TEXTMID (list (car (nth i listOfPointsOnLine)) (+ (cadr (nth i listOfPointsOnLine)) (/ (nth 1 listConfigVals) 2.0) (* fPostSizeMFwrtTxtSize (nth 1 listConfigVals)))) "E.P" 0.0 (nth 1 listConfigVals) strLyr)
                    )
                )
                (setq i (+ i 1))
            )
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            (command "linetype" "s" strObjLType "")
            (POLYLINE listOfPointsOnLine  nil strLyr)
            (command "linetype" "s" strCurLType "")
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        )
        (progn
            (alert "ERROR: In Abnormal Path Func.: 'DrawElecOrTelphonePost 01'")
        )
    )
)
(defun DrawDoubleLineObjects(listLabelObject FILEptr listObjConfigs listConfigVals strLyr / strLabel strCurLType strObjLType iNoOfPoints listOfPointsOnLine fObjWidth)
    (setq strLabel (strcase (car listLabelObject)))
    (if (or (= strLabel "KANCHA_ROAD_CEN") (= strLabel "FOOTPATH_CEN")
            (= strLabel "NALLAH_CEN") (= strLabel "MOORUM_ROAD_CEN")
        )
        (progn
            (setq strCurLType (getvar "celtype")
                  strObjLType (CheckForValidLabel strLabel listObjConfigs)
                  iNoOfPoints (atoi (cadr listLabelObject))
                  listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
                  fObjWidth (atof (nth 2 listLabelObject))
            )
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            (command "linetype" "s" strObjLType "")
            (GenerateParaLine listOfPointsOnLine (/ fObjWidth 2.0) 0 T strLyr)
            (GenerateParaLine listOfPointsOnLine (- 0.0 (/ fObjWidth 2.0)) 0 T strLyr)
            (command "linetype" "s" strCurLType "")
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        )
        (progn
            (alert "ERROR: In Abnormal Path Func.: 'DrawDoubleLineObjects 02'")
        )
    )
)
(PrintLoadMsg 61)
(defun DrawPondOrHutmentOrPBuilding(listLabelObject FILEptr listObjConfigs listConfigVals strLyr / GENHUTMENTSYMBOL strLabel strCurLType strObjLType iNoOfPoints listOfPointsOnLine entNameTemp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun GenHutmentSymbol(listOfPoints strLyr / listPt01 listPt02 listPt03 listPt04 fSpaceMFwrtLen listPtTemp listPt05 listPt06)
        (setq listPt01 (nth 0 listOfPoints) listPt02 (nth 1 listOfPoints)
              listPt03 (nth 2 listOfPoints) listPt04 (nth 3 listOfPoints)
        )
        (setq fSpaceMFwrtLen 0.4)
        (setq listPtTemp (polar listPt01 (angle listPt01 listPt04) (/ (distance listPt01 listPt04) 2.0))
              listPt05 (polar listPtTemp (angle listPt01 listPt02) (* fSpaceMFwrtLen 0.5 (distance listPt01 listPt02)))
              listPt06 (polar listPt05 (angle listPt01 listPt02)   (- (distance listPt01 listPt02) (* fSpaceMFwrtLen (distance listPt01 listPt02))))
        )
        (POLYLINE (list listPt01 listPt02 listPt03 listPt04) T strLyr)
        (POLYLINE (list listPt01 listPt05 listPt04) nil strLyr)
        (POLYLINE (list listPt02 listPt06 listPt03) nil strLyr)
        (LINE listPt05 listPt06 strLyr)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq strLabel (strcase (car listLabelObject)))
    (if (or (= strLabel "POND") (= strLabel "HUTMENT") (= strLabel "PUCCA_BUILDING"))
        (progn
            (setq strCurLType (getvar "celtype")
                  strObjLType (CheckForValidLabel strLabel listObjConfigs)
                  iNoOfPoints (atoi (cadr listLabelObject))
                  listOfPointsOnLine (ReadPointData iNoOfPoints  FILEptr)
            )
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            (command "linetype" "s" strObjLType "")
            (setq entNameTemp (POLYLINE listOfPointsOnLine T strLyr))
            (if (= strLabel "HUTMENT")
               (progn ;;;For Hutment drawing
                  (if (/= (length listOfPointsOnLine) 4)
                    (alert "ERROR: Unable to Draw 'HUTMENT' symbol\nNumber of Points should be 4")
                    (GenHutmentSymbol listOfPointsOnLine strLyr)
                  )
               );progn
            )
            (if (= strLabel "PUCCA_BUILDING")
                (SIMPLEHATCH entNameTemp (* (nth 1 listConfigVals) 2.0) 45.0 strLyr)
            )
            (if (= strLabel "POND")
                (TEXTLEFT (car listOfPointsOnLine) "POND" 0.0 (* (nth 1 listConfigVals) 1.5) strLyr)
            )
            (command "linetype" "s" strCurLType "")
            ;;>>>>>>>>>>>Draw Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        )
        (progn
            (alert "ERROR: In Abnormal Path Func.: 'DrawPondOrHutmentOrPBuilding 01'")
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SURVGRID.LSP Start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlotGrid(listConfigVals strLyr / JoinPointsWithLine PlotXGrid AnnotateHorzGrid AnnotateVertGrid fOrgX fOrgY fWid fHt fGridSpaHor fGridSpaVer fCumOrgY bLoopFlag listHorzPts01 listHorzPts02 i listHorzPtsStart listHorzPtsEnd )
    (defun JoinPointsWithLine (listPts01 listPts02 strLyr / i iLoopLim)
        (if (or (= listPts01 nil) (= listPts02 nil)) (alert "Bad argument passed in Func 'JoinPointsWithLine'"))
        (if (/= (length listPts01) (length listPts02)) (alert "Bad argument passed in Func 'JoinPointsWithLine'"))
        (setq i 0 iLoopLim (length listPts01)) (while (< i iLoopLim) (LINE (nth i listPts01) (nth i listPts02) strLyr) (setq i (+ i 1)))
    )
    (defun PlotXGrid (fOrgX fOrgY fWid fGridSpaHor strLyr / fCumOrgX i bLoopFlag listReturn)
        (setq fCumOrgX fOrgX i 0 bLoopFlag T listReturn (list ()))
        (while (= bLoopFlag T)
            ;;Commented (if (> i 0) (LINE (list (- fCumOrgX fGridSpaHor) fOrgY) (list fCumOrgX fOrgY) strLyr))
            (setq listReturn (append listReturn (list (list fCumOrgX fOrgY))))
            (if (<= (- fWid (- fCumOrgX fOrgX)) fGridSpaHor)
                (progn
                    ;;Commented (LINE (list fCumOrgX fOrgY) (list (+ fOrgX fWid) fOrgY) strLyr)
                    (setq listReturn (append listReturn (list (list (+ fOrgX fWid) fOrgY))) bLoopFlag nil)
                )
            )
            (setq i (+ i 1) fCumOrgX (+ fOrgX (* i fGridSpaHor)))
        )
        (setq listReturn (cdr listReturn))
        (LINE (nth 0 listReturn) (last listReturn) strLyr)
        (setq listReturn listReturn);Return
    )
    (defun AnnotateHorzGrid (listPts listConfigVals strLyr bTopOrBottom / iLUPREC fLabelTextSize i iLoopLim fShiftTxt ptOnGrid ptStart strEasting)
        (setq iLUPREC (getvar "luprec")) (setvar "luprec" 0)
        (setq fLabelTextSize (* 1.5 (nth 1 listConfigVals)) fShiftTxt (* fLabelTextSize 0.7))
        (if (= bTopOrBottom nil) (setq fShiftTxt (- 0.0 fShiftTxt)))
        (setq i 0 iLoopLim (length listPts))
        (while (< i iLoopLim)
           (setq ptOnGrid (nth i listPts)
                 ptStart (list (+ (car ptOnGrid) (/ fLabelTextSize 2.0)) (+ (cadr ptOnGrid) fShiftTxt))
                 strEasting (strcat (rtos (car ptOnGrid)) "E")
           )
           (if (= bTopOrBottom nil)
               (TEXTRIGHT ptStart strEasting 90.0 fLabelTextSize strLyr)
               (TEXTLEFT ptStart strEasting 90.0 fLabelTextSize strLyr)
           )
           (setq i (+ i 1))
        )
        (setvar "luprec" iLUPREC)
    )
    (defun AnnotateVertGrid (listPts listConfigVals strLyr / iLUPREC fLabelTextSize fShiftTxt ptOnGrid ptStart strNorthing)
        (setq iLUPREC (getvar "luprec")) (setvar "luprec" 0)
        (setq fLabelTextSize (* 1.5 (nth 1 listConfigVals)) fShiftTxt (* fLabelTextSize 0.7))
        (setq strNorthing (strcat (rtos (cadr (nth 0 listPts))) "N"))

        (setq ptOnGrid (nth 0 listPts)
              ptStart (list (- (car ptOnGrid) fShiftTxt) (- (cadr ptOnGrid) (/ fLabelTextSize 2.0)))
        )
        (TEXTRIGHT ptStart strNorthing 0.0 fLabelTextSize strLyr)
        (setq ptOnGrid (last listPts)
              ptStart (list (+ (car ptOnGrid) fShiftTxt) (- (cadr ptOnGrid) (/ fLabelTextSize 2.0)))
        )
        (TEXTLEFT ptStart strNorthing 0.0 fLabelTextSize strLyr)
        (setvar "luprec" iLUPREC)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq fOrgX (nth 4 listConfigVals) fOrgY (nth 5 listConfigVals) fWid (nth 6 listConfigVals) fHt (nth 7 listConfigVals) fGridSpaHor (nth 8 listConfigVals) fGridSpaVer (nth 9 listConfigVals))
    ;;Set Drawing Setting i.e LTSCALE/LIMITS
    (setvar "LIMMIN" (list fOrgX fOrgY)) (setvar "LIMMAX" (list (+ fOrgX fWid) (+ fOrgY fHt))) (setvar "LTSCALE" (/ fHt 80.0))

    (setq fCumOrgY fOrgY i 0 bLoopFlag T listHorzPts01 nil listHorzPts02 nil)
    (while (= bLoopFlag T)
        (setq listHorzPts02 (PlotXGrid fOrgX fCumOrgY fWid fGridSpaHor strLyr))
        (AnnotateVertGrid listHorzPts02 listConfigVals strLyr)
        (if (= listHorzPts01 nil)
            (progn
                (AnnotateHorzGrid listHorzPts02 listConfigVals strLyr nil)
                (setq listHorzPtsStart listHorzPts02)
            )
        )
        ;Commented (if (and (/= listHorzPts01 nil) (/= listHorzPts02 nil))
        ;Commented     (princ)
        ;Commented     (JoinPointsWithLine listHorzPts01 listHorzPts02 strLyr)
        ;Commented )
        (setq listHorzPts01 listHorzPts02)
        (setq fCumOrgY (+ fCumOrgY fGridSpaVer))
        
        (if (>= fCumOrgY (+ fOrgY fHt))
            (progn
                (setq listHorzPts02 (PlotXGrid fOrgX (+ fOrgY fHt) fWid fGridSpaHor strLyr)
                      listHorzPtsEnd listHorzPts02
                )
                (AnnotateVertGrid listHorzPts02 listConfigVals strLyr)
                ;;Commented (JoinPointsWithLine listHorzPts01 listHorzPts02 strLyr)
                (setq bLoopFlag nil)
            )
        )
    )
    (JoinPointsWithLine listHorzPtsStart listHorzPtsEnd strLyr)
    (if (/= listHorzPts02 nil)
        (AnnotateHorzGrid listHorzPts02 listConfigVals strLyr T)
    )
)
(PrintLoadMsg 70)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SurvDCL.LSP Start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DCL Driver ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun GetSurvPlanParamFrDialog (strDCLFile / OnOk OnCancel OnFileDlgButton dcl_id strDwgName listDataTile listData retList)
   (defun OnOk(listDataTile listData / i iLoopLim iIndexInvalid)
        (setq listData (mapcar 'get_tile listDataTile));;Save Data
        (setq i 0 iLoopLim (- (length listData) 2) iIndexInvalid -1)
        (while (< i iLoopLim)
            (if (= (findfile (nth i listData)) nil)
                (setq iIndexInvalid i i (+ iLoopLim 100))
            )
            (setq i (+ i 1))
        )
        (if (= (tblsearch "BLOCK" (nth (- (length listData) 2) listData)) nil) (setq iIndexInvalid (- (length listData) 2)))
        (if (= iIndexInvalid -1) (done_dialog 0) (progn (mode_tile (nth iIndexInvalid listDataTile) 2) (set_tile "err" "ERROR: Invalid Data")))
        (setq listData listData);Return
   )
   (defun OnCancel(/ retList) (done_dialog 1) (setq retList nil))
   (defun OnFileDlgButton (iIndexTile listDataTile listData / strFname)
      (cond 
           ((= iIndexTile 0) (setq strLabel "Select Sheet Configuration File" strFiletype "DEF"))
           ((= iIndexTile 1) (setq strLabel "Select Object Configuration File" strFiletype "DAT"))
           ((= iIndexTile 2) (setq strLabel "Select Object Data File" strFiletype "SOB"))
           ((= iIndexTile 3) (setq strLabel "Select Spot Level Data File" strFiletype "SSL"))
      )
      (setq strFname (getfiled strLabel "" strFiletype 2))
      (if (/= strFname nil) (set_tile (nth iIndexTile listDataTile) strFname))
   )
   ;====================================================================================
   (setq dcl_id (load_dialog strDCLFile))
   (setq strDwgName (getvar "dwgname")
         listDataTile (list "ShConfFileNameEBox" "ObConfFileNameEBox" "ObDataFileNameEBox" "SpLevFileNameEBox" "PtMarkBlk" "SpotLevelDataIn2D")
         listData (list (strcat strDwgName ".DEF") (strcat strDwgName ".DAT") (strcat strDwgName ".SOB") (strcat strDwgName ".SSL") "Point" "1")
         ;;Debug Testing only
		 ;;listData (list "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.DEF" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.DAT" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.SOB" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.SSL" "Point" "1")
         ;;Dialog Debug>>listData (list "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.DEF" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.DAT" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.SOB" "C:\\\Development\\FieldSurvey\\Survey\\Dist\\Blank.SSL" "Point" "1")
   )
   (new_dialog "GetSurvPlanData" dcl_id)
   ;; Initialize data in tiles
   (mapcar 'set_tile listDataTile listData)

   (action_tile "accept" "(setq retList (OnOk listDataTile listData))")
   (action_tile "cancel" "(setq retList (OnCancel))")
   (action_tile "ShConfFileNameButt" "(OnFileDlgButton 0 listDataTile listData)")
   (action_tile "ObConfFileNameButt" "(OnFileDlgButton 1 listDataTile listData)")
   (action_tile "ObDataFileNameButt" "(OnFileDlgButton 2 listDataTile listData)")
   (action_tile "SpLevDataFileNameButt" "(OnFileDlgButton 3 listDataTile listData)")
   (start_dialog)
   (unload_dialog dcl_id)
   (setq retList retList);Return List;;NB: the flag to decide spot level data 2d/3d kept as string
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(PrintLoadMsg 76)

(defun C:SurveyPlan (/ *ERROR* *ERR_OLD* strGridConfigFileName strObjConfigFileName strSpotLevFileName strObjFileName strMarkBlkName strLyrGridNPts strLyrObjects listConfigVals iCmdecho iLuprec listUserData b2DDataFile )
  ;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;(setq *ERR_OLD* *ERROR*)
  ;;(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq listUserData (GetSurvPlanParamFrDialog (strcat gstrSrvPlanPath "SURVPLAN")))
  (if (/= listUserData nil)
      (progn
          (princ "\nPlease wait...Setting up drawing environment")
          (setq iCmdecho (getvar "cmdecho") iLuprec (getvar "luprec"))
          (setvar "cmdecho" 0)
          (setq strGridConfigFileName (nth 0 listUserData)
                strObjConfigFileName (nth 1 listUserData)
                strObjFileName (nth 2 listUserData)
                strSpotLevFileName (nth 3 listUserData)
                strMarkBlkName (nth 4 listUserData)
				b2DDataFile (atoi (nth 5 listUserData))
                strLyrGridNPts "SURVEY-GRID-POINTS"
                strLyrObjects "SURVEY-OBJECTS"
          )
          (CreateLayer (list strLyrGridNPts strLyrObjects))
  
          (setq listConfigVals (ReadSurveyConfig strGridConfigFileName))

          (PlotGrid listConfigVals strLyrGridNPts)
          (setvar "luprec" 3)
          (PlotSpotLevels strSpotLevFileName b2DDataFile listConfigVals strMarkBlkName strLyrGridNPts)
          (setvar "luprec" 0)
          (DrawAllObjects strObjFileName strObjConfigFileName listConfigVals strLyrObjects)

          (setvar "luprec" iLuprec)(setvar "cmdecho" iCmdecho)
          (princ "\nDone !")
      );progn
  )
  ;;(setq *ERROR* *ERR_OLD*)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Plot Related Funcs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlotSpotLevels(strSrcFileName b2DDataFile listConfigVals strMarkBlkName strLyr / FILEptr strLine listTemp i)
    (if (= (findfile strSrcFileName) nil) (alert (strcat "ERROR: File '" strSrcFileName "' not found!")))
    (if (= (setq FILEptr (open strSrcFileName "r")) nil) (alert (strcat "ERROR: Opening File '" strSrcFileName "'")))
    (princ "\nPlease wait...Drawing Spot Levels\n")
    (setq strLine "Initialization" i 1)
    (while (not (= strLine nil))
        (setq strLine (read-line FILEptr))
        (if (not (= strLine nil))
            (progn
                (if (and (/= strLine "") (= (IsCommentLine strLine "*") nil))
                    (progn
                        (setq listTemp (WordListToRealList (XtractAllWords strLine)))
                        ;;Insert Drawing Routine Here <<<<<<<<<<<<<<<<<<<<<<<<<<<
                        (if (= b2DDataFile 1)
							(progn
								;Set Insertion point as 2D..elevation as Zero
								(DrawSpotLevel (nth 2 listTemp) (list (nth 1 listTemp) (nth 0 listTemp) 0.0) (nth 1 listConfigVals) (D2R (nth 2 listConfigVals)) strMarkBlkName (nth 3 listConfigVals) strLyr)
							)
							(progn
								;Set Insertion point as 3D
								(DrawSpotLevel (nth 2 listTemp) (list (nth 1 listTemp) (nth 0 listTemp) (nth 2 listTemp)) (nth 1 listConfigVals) (D2R (nth 2 listConfigVals)) strMarkBlkName (nth 3 listConfigVals) strLyr)
							)
						)
                        (princ (strcat "\rDrawing Spot Level #" (itoa i)))
                        (setq i (+ i 1))
                        ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    );progn
                );
            )
        )
    )
    (close FILEptr)
)
(PrintLoadMsg 83)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Configuration Related Funcs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ReadSurveyConfig(strSrcFileName / FILEptr strLine listTemp i iLoopLim listDwgSettings)
    (if (= (findfile strSrcFileName) nil) (alert (strcat "ERROR: File '" strSrcFileName "' not found!")))
    (if (= (setq FILEptr (open strSrcFileName "r")) nil) (alert (strcat "ERROR: Opening File '" strSrcFileName "'")))
    ;;;;list>> <<fDwgScale fTextSize fTextAngle fMarkerSize fSurvOrgLeftX fSurvOrgLeftY fSurvWidth fSurvHeight fGridSpacingH fGridSpacingV>>;;;;;;;;;;;;;;
    (setq listDwgSettings (InitializeList 10 nil)  strLine T)
    (while (not (= strLine nil))
        (setq strLine (read-line FILEptr))
        (if (not (= strLine nil))
            (progn
                (if (and (/= strLine "") (= (IsCommentLine strLine "*") nil))
                    (progn
                        (setq listTemp (XtractAllWords strLine))
                        ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                        ;;Xtract Settings here <<<<<<<<<<<
                        (if (= (CheckSurveyConfigLine listTemp) T)
                            (progn
                                (setq listDwgSettings (ExtractConfigVals listTemp listDwgSettings))
                            );progn
                        )
                        ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                    );progn
                );
            )
        )
    )
    ;;Check wheather all Vals are present or not
    (setq i 0 iLoopLim (length listDwgSettings))
    (while (< i iLoopLim)
        (if (= (nth i listDwgSettings) nil)
            (progn
                (setq i (+ iLoopLim 100) listDwgSettings nil) (alert "Please Check the Configuration file, invalid data found!!")
            )
        )
        (setq i (+ i 1))
    )
    (if (= i iLoopLim) (setq listDwgSettings (ModifyConfigVals listDwgSettings)))
    (close FILEptr)
    (setq listDwgSettings listDwgSettings);;// Return Value
)
(PrintLoadMsg 88)
(defun CheckSurveyConfigLine(listOfWords / strTemp bRet)
    (setq bRet T)
    (if (> (length listOfWords) 2) (progn (setq bRet nil) (alert "Invalid entry found in Configuration file")))
    (setq strTemp (strcase (car listOfWords)))
    (if (= bRet T)
        (progn
            (if (and (/= strTemp "DWGSCALE")
                     (/= strTemp "TEXTSIZE")
                     (/= strTemp "TEXTANGLE")
                     (/= strTemp "MARKERSIZE")
                     (/= strTemp "SURVORGLEFTX")
                     (/= strTemp "SURVORGLEFTY")
                     (/= strTemp "SURVWIDTH")
                     (/= strTemp "SURVHEIGHT")
                     (/= strTemp "GRIDSPACINGH")
                     (/= strTemp "GRIDSPACINGV")
                )
                (progn (setq bRet nil) (alert (strcat "Invalid entry '" (car listOfWords) "' found in Configuration file")))
            )
        );progn
    );if
    (setq bRet bRet);;Return Value
)
(defun ExtractConfigVals(listOfWords listVars / strTemp)
    (setq strTemp (strcase (car listOfWords)))
    (cond
        ((= strTemp "DWGSCALE") (setq iIndex 0))
        ((= strTemp "TEXTSIZE") (setq iIndex 1))
        ((= strTemp "TEXTANGLE") (setq iIndex 2))
        ((= strTemp "MARKERSIZE") (setq iIndex 3))
        ((= strTemp "SURVORGLEFTX") (setq iIndex 4))
        ((= strTemp "SURVORGLEFTY") (setq iIndex 5))
        ((= strTemp "SURVWIDTH") (setq iIndex 6))
        ((= strTemp "SURVHEIGHT") (setq iIndex 7))
        ((= strTemp "GRIDSPACINGH") (setq iIndex 8))
        ((= strTemp "GRIDSPACINGV") (setq iIndex 9))
    )
    ;;Return Value
    (if (> iIndex 3)
        (setq listVars (SubstValInList listVars iIndex (atof (cadr listOfWords))) listVars listVars)
        (setq listVars (SubstValInList listVars iIndex (atof (cadr listOfWords))) listVars listVars)
    )
)
(PrintLoadMsg 95)
(defun ModifyConfigVals(listVars / strTemp)
    (if (and (= (nth 0 listVars) 0.0) (or (= (nth 1 listVars) 0.0) (= (nth 3 listVars) 0.0)))
        (progn
            (if (= (nth 1 listVars) 0.0) (alert "ERROR: Both 'DWGSCALE' and 'TEXTSIZE' found 0.0"))
            (if (= (nth 3 listVars) 0.0) (alert "ERROR: Both 'DWGSCALE' and 'MARKERSIZE' found 0.0"))
        )
        (progn
            (if (= (nth 1 listVars) 0.0)
                (setq listVars (SubstValInList listVars 1 (* 2.0 (nth 0 listVars))))
            )
            (if (= (nth 3 listVars) 0.0)
                (setq listVars (SubstValInList listVars 3 (* 1.0 (nth 0 listVars))))
            )
        )
    )
    (setq listVars listVars) ; Return Value
)
(defun IsLayerReadOnly(strLyr / listLyrDet retFlag)
    (setq listLyrDet (tblsearch "LAYER" strLyr) retFlag nil)
    (if (= nil listLyrDet)
        (progn (setq retFlag nil) (princ (strcat "\nERROR: Layer '" strLyr "' not found")))
        (progn (setq retFlag T))
    )
    (setq retFlag retFlag);Return
)
(PrintLoadMsg 99)
(defun ChangeCurrentLayer(strLyrNew / strLyrCur iCMDECHO)
    (setq strLyrCur (getvar "clayer"))
    (if (and (= (IsLayerReadOnly strLyrNew) T) (/= (strcase (getvar "clayer")) (strcase strLyrNew)))
        (progn
            (setq iCMDECHO (getvar "cmdecho"))
            (setvar "cmdecho" 0) (command "layer" "s" strLyrNew "") (setvar "cmdecho" iCMDECHO)
        );progn
    )
    (setq strLyrCur strLyrCur);Return
)
(PrintLoadMsg 100)
