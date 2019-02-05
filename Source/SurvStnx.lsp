;;
;;For Special kind of Stn Data having Northing Easting Values instead of Offsets
;;
(defun PlotStationsEx(strPath / *ERR_OLD* listParam listPIParam listDwgOrg)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(setq *ERR_OLD* *ERROR*)
	;(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listParam (GetStnDetailData strPath 1))
	(if(/= listParam nil)
		(progn
			(setq listPIParam (ReadPIDFile (nth 1 listParam)))
			(setq listDwgOrg (list 0.0 0.0 0.0))
			(DrawStationEx listDwgOrg listParam listPIParam)
		)
	)
	;(setq *ERROR* *ERR_OLD*)
	(princ)
)
;;
;;GUI to take Data
;;
(defun GetStnDetailData(strPath bIsSplOffElevData / OnOk retValue dcl_id)
   (defun OnOk (/ strFileStn strFilePI strText strBlkName fLabTxtSz fNumTxtSz retValue bError strOutDataFile)
	 (setq strFileStn (get_tile (nth 0 listDataKey))
		   strFilePI (get_tile (nth 1 listDataKey))
		   bError 0
	 )
     (if (or (= strFileStn nil) (= (strlen strFileStn) 0) (= (findfile strFileStn) nil)) ;;"strFile"
    	 (progn (set_tile "err" "ERROR: Invalid Data file") (mode_tile (nth 0 listDataKey) 2) (setq bError 1))
     )
	 (if (and (or (= strFilePI nil) (= (strlen strFilePI) 0) (= (findfile strFilePI) nil)) (= bError 0))
    	 (progn (set_tile "err" "ERROR: Invalid Data file") (mode_tile (nth 1 listDataKey) 2) (setq bError 1))
	 )
	 ;;NumAnnTxtSizeEB
	 (setq strText (get_tile (nth 2 listDataKey)) fNumTxtSz (atof strText))
	 (if (and (<= fNumTxtSz 0.0) (= bError 0))
    	 (progn (set_tile "err" "ERROR: Invalid Text Size") (mode_tile (nth 2 listDataKey) 2) (setq bError 1))
	 )
	 ;;LabAnnTxtSizeEB
	 (setq strText (get_tile (nth 3 listDataKey)) fLabTxtSz (atof strText))
	 (if (and (<= fLabTxtSz 0.0) (= bError 0))
    	 (progn (set_tile "err" "ERROR: Invalid Text Size") (mode_tile (nth 3 listDataKey) 2) (setq bError 1))
	 )
	 ;;"LabTxtEB"
	 (setq strText (get_tile (nth 4 listDataKey)))
	 ;;BlkNameEB
	 (setq strBlkName (get_tile (nth 5 listDataKey)))
	 (if (and (= (tblsearch "BLOCK" strBlkName) nil) (= bError 0))
    	 (progn (set_tile "err" "ERROR: Block not present in current dwg") (mode_tile (nth 5 listDataKey) 2) (setq bError 1))
	 )
	 ;;"OutDatFileNameEBox"
	 (setq strOutDataFile (get_tile (nth 6 listDataKey)))
	 (if (<= (strlen strOutDataFile) 0)
		(setq strOutDataFile nil)
	 )
	 (if (= bError 0)
		(progn (done_dialog 0) (setq retValue (list strFileStn strFilePI fNumTxtSz fLabTxtSz strText strBlkName strOutDataFile)))
		(setq retValue nil)
	 )
	 (setq retValue retValue);Return Statement
   )
   (defun OnFileDlgButton (strFiletype strTileFileEBox iOpenMode / strLabel strFname)
      (setq strLabel "Select Data File"
		    strFname (getfiled strLabel "" strFiletype iOpenMode)
	  )
      (if (/= strFname nil) (set_tile strTileFileEBox strFname))
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq dcl_id (load_dialog (strcat strPath "SurvPlan")))
   (setq ;listData (list "K:\\NEZ1.stn" "K:\\NEZ1.pid" "2" "3" "Station Detail" "POINT" "")
		 ;listData (list "K:\\Data\\Station\\NEZ.stn" "K:\\Data\\Station\\NEZ.pid" "2" "3" "Station Detail" "POINT" "")
		 listData (list "" "" "2" "3" "Station Detail" "POINT" "")
		 listDataKey (list "StnDatFileEBox" "PIDatFileNameEBox" "NumAnnTxtSizeEB" "LabAnnTxtSizeEB" "LabTxtEB" "BlkNameEB" "OutDatFileNameEBox")
   )
   (new_dialog "GetStnDetailData" dcl_id)
   ;;Init Controls
   (mapcar 'set_tile listDataKey listData)
   (if (= bIsSplOffElevData 1)
	(progn
		;Disable Dump file name Input ctrls
		(mode_tile (last listDataKey) 1)
		(mode_tile "OutDatFileNameButt" 1)
	)
   )
   ;;Map Handlers
   (action_tile "accept" "(setq retValue (OnOk))")
   (action_tile "cancel" "(progn (setq retValue nil)(done_dialog 1))")
   (action_tile "StnDatFileNameButt" "(OnFileDlgButton \"STN\" (nth 0 listDataKey) 2)")
   (action_tile "PIDatFileNameButt" "(OnFileDlgButton \"PID\" (nth 1 listDataKey) 2)")
   (action_tile "OutDatFileNameButt" "(OnFileDlgButton \"TXT\" (nth 6 listDataKey) 1)")
   (start_dialog)
   (unload_dialog dcl_id)
   (setq retValue retValue);;Return
)
;;
;;Reads Next Valid Survey Line
;;
(defun ReadNextValidLine(hSrcFile / GetFirstWord IsCommentLine XtractAllWords bReadStatus listData strLine)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	(defun IsCommentLine (strLine chComment / bRet)
		(setq strTemp (GetFirstWord strLine))
		(if (or (= strTemp "") (= (substr strTemp 1 1) chComment)) (setq bRet T) (setq bRet nil))
		(setq bRet bRet)
	)
	;;From a Line Data extracts All Words of a Line &
	;;Returns the Words in a list
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
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq strLine (read-line hSrcFile) bReadStatus 1)
	(while (= bReadStatus 1)
		(if (/= strLine nil)
			(progn
				(if (= (IsCommentLine strLine "*") nil)
					(progn
						(setq listData (XtractAllWords strLine)
							  bReadStatus 2  ;;Stop Reading
						)
					)
					(progn
						(setq strLine (read-line hSrcFile)
							  bReadStatus 1
						);;Continue Read
					)
				)
			)
			(setq bReadStatus 0);;File Completely read
		)
	)
	(if (= bReadStatus 0) (setq listData nil));;File Completely read
	(setq listData listData);; Return
)
(defun ConvertStrListToNumList(listParam bIsInt fMultFac / i iLoopLim retList)
	(setq i 0 iLoopLim (length listParam) retList (list ()))
	(while (< i iLoopLim)
		(if (= bIsInt 1)
			(setq retList (append retList (list (* fMultFac (atoi (nth i listParam))))))
			(setq retList (append retList (list (* fMultFac (atof (nth i listParam))))))
		)
		(setq i (+ i 1))
	)
	(setq retList (cdr retList));Return
)
(defun ReadPIDFile (strDataFile / i hsrcFile listOfWords listToRet)
	(setq i 0 hsrcFile (open strDataFile "r") listToRet (list ()))
	(if (/= hsrcFile nil)
		(progn
			(princ "\nReading Point Of Intersection data file\n")
			(setq listOfWords 0)
			(while (/= listOfWords nil)
				(setq listOfWords (ReadNextValidLine hSrcFile))
				(if (/= listOfWords nil)
					(setq listOfWords (ConvertStrListToNumList listOfWords 0 1.0)
					      listToRet (append listToRet (list listOfWords))
					)
				)
				(princ (strcat "\r " (itoa i) " No. of Lines read"))
				(setq i (+ i 1))
			)
			(close hsrcFile)
			(princ (strcat "\rTotal " (itoa i) " No. of lines read...Done\n"))
		)
		(progn
			(alert "ERROR: opening Point Of Intersection data file !!")
		)
	)
	(setq listToRet (cdr listToRet));;Return
)
;;
;;Read Next Station Data 
;;NB:This func.will work for both Normal & Special kind of Station Data
;;
(defun ReadNextStnData (hsrcFile / i hsrcFile listOfWords listToRet bContinueRead)
	(setq i 0 listOfWords 0 bContinueRead 1 listToRet (list ()))
	
	(while (and (= bContinueRead 1) (/= listOfWords nil))
		(setq listOfWords (ReadNextValidLine hSrcFile))
		(if (/= listOfWords nil)
			(progn
				(if (= i 0)
					(progn ;;Chk Start Stn Data mark
						(if (or (< (length listOfWords) 2) (/= (strcase (nth 0  listOfWords)) "START") (/= (strcase (nth 1 listOfWords)) "CHAINAGE"))
							(progn (alert "FATAL ERROR: Starting data segment for station invalid!!") (exit))
							(setq listToRet (list (list -1)));;No Chainage available in data file..so make it -1
						)
					)
					(progn ;;Chk Other Stn Data mark
						(if (< (length listOfWords) 2)
							(progn (alert "FATAL ERROR: Northing Easting data invalid!!") (exit))
							(progn
								;;Chk whether EO Stn reached or not
								(if (and (= (strcase (nth 0  listOfWords)) "END") (= (strcase (nth 1 listOfWords)) "CHAINAGE"))
									(setq bContinueRead 0)
									(setq listOfWords (ConvertStrListToNumList listOfWords 0 1.0)
										  listToRet (append listToRet (list listOfWords))
									)
								)
							)
						)
					)
				)
				(setq i (+ i 1))
			)
		)
	)
	(if (= listOfWords nil)
		(setq listToRet nil);;Return
		(setq listToRet listToRet);;Return
	)
	(setq listToRet listToRet);;Return
)
;; General utility file for Drawing
;;
(defun D2R (fAng) (* fAng (/ pi 180.0)))
(defun R2D (fAng) (* (/ 180.0  pi) fAng))
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
;;
;;For Special kind of Stn Data having Northing Easting Values instead of Offsets
;;
(defun DrawStationEx (listDwgOrg listGUIParam listPIParam / TransformPtWrtUsersOrg GenCumChainages DrawAllOffsets FindStnDataEndPoints FindDirection listStnData hsrcFile listCumChainages iIndex i listStnPoint fTxtSize fAngle strBlk dMarkSize strLyr listStnPointPrev)
	(defun TransformPtWrtUsersOrg (listDwgOrg listPtParam / fAng fDist listAbsOrg)
		(setq listAbsOrg (list 0.0 0.0 0.0)
			  fAng (angle listAbsOrg listPtParam)
			  fDist (distance listAbsOrg listPtParam)
			  listPtParam (polar listDwgOrg fAng fDist)
		)
		(setq listPtParam listPtParam);Return
	)
	(defun GenCumChainages (listPIParam fStartChainage / i listRet fCumDist)
		(setq i 1 fCumDist fStartChainage listRet (list fCumDist))
		(while (< i (length listPIParam))
			(setq fCumDist (+ fCumDist (distance (reverse (nth (- i 1) listPIParam)) (reverse (nth i listPIParam))))
				  i (+ i 1)
				  listRet (append listRet (list fCumDist))
			)
		)
		(setq listRet listRet);;Return
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
	(defun DrawAllOffsets (listStnData fAngle fTxtSize  strBlk dMarkSize strLyr / listNEasting listSpLevelPt i iLim)
		(setq i 0 iLim (length listStnData))
		(while (< i iLim)
			;;Data listNEasting -->> <Nothing><Easing><Elev>
			(setq listSpLevelPt (list (cadr (nth i listStnData)) (car (nth i listStnData)))
				  listSpLevelPt (append listSpLevelPt (list (cadr (cdr (nth i listStnData)))))
			)
			(DrawSpotLevel (cadr (cdr (nth i listStnData))) listSpLevelPt fTxtSize fAngle strBlk dMarkSize strLyr)
			(setq i (+ i 1))
		)
	)
	(defun FindStnDataEndPoints(listStnData / i iLim iIndex listOfIndexPairs fMaxDist listToRet)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(defun NEZtoEN(listNEZ) (cdr (reverse listNEZ)))
		(defun GetIndexPairs(listStnData / i iLim listOfIndexPairs)
			(defun GetMostDistantPtIndex(iIndex listStnData / i iLim ptBase iIndexToFind fMaxDist)
				(setq i 0 iLim (length listStnData) ptBase (NEZtoEN (nth iIndex listStnData)) iIndexToFind -1 fMaxDist -1.0)
				(while (< i iLim)
					(if (/= iIndex i)
						(progn
							(if (> (distance ptBase (NEZtoEN (nth i listStnData))) fMaxDist)
								(setq fMaxDist (distance ptBase (NEZtoEN (nth i listStnData)))
									  iIndexToFind i
								)
							)
						)
					)
					(setq i (+ i 1))
				)
				(setq iIndexToFind iIndexToFind);;Return
			)
			(setq i 0 iLim (length listStnData) listOfIndexPairs (list nil))
			(while (< i iLim)
				(setq listOfIndexPairs (append listOfIndexPairs (list (list i (GetMostDistantPtIndex i listStnData))))
					  i (+ i 1)
				)
			)
			(setq listOfIndexPairs (cdr listOfIndexPairs));;Remove first nil item..& Return
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq listOfIndexPairs (GetIndexPairs listStnData))
		(setq i 0 iLim (length listOfIndexPairs) fMaxDist -1.0 iIndex -1)
		(while (< i iLim)
			(setq listPt01 (NEZtoEN (nth (car (nth i listOfIndexPairs)) listStnData))
				  listPt02 (NEZtoEN (nth (cadr (nth i listOfIndexPairs)) listStnData))
			)
			(if (> (distance listPt01 listPt02) fMaxDist)
				(setq fMaxDist (distance listPt01 listPt02) iIndex i)
			)
			(setq i (+ i 1))
		)
		(setq listPt01 (NEZtoEN (nth (car (nth iIndex listOfIndexPairs)) listStnData))
			  listPt02 (NEZtoEN (nth (cadr (nth iIndex listOfIndexPairs)) listStnData))
		)
		(setq listToRet (list listPt01 listPt02));;Return
	)
	(defun FindDirection(listStnData listPIParam / i iLim iIndex listTmp listLeftMostPt listRtMostPt listIntersPt fAngToRet)
		(setq listTmp (FindStnDataEndPoints listStnData)
			  listLeftMostPt (car listTmp)
			  listRtMostPt (cadr listTmp)
		)
		(setq fAngToRet nil i 1 iLim (length listPIParam) iIndex -1)
		(while (< i iLim)
			(setq listIntersPt (inters listLeftMostPt listRtMostPt (reverse (nth (- i 1) listPIParam)) (reverse (nth i listPIParam))))
			(if (/= listIntersPt nil)
				(setq iIndex i i (+ iLim 1));;Get the index of PI & break the loop
				(setq i (+ i 1))
			)
		)
		(if (= iIndex nil)
			(progn
				(alert "ERROR: Invalid Station data found\nSkipping Drawing.....")
				(setq fAngToRet nil)
			)
			(progn
				(cond
					((> iIndex 0)
					 (setq fAngToRet (angle (reverse (nth (- iIndex 1) listPIParam)) (reverse (nth iIndex listPIParam))))
					)
					((= iIndex 0)
					 (setq fAngToRet (angle (reverse (nth iIndex listPIParam)) (reverse (nth (+ iIndex 1) listPIParam))))
					)
				)
			)
		)
		(setq fAngToRet fAngToRet);;Return
	)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq fTxtSize (nth 2 listGUIParam) strBlk (nth 5 listGUIParam) dMarkSize (/ fTxtSize 5.0) strLyr "SURV_STN_PLOT")
	(princ "\nPlease wait....Drawing stations...\n")

	;;Join all PIs 
	(setq i 0 listStnPoint nil listStnPointPrev nil)
	(while (< i (length listPIParam))
		(setq listStnPoint (reverse (nth i listPIParam)))
		(if(and (/= listStnPointPrev nil) (/= listStnPoint nil))
			(LINE (TransformPtWrtUsersOrg listDwgOrg listStnPoint) (TransformPtWrtUsersOrg listDwgOrg listStnPointPrev) strLyr)
		)
		(setq listStnPointPrev listStnPoint i (+ i 1))
	)
	(setq listStnData T hsrcFile (open (nth 0 listGUIParam) "r") i 0 listStnPoint nil)
	(while (and (/= listStnData nil) (/= hsrcFile nil))
		(setq listStnData (ReadNextStnData hsrcFile)) 
		(if (/= listStnData nil)
			(progn
				(setq i (+ i 1))
				(setq fAngle (FindDirection (cdr listStnData) listPIParam))
				(princ (strcat "\rAnnotating station #" (itoa i)))
				;;Start DWG routine calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				(if (/= fAngle nil)
					(DrawAllOffsets (cdr listStnData) fAngle fTxtSize strBlk dMarkSize strLyr)
				)
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			)
		)
	)
	(if (/= hsrcFile nil) (close hsrcFile))
	(princ)
)
(defun C:EA () (command ".Erase" "all" "")(princ))
(princ)