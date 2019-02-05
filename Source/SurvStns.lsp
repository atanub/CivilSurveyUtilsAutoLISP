;;
;;Main EXEc Routine
;;
(defun PlotStations(strPath / *ERR_OLD* listParam listPIParam listDwgOrg)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(setq *ERR_OLD* *ERROR*)
	;(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listParam (GetStnDetailData strPath 0))
	(if(/= listParam nil)
		(progn
			(setq listPIParam (ReadPIDFile (nth 1 listParam)))
			(setq listDwgOrg (list 0.0 0.0 0.0))
			(DrawStation listDwgOrg listParam listPIParam)
		)
	)
	(setq *ERROR* *ERR_OLD*)
	(princ)
)
(defun DrawStation (listDwgOrg listGUIParam listPIParam / TransformPtWrtUsersOrg GenCumChainages FindSegmentIndex FindAbsCoOrd DrawAllOffsets listStnData hsrcFile fChainage listCumChainages iIndex i listStnPoint listCenPt fTxtSize fAngle strBlk dMarkSize strLyr listStnPointPrev hSrcFileToDump)
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
	(defun FindSegmentIndex (fChainage listCumChainages / i iIndex)
		(if (> fChainage (last listCumChainages))
			(progn
				(alert (strcat "FATAL ERROR: Invalid station at Ch:" (rtos fChainage) " specified to draw!!"))
				(exit)
			)
		)
		(setq i 0 iIndex 0)
		(while (< i (length listCumChainages))
			(if (>= (nth i listCumChainages) fChainage)
				(setq iIndex (- i 1) i (length listCumChainages))
			)
			(setq i (+ i 1))
		)
		(if (< iIndex 0) (setq iIndex 0))
		(setq iIndex iIndex);;Return
	)
	(defun FindAbsCoOrd(fChainage listPIParam listCumChainages / iIndex fBase fAngle fDist listRet)
		(setq iIndex (FindSegmentIndex fChainage listCumChainages))
		(setq fBase (nth iIndex listCumChainages)
			  fAngle (angle (reverse (nth iIndex listPIParam)) (reverse (nth (+ iIndex 1) listPIParam)))
			  fDist (- fChainage fBase)
			  listRet (polar (reverse (nth iIndex listPIParam)) fAngle fDist)
		)
		(setq listRet (list fAngle listRet));Return
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
	(defun DrawAllOffsets (hSrcFileToDump listDwgOrg listCenPt listStnData fAngle fTxtSize  strBlk dMarkSize strLyr / listOffElev listSpLevelPt i iLim)
		(setq listCenPoint (TransformPtWrtUsersOrg listDwgOrg listCenPt) listOffElev (cdr listStnData) i 0 iLim (length listOffElev))
		(if (/= hSrcFileToDump nil)
			(progn
				(princ "\rWriting Northing-Easting Data to file")
				(princ (strcat "*Detail at Chainage : " (rtos (car (car listStnData))) "\n") hSrcFileToDump)
				(princ "*|<< Offset >>|<< Elevation >>|<< Northing >>|<< Easting >>|\n" hSrcFileToDump)
			)
		)
		(while (< i iLim)
			(setq listSpLevelPt (polar listCenPoint fAngle (car (nth i listOffElev)))
				  listSpLevelPt (reverse (cdr (reverse listSpLevelPt))) ; Remove Z-Value
				  listSpLevelPt (append listSpLevelPt (list (cadr (nth i listOffElev))))
			)
			(DrawSpotLevel (cadr (nth i listOffElev)) listSpLevelPt fTxtSize (+ fAngle (/ PI 2.0)) strBlk dMarkSize strLyr)
			(if (/= hSrcFileToDump nil)
				(progn
					(setq listSpLevelPt (polar listCenPt fAngle (car (nth i listOffElev))))
					(princ (rtos (car (nth i listOffElev))) hSrcFileToDump) (princ " \t" hSrcFileToDump)
					(princ (rtos (cadr (nth i listOffElev))) hSrcFileToDump) (princ " \t" hSrcFileToDump)
					(princ (rtos (cadr listSpLevelPt)) hSrcFileToDump) (princ " \t" hSrcFileToDump)
					(princ (rtos (car listSpLevelPt)) hSrcFileToDump) (princ "\n" hSrcFileToDump)
				)
			)
			(setq i (+ i 1))
		)
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
	(if (/= (nth 6 listGUIParam) nil)
		(setq hSrcFileToDump (open (nth 6 listGUIParam) "w"))
		(setq hSrcFileToDump nil)
	)
	(setq listStnData T hsrcFile (open (nth 0 listGUIParam) "r") i 0 listStnPoint nil)
	(while (and (/= listStnData nil) (/= hsrcFile nil))
		(setq listStnData (ReadNextStnData hsrcFile)) 
		(if (/= listStnData nil)
			(progn
				(setq fChainage (car (car listStnData)))
				(if (= i 0)
					(setq listCumChainages (GenCumChainages listPIParam fChainage))
				)
				(setq iIndex (FindSegmentIndex fChainage listCumChainages)
				      i (+ i 1)
					  listStnPoint (FindAbsCoOrd fChainage listPIParam listCumChainages)
			    )
				(princ (strcat "\rAnnotating station at CH: " (rtos fChainage)))
				;;Start DWG routine calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				(setq listCenPt (cadr listStnPoint) fAngle (- (car listStnPoint) (/ PI 2.0)))
				(DrawAllOffsets hSrcFileToDump listDwgOrg listCenPt listStnData fAngle fTxtSize  strBlk dMarkSize strLyr)
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			)
		)
	)
	(if (/= hsrcFile nil) (close hsrcFile))
	(if (/= hSrcFileToDump nil) (close hSrcFileToDump))
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
   (setq listData (list "" "" "2.0" "3.0" "Station Detail" "POINT" "")
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
;;NB:This func.will work for Special kind of Station Data
;;
(defun ReadNextStnDataEx (hsrcFile / i hsrcFile listOfWords listToRet bContinueRead)
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
;;
;;Read Next Station Data 
;;
(defun ReadNextStnData (hsrcFile / i hsrcFile listOfWords listToRet bContinueRead)
	(setq i 0 listOfWords 0 bContinueRead 1 listToRet (list ()))
	
	(while (and (= bContinueRead 1) (/= listOfWords nil))
		(setq listOfWords (ReadNextValidLine hSrcFile))
		(if (/= listOfWords nil)
			(progn
				(if (= i 0)
					(progn ;;Chk Start Stn Data mark
						(if (or (/= (length listOfWords) 3) (/= (strcase (nth 0  listOfWords)) "START") (/= (strcase (nth 1 listOfWords)) "CHAINAGE"))
							(progn (alert "FATAL ERROR: Starting data segment for station invalid!!") (exit))
							(setq listToRet (list (list (atof (nth 2 listOfWords)))))
						)
					)
					(progn ;;Chk Other Stn Data mark
						(if (< (length listOfWords) 2)
							(progn (alert "FATAL ERROR: Offset data invalid!!") (exit))
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
;;Creates a new STNData file adding value to RL at each offset
;;
(defun ModifyStnDataFile(bIsSplStationData / *ERR_OLD* GetData WriteStnData listStnData hsrcFile hOutputFile strInputFile strOutputFile fRLIncrement)
    (princ "\n\nCreates a new Station Data file (.stn) from a source Station data file adding user specified value to RL of each ")
	(if(= bIsSplStationData 1)
		(princ "Northing-Easting data point")
		(princ "offset")
	)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq *ERR_OLD* *ERROR*)
	(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun GetData(/ strInpFile strOutputFile fRLIncrement retList)
		(setq strInpFile (getstring "\nEnter Station data file to convert :"))
		(setq strInpFile (findfile strInpFile))
		(if (/= strInpFile nil)
			(progn
				(setq strOutputFile (getstring "\nEnter Destination Station data file :"))
				(setq fRLIncrement (getreal "\nEnter increment to RL value :"))
			)
			(princ "\nERROR: Source File doesn't exists")
		)
		(if (= strInpFile nil) (setq retList nil) (setq retList (list fRLIncrement strInpFile strOutputFile)))
		;;Return 
		(setq retList retList)
	) 
	(defun WriteStnData(bIsSplStationData listStnData hOutputFile fRLIncrement / fChainage listOffElev i iLoopLim listTemp fVal01 fVal02 fVal03) 
		(setq fChainage (car (car listStnData)) listOffElev (cdr listStnData))
		(princ (strcat "\rWriting data of Station at CH: " (rtos fChainage) "   "))
		(princ (strcat "\nSTART CHAINAGE " (rtos fChainage 2 3)) hOutputFile);;Header
		(setq i 0 iLoopLim (length listOffElev))
		(while (< i iLoopLim)
			(if(= bIsSplStationData 1)
				(progn
					(setq listTemp (nth i listOffElev)
						  fVal01 (car listTemp)
						  fVal02 (cadr listTemp)
						  fVal03 (caddr listTemp)
						  fVal03 (+ fRLIncrement fVal03)
					)
					(princ (strcat "\n" (rtos fVal01 2 3) "\t" (rtos fVal02 2 3) "\t" (rtos fVal03 2 3)) hOutputFile)
				)
				(progn
					(setq listTemp (nth i listOffElev)
						  fVal01 (car listTemp)
						  fVal02 (cadr listTemp)
						  fVal02 (+ fRLIncrement fVal02)
					)
					(princ (strcat "\n" (rtos fVal01 2 3) "\t" (rtos fVal02 2 3)) hOutputFile)
				)
			)
			(setq i (+ i 1))
		)
		(princ (strcat "\nEND CHAINAGE " (rtos fChainage 2 3)) hOutputFile)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listStnData (GetData))
	(if (/= listStnData nil)
		(progn
			(setq fRLIncrement (nth 0 listStnData)
				  strInputFile (nth 1 listStnData)
				  strOutputFile (nth 2 listStnData)
				  listStnData T 
				  hsrcFile (open strInputFile "r")
				  hOutputFile (open strOutputFile "w")
			)
			(while (and (/= listStnData nil) (/= hsrcFile nil) (/= hOutputFile nil))
				(if (= bIsSplStationData 1)
					(setq listStnData (ReadNextStnDataEx hsrcFile)) 
					(setq listStnData (ReadNextStnData hsrcFile)) 
				)
				(if (/= listStnData nil)
					(progn
						(WriteStnData bIsSplStationData listStnData hOutputFile fRLIncrement)
					)
				)
			)
			(if (/= hsrcFile nil) (close hsrcFile) (princ "\nERROR: Source Station Data file not found"))
			(if (/= hOutputFile nil) (close hOutputFile) (princ "\nERROR: Destination Station Data file path invalid"))
		)
		(princ "\nERROR: Invalid Source & Destination Station Data file name")
	);if
	(setq *ERROR* *ERR_OLD*)
	(princ)
)
;;
;;For Special kind of Stn Data having Northing Easting Values instead of Offsets
;;
(defun PlotStationsEx(strPath / *ERR_OLD* listParam listPIParam listDwgOrg)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq *ERR_OLD* *ERROR*)
	(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listParam (GetStnDetailData strPath 1))
	(if(/= listParam nil)
		(progn
			(setq listPIParam (ReadPIDFile (nth 1 listParam)))
			(setq listDwgOrg (list 0.0 0.0 0.0))
			(DrawStationEx listDwgOrg listParam listPIParam)
		)
	)
	(setq *ERROR* *ERR_OLD*)
	(princ)
)
;;
;;For Special kind of Stn Data having Northing Easting Values instead of Offsets
;;
(defun DrawStationEx (listDwgOrg listGUIParam listPIParam / TransformPtWrtUsersOrg GenCumChainages FindSegmentIndex FindAbsCoOrd DrawAllOffsets listStnData hsrcFile fChainage listCumChainages iIndex i listStnPoint listCenPt fTxtSize fAngle strBlk dMarkSize strLyr listStnPointPrev)
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
	(defun FindSegmentIndex (fChainage listCumChainages / i iIndex)
		(if (> fChainage (last listCumChainages))
			(progn
				(alert (strcat "FATAL ERROR: Invalid station at Ch:" (rtos fChainage) " specified to draw!!"))
				(exit)
			)
		)
		(setq i 0 iIndex 0)
		(while (< i (length listCumChainages))
			(if (>= (nth i listCumChainages) fChainage)
				(setq iIndex (- i 1) i (length listCumChainages))
			)
			(setq i (+ i 1))
		)
		(if (< iIndex 0) (setq iIndex 0))
		(setq iIndex iIndex);;Return
	)
	(defun FindAbsCoOrd(fChainage listPIParam listCumChainages / iIndex fBase fAngle fDist listRet)
		(setq iIndex (FindSegmentIndex fChainage listCumChainages))
		(setq fBase (nth iIndex listCumChainages)
			  fAngle (angle (reverse (nth iIndex listPIParam)) (reverse (nth (+ iIndex 1) listPIParam)))
			  fDist (- fChainage fBase)
			  listRet (polar (reverse (nth iIndex listPIParam)) fAngle fDist)
		)
		(setq listRet (list fAngle listRet));Return
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
	
	(defun DrawAllOffsets (listDwgOrg listCenPt listStnData fAngle fTxtSize  strBlk dMarkSize strLyr / listNEasting listSpLevelPt i iLim)
		(setq listCenPoint (TransformPtWrtUsersOrg listDwgOrg listCenPt) listNEasting (cdr listStnData) i 0 iLim (length listNEasting))
		(while (< i iLim)
			;;Data listNEasting -->> <Nothing><Easing><Elev>
			(setq listSpLevelPt (list (cadr (nth i listNEasting)) (car (nth i listNEasting)))
				  listSpLevelPt (append listSpLevelPt (list (cadr (cdr (nth i listNEasting)))))
			)
			(DrawSpotLevel (cadr (cdr (nth i listNEasting))) listSpLevelPt fTxtSize (+ fAngle (/ PI 2.0)) strBlk dMarkSize strLyr)
			(setq i (+ i 1))
		)
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
				(setq fChainage (car (car listStnData)))
				(if (= i 0)
					(setq listCumChainages (GenCumChainages listPIParam fChainage))
				)
				(setq iIndex (FindSegmentIndex fChainage listCumChainages)
				      i (+ i 1)
					  listStnPoint (FindAbsCoOrd fChainage listPIParam listCumChainages)
			    )
				(princ (strcat "\rAnnotating station at CH: " (rtos fChainage)))
				;;Start DWG routine calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				(setq listCenPt (cadr listStnPoint) fAngle (- (car listStnPoint) (/ PI 2.0)))
				(DrawAllOffsets listDwgOrg listCenPt listStnData fAngle fTxtSize  strBlk dMarkSize strLyr)
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			)
		)
	)
	(if (/= hsrcFile nil) (close hsrcFile))
	(princ)
)

(defun C:EA () (command ".Erase" "all" "")(princ))
(princ)