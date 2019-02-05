(UNTRACE ModifyR14PLine)
(defun ChTxtHt( / ModifyTXT fTxtHt ssetEnt i iLoopLimit ent entDet         )
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun ModifyTXT(entDet fTxtHt)
		(setq entDet (subst (cons 40 fTxtHt) (assoc 40 entDet) entDet))
		(princ entDet)(princ"\n")
		(if (/= fTxtHt 0.0) (entmod entDet))
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(princ "\nSelect text entities to modify height....")
	(setq ssetEnt (ssget))
	(if (/= ssetEnt nil)
		(progn
			(initget 1)
			(setq fTxtHt (getreal "Enter next text height :"))
			(if(/= fTxtHt 0.0)
				(progn
					(setq i 0 iLoopLimit (sslength ssetEnt))
					(while (< i iLoopLimit)
						(setq ent (ssname ssetEnt i)
							  entDet (entget ent)
							  i (+ i 1)
						)
						(if(= "TEXT" (cdr (assoc 0 entDet)))
							(progn
								(ModifyTXT entDet fTxtHt)
								(princ (strcat "\r " (itoa i) "# of entities modified"))
							)
						)
					)
				)
				(princ "\nERROR: Invalid text height input")
			);if
		)
	)
	(princ)
)

(defun InsertBlks(strPath / *ERR_OLD* listData bHasZValue)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq *ERR_OLD* *ERROR*)
	(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listData (GetMultiBlkInsData strPath))
	(if (/= listData nil)
		(progn
			(princ "\n")
			(setq i 0 hsrcFile (open (nth 0 listData) "r") bHasZValue (nth 2 listData) listOfInsParam nil)
			(if (/= hsrcFile nil)
				(progn
					(setq listOfWords 0)
					(while (/= listOfWords nil)
						(setq listOfWords (ReadNextValidLine hSrcFile))
						(if (= i 0)
							(progn
								(if (< (length listOfWords) 4)
									(progn
										(alert "ERROR: Block Insertion Parameter not found!!")
										(setq listOfWords nil)
									)
									(setq listOfInsParam (list (atof (nth 0 listOfWords)) (atof (nth 1 listOfWords)) (atof (nth 2 listOfWords)) (atof (nth 3 listOfWords))))
								)
							)
							(if (/= listOfWords nil)
								(progn
									(princ (strcat "\r " (itoa i) " No. of block inserted"))
									(InsertBlocks bHasZValue (nth 1 listData) listOfWords listOfInsParam (getvar "clayer"))
								)
							)
						)
						(setq i (+ i 1))
					)
					(close hsrcFile)
					(princ (strcat "\rTotal " (itoa i) " No. of block inserted...Done\n"))
				)
				(alert "ERROR: opening file!!")
			)
		);
	);if
	(setq *ERROR* *ERR_OLD*)
	(princ)
)
(defun GetMultiBlkInsData(strPath / OnOk retValue bZValFlagTogg dcl_id)
   (defun OnOk (/ strFile strBlk retValue bError)
	 (setq strFile (get_tile "FileEBox")
		   strBlk (get_tile "BlkEbox")
		   bError 0
		   bZValFlagTogg (atoi (get_tile "ZValFlagTogg"))
	 )
     (if (or (= strFile nil) (= (strlen strFile) 0) (= (findfile strFile) nil)) ;;"strFile"
    	 (progn (set_tile "err" "ERROR: Invalid Data file") (mode_tile "FileEBox" 2) (setq bError 1))
     )
	 (if (and (or (= strBlk nil) (= (tblsearch "BLOCK" strBlk) nil)) (= bError 0))
    	 (progn (set_tile "err" "ERROR: Block not present in current dwg") (mode_tile "BlkEbox" 2) (setq bError 1))
	 )
	 (if (= bError 0)
		(progn (done_dialog 0) (setq retValue (list strFile strBlk bZValFlagTogg)))
		(setq retValue nil)
	 )
	 (setq retValue retValue);Return Statement
   )
   (defun OnFileDlgButton (strTileFileEBox / strLabel strFname strFiletype)
      (setq strLabel "Select Data File" strFiletype "*"
		    strFname (getfiled strLabel "" strFiletype 2)
	  )
      (if (/= strFname nil) (set_tile strTileFileEBox strFname))
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq dcl_id (load_dialog (strcat strPath "SurvPlan")))
   (setq listData (list "" "" "0") listDataKey (list "FileEBox" "BlkEbox" "ZValFlagTogg"))
   (mapcar 'set_tile listDataKey (list "FileEBox" "BlkEbox" "ZValFlagTogg"))

   (new_dialog "GetMultiBlkInsData" dcl_id)
   (action_tile "accept" "(setq retValue (OnOk))")
   (action_tile "cancel" "(progn (setq retValue nil)(done_dialog 1))")
   (action_tile "FileButt" "(OnFileDlgButton (nth 0 listDataKey))")
   (start_dialog)
   (unload_dialog dcl_id)
   (setq retValue retValue);;Return
)

;(defun GetData( / *ERR_OLD* listData strBlkName strDataFile)
;	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	(setq *ERR_OLD* *ERROR*)
;	(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	(setq strBlkName nil strDataFile nil)
;	(while (= strBlkName nil)
;		(setq strBlkName (getstring "\nEnter Block name to insert :"))
;		(if (= (tblsearch "BLOCK" strBlkName) nil)
;			(progn
;				(princ (strcat "\nBlock \"" strBlkName "\" doesn't exists"))
;				(setq strBlkName nil)
;			)
;		)
;	)
;	(while (= strDataFile nil)
;		(setq strDataFile (getstring "\nEnter Data file name :"))
;		(if (= (findfile strDataFile) nil)
;			(progn
;				(princ (strcat "\nFile \"" strDataFile "\" doesn't exists"))
;				(setq strDataFile nil)
;			)
;		)
;	)
;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	(setq *ERROR* *ERR_OLD*)
;	(setq listData (list strBlkName strDataFile))
;)

(defun D2R (fAng) (* fAng (/ pi 180.0)))
(defun R2D (fAng) (* (/ 180.0  pi) fAng))
(defun TEXTLEFT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str)  (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun BLOCK (strBlkName ptIns fX fY fRot strLyr / strPreLyr bReturn )
  (setq bReturn 0)
  (if (and (/= strBlkName nil) (/= (tblsearch "BLOCK" strBlkName) nil))
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
	  (setq strPreLyr (getvar "CLAYER"))
	  (command "layer" "s" strLyr "" "insert" strBlkName ptIns fX fY fRot "layer" "s" strPreLyr "")
	  (setq bReturn 1)
	)
      )
    )
  );if
  (if (= bReturn 0) (setq bReturn nil) (setq bReturn 1));; Return Statement
)



;;;;
;;;;Returns : nil >> if EOF reached
;;;;         (list 'of words') >>if successful
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
(defun InsertBlocks(bHasZValue  strBlkName strRawInsData listScaleNRot strLyr / ptIns ptTxtIns strAnn)
	;;Note 'strRawInsData' contains co-ordinate in N-E format
	(if (= bHasZValue 0)
		(progn
			(if (< (length strRawInsData) 2)
				(progn
					(princ "FATAL ERROR: Check Data file...Invalid data sample as following..\n")
					(princ strRawInsData)
					(princ "\n")
					(alert "FATAL ERROR: Data without N/E value")
					(exit)
				)
			)
			(setq ptIns (list (atof (nth 1 strRawInsData)) (atof (nth 0 strRawInsData))))
		)
		(progn
			(if (< (length strRawInsData) 3)
				(progn
					(princ "FATAL ERROR: Check Data file...Invalid data sample as following..\n")
					(princ strRawInsData)
					(princ "\n")
					(alert "FATAL ERROR: Data without Elevation value")
					(exit)
				)
			)
			(setq ptIns (list (atof (nth 1 strRawInsData)) (atof (nth 0 strRawInsData)) (atof (nth 2 strRawInsData))))
		)
	)
	(cond 
		((= bHasZValue 0) (if (> (length strRawInsData) 2) (setq strAnn (nth 2 strRawInsData)) (setq strAnn nil)))
		((= bHasZValue 1) (if (> (length strRawInsData) 3) (setq strAnn (nth 3 strRawInsData)) (setq strAnn nil)))
	)
	(BLOCK strBlkName ptIns (nth 0 listScaleNRot) (nth 1 listScaleNRot) (nth 2 listScaleNRot) strLyr)
	;Calcualate Txt ins pt
	(if(/= strAnn nil)
		(progn
			(setq ptTxtIns (polar ptIns (D2R (nth 2 listScaleNRot)) (nth 3 listScaleNRot)))
			(TEXTLEFT ptTxtIns strAnn (nth 2 listScaleNRot) (nth 3 listScaleNRot) strLyr)
		)
	)
)
;;
;;
;;Changes all entities' Z-Value if those are not = 0.0
;;
(defun UpdateZValue(/ *ERR_OLD* fZIncriment)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq *ERR_OLD* *ERROR*)
	(defun *ERROR* (voidVal) (princ "\nERROR: User termination..execution cancelled!") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(princ "\n\nThis utility changes all entities' Z-Value if those are not = 0.0")
	(setq fZIncriment (getreal "\nEnter value to add with Z-Value :"))
	(if(/= fZIncriment 0.0)
		(ModifyZValueOfAllEntities fZIncriment)
		(princ "\nZero value entered..nothing to do")
	)
	(setq *ERROR* *ERR_OLD*)
	(princ)
)
(defun ModifyZValueOfAllEntities(fZIncriment / SubstValInList ModifyEntity ssetEnt i iLoopLimit ent)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun SubstValInList (listParam iIndex voidVal / i retList iLoopLim)
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
	(defun ModifyEntity(entity fZIncriment iEntSlNo / ModifyLine ModifyGenEnity ModifyPLine ModifyR14PLine ModifyAttrDef entDet strEntType)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;//Line, Text
		(defun ModifyLine(entDet fZIncriment / ptIns fZValue01 fZValue02 ptInsNew)
			(setq ptIns (cdr (assoc 10 entDet))
				  fZValue01 (cadr (cdr ptIns))
				  ptInsNew (SubstValInList ptIns 2 (+ fZValue01 fZIncriment))
				  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
			)
			(setq ptIns (cdr (assoc 11 entDet))
				  fZValue02 (cadr (cdr ptIns))
				  ptInsNew (SubstValInList ptIns 2 (+ fZValue02 fZIncriment))
				  entDet (subst (cons 11 ptInsNew) (assoc 11 entDet) entDet);;Update entity list 
			)
			(if (or (/= fZValue01 0.0) (/= fZValue02 0.0))
				(entmod entDet)
			)
		)
		;//Circle, Point, Arc, Block
		(defun ModifyGenEnity(entDet fZIncriment / ptIns fZValue ptInsNew)
			(setq ptIns (cdr (assoc 10 entDet))
				  fZValue (cadr (cdr ptIns))
				  ptInsNew (SubstValInList ptIns 2 (+ fZValue fZIncriment))
				  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
			)
			(if (/= fZValue 0.0) (entmod entDet))
		)
		(defun ModifyPLine(entName fZIncriment / entMain entDet ptIns fZValue ptInsNew)
            (setq entMain entName entDet (entget entName))
			(if(/= (boole 1 (cdr (assoc 70 entDet)) 8) 0)
				(progn ;;For 3DPolyline only
					(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
						(setq ptIns (cdr (assoc 10 entDet))
							  fZValue (cadr (cdr ptIns))
							  ptInsNew (SubstValInList ptIns 2 (+ fZValue fZIncriment))
							  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
						)
						(entmod entDet)
						(setq  entName (entnext entName)
							   entDet (entget entName)
						)
					)
					(entupd entMain)
				)
				(progn 
					(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
						(setq ptIns (cdr (assoc 10 entDet))
							  fZValue (cadr (cdr ptIns))
						)
						(if(and (/= fZValue 0.0) (/= fZValue nil))
							(progn 
								(setq ptInsNew (SubstValInList ptIns 2 (+ fZValue fZIncriment))
									  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
								)
								(entmod entDet)
							);progn 
						)
						(setq  entName (entnext entName)
							   entDet (entget entName)
						)
					)
					(entupd entMain)
				)
			)
		)
		(defun ModifyR14PLine(entName fZIncriment / entMain entDet fZValue)
			(setq entDet (entget entName))
			(if(/= (cdr (assoc 38 entDet)) 0.0)
				(progn
					(setq entMain entName entDet (entget entName))
					(if (/= (cdr (assoc 38 entDet)) 0.0)
						(progn
							(setq fZValue (+ fZIncriment (cdr (assoc 38 entDet)))
								  entDet (subst (cons 38 fZValue) (assoc 38 entDet) entDet);;Update entity list 
							)
							(entmod entDet)
							(entupd entMain)
						)
					)
				)
			)
		)
		(defun ModifyAttrDef(entName fZIncriment / entMain entName entDet ptIns fZValue ptInsNew)
			(setq entDet (entget entName))
			;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START
			(if (= (cdr (assoc 2 entDet)) "POINTBLK") (setq bFlag T) (setq bFlag FALSE))
			;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END

			(if(= (cdr (assoc 66 entDet)) 1)
				(progn
					(setq entMain entName entDet (entget entName))
					(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
						(setq ptIns (cdr (assoc 10 entDet))
							  fZValue (cadr (cdr ptIns))
							  ptInsNew (SubstValInList ptIns 2 (+ fZValue fZIncriment))
							  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
						)
						;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START
						(if(= bFlag T)
							(progn
								(if (= (cdr (assoc 2 entDet)) "ELEV")
									(progn
										(setq entDet (subst (cons 1 (rtos (+ fZValue fZIncriment) 2 3)) (assoc 1 entDet) entDet))
									)
								)
							)
						)
						;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END
						(entmod entDet)
						(setq  entName (entnext entName)
							   entDet (entget entName)
						)
					)
					(entupd entMain)
				)
			)
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq entDet (entget entity) strEntType (cdr (assoc 0 entDet)))
		(cond
			((or (= strEntType "LINE") (= strEntType "TEXT"))
			 (ModifyLine entDet fZIncriment)
			)
			((= strEntType "INSERT")
			 (if(= (cdr (assoc 66 entDet)) 1)
				(ModifyAttrDef entity fZIncriment)
				(ModifyGenEnity entDet fZIncriment)
			 )
			)
			((or (= strEntType "CIRCLE") (= strEntType "POINT") (= strEntType "ARC"))
			 (ModifyGenEnity entDet fZIncriment)
			)
			((= strEntType "POLYLINE") (ModifyPLine entity fZIncriment))
			((= strEntType "LWPOLYLINE") (ModifyR14PLine entity fZIncriment))
			(T (princ (strcat "\rUnhandled entity : " strEntType "\n")))
		)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(setq ssetEnt (ssget "x"))
	(setq ssetEnt (ssget))
	(if (/= ssetEnt nil)
		(progn
			(setq i 0 iLoopLimit (sslength ssetEnt))
			(while (< i iLoopLimit)
				(setq ent (ssname ssetEnt i)
					  i (+ i 1)
				)
				(ModifyEntity ent fZIncriment (+ i 1))
				(princ (strcat "\r " (itoa i) "# of entities modified"))
			)
		)
	)
	(princ (strcat "\rDone Total " (itoa i) " # of entities modified"))
	(princ)
)
(princ)
;;
;;
;;Changes all entities' Z-Value as specified
;;
(defun SetZValue(/ *ERR_OLD* SetZValueOfAllEntities fZVal)
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq *ERR_OLD* *ERROR*)
	(defun *ERROR* (voidVal) (princ "\nERROR: User termination..execution cancelled!") (setq *ERROR* *ERR_OLD*) (princ))
	(defun SetZValueOfAllEntities(fZValueNew / SubstValInList ModifyEntity ssetEnt i iLoopLimit ent)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(defun SubstValInList (listParam iIndex voidVal / i retList iLoopLim)
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
		(defun ModifyEntity(entity fZValueNew iEntSlNo / ModifyLine ModifyGenEnity ModifyPLine ModifyR14PLine ModifyAttrDef entDet strEntType)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;//Line
			(defun ModifyLine(entDet fZValueNew / ptIns ptInsNew)
				(setq ptIns (cdr (assoc 10 entDet))
					  ptInsNew (SubstValInList ptIns 2 fZValueNew)
					  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
				)
				(setq ptIns (cdr (assoc 11 entDet))
					  ptInsNew (SubstValInList ptIns 2 fZValueNew)
					  entDet (subst (cons 11 ptInsNew) (assoc 11 entDet) entDet);;Update entity list 
				)
				(entmod entDet)
			)
			;//Circle, Point, Arc, Block
			(defun ModifyGenEnity(entDet fZValueNew / ptIns ptInsNew)
				(setq ptIns (cdr (assoc 10 entDet))
					  ptInsNew (SubstValInList ptIns 2 fZValueNew)
					  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
				)
				(entmod entDet)
			)
			(defun ModifyPLine(entName fZValueNew / entMain entDet ptIns ptInsNew)
				(setq entMain entName entDet (entget entName))
				(if(/= (boole 1 (cdr (assoc 70 entDet)) 8) 0)
					(progn ;;For 3DPolyline only
						(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
							(setq ptIns (cdr (assoc 10 entDet))
								  ptInsNew (SubstValInList ptIns 2 fZValueNew)
								  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
							)
							(entmod entDet)
							(setq  entName (entnext entName)
								   entDet (entget entName)
							)
						)
						(entupd entMain)
					)
					(progn 
						(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
							(setq ptIns (cdr (assoc 10 entDet))
								  ptInsNew (SubstValInList ptIns 2 fZValueNew)
								  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
							)
							(entmod entDet)
							(setq  entName (entnext entName)
								   entDet (entget entName)
							)
						)
						(entupd entMain)
					)
				)
			)
			(defun ModifyR14PLine(entName fZValueNew / entMain entDet)
				(setq entDet (entget entName))
				(if(/= (cdr (assoc 38 entDet)) 0.0)
					(progn
						(setq entMain entName entDet (entget entName))
						(if (/= (cdr (assoc 38 entDet)) 0.0)
							(progn
								(setq entDet (subst (cons 38 fZValueNew) (assoc 38 entDet) entDet));;Update entity list 
								(entmod entDet)
								(entupd entMain)
							)
						)
					)
				)
			)
			(defun ModifyAttrDef(entName fZValueNew / entMain entName entDet ptIns ptInsNew)
				(setq entDet (entget entName))
				;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START
				(if (= (cdr (assoc 2 entDet)) "POINTBLK") (setq bFlag T) (setq bFlag FALSE))
				;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END

				(if(= (cdr (assoc 66 entDet)) 1)
					(progn
						(setq entMain entName entDet (entget entName))
						(while (not (= (cdr (assoc 0 entDet)) "SEQEND"))
							(setq ptIns (cdr (assoc 10 entDet))
								  ptInsNew (SubstValInList ptIns 2 fZValueNew)
								  entDet (subst (cons 10 ptInsNew) (assoc 10 entDet) entDet);;Update entity list 
							)
							;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START
							(if(= bFlag T)
								(progn
									(if (= (cdr (assoc 2 entDet)) "ELEV")
										(progn
											(setq entDet (subst (cons 1 (rtos fZValueNew 2 3)) (assoc 1 entDet) entDet))
										)
									)
								)
							)
							;;Special case....Project dependent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END
							(entmod entDet)
							(setq  entName (entnext entName)
								   entDet (entget entName)
							)
						)
						(entupd entMain)
					)
				)
			)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			(setq entDet (entget entity) strEntType (cdr (assoc 0 entDet)))
			(cond
				((or (= strEntType "LINE") (= strEntType "TEXT"))
				 (ModifyLine entDet fZValueNew)
				)
				((= strEntType "INSERT")
				 (if(= (cdr (assoc 66 entDet)) 1)
					(ModifyAttrDef entity fZValueNew)
					(ModifyGenEnity entDet fZValueNew)
				 )
				)
				((or (= strEntType "CIRCLE") (= strEntType "POINT") (= strEntType "ARC"))
				 (ModifyGenEnity entDet fZValueNew)
				)
				((= strEntType "POLYLINE") (ModifyPLine entity fZValueNew))
				((= strEntType "LWPOLYLINE") (ModifyR14PLine entity fZValueNew))
				(T (princ (strcat "\rUnhandled entity : " strEntType "\n")))
			)
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;(setq ssetEnt (ssget "x"))
		(setq ssetEnt (ssget))
		(if (/= ssetEnt nil)
			(progn
				(setq i 0 iLoopLimit (sslength ssetEnt))
				(while (< i iLoopLimit)
					(setq ent (ssname ssetEnt i)
						  i (+ i 1)
					)
					(ModifyEntity ent fZValueNew (+ i 1))
					(princ (strcat "\r " (itoa i) "# of entities modified"))
				)
			)
		)
		(princ (strcat "\rDone Total " (itoa i) " # of entities modified"))
		(princ)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(princ "\n\nThis utility changes all entities' Z-Value(s) as specified")
	(setq fZVal (getreal "\nEnter new Z-Value <0.0> :"))
	(if(= fZVal nil)
		(setq fZVal 0.0)
	)
	(SetZValueOfAllEntities fZVal)
	(setq *ERROR* *ERR_OLD*)
	(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun C:EG()
	(setq ent (entsel))
	(if(/= ent nil)
		(progn
			(princ "\n") (princ (entget (car ent))) (princ "\n") 
			(setq ent (car ent))
		)
		(princ)
	)
)
;(defun C:TT()
;	(ModifyZValueOfAllEntities 1111.111)
;)
(princ)
