;;SurvPr14.LSP
(defun PrintLoadMsg(iPerCent) (if (= iPerCent 0) (princ "\nPlease wait...Loading neccessary files\n") (princ (strcat "\r" (itoa iPerCent) "% Loaded"))) (if (= iPerCent 100) (princ "\nLoading Successful\n"))(princ))
(PrintLoadMsg 0)
;;
;; 
;; Main Exe SubR >>> (DrawQProfileStarter ??) defined in this File 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Main Exe SubR                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawQProfileStarterP(strPath / *ERR_OLD* GetQPRDataThruDCL listDwgParam dXScale dYScale dDatum dNumAnnSize dRowAnnSize dRowHt szElevTag szNorthingTag szEastingTag szCumDistTag szDistTag szDatumTag szLyr PtOrgX PtOrgY PtOrgZ )
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(setq *ERR_OLD* *ERROR*)
	;(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun GetQPRDataThruDCL (/
							ModifyDataList EliminateNILVals SaveAllData RestoreAllData OnOk 
							returnList dcl_id listKeyMainDlg listDataMainDlg iDoneDialogFlag
							)
	   (defun ModifyDataList (listDataParam listPoint / i iLoopLim listRet iIndexOfXOrd)
		  (setq i 0 iLoopLim (length listDataParam) listRet (list ()))
		  (setq iIndexOfXOrd (- iLoopLim 3));; Point Ord Starts from third last index of the list
		  (while (< i iLoopLim)
			(if (and (>= i iIndexOfXOrd) (<= i (+ iIndexOfXOrd 3)))
			   (setq listRet (append listRet (list (rtos (nth (- i iIndexOfXOrd) listPoint)))))
 			   (setq listRet (append listRet (list (nth i listDataParam))))
			)
			(setq i (+ i 1))
		  );while
		  (setq listRet (cdr listRet))
	   )
	   (defun EliminateNILVals (listParam / i iLoopLim listRet)
		 (setq i 0 iLoopLim (length listParam) listRet (list ()))
		 (while (< i iLoopLim)
		   (if (= (nth i listParam) nil)
			  (setq listRet (append listRet (list "")))
			  (setq listRet (append listRet (list (nth i listParam))))
		   )
		   (setq i (+ i 1))
		 );while
		 (setq listRet (cdr listRet))
	   )
	   (defun SaveAllData () (setq listDataMainDlg (EliminateNILVals (mapcar 'get_tile listKeyMainDlg))))
	   (defun RestoreAllData () (mapcar 'set_tile listKeyMainDlg listDataMainDlg))
	   (defun OnOk (listKey / retList bError strProfName)
		 ;Return Statement
		  (setq retList (EliminateNILVals (mapcar 'get_tile listKey)) bError 0)

		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  (if (and (<= (atof (nth 0 retList)) 0.0) (= bError 0)) ;;"HorSca"
			(progn (set_tile "err" "ERROR: Bad Horz. scale") (mode_tile (nth 0 listKey) 2) (setq bError 1))
		  )
		  (if (and (<= (atof (nth 1 retList)) 0.0) (= bError 0)) ;;"VerSca"
			(progn (set_tile "err" "ERROR: Bad Vert. scale") (mode_tile (nth 1 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 3 retList)) 0.0) (= bError 0)) ;;"NumAnnSize"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 3 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 4 retList)) 0.0) (= bError 0)) ;;"RowAnnSize"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 4 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 5 retList)) 0.0) (= bError 0)) ;;"RowHt"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 5 listKey) 2) (setq bError 1));progn
		  )
		  (if (= bError 0) (done_dialog 0))
		  (setq retList retList)
	   )

	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (setq returnList nil dcl_id (load_dialog (strcat gstrSrvPlanPath "SurvPlan")))

	   (setq listKeyMainDlg (list "HorSca" "VerSca" "Datum" "NumAnnSize" "RowAnnSize" "RowHt" "ElevTag" "NorthingTag" "EastingTag" "CumDistTag" "DistTag" "DatumTag" "Layer" "OrdX" "OrdY" "OrdZ")
			 listDataMainDlg (list "5.0" "5.0" "0.0" "1.0" "2.0" "10" "Elevation :" "Northing :" "Easting :" "Cumulative Distance : " "Distance : " "Datum : " "QUICK_PROFILE" "0.0" "0.0" "0.0")
	   )
	   (setq iDoneDialogFlag 10)
	   (while (> iDoneDialogFlag 1)
		   (new_dialog "GetQProfileData" dcl_id)
		   (RestoreAllData)
   		   (action_tile "PointButt" "(progn (SaveAllData)(done_dialog 5))")
		   (action_tile "accept" "(setq returnList T listDataMainDlg (OnOk listKeyMainDlg))")
		   (action_tile "cancel" "(progn (setq returnList nil)(done_dialog 1))")
		   (setq iDoneDialogFlag (start_dialog))
		   (cond 
			 ((= iDoneDialogFlag 5)
			  (initget 1) (setq point (getpoint "\nPick Profile Plot Origin:"))
			  (setq listDataMainDlg (ModifyDataList listDataMainDlg point)) 
			 )
		   )
	   )
	   (unload_dialog dcl_id)
	   (if (= returnList nil)
		   (setq returnList nil)
		   (setq returnList listDataMainDlg)
	   )
	   (setq returnList returnList)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq gstrSrvPlanPath strPath)
	(setq listDwgParam (GetQPRDataThruDCL));;Dialog Interface return data
	(if(/= listDwgParam nil)
		(progn
			(if (= (member "survutil.arx" (ARX)) nil) (arxload (strcat gstrSrvPlanPath "SurvUtil.arx")))
			(setq 
					dXScale (atof (nth 0 listDwgParam))
					dYScale (atof (nth 1 listDwgParam))
					dDatum (atof (nth 2 listDwgParam))
					dNumAnnSize (atof (nth 3 listDwgParam))
					dRowAnnSize (atof (nth 4 listDwgParam))
					dRowHt (atof (nth 5 listDwgParam))

					szElevTag (nth 6 listDwgParam)
					szNorthingTag (nth 7 listDwgParam)
					szEastingTag (nth 8 listDwgParam)
					szCumDistTag (nth 9 listDwgParam)
					szDistTag (nth 10 listDwgParam)
					szDatumTag (nth 11 listDwgParam)
					szLyr (nth 12 listDwgParam)
	
					PtOrgX (atof (nth 13 listDwgParam))
					PtOrgY (atof (nth 14 listDwgParam))
					PtOrgZ (atof (nth 15 listDwgParam))
			)
			;;Call ARX 
			(DrawQProfileFrPts dXScale dYScale dDatum dNumAnnSize dRowAnnSize dRowHt szElevTag szNorthingTag szEastingTag szCumDistTag szDistTag szDatumTag szLyr PtOrgX PtOrgY PtOrgZ )
		)
	)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;(setq *ERROR* *ERR_OLD*)
    (princ)
)
(PrintLoadMsg 5)
(defun DrawQProfileStarterL(strPath / *ERR_OLD* GetQPRDataThruDCL listDwgParam dXScale dYScale dDatum dNumAnnSize dRowAnnSize dRowHt szElevTag szNorthingTag szEastingTag szCumDistTag szDistTag szDatumTag szLyr PtOrgX PtOrgY PtOrgZ )
	;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(setq *ERR_OLD* *ERROR*)
	;(defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun GetQPRDataThruDCL (/
							ModifyDataList EliminateNILVals SaveAllData RestoreAllData OnOk 
							returnList dcl_id listKeyMainDlg listDataMainDlg iDoneDialogFlag
							)
	   (defun ModifyDataList (listDataParam listPoint / i iLoopLim listRet iIndexOfXOrd)
		  (setq i 0 iLoopLim (length listDataParam) listRet (list ()))
		  (setq iIndexOfXOrd (- iLoopLim 3));; Point Ord Starts from third last index of the list
		  (while (< i iLoopLim)
			(if (and (>= i iIndexOfXOrd) (<= i (+ iIndexOfXOrd 3)))
			   (setq listRet (append listRet (list (rtos (nth (- i iIndexOfXOrd) listPoint)))))
 			   (setq listRet (append listRet (list (nth i listDataParam))))
			)
			(setq i (+ i 1))
		  );while
		  (setq listRet (cdr listRet))
	   )
	   (defun EliminateNILVals (listParam / i iLoopLim listRet)
		 (setq i 0 iLoopLim (length listParam) listRet (list ()))
		 (while (< i iLoopLim)
		   (if (= (nth i listParam) nil)
			  (setq listRet (append listRet (list "")))
			  (setq listRet (append listRet (list (nth i listParam))))
		   )
		   (setq i (+ i 1))
		 );while
		 (setq listRet (cdr listRet))
	   )
	   (defun SaveAllData () (setq listDataMainDlg (EliminateNILVals (mapcar 'get_tile listKeyMainDlg))))
	   (defun RestoreAllData () (mapcar 'set_tile listKeyMainDlg listDataMainDlg))
	   (defun OnOk (listKey / retList bError strProfName)
		 ;Return Statement
		  (setq retList (EliminateNILVals (mapcar 'get_tile listKey)) bError 0)

		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  (if (and (<= (atof (nth 0 retList)) 0.0) (= bError 0)) ;;"HorSca"
			(progn (set_tile "err" "ERROR: Bad Horz. scale") (mode_tile (nth 0 listKey) 2) (setq bError 1))
		  )
		  (if (and (<= (atof (nth 1 retList)) 0.0) (= bError 0)) ;;"VerSca"
			(progn (set_tile "err" "ERROR: Bad Vert. scale") (mode_tile (nth 1 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 3 retList)) 0.0) (= bError 0)) ;;"NumAnnSize"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 3 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 4 retList)) 0.0) (= bError 0)) ;;"RowAnnSize"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 4 listKey) 2) (setq bError 1));progn
		  )
		  (if (and (<= (atof (nth 5 retList)) 0.0) (= bError 0)) ;;"RowHt"
			(progn (set_tile "err" "ERROR: Invalid annotation attribute") (mode_tile (nth 5 listKey) 2) (setq bError 1));progn
		  )
		  (if (= bError 0) (done_dialog 0))
		  (setq retList retList)
	   )

	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (setq returnList nil dcl_id (load_dialog (strcat gstrSrvPlanPath "SurvPlan")))

	   (setq listKeyMainDlg (list "HorSca" "VerSca" "Datum" "NumAnnSize" "RowAnnSize" "RowHt" "ElevTag" "NorthingTag" "EastingTag" "CumDistTag" "DistTag" "DatumTag" "Layer" "OrdX" "OrdY" "OrdZ")
			 listDataMainDlg (list "5.0" "5.0" "0.0" "1.0" "2.0" "10" "Elevation :" "Northing :" "Easting :" "Cumulative Distance : " "Distance : " "Datum : " "QUICK_PROFILE" "0.0" "0.0" "0.0")
	   )
	   (setq iDoneDialogFlag 10)
	   (while (> iDoneDialogFlag 1)
		   (new_dialog "GetQProfileData" dcl_id)
		   (RestoreAllData)
   		   (action_tile "PointButt" "(progn (SaveAllData)(done_dialog 5))")
		   (action_tile "accept" "(setq returnList T listDataMainDlg (OnOk listKeyMainDlg))")
		   (action_tile "cancel" "(progn (setq returnList nil)(done_dialog 1))")
		   (setq iDoneDialogFlag (start_dialog))
		   (cond 
			 ((= iDoneDialogFlag 5)
			  (initget 1) (setq point (getpoint "\nPick Profile Plot Origin:"))
			  (setq listDataMainDlg (ModifyDataList listDataMainDlg point)) 
			 )
		   )
	   )
	   (unload_dialog dcl_id)
	   (if (= returnList nil)
		   (setq returnList nil)
		   (setq returnList listDataMainDlg)
	   )
	   (setq returnList returnList)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq gstrSrvPlanPath strPath)
	(setq listDwgParam (GetQPRDataThruDCL));;Dialog Interface return data
	(if(/= listDwgParam nil)
		(progn
			(if (= (member "survutil.arx" (ARX)) nil) (arxload (strcat gstrSrvPlanPath "SurvUtil.arx")))
			(setq 
					dXScale (atof (nth 0 listDwgParam))
					dYScale (atof (nth 1 listDwgParam))
					dDatum (atof (nth 2 listDwgParam))
					dNumAnnSize (atof (nth 3 listDwgParam))
					dRowAnnSize (atof (nth 4 listDwgParam))
					dRowHt (atof (nth 5 listDwgParam))

					szElevTag (nth 6 listDwgParam)
					szNorthingTag (nth 7 listDwgParam)
					szEastingTag (nth 8 listDwgParam)
					szCumDistTag (nth 9 listDwgParam)
					szDistTag (nth 10 listDwgParam)
					szDatumTag (nth 11 listDwgParam)
					szLyr (nth 12 listDwgParam)
	
					PtOrgX (atof (nth 13 listDwgParam))
					PtOrgY (atof (nth 14 listDwgParam))
					PtOrgZ (atof (nth 15 listDwgParam))
			)
			;;Call ARX 
			(DrawQProfileFrLines dXScale dYScale dDatum dNumAnnSize dRowAnnSize dRowHt szElevTag szNorthingTag szEastingTag szCumDistTag szDistTag szDatumTag szLyr PtOrgX PtOrgY PtOrgZ )
		)
	)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;(setq *ERROR* *ERR_OLD*)
    (princ)
)
(PrintLoadMsg 19)
(PrintLoadMsg 20)
(PrintLoadMsg 31)
(PrintLoadMsg 49)
(PrintLoadMsg 63)
(PrintLoadMsg 68)
(PrintLoadMsg 87)
(PrintLoadMsg 88)
(PrintLoadMsg 95)
(PrintLoadMsg 100)
(princ)
