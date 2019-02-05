;;SurvPr.LSP
(defun PrintLoadMsg(iPerCent) (if (= iPerCent 0) (princ "\nPlease wait...Loading neccessary files\n") (princ (strcat "\r" (itoa iPerCent) "% Loaded"))) (if (= iPerCent 100) (princ "\nLoading Successful\n"))(princ))
(PrintLoadMsg 0)
;;
;; 
;; Main Exe SubR >>> (DrawProfileStarter ??) defined in this File 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Main Exe SubR                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawProfileStarter(strPath / *ERR_OLD* listDwgParam strPRDataFile strPRDataFileXtn)
  ;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq *ERR_OLD* *ERROR*)
  (defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (= (atoi (getvar "acadver")) 14)
	(progn
		(alert "ERROR: This utility will not run on AutoCAD Release 14.X\nTry with older versions")
		(exit)
	)
  )
  
  (setq gstrSrvElevPath strPath gStrAppName "Atanu_Banik")
  (setq listDwgParam (GetDwgParam));;Dialog Interface return data
  
  (if (/= listDwgParam nil)
		(progn
			(setq strPRDataFile (car (car listDwgParam))
				  strPRDataFileXtn (substr strPRDataFile (- (strlen strPRDataFile) 2) (strlen strPRDataFile))
			)
			(if (= (strcase strPRDataFileXtn) "PRD")
				(DrawProfilePRD listDwgParam)
				(DrawProfilePRX listDwgParam)
			)
		)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq *ERROR* *ERR_OLD*)
  (princ)
)
(PrintLoadMsg 5)
;;Ok 101297
(defun DrawProfilePRD (listDwgParam / list_01 list_02 strDataFile hSrcFile fFrStation fEndStation  strPrGrphLyr strGenPrLyr iLabelRowMF listOrgOfGraph listReadNextOrdDataRet listVertFirst listVertLast bContinueDraw)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;Ok 111297
	(defun ReadNextOrdData (hSrcFile / XtractData strLine bReadStatus listData)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;Ok 111297
		(defun XtractData (strLine hSrcFile / listTemp iCode fChainage fElev strTextForAnn strBlockNameForAnn)
			(setq listTemp (XtractAllWords strLine))
			(if (< (length listTemp) 3)
				(progn
				    (alert (strcat "Error: Invalid data found!\nPlease check the following line in input file\n" strLine))
				    (close hSrcFile)(exit)
				)
			)
			(setq iCode (atoi (nth 0 listTemp))
				  fChainage (atof (nth 1 listTemp))
				  fElev (atof (nth 2 listTemp))
			)
			;;Check the bit code & corresponding data
			;******************************************************
			;*Bit Code Specification for each Line of Station data
			;*	0 = No Text/Block Annotation & Ordinate Line
			;*	1 = Only Draws Ordinate Line
			;*	2 = Only Draws Text Annotation
			;*	4 = Only Inserts the specified Blockname
			;******************************************************
            ;;For Text annotation
            (if (or (= iCode 2) (= iCode 3) (= iCode 6) (= iCode 7))
               (progn
                 (if (>= (length listTemp) 4)
                    (setq strTextForAnn (nth 3 listTemp))
                    (progn
						(alert (strcat "ERROR: Annotation Text Not Found at Chainage : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
					)
                 )
               )
            )
            ;;For Blk Name
            (if (or (= iCode 4) (= iCode 5) (= iCode 6) (= iCode 7))
               (progn
                 (if (>= (length listTemp) 4)
                    (progn
						(if (or (= iCode 4) (= iCode 5))
							(progn
								(setq strBlockNameForAnn (nth 3 listTemp))
							)
							(progn
								(if (>= (length listTemp) 5)
									(progn
										(setq strBlockNameForAnn (nth 4 listTemp))
									)
									(progn
										(alert (strcat "ERROR: Block Name Not Found at Chainage : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
										(close hSrcFile)(exit)
									)
								)
							)
						)
					)
                    (progn
						(alert (strcat "ERROR: Block Name Not Found at Chainage : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
					)
                 );if
               )
            );if
            ;;For Blk/Text Both
            (if (or (= iCode 6) (= iCode 7))
               (progn
                 (if (/= (length listTemp) 5)
                    (progn
						(alert (strcat "ERROR: Block Name Not Found at Chainage : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
                    )
                 );if
               )
            );if
			(setq listTemp (list iCode fChainage fElev strTextForAnn strBlockNameForAnn))
			(setq listTemp listTemp);;Return
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq strLine (read-line hSrcFile) bReadStatus 1)
		(while (= bReadStatus 1)
			(if (/= strLine nil)
				(progn
					(if (= (IsCommentLine strLine "*") nil)
						(progn
							(setq listData (XtractData strLine hSrcFile)
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
	;;;End of SubRs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq list_01 (nth 0 listDwgParam) list_02 (nth 1 listDwgParam))
	(setq strDataFile (nth 0 list_01)
		  hSrcFile (open strDataFile "r")
		  fFrStation (nth 5 list_01)
		  fEndStation (nth 6 list_01)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq strPrGrphLyr "SURVEY-PROFILE" strGenPrLyr "SURVEY-PROFILE-GEN")
  (CreateLayer (list strPrGrphLyr strGenPrLyr))
	(setq iLabelRowMF 5
		  ;;;1 >> 'Caz it's for PRD File
		  listOrgOfGraph (DrawAxis 1 listDwgParam iLabelRowMF strGenPrLyr)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listReadNextOrdDataRet T
		  listVertFirst nil listVertLast nil
		  bContinueDraw T
	)
	(while (and (/= listReadNextOrdDataRet nil) (= bContinueDraw T))
		(setq listReadNextOrdDataRet (ReadNextOrdData hSrcFile))
		(if (/= listReadNextOrdDataRet nil)
			(progn
				(setq fChainage (nth 1 listReadNextOrdDataRet))
				(if (> fChainage fEndStation)
					(progn 
						(setq bContinueDraw nil);;Don't draw any more.. exit normally
					)
				)
				(if (= bContinueDraw T)
					(progn
						(if (>= fChainage fFrStation)
							(progn
								;;Send listXDataForPRXFile param as nil...Caz it's for PRD file rendering
								(setq listVertFirst (DrawOrdinate nil listDwgParam listOrgOfGraph listReadNextOrdDataRet iLabelRowMF strPrGrphLyr))
								(if (/= listVertLast nil)
									(progn
										(LINE listVertFirst listVertLast strPrGrphLyr)
									)
								)
								(setq listVertLast listVertFirst)
							)
						)
					)
				)
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			)
		)
	)
	(close hSrcFile)
	(princ)
)
(defun DrawProfilePRX (listDwgParam / ReadNextOrdData FindHorzExtent strDataFile hSrcFile fFrStation fEndStation  strPrGrphLyr strGenPrLyr iLabelRowMF listOrgOfGraph listOrdData listVertFirst listVertLast list01 list02  listOrdDataPrev fNorthing fEasting)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;Ok 111297
	(defun ReadNextOrdData (hSrcFile / XtractData strLine bReadStatus listData)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;Ok 111297
		(defun XtractData (strLine hSrcFile / listTemp iCode fNorthing fEasting fElev strTextForAnn strBlockNameForAnn)
			(setq listTemp (XtractAllWords strLine))
			(if (< (length listTemp) 4)
				(progn
				    (alert (strcat "Error: Invalid data found!\nPlease check the following line in input file\n" strLine))
				    (close hSrcFile)(exit)
				)
			)
			(setq iCode (atoi (nth 0 listTemp))
				  fNorthing (atof (nth 1 listTemp))
				  fEasting (atof (nth 2 listTemp))
				  fElev (atof (nth 3 listTemp))
			)
			;;Check the bit code & corresponding data
			;******************************************************
			;*Bit Code Specification for each Line of Station data
			;*	0 = No Text/Block Annotation & Ordinate Line
			;*	1 = Only Draws Ordinate Line
			;*	2 = Only Draws Text Annotation
			;*	4 = Only Inserts the specified Blockname
			;******************************************************
            ;;For Text annotation
            (if (or (= iCode 2) (= iCode 3) (= iCode 6) (= iCode 7))
               (progn
                 (if (> (length listTemp) 4)
                    (setq strTextForAnn (nth 4 listTemp))
                    (progn
						(alert (strcat "ERROR: Annotation Text Not Found at Northing-Easting: " (nth 1 listTemp) (nth 2 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
					)
                 )
               )
            )
            ;;For Blk Name
            (if (or (= iCode 4) (= iCode 5) (= iCode 6) (= iCode 7))
               (progn
                 (if (> (length listTemp) 4)
                    (progn
						(if (or (= iCode 4) (= iCode 5))
							(progn
								(setq strBlockNameForAnn (nth 4 listTemp))
							)
							(progn
								(if (> (length listTemp) 5)
									(progn
										(setq strBlockNameForAnn (nth 5 listTemp))
									)
									(progn
										(alert (strcat "ERROR: Block Name Not Found at Northing-Easting: " (nth 1 listTemp) (nth 2 listTemp) "\nPlease Check the Input File and try again..."))
										(close hSrcFile)(exit)
									)
								)
							)
						)
					)
                    (progn
						(alert (strcat "ERROR: Block Name Not Found at Northing-Easting: " (nth 1 listTemp) (nth 2 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
					)
                 );if
               )
            );if
            ;;For Blk/Text Both
            (if (or (= iCode 6) (= iCode 7))
               (progn
                 (if (/= (length listTemp) 6)
                    (progn
						(alert (strcat "ERROR: Block Name Not Found at Northing-Easting: " (nth 1 listTemp) (nth 2 listTemp) "\nPlease Check the Input File and try again..."))
						(close hSrcFile)(exit)
                    )
                 );if
               )
            );if
			(setq listTemp (list iCode fNorthing fEasting fElev strTextForAnn strBlockNameForAnn))
			(setq listTemp listTemp);;Return
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq strLine (read-line hSrcFile) bReadStatus 1)
		(while (= bReadStatus 1)
			(if (/= strLine nil)
				(progn
					(if (= (IsCommentLine strLine "*") nil)
						(progn
							(setq listData (XtractData strLine hSrcFile)
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
	;;Returns the Horz. extent of the profile
	(defun FindHorzExtent (strDataFile / hSrcFile listOrdData i fNorthing fEasting listNE_01 listNE_02 fHorzExtent)
		(setq hSrcFile (open strDataFile "r"))
		(setq listOrdData T i 0 listNE_01 nil listNE_02 nil fHorzExtent 0.0)
		(while (/= listOrdData nil)
			(princ (strcat "\rPlease wait..Reading file ..line #" (itoa i)))
			(setq listOrdData (ReadNextOrdData hSrcFile))
			(if (/= listOrdData nil)
				(progn
					(setq fNorthing (nth 1 listOrdData)
						  fEasting (nth 2 listOrdData)
						  listNE_01 (list fNorthing fEasting)
						  i (+ i 1)
					)
					(if (and (/= listNE_01 nil) (/= listNE_02 nil))
						(setq fHorzExtent (+ fHorzExtent (distance (reverse listNE_01) (reverse listNE_02))))
					)
					(setq listNE_02 listNE_01)
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				)
			)
		)
		(close hSrcFile)
		(princ "\n")
		(setq fHorzExtent fHorzExtent);;;Return Value
	)
	;;;End of SubRs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq strDataFile (nth 0 (nth 0 listDwgParam))
		  fFrStation 0.0 
		  fEndStation (FindHorzExtent strDataFile)
		  ;;Modify 'listDwgParam' to change 'fFrStation' & 'fEndStation' values
		  list01 (nth 0 listDwgParam)
		  list02 (nth 1 listDwgParam)
		  list01 (SubstValInList list01 5 fFrStation)
		  list01 (SubstValInList list01 6 fEndStation)
		  listDwgParam (SubstValInList listDwgParam 0 list01)
	)
	(setq hSrcFile (open strDataFile "r"))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq strPrGrphLyr "SURVEY-PROFILE" strGenPrLyr "SURVEY-PROFILE-GEN")
	(CreateLayer (list strPrGrphLyr strGenPrLyr))
	(setq iLabelRowMF 5
		  ;;;1 >> 'Caz it's for PRX File
		  listOrgOfGraph (DrawAxis 0 listDwgParam iLabelRowMF strGenPrLyr)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listOrdData T listOrdDataPrev nil
		  listVertFirst nil listVertLast nil
	)
	(while (/= listOrdData nil)
		(setq listOrdData (ReadNextOrdData hSrcFile))
		(if (/= listOrdData nil)
			(progn
				(setq fNorthing (nth 1 listOrdData) fEasting (nth 2 listOrdData))
				(if (= listOrdDataPrev nil)
					(setq fChainage 0.0)
					(setq fChainage (+ fChainage (distance  (list (nth 2 listOrdData) (nth 1 listOrdData)) (list (nth 2 listOrdDataPrev) (nth 1 listOrdDataPrev)))))
				)
				(setq listOrdDataPrev listOrdData)
				;;Modify 'listOrdData' to replace  'fChainage' instead of N-E data
				(setq listOrdData (RemoveValFrList listOrdData 1)
					  listOrdData (SubstValInList listOrdData 1 fChainage)
				)
				;;;Annotate each Vertex.........................................
				;;Send listXDataForPRXFile param(N-E Data Values) ...'Caz it's for PRX file rendering
				(setq listVertFirst (DrawOrdinate (list fNorthing fEasting) listDwgParam listOrgOfGraph listOrdData iLabelRowMF strPrGrphLyr))
				(if (and (/= listVertFirst nil) (/= listVertLast nil))
					(LINE listVertFirst listVertLast strPrGrphLyr)
				)
				(setq listVertLast listVertFirst)
			)
		)
	)
	(close hSrcFile)
	(princ)
)
(PrintLoadMsg 19)
(defun FindCoOrd (listDwgParam listOrgPoint fChainage fElev / retList list_01 list_02 fMinElev fMinChainage fHorSca fVerSca)
	(setq list_01 (nth 0 listDwgParam) list_02 (nth 1 listDwgParam)
		  fMinChainage (nth 5 list_01) fHorSca (nth 3 list_01)
		  fMinElev (nth 1 list_01) fVerSca (nth 4 list_01)
	)
	;;Return
	(setq retList (list (+ (car listOrgPoint) (* (- fChainage fMinChainage) fHorSca)) (+ (cadr listOrgPoint) (* (- fElev fMinElev) fVerSca))))
)
(PrintLoadMsg 20)
;;Ok 111297
(defun DrawAxis (bIsForPRDFile listDwgParam iLabelRowMF strLyr / 
				 list_01 list_02 fMinElev fMaxElev fHorSca fVerSca fFrStation fToStation listProfOrgPoint
				 fLabelAnnSize fStnAnnSize strLabelChain strLabelLevel strLabelStation
				 fLabelRowHt iMaxLen fMaxLenOfText listGrphOrgPoint listGrphEndPoint
				 listProfStPointOnXAxis listProfEndPointOnXAxis listProfEndPointOnYAxis fTemp
				)
	(setq list_01 (nth 0 listDwgParam) list_02 (nth 1 listDwgParam))
	;(setq list_01 (list strProfileName fMinElev fMaxElev fHorSca fVerSca fFrStation fToStation listOrgPoint)
	;	   list_02 (list fNumAnnSize fLabelAnnSize fStnAnnSize strLabelChain strLabelLevel strLabelStation)
	;)
	(setq fMinElev (nth 1 list_01)
	      fMaxElev (nth 2 list_01)
	      fHorSca (nth 3 list_01)
	      fVerSca (nth 4 list_01)
	      fFrStation (nth 5 list_01)
	      fToStation (nth 6 list_01)
		  listProfOrgPoint (nth 7 list_01)
	)
	(setq fLabelAnnSize (nth 1 list_02)
	      fStnAnnSize (nth 2 list_02)
	      strLabelChain (nth 3 list_02)
	      strLabelLevel (nth 4 list_02)
	      strLabelStation (nth 5 list_02)
	)
	(setq fLabelRowHt (* iLabelRowMF fLabelAnnSize))
	;; Find the Max Length of Str
	(setq iMaxLen (* (max (strlen strLabelChain) (strlen strLabelLevel)) 1.2) ; 20% hike
		  fMaxLenOfText (* iMaxLen fLabelAnnSize 0.9)
	)
	(setq listGrphOrgPoint (list (+ (car listProfOrgPoint) fMaxLenOfText) (+ (cadr listProfOrgPoint) (* 2.0 fLabelRowHt)))
		  listGrphEndPoint (FindCoOrd listDwgParam listGrphOrgPoint fToStation fMinElev)
	)
	(setq listProfStPointOnXAxis (list (- (car listGrphOrgPoint) fMaxLenOfText) (cadr listGrphOrgPoint))
		  listProfEndPointOnXAxis listGrphEndPoint
		  listProfEndPointOnYAxis (FindCoOrd listDwgParam listGrphOrgPoint fFrStation fMaxElev)
	)
	;;Draw X-Axis Line
	(LINE listProfStPointOnXAxis listProfEndPointOnXAxis strLyr)
	;;Draw Y-Axis Line
	(LINE listGrphOrgPoint listProfEndPointOnYAxis strLyr)

	;;Other two lines parallel to X-Axis
	(LINE (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 1.0 fLabelRowHt))) (list (car listProfEndPointOnXAxis) (- (cadr listProfEndPointOnXAxis) (* 1.0 fLabelRowHt))) strLyr)
	(LINE (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 2.0 fLabelRowHt))) (list (car listProfEndPointOnXAxis) (- (cadr listProfEndPointOnXAxis) (* 2.0 fLabelRowHt))) strLyr)
	;;Add one xtra line parallel to X-Axis if it's for PRX file
	(if(= bIsForPRDFile 0)
		(LINE (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 3.0 fLabelRowHt))) (list (car listProfEndPointOnXAxis) (- (cadr listProfEndPointOnXAxis) (* 3.0 fLabelRowHt))) strLyr)
	)
	
	;;Vert Line joining xtreme left points
	(if(= bIsForPRDFile 1)
		(LINE (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 0.0 fLabelRowHt))) (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 2.0 fLabelRowHt))) strLyr)
		(LINE (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 0.0 fLabelRowHt))) (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (* 3.0 fLabelRowHt))) strLyr)
	)
	;;Draw Text "Cum. Chainage" Label==============for PRX file only
	;;Note: annotation txt is not flexible...
	(if(= bIsForPRDFile 0)
		(TEXTLEFT (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (+ (* 2.5 fLabelRowHt) (* 0.5 fLabelAnnSize)))) "Approx. Cum. Chainage (m)" 0.0 fLabelAnnSize  strLyr)
	)
	;;Draw Text Elevaton Label==============
	(TEXTLEFT (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (+ (* 0.5 fLabelRowHt) (* 0.5 fLabelAnnSize)))) strLabelLevel 0.0 fLabelAnnSize  strLyr)
	;;Draw Text Chainage Label==============
	(TEXTLEFT (list (car listProfStPointOnXAxis) (- (cadr listProfStPointOnXAxis) (+ (* 1.5 fLabelRowHt) (* 0.5 fLabelAnnSize)))) strLabelChain 0.0 fLabelAnnSize  strLyr)
	;;Draw Text DATUM Label==============
	(TEXTLEFT (list (car listProfStPointOnXAxis) (+ (cadr listProfStPointOnXAxis) (+ (* 0.0 fLabelRowHt) (* 0.5 fLabelAnnSize)))) (strcat "DATUM=" (rtos fMinElev 2)) 0.0 fLabelAnnSize  strLyr)

	;;Draw Profile Caption ==============
	(setq fTemp (/ (distance listProfStPointOnXAxis listProfEndPointOnXAxis) 2.0))
	(TEXTMID (list (+ (car listProfStPointOnXAxis) fTemp) (- (cadr listProfStPointOnXAxis) (* 4.0 fLabelRowHt))) strLabelStation 0.0 fStnAnnSize strLyr)
	
	(setq listGrphOrgPoint listGrphOrgPoint) ;;Return
)
(PrintLoadMsg 31)
;;Ok 111297
(defun DrawOrdinate(listXDataForPRXFile listDwgParam listOrgPoint listOrdData iLabelRowMF strLyr /
					list_01 list_02 fMinElev fMaxElev fHorSca fVerSca fFrStation fToStation listProfOrgPoint
					fNumAnnSize fLabelAnnSize fStnAnnSize strLabelChain strLabelLevel strLabelStation
					fLabelRowHt listTopPoint listBotPoint iCode strAnnText strBlkName bAnnotationDone strChainageAnn entlityLast
				    )
	(princ (strcat "\rDrawing Ordinate at Chainage : " (rtos (nth 1 listOrdData) 2) "             "))
	(setq list_01 (nth 0 listDwgParam) list_02 (nth 1 listDwgParam))
	(setq fMinElev (nth 1 list_01)
	      fMaxElev (nth 2 list_01)
	      fHorSca (nth 3 list_01)
	      fVerSca (nth 4 list_01)
	      fFrStation (nth 5 list_01)
	      fToStation (nth 6 list_01)
		  listProfOrgPoint (nth 7 list_01)
	)
	(setq fNumAnnSize (nth 0 list_02)
	      fLabelAnnSize (nth 1 list_02)
	      fStnAnnSize (nth 2 list_02)
	      strLabelChain (nth 3 list_02)
	      strLabelLevel (nth 4 list_02)
	      strLabelStation (nth 5 list_02)
	)
	(setq fLabelRowHt (* iLabelRowMF fLabelAnnSize))

	(setq listTopPoint (FindCoOrd listDwgParam listOrgPoint (nth 1 listOrdData) (nth 2 listOrdData))
	      listBotPoint (list (car listTopPoint) (cadr listOrgPoint))
	)
	(setq iCode (nth 0 listOrdData)
		  strAnnText (nth 3 listOrdData)
		  strBlkName (nth 4 listOrdData)
	)
	;;Draw Ordinate Line
	(if (or (= iCode 1) (= iCode 3) (= iCode 5) (= iCode 7))
		(LINE listTopPoint listBotPoint strLyr)
	)
	(setq fTickSizeMFwrtText 0.4 fTickSize (* fTickSizeMFwrtText fNumAnnSize))
	;>>>>>>>>>>>>>Elevation Row Annotation>>>>>>>>>>>>>>>>>>>>>>>>>>
	;;Tick Mark(top)
	(LINE (list (car listBotPoint) (cadr listBotPoint)) (list (car listBotPoint) (- (cadr listBotPoint) fTickSize)) strLyr)
	;;Tick Mark(Bottom)
	(LINE (list (car listBotPoint) (- (cadr listBotPoint) (* 1.0 fLabelRowHt))) (list (car listBotPoint) (- (cadr listBotPoint) (- (* 1.0 fLabelRowHt) fTickSize))) strLyr)
	;;Elev Text
	(setq entlityLast (TEXTMID (list (car listBotPoint) (- (cadr listBotPoint) (* 0.5 fLabelRowHt))) (rtos (nth 2 listOrdData) 2) 90.0 fNumAnnSize strLyr))
	(AddStrXData entlityLast gStrAppName "PR?:ELEV")

	;>>>>>>>>>>>>>Chainage Row Annotation>>>>>>>>>>>>>>>>>>>>>>>>>>
	;;Tick Mark(top)
	(LINE (list (car listBotPoint) (- (cadr listBotPoint) (* 1.0 fLabelRowHt))) (list (car listBotPoint) (- (cadr listBotPoint) (* 1.0 fLabelRowHt) fTickSize)) strLyr)
	;;Tick Mark(Bottom)
	(LINE (list (car listBotPoint) (- (cadr listBotPoint) (* 2.0 fLabelRowHt))) (list (car listBotPoint) (- (cadr listBotPoint) (- (* 2.0 fLabelRowHt) fTickSize))) strLyr)
	;;Chainage Text
	(if (/= listXDataForPRXFile nil)
		(progn
			(setq strChainageAnn (rtos (nth 0 listXDataForPRXFile) 2))
			(setq entlityLast (TEXTMID (list (- (car listBotPoint) fNumAnnSize) (- (cadr listBotPoint) (* 1.5 fLabelRowHt))) strChainageAnn 90.0 fNumAnnSize strLyr))
			(AddStrXData entlityLast gStrAppName "PR?:NORT")
			(setq strChainageAnn (rtos (nth 1 listXDataForPRXFile) 2))
			(setq entlityLast (TEXTMID (list (+ (car listBotPoint) fNumAnnSize) (- (cadr listBotPoint) (* 1.5 fLabelRowHt))) strChainageAnn 90.0 fNumAnnSize strLyr))
			(AddStrXData entlityLast gStrAppName "PR?:EAST")

			;;Cum. Chainage
			(setq entlityLast (TEXTMID (list (car listBotPoint) (- (cadr listBotPoint) (* 2.5 fLabelRowHt))) (rtos (nth 1 listOrdData) 2) 90.0 fNumAnnSize strLyr))
			(AddStrXData entlityLast gStrAppName "PR?:CHAI")

			;;Tick Mark(top)
			(LINE (list (car listBotPoint) (- (cadr listBotPoint) (* 2.0 fLabelRowHt))) (list (car listBotPoint) (- (cadr listBotPoint) (* 2.0 fLabelRowHt) fTickSize)) strLyr)
			;;Tick Mark(Bottom)
			(LINE (list (car listBotPoint) (- (cadr listBotPoint) (* 3.0 fLabelRowHt))) (list (car listBotPoint) (- (cadr listBotPoint) (- (* 3.0 fLabelRowHt) fTickSize))) strLyr)
		)
		(progn
			(setq entlityLast (TEXTMID (list (car listBotPoint) (- (cadr listBotPoint) (* 1.5 fLabelRowHt))) (rtos (nth 1 listOrdData) 2) 90.0 fNumAnnSize strLyr))
			(AddStrXData entlityLast gStrAppName "PR?:CHAI")
		)
	)
	
	;;To be modified >>>>>>>>>>>>>>>>>>>>
	;******************************************************
	;*Bit Code Specification for each Line of Station data
	;*	0 = No Text/Block Annotation & Ordinate Line
	;*	1 = Only Draws Ordinate Line
	;*	2 = Only Draws Text Annotation
	;*	4 = Only Inserts the specified Blockname
	;******************************************************
	(setq bAnnotationDone nil)
	(if (and (/= strAnnText nil) (/= strBlkName nil))
		(progn
			;; Blk & Text Both
			(if (= (BLOCK strBlkName listTopPoint fNumAnnSize fNumAnnSize 0.0 strLyr) nil)
				(progn
					(if (/= strBlkName nil)
						(alert (strcat "ERROR: Block \"" strBlkName "\" Not Found !"))
						(princ "\nERROR: Exception occured, check input file\n")
					);
				);
			)
			(TEXTLEFT (list (+ (car listTopPoint) (* 0.5 fNumAnnSize)) (+ (cadr listTopPoint) (* fNumAnnSize 4.0))) strAnnText 90.0 fNumAnnSize strLyr)
			(setq bAnnotationDone T)
		)
	)
	(if (and (/= strAnnText nil) (= bAnnotationDone nil))
		(progn
			(TEXTLEFT (list (+ (car listTopPoint) (* 0.5 fNumAnnSize)) (+ (cadr listTopPoint) (* fNumAnnSize 2.0))) strAnnText 90.0 fNumAnnSize strLyr)
			(setq bAnnotationDone T)
		)
	)
	(if (and (/= strBlkName nil) (= bAnnotationDone nil))
		(progn
			(if (= (BLOCK strBlkName listTopPoint fNumAnnSize fNumAnnSize 0.0 strLyr) nil)
				(progn
					(if (/= strBlkName nil)
						(alert (strcat "ERROR: Block \"" strBlkName "\" Not Found !"))
						(princ "\nERROR: Exception occured, check input file\n")
					);
				);
			)
			(setq bAnnotationDone T)
		)
	)
	;;To be modified >>>>>>>>>>>>>>>>>>>>
	(setq listTopPoint listTopPoint);;Return Vertex
)
(PrintLoadMsg 49)
(defun GetFileXtn (strFile / strFileXtn)
	(if (>= (strlen strFile) 3)
		(setq strFileXtn (substr strFile (- (strlen strFile) 2) (strlen strFile)))
		(setq strFileXtn nil)
	)
	(setq strFileXtn strFileXtn)
)
(defun GetDwgParam ( / ModifyRawDlgData listDwgParam iTemp)
   (defun ModifyRawDlgData (listDwgParam / listReturn list_01 list_02 strProfileName fMinElev fMaxElev fHorSca fVerSca fFrStation fToStation listOrgPoint fNumAnnSize fLabelAnnSize fStnAnnSize strLabelChain strLabelLevel strLabelStation )
		(setq list_01 (nth 0 listDwgParam) list_02 (nth 1 listDwgParam))
		(setq strProfileName (nth 0 list_01)
			  fMinElev (atof (nth 1 list_01))
			  fMaxElev (atof (nth 2 list_01))
			  fHorSca (atof (nth 3 list_01))
			  fVerSca (atof (nth 4 list_01))
			  fFrStation (atof (nth 5 list_01))
			  fToStation (atof (nth 6 list_01))
			  listOrgPoint (list (atof (nth 7 list_01)) (atof (nth 8 list_01)) (atof (nth 9 list_01)))
		)
		(setq fNumAnnSize (atof (nth 0 list_02))
			  fLabelAnnSize (atof (nth 1 list_02))
			  fStnAnnSize (atof (nth 2 list_02))
			  strLabelChain (nth 3 list_02)
			  strLabelLevel (nth 4 list_02)
			  strLabelStation (nth 5 list_02)
		)
		(setq list_01 (list strProfileName fMinElev fMaxElev fHorSca fVerSca fFrStation fToStation listOrgPoint)
			  list_02 (list fNumAnnSize fLabelAnnSize fStnAnnSize strLabelChain strLabelLevel strLabelStation)
			  listReturn (list list_01 list_02)
		)
		(setq listReturn listReturn);;Return
   )
   (setq iTemp (GetPRDataType))
   (if (= iTemp 1) (setq listDwgParam (GetPRDataThruDCL "PRD")))
   (if (= iTemp 0) (setq listDwgParam (GetPRDataThruDCL "PRX")))

   (if (/= listDwgParam nil)
		(setq listDwgParam (ModifyRawDlgData listDwgParam))
		(princ "\nUser cancelled the dialog!")
   )
   (setq listDwgParam listDwgParam);;Return Val
)
(PrintLoadMsg 63)
(defun GetPRDataType(/ OnOk retValue dcl_id)
   (defun OnOk (/ retValue)
     ;Return Statement
	 (setq retValue (atoi (get_tile "ChainageButt")))
	 (done_dialog 0)
	 (setq retValue retValue)
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq dcl_id (load_dialog (strcat gstrSrvElevPath "SurvPlan")))

   (new_dialog "GetProfileType" dcl_id)
   (set_tile "ChainageButt" "1")
   (action_tile "accept" "(setq retValue (OnOk))")
   (action_tile "cancel" "(progn (setq retValue -1)(done_dialog 1))")
   (start_dialog)
   (unload_dialog dcl_id)
   (setq retValue retValue)
)
;For PRD file..............................................................
(defun GetPRDataThruDCL (strDefaultXtn /
						ModifyDataList EliminateNILVals EnableAllTiles SaveAllData RestoreAllData TileInit OnOk OnProfileNameEBox OnAnnTextButt OnFileDlgButton
						returnList dcl_id listAllkey listKeyMainDlg listKeyAnnTextDlg listDataMainDlg listDataAnnTextDlg iDoneDialogFlag
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
   (defun EnableAllTiles (strDefaultXtn bOnOffFlag / i iLoopLim bFlag)
       (setq i 1 iLoopLim (length listAllkey))
       (if (= bOnOffFlag 0) (setq bFlag 1) (setq bFlag 0))
       (while  (< i iLoopLim) (mode_tile (nth i listAllkey) bFlag) (setq i (+ i 1)))
       (if (= (strcase strDefaultXtn) "PRX")
			(progn
			   (mode_tile "BoxedRowStnFrTo" 1)
			   (mode_tile "FromStn" 1)
			   (mode_tile "UptoStn" 1)
			)
       )
   )
   (defun SaveAllData () (setq listDataMainDlg (EliminateNILVals (mapcar 'get_tile listKeyMainDlg))))
   (defun RestoreAllData () (mapcar 'set_tile listKeyMainDlg listDataMainDlg))
   (defun TileInit (iDoneDialogFlag strDefaultXtn / strAlgn bOnOffFlag)
     (setq strAlgn (car listDataMainDlg))
     (if (= (findfile strAlgn) nil)
	    (setq bOnOffFlag 0) (setq bOnOffFlag 1)
     )
     (EnableAllTiles strDefaultXtn bOnOffFlag) (RestoreAllData)
   )
   (defun OnOk (listKey / retList bError strProfName)
     ;Return Statement
	  (setq retList (EliminateNILVals (mapcar 'get_tile listKey)) bError 0)
	  (setq strProfName (nth 0 retList))

      (if (or (= (strlen strProfName) 0) (= (findfile strProfName) nil)) ;;"ProfileName"
    	 (progn (set_tile "err" "ERROR: Invalid Profile Data file") (mode_tile (nth 0 listKey) 2) (setq bError 1))
	  )
	  ;;Check is not neccessary because elevs can be anything
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;(if (and (<= (atof (nth 1 retList)) 0.0) (= bError 0)) ;;"MinElev"
	  ;  (progn (set_tile "err" "ERROR: Bad Min Elevation Value!") (mode_tile (nth 1 listKey) 2) (setq bError 1))
	  ;)
	  ;(if (and (<= (atof (nth 2 retList)) 0.0) (= bError 0)) ;;"MaxElev"
	  ;  (progn (set_tile "err" "ERROR: Bad Max Elevation Value!") (mode_tile (nth 2 listKey) 2) (setq bError 1))
	  ;)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  (if (and (<= (atof (nth 3 retList)) 0.0) (= bError 0)) ;;"HorSca"
	    (progn (set_tile "err" "ERROR: Bad Horz. Scale!") (mode_tile (nth 3 listKey) 2) (setq bError 1))
	  )
	  (if (and (<= (atof (nth 4 retList)) 0.0) (= bError 0)) ;;"VerSca"
	    (progn (set_tile "err" "ERROR: Bad Vert. Scale!") (mode_tile (nth 4 listKey) 2) (setq bError 1));progn
	  )
	  (if (= bError 0) (done_dialog 0))
	  (setq retList retList)
   )
   (defun OnProfileNameEBox ($key $value $reason strDefaultXtn / strFile bErr)
    (setq bErr 0 strFile (findfile $value))
    (if (not (= strFile nil))
      (progn
	   (setq bErr 0) (EnableAllTiles strDefaultXtn 1) (set_tile "err" "")
	  )
      (progn ;Disable
	   (setq bErr 1)
	   (EnableAllTiles strDefaultXtn 0) (set_tile "err" "Profile data not found!")
	  )
	)
   )
   (defun OnAnnTextButt ($key $value $reason dcl_id listKey listData / OnOk retList OnCancel)
	   ;====================================================================================
	   (defun OnOk(listKey / retList bError)
			(setq retList (EliminateNILVals (mapcar 'get_tile listKey)) bError 0)
			(cond 
			  ((and (<= (atof (nth 0 retList)) 0.0) (= bError 0));;"NumAnnSize"
			   (set_tile "err" "ERROR: Invalid numeric annotation text size")
			   (mode_tile (nth 0 listKey) 2) (setq bError 1)
			  )
			  ((and (<= (atof (nth 1 retList)) 0.0) (= bError 0));;"LabelAnnSize"
			   (set_tile "err" "ERROR: Invalid label annotation text size")
			   (mode_tile (nth 1 listKey) 2) (setq bError 1)
			  )
			  ((and (<= (atof (nth 2 retList)) 0.0) (= bError 0));;"StnAnnSize"
			   (set_tile "err" "ERROR: Invalid Station annotation text size")
			   (mode_tile (nth 2 listKey) 2) (setq bError 1)
			  )
			)
	        (if (= bError 0) (done_dialog 0))
			(setq retList retList)
	   )
	   (defun OnCancel()  (done_dialog 1) (setq retList listData))
	   ;====================================================================================
	   (new_dialog "GetPR12DataAnnotation" dcl_id)
	   (mapcar 'set_tile listKey listData)
	   (mode_tile "DatumLevel" 1) ;; Disable it
	   (action_tile "accept" "(setq retList (OnOk listKey))")
	   (action_tile "cancel" "(setq retList (OnCancel))")
	   (start_dialog)
       (setq retList retList);; Return
   )
   (defun OnFileDlgButton (strFiletype strTileFileEBox / strLabel strFname)
      (setq strLabel "Select Profile Data File")
      (setq strFname (getfiled strLabel "" strFiletype 2))
      (if (/= strFname nil) (set_tile strTileFileEBox strFname))
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq returnList nil dcl_id (load_dialog (strcat gstrSrvElevPath "SurvPlan")))

   (setq listAllkey (list "ProfileName" "MinElev" "MaxElev" "HorSca" "VerSca" "FromStn" "UptoStn" "OrdX" "OrdY" "OrdZ" "PointButt" "AnnTextButt")
		 listKeyMainDlg (list "ProfileName" "MinElev" "MaxElev" "HorSca" "VerSca" "FromStn" "UptoStn" "OrdX" "OrdY" "OrdZ")
         listKeyAnnTextDlg (list "NumAnnSize" "LabelAnnSize" "StnAnnSize" "LabelChain" "LabelLevel" "LabelStation" "DatumLevel")
   )
   (if (= (strcase strDefaultXtn) "PRD")
	   (setq listDataMainDlg (list (strcat (getvar "dwgname") "." strDefaultXtn) "100.0" "120.0" "1" "10" "0.000" "10.0" "0.0" "0.0" "0.0")
			 listDataAnnTextDlg (list "2.0" "3.0" "4.0" "Distance in Metre" "Reduced Level in Metre" "Profile Section" "Not Used")
	   )
	   (setq listDataMainDlg (list (strcat (getvar "dwgname") "." strDefaultXtn) "100.0" "120.0" "1" "10" "0.000" "0.000" "0.0" "0.0" "0.0")
			 listDataAnnTextDlg (list "2.0" "3.0" "4.0" "Co-Ordinate of C.L (N/E)" "Reduced Level in Metre" "Profile Section" "Not Used")
	   )
   )
   (setq iDoneDialogFlag 10)
   (while (> iDoneDialogFlag 1)
	   (new_dialog "GetProfileData" dcl_id)
	   (TileInit iDoneDialogFlag strDefaultXtn)
	   (action_tile "ProfileName" "(OnProfileNameEBox $key $value $reason strDefaultXtn)")
   	   (action_tile "AnnTextButt" "(setq listDataAnnTextDlg (OnAnnTextButt $key $value $reason dcl_id listKeyAnnTextDlg listDataAnnTextDlg))")
   	   (action_tile "PointButt" "(progn (SaveAllData)(done_dialog 5))")
	   (action_tile "accept" "(setq returnList T listDataMainDlg (OnOk listKeyMainDlg))")
	   (action_tile "ProfNameButton" "(progn (OnFileDlgButton strDefaultXtn (nth 0 listKeyMainDlg))(SaveAllData)(TileInit iDoneDialogFlag strDefaultXtn))")
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
	   (setq returnList (list listDataMainDlg listDataAnnTextDlg))
   )
   (setq returnList returnList)
)
(defun GetModifyAnnotationData(strPath / FindStrMatchInList Tile_Init OnOk OnRadioButton retValue dcl_id)
	(defun FindStrMatchInList (strToFind listOfStrs / i iIndex)
		(setq i 0 iIndex -1)
		(while (< i (length listOfStrs))
			(if (= (nth i listOfStrs) strToFind)
				(setq iIndex i i (length listOfStrs))
			)
			(setq i (+ i 1))
		)
		(setq iIndex iIndex);;Return Val
	)
	(defun Tile_Init (/ i)
		(setq i 0)
		(while (< i (length listEBkeys))
			(set_tile (nth i listEBkeys) (rtos (nth i listEBVals)))
			(setq i (+ i 1))
		)
		(setq i 0)
		(while (< i (length listRBkeys))
			(set_tile (nth i listRBkeys) (itoa (nth i listRBVals)))
			(setq i (+ i 1))
		)
		(set_tile (nth 0 listRBkeys) "1")
		(OnRadioButton (nth 0 listRBkeys) nil "1")
	)
	(defun OnOk (/ ConvertStrListToNumList i retList)
		(defun ConvertStrListToNumList(listParam bIsInt / i iLoopLim retList)
			(setq i 0 iLoopLim (length listParam) retList (list ()))
			(while (< i iLoopLim)
				(if (= bIsInt 1)
					(setq retList (append retList (list (atoi (nth i listParam)))))
					(setq retList (append retList (list (atof (nth i listParam)))))
				)
				(setq i (+ i 1))
			)
			(setq retList (cdr retList));Return
		)
		(setq listEBVals (ConvertStrListToNumList (mapcar 'get_tile listEBkeys) 0)
			  listRBVals (ConvertStrListToNumList (mapcar 'get_tile listRBkeys) 1)
		)
		(setq i 0)
		(while (< i (length listRBVals))
			(if (= (nth i listRBVals) 1)
				(setq retList (list i) i (length listRBVals))
			)
			(setq i (+ i 1))
		)
		(cond
			((= (car retList) 0) (setq retList (list (car retList) (nth 0 listEBVals))))
			((= (car retList) 1) (setq retList (list (car retList) (nth 1 listEBVals))))
			((= (car retList) 2) (setq retList (list (car retList) (nth 2 listEBVals))))
			((= (car retList) 3) (setq retList (list (car retList) (nth 3 listEBVals))))
			(t)
		)
		(done_dialog 0)
		(setq retList retList)
	)
	(defun OnRadioButton ($key $value $reason / iIndex)
		(setq iIndex (FindStrMatchInList $Key listRBkeys))
		;;Disables all the EBoxes 1st
		(mapcar 'mode_tile listEBkeys (list 1 1 1 1))
		(mode_tile (nth iIndex listEBkeys) 0)
	)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq dcl_id (load_dialog (strcat strPath "SurvPlan")))
   (setq listEBkeys (list "ElevEBox" "ChainageEBox" "NEBox" "EEBox")
		 listRBkeys (list "ElevButt" "ChainageButt" "NButt" "EButt")
		 listRBVals (list 0 0 1 0)
		 listEBVals (list 0.0 0.0 0.0 0.0)
   )

   (new_dialog "GetXDataType" dcl_id)
   (Tile_Init)
   (action_tile (nth 0 listRBkeys) "(OnRadioButton $key $value $reason)")
   (action_tile (nth 1 listRBkeys) "(OnRadioButton $key $value $reason)")
   (action_tile (nth 2 listRBkeys) "(OnRadioButton $key $value $reason)")
   (action_tile (nth 3 listRBkeys) "(OnRadioButton $key $value $reason)")
   (action_tile "accept" "(setq retValue (OnOk))")
   (action_tile "cancel" "(progn (setq retValue nil)(done_dialog 1))")
   (start_dialog)
   (unload_dialog dcl_id)
   (setq retValue retValue)
)
(PrintLoadMsg 68)
;;Utilities 
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
(PrintLoadMsg 75)
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
(PrintLoadMsg 79)
(defun IsCommentLine (strLine chComment / bRet)
    (setq strTemp (GetFirstWord strLine))
    (if (or (= strTemp "") (= (substr strTemp 1 1) chComment)) (setq bRet T) (setq bRet nil))
    (setq bRet bRet)
)
(defun D2R (fAng) (* fAng (/ pi 180.0)))
(defun R2D (fAng) (* (/ 180.0  pi) fAng))
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
(defun TEXTLEFT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str)  (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))) (entlast))
(defun TEXTMID (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str) (cons 71 0) (cons 72 4) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))) (entlast))
(defun TEXTRIGHT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle"))  (cons 1 str) (cons 71 0) (cons 72 2) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))) (entlast))
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
(PrintLoadMsg 83)
(defun CreateLayer (listOfLyrNameStr / strPrevLyr i iLoopLim iCmdEcho)
	(setq i 0 iLoopLim (length listOfLyrNameStr) strPrevLyr (getvar "clayer") iCmdEcho (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(while (< i iLoopLim)
		(if (= (tblsearch "LAYER" (nth i listOfLyrNameStr)) nil)
			(command "layer" "m" (nth i listOfLyrNameStr) "c" (+ i 1) "" ""s)
		)
		(setq i (+ i 1))
	)
	(if (> i 0) (command "layer" "s" strPrevLyr ""))
	(setvar "cmdecho" iCmdEcho)
	(princ)
)
(PrintLoadMsg 87)
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
(defun RemoveValFrList (listParam iIndex / i retList iLoopLim)
    (setq i 0 iLoopLim (length listParam) retList (list ()))
    (while (< i iLoopLim)
        (if (/= i iIndex)
            (setq retList (append retList (list (nth i listParam))))
        )
        (setq i (+ i 1))
    )
    (setq retList (cdr retList));Return
)
(PrintLoadMsg 88)
;;;;;;;;;;;;;;;;;;;;;;;;; Xtended Data Related Funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AddStrXData(entity strApp strAsXData / RegApplication listData listXData iSize bFlag)
	(defun RegApplication (strApp / bFlag)
	   (setq appname strApp bFlag nil)
	   (if (= (tblsearch "appid" strApp) nil)
		   (progn
				(setq bFlag (regapp strApp))
				(if (= bFlag nil) (princ (strcat "\nFATAL ERROR : \""  strApp "\" registration failed !")))
		   )
	   )
	   (setq bFlag bFlag);Return
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (= (tblsearch "appid" strApp) nil) (RegApplication strApp))

	(setq listData (entget entity) bFlag nil)
	(setq listXData (list '(1002 . "}"))
		  listXData (cons (cons 1000 strAsXData) listXData)
		  listXData (cons '(1002 . "{") listXData)
		  listXData (cons strApp listXData)
		  listXData (list -3 listXData)
		  iSize (xdsize listXData)
	)
	(if (< iSize (xdroom entity))     ; If there is room for more...
		(progn
			(setq listData (cons listXData listData) bFlag T)
			(entmod listData)
		)
		(princ (strcat "\nFATAL ERROR : Not enhough memory to proceed"))
	)
	(setq bFlag bFlag);Return
)
(PrintLoadMsg 92)
(defun FetchStrXData(entity strApp / listData)
	(setq listData (entget entity (list strApp))
		  listData (assoc -3 listData)
	)
	(if (/= listData nil)
		(setq listData (cdr (assoc 1000 (cdr (car (cdr listData))))))
	)
	(setq listData listData)
)
(defun C:XD(/ ent )
	(setq ent (car (entsel)))
	(princ "\nXData is : ")
	(princ (FetchStrXData ent gStrAppName))
	(princ "\n")(princ)
)
(defun UpdateAnnText(strPath / GetFilteredEntList UpdateEntities strApp strSearchTag)
	(defun GetFilteredEntList(strApp strSearchTag / IsValidEntity i j sset ssFiltered retVal)
		(defun IsValidEntity(ent strApp strSearchTag / listData retVal)
			(setq listData (entget ent (list strApp))
				  listData (assoc -3 listData)
				  retVal 0
			)
			(if (/= listData nil)
				(progn
					(setq listData (cdr (assoc 1000 (cdr (car (cdr listData))))))
					(if (and (/= listData nil) (= strSearchTag listData))
						(setq retVal 1)
					)
				)
			)
			(setq retVal retVal);;Return
		)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq sset nil)
		(while (= sset nil)
			(princ "\nSelect Entities to modify :")
			(setq sset (ssget))
			(if (= sset nil)
				(princ "\nERROR: No Entity selected...Pl. try again..")
			)
		)
		;;Filter Entieies.......
		(setq i 0 j 0 ssFiltered nil ssFiltered (ssadd))
		(while (< i (sslength sset))
			(if (= (IsValidEntity (ssname sset i) strApp strSearchTag) 1)
				(setq ssFiltered (ssadd (ssname sset i) ssFiltered));;Add to new SS
				(progn (setq j (+ j 1)) (princ (strcat "\r" (itoa j) " No. of files filtered out")))
			)
			(setq i (+ i 1))
		)
		(if (or (= ssFiltered nil) (<= (sslength ssFiltered) 0))
			(setq ssFiltered nil)
		)
		(setq ssFiltered ssFiltered);;Return Value
	)
	(defun UpdateEntities(strApp strSearchTag ValueToAdd / sset IsValidEntity i iLim listEnt strTxt)
		(setq sset (GetFilteredEntList strApp strSearchTag))
		(if (/=  sset nil)
			(progn
				(setq i 0 iLim (sslength sset))
				(while (< i iLim)
					(setq listEnt (entget (ssname sset i))
					      strTxt (cdr (assoc 1 listEnt))
						  strTxt (rtos (+ (atof strTxt) ValueToAdd))
						  listEnt (subst (cons 1 strTxt) (assoc 1 listEnt) listEnt);;Update entity list 
					)
					(entmod listEnt) ;;Update entity
					(setq i (+ i 1))
				)
			)
		)
		(setq sset sset);;Return
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq listParams (GetModifyAnnotationData strPath))
	(if(/= listParams nil)
		(progn
			(setq strApp "Atanu_Banik")
			(cond 
				((= (car listParams) 0) (setq strSearchTag "PR?:ELEV") (UpdateEntities strApp strSearchTag (cadr listParams)))
				((= (car listParams) 1) (setq strSearchTag "PR?:CHAI") (UpdateEntities strApp strSearchTag (cadr listParams)))
				((= (car listParams) 2) (setq strSearchTag "PR?:NORT") (UpdateEntities strApp strSearchTag (cadr listParams)))
				((= (car listParams) 3) (setq strSearchTag "PR?:EAST") (UpdateEntities strApp strSearchTag (cadr listParams)))
				(T)
			)
		)
	)
	(princ)
)
(PrintLoadMsg 95)
;;;;;;;;;;;;;;;;;;;;;;;;; Xtended Data Related Funcs END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(PrintLoadMsg 100)
(princ)
