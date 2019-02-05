;;XD Complient
(defun PrintLoadMsg(iPerCent) (if (= iPerCent 0) (princ "\nPlease wait...Loading neccessary files\n") (princ (strcat "\r" (itoa iPerCent) "% Loaded"))) (if (= iPerCent 100) (princ "\nLoading Successful\n"))(princ))
(PrintLoadMsg 0)
;;
;; 
;; Exe SubR >>> (DrawCSStarter ??) defined in this File
;; Exe SubR >>> (FileUpdAnnotation) defined in this 
;; 
;; 

(defun DrawCSStarter(strPath / *ERR_OLD*)
  ;;;;;Error Handler;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq *ERR_OLD* *ERROR*)
  (defun *ERROR* (voidVal) (princ "\nERROR: Can't continue due to invalid data !\nPlease check data files for validity !") (setq *ERROR* *ERR_OLD*) (princ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;
  (XDataSetup);;Attaching Xtended data............
  ;;;;;;;;;;;;;;;;;;;;
  (setq gstrSrvElevPath strPath)
  (DrawCustomCrossSecn)
  (setq *ERROR* *ERR_OLD*)
  (princ)
)
(defun UpdAnnotation(/ iAnnTypeCode fValueToAdd)
	(initget 1)
	(setq fValueToAdd (getreal "\nEnter value to add with Elevation Annotations :"))
	(if (/= fValueToAdd nil)
		(progn
			(if (/= fValueToAdd 0.0)
				(progn
					(UpdAnnTxt 0 fValueToAdd)
				)
				(progn
					(princ "\nNothing to do!")
				)
			)
		)
		(princ "\nInvalid input!")
	)
	(princ)
)
;;
;;DrawCS.lsp
;;C/S Drawing Routines
;;
;; Main Func for Drawing of All C/S
(defun DrawCustomCrossSecn (/ iPrevCmdEcho listParam bContinueDraw listPlotData  listPointOrg fStartStn fEndStn iRow iCol strSrcFileName strSrcFileNameXist hSrcFile i j OriginPoint fGrphWid fGrphHt listDrawStnReturn list_01 list_02 list_03 )
   (setq iPrevCmdEcho (getvar "cmdecho")) (setvar "cmdecho" 0)
   (setq listParam (DrawingParam));;DCL driver
   (if (= listParam nil) (setq bContinueDraw 0) (setq bContinueDraw 1))
   (if (= bContinueDraw 1)
     (progn
       (princ "\n")
	   (setq list_01 (nth 0 listParam) list_02 (nth 1 listParam) list_03 (nth 2 listParam))
	   (setq listPlotData (nth 4 listParam)
			 strSrcFileName (nth 0 list_01) 
			 fStartStn (nth 5 list_01) fEndStn (nth 6 list_01)
			 listPointOrg (nth 7 list_01)
			 iRow (nth 0 list_02) iCol (nth 1 list_02) 
	   )
	   (setq hSrcFile (open strSrcFileName "r"))

	   (setq i 0 j 0 OriginPoint listPointOrg  fGrphWid 0.0 fGrphHt 0.0)
	   (while (and (< i iRow) (= bContinueDraw 1))
		  (setq j 0 OriginPoint (list (car listPointOrg) (+ (cadr OriginPoint) (* 1.4 fGrphHt))))
		  (while (and (< j iCol) (= bContinueDraw 1))
			  (setq stnDataList (ReadNextStnData hSrcFile))
			  (if (= stnDataList nil)
				 (setq bContinueDraw 0)
				 (progn
					  ;;Added============
					  (if (and (>= (car stnDataList) fStartStn) (<= (car stnDataList) fEndStn))
							(setq bDrawCurStation 1)
							(setq bDrawCurStation 0)
					  )
					  (if (= bDrawCurStation 1)
						  (progn
							  (setq listDrawStnReturn (DrawStation stnDataList OriginPoint listParam))
							  (setq fGrphWid (car (nth 0 listDrawStnReturn)) fGrphHt (cadr (nth 0 listDrawStnReturn)))
							  (setq OriginPoint (list (+ (car OriginPoint) (* 1.4 fGrphWid)) (cadr OriginPoint)))
						  )
					  )
					  ;;Added============
				 )
			  )
			  (setq j (+ j 1))
		  );while
		  (setq i (+ i 1))
	   );while 
	   (close hSrcFile)
     );progn
   );if (= bContinueDraw 1)
   (princ)
   (setvar "cmdecho" iPrevCmdEcho)
)


(PrintLoadMsg 7)
;; LocVar: Upd
;; Draws A Station 
;;
(defun DrawStation (StnDataList OriginPoint listAllParam / ptVertexList fChainage PropCodeList PropOffList PropEleList PropTxtList PropBlkList listAllParam ptVertexList listMinMaxOffset)
   ;;; SubR Starts;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;
	;; returns the Max  Graph Size in a list (list HorDim VerDim) (list OrgShiftInX OrgShiftInY)
	;; 
	(defun GetGraphInfo (listAllParam listMinMaxOffset /  list_01	list_03 list_02 fMinOff fHorzScale fMinElev fVertScale strChainage strLevelCaption strCSLabelPrefix fNumAnnTextSize fLabelTextSize fCSLabelTextSize iMaxLen fMaxLenOfText retGrphWid retOrgShiftX fMaxLenOfText retGrphHt retOrgShiftY retList fRowHeight)
	  (setq list_01 (nth 0 listAllParam) list_02 (nth 1 listAllParam)
			list_03 (nth 2 listAllParam)
	  )
	  (if (= listMinMaxOffset nil)
		  (alert "Abnormal!!(DrawStation)")
		  (setq fMinOff (abs (nth 0 listMinMaxOffset)) fMaxOff (abs (nth 1 listMinMaxOffset)))
	  )
	  (setq fMinElev (nth 1 list_01) fMaxElev (nth 2 list_01)
			fHorzScale (nth 3 list_01) fVertScale (nth 4 list_01)
	  )
      (setq strChainage (nth 0 list_03)
            strLevelCaption  (nth 1 list_03)
            strCSLabelPrefix (nth 2 list_03)
            fNumAnnTextSize (nth 3 list_03)
			fLabelTextSize (nth 4 list_03)
			fCSLabelTextSize (nth 5 list_03)
			fRowHeight (nth 6 list_03)
	  )
	  ;; Find the Max Length of Str
	  (setq iMaxLen (* (max (strlen strLevelCaption) (strlen strChainage)) 1.2) ; 20% hike
			fMaxLenOfText (* (* iMaxLen fLabelTextSize) 0.9)
	  )
	  ;;for Return the width and Origin shift in X of graph
	  (setq retGrphWid (+ (* (+ fMinOff fMaxOff) fHorzScale) fMaxLenOfText)
			retOrgShiftX fMaxLenOfText
			retGrphHt (+ (* (- fMaxElev fMinElev) fVertScale) (* 3.0 fRowHeight)) 
			retOrgShiftY (* 3.0 fRowHeight)
	  )
	  (setq retList (list (list retGrphWid retGrphHt) (list retOrgShiftX retOrgShiftY)))
	)
	(defun DrawAxis (OriginPoint listAllParam listMinMaxOffset fLabelRowHt strLyrName
				 / list_01 list_02 list_03 fMinOff fMaxOff fHorzScale fMinElev fMaxElev fVertScale strChainage strLevelCaption strCSLabelPrefix fNumAnnTextSize fLabelTextSize fCSLabelTextSize UpLeftPtOfXAnn iMaxLen fMaxLenOfText ptTempSt ptTempEnd retList
                )
	  (setq list_01 (nth 0 listAllParam) list_02 (nth 1 listAllParam)
			list_03 (nth 2 listAllParam)
	  )
	  (setq fMinElev (nth 1 list_01) fMaxElev (nth 2 list_01)
			fHorzScale (nth 3 list_01) fVertScale (nth 4 list_01) 
			fMinOff (abs (nth 0 listMinMaxOffset))
			fMaxOff (abs (nth 1 listMinMaxOffset))
	  )
	  (setq strChainage (nth 0 list_03)
            strLevelCaption  (nth 1 list_03)
            strCSLabelPrefix (nth 2 list_03)
            fNumAnnTextSize (nth 3 list_03)
			fLabelTextSize (nth 4 list_03)
			fCSLabelTextSize (nth 5 list_03)
	  )
	  ;;Draw Horz Axis Line........................
	  (LINE OriginPoint (list (+ (* (+ fMinOff fMaxOff) fHorzScale) (car OriginPoint)) (cadr OriginPoint)) strLyrName)

	  ;;Draw Vert Axis Line
	  ;;(LINE OriginPoint (list (car OriginPoint) (+ (* (- fMaxElev fMinElev) fVertScale) (cadr OriginPoint))) strLyrName)
	  ;;^Above line is commented for A Field Survey Company>>

	  ;; Extra Annotation for Designline/Xisting line........................
	  (setq UpLeftPtOfXAnn (list (car OriginPoint) (- (cadr OriginPoint) (* 0 fNumAnnTextSize))))
      (setq iMaxLen (max (strlen strChainage) (strlen strLevelCaption)))
	  (setq iMaxLen (* iMaxLen 1.2) ; 20% hike
		   fMaxLenOfText (* (* iMaxLen fLabelTextSize) 0.9)
		   ptTempSt (list (- (car UpLeftPtOfXAnn) fMaxLenOfText) (cadr UpLeftPtOfXAnn))
		   ptTempEnd (list (+ (car OriginPoint) (* (+ fMinOff fMaxOff) fHorzScale)) (cadr ptTempSt))
	  )
	  (LINE ptTempSt (list (car ptTempSt) (- (cadr ptTempSt) (* 2.0 fLabelRowHt))) strLyrName)

	  ;;Draw DATUM Elevaton Label==============
	  (TEXTLEFT (list (car ptTempSt) (+ (cadr UpLeftPtOfXAnn) (* 0.1 fLabelRowHt))) (strcat "DATUM=" (rtos fMinElev 2 3) " M.") 0.0 fLabelTextSize  strLyrName)
	  ;;Draw Top of Elevation Line==============
	  (LINE ptTempSt ptTempEnd strLyrName)
	  ;;Draw Text Elevaton Label==============
	  (TEXTLEFT (list (car ptTempSt) (- (cadr ptTempSt) (* 0.67 fLabelRowHt))) strLevelCaption 0.0 fLabelTextSize  strLyrName)
	  ;;Draw Bottom of Elevation Line==============
	  ;(LINE (list (car ptTempSt) (- (cadr ptTempSt) fLabelRowHt)) (list (car ptTempEnd) (- (cadr ptTempEnd) fLabelRowHt)) strLyrName)

	  ;;Draw Chainage Label Text ==============
	  (TEXTLEFT (list (car ptTempSt) (- (cadr ptTempSt) (* 1.67 fLabelRowHt))) strChainage 0.0 fLabelTextSize  strLyrName)
	  ;;Bottom of Chainage Line
	  (LINE (list (car ptTempSt) (- (cadr ptTempSt) (* 2 fLabelRowHt))) (list (car ptTempEnd) (- (cadr ptTempSt) (* 2 fLabelRowHt))) strLyrName)

   	  ;For lines joining Ticks(Horz)
	  (LINE (list (car ptTempSt) (- (cadr ptTempSt) (* 0.0 fLabelRowHt) (* +1.0 fNumAnnTextSize))) (list (car ptTempEnd) (- (cadr ptTempSt) (* 0.0 fLabelRowHt) (* +1.0 fNumAnnTextSize))) strLyrName)
	  (LINE (list (car ptTempSt) (- (cadr ptTempSt) (* 1.0 fLabelRowHt) (* -0.5 fNumAnnTextSize))) (list (car ptTempEnd) (- (cadr ptTempSt) (* 1.0 fLabelRowHt) (* -0.5 fNumAnnTextSize))) strLyrName)
	  (LINE (list (car ptTempSt) (- (cadr ptTempSt) (* 1.0 fLabelRowHt) (* +0.5 fNumAnnTextSize))) (list (car ptTempEnd) (- (cadr ptTempSt) (* 1.0 fLabelRowHt) (* +0.5 fNumAnnTextSize))) strLyrName)
	  (LINE (list (car ptTempSt) (- (cadr ptTempSt) (* 2.0 fLabelRowHt) (* -1.0 fNumAnnTextSize))) (list (car ptTempEnd) (- (cadr ptTempSt) (* 2.0 fLabelRowHt) (* -1.0 fNumAnnTextSize))) strLyrName)

	  (setq retList nil) ; Return 
	)
	;; LocVar : UPD
	(defun FindDwgCoOrd (OriginPoint fOffset fElev listAllParam listMinMaxOffset / fTemp list_01 list_02 fMinOff fMaxOff fHorzScale fMinElev fVertScale fXOrd fYOrd retPoint fTemp)
	  (setq list_01 (nth 0 listAllParam) list_02 (nth 1 listAllParam))
	  (setq fHorzScale (nth 3 list_01)
			fMinOff (nth 0 listMinMaxOffset)
			fMaxOff (nth 1 listMinMaxOffset)
	  )
	  (setq fMinElev (nth 1 list_01) fMaxElev (nth 2 list_01)
			fVertScale (nth 4 list_01)
	  )
	  (setq fTemp (* (+ (abs fMaxOff) (abs fMinOff)) fHorzScale))
	  (if (< fOffset 0.0)
		  (setq fXOrd (* (- (abs fMinOff) (abs fOffset)) fHorzScale))
		  (setq fXOrd (- fTemp (* (- fMaxOff fOffset) fHorzScale)))
	  )
	  (setq fXOrd (+ (car OriginPoint) fXOrd)
			fYOrd (+ (cadr OriginPoint) (* (- fElev fMinElev) fVertScale))
	  )
	  (setq retPoint (list fXOrd fYOrd));; Return
	)
	;Now Draw All Proposed Points Ordinates 
	;; LocVar : UPD
	(defun DrawAllOrds (OriginPoint listBitCode listOffset listElevattion listTextString listBlockName listAllParam listMinMaxOffset strLyr / i iLoopLim ptVertexList ptOnGraph ptVertexList bDrawVertOrds)
	  (setq i 0 iLoopLim (length listOffset) ptVertexList (list ()))
	  ;; Draw the Ordinates 
	  (while (< i iLoopLim)
		(setq ptOnGraph (FindDwgCoOrd OriginPoint (nth i listOffset) (nth i listElevattion) listAllParam listMinMaxOffset)
			  ptVertexList (append ptVertexList (list ptOnGraph))
		)
		(setq bDrawVertOrds (nth i listBitCode))
        ;******************************************************
        ;*Bit Code Specification for each Line of Station data
        ;*	0 = No Text/Block Annotation & Ordinate Line
        ;*	1 = Only Draws Ordinate Line
        ;*	2 = Only Draws Text Annotation
        ;*	4 = Only Inserts the specified Blockname
        ;******************************************************
		(if (or (= bDrawVertOrds 1) (= bDrawVertOrds 3) (= bDrawVertOrds 5) (= bDrawVertOrds 7))
           (LINE ptOnGraph (list (car ptOnGraph) (cadr OriginPoint)) strLyr)
        )
		(setq i (+ i 1))
	  )
	  (setq ptVertexList (cdr ptVertexList))
	  ;; Draw the Top Surface
	  (setq i 1 iLoopLim (length ptVertexList))
	  (while (< i iLoopLim)
		(LINE (nth (- i 1) ptVertexList) (nth i ptVertexList) strLyr)
		(setq i (+ i 1))
	  )
	  (setq ptVertexList ptVertexList);; Return Statement
	)
	;; LocVar : Upd
	(defun AnnotateAllVertex (listBitCode ptVertexList listTextString listBlockName strLyrName fTextSize / i iLoopLim iCode iTxtListIndex iBlkListIndex ptVertex strAnnText strBlkName)
	  (setq i 0 iLoopLim (length ptVertexList))
	  (setq iCode 0 iTxtListIndex 0 iBlkListIndex 0)
	  (while (< i iLoopLim)
		(setq ptVertex (nth i ptVertexList)
			  iCode (nth i listBitCode)
		)
        ;******************************************************
        ;*Bit Code Specification for each Line of Station data
        ;*	0 = No Text/Block Annotation & Ordinate Line
        ;*	1 = Only Draws Ordinate Line
        ;*	2 = Only Draws Text Annotation
        ;*	4 = Only Inserts the specified Blockname
        ;******************************************************
		(if (or (= iCode 2) (= iCode 3) (= iCode 6) (= iCode 7));; Text Only 
		  (progn
			(setq strAnnText (nth iTxtListIndex listTextString) iTxtListIndex (+ iTxtListIndex 1))
			(if (or (= iCode 6) (= iCode 7)) ;; Blk & Text Both / Only Text
			  (TEXTLEFT (list (+ (car ptVertex) (* 0.5 fTextSize)) (+ (cadr ptVertex) (* fTextSize 4.0))) strAnnText 90.0 fTextSize strLyrName)
			  (TEXTLEFT (list (+ (car ptVertex) (* 0.5 fTextSize)) (+ (cadr ptVertex) (* fTextSize 0.0))) strAnnText 90.0 fTextSize strLyrName)
			)
		  )
		)
		(if (or (= iCode 4) (= iCode 5) (= iCode 6) (= iCode 7))
		  (progn
			(setq strBlkName (nth iBlkListIndex listBlockName) iBlkListIndex (+ iBlkListIndex 1))
			(if (= (BLOCK strBlkName ptVertex fTextSize fTextSize 0.0 strLyrName) nil)
                (progn
                    (if (/= strBlkName nil)
                        (alert (strcat "ERROR: Block \"" strBlkName "\" Not Found !"))
                        (princ "\nERROR: Exception occured, check input file\n")
                    );
                );
            )
		  )
		)
		(setq i (+ i 1))
	  )
	)
	;LocVar:UPD
	(defun AnnotateChainage (OriginPoint listAllParam listMinMaxOffset fLabelRowHt listOffset strLyrName / i iLoopLim fTextSize fDepthBelowHorAxis fCurOff ptMidOfText ptMidOfText strXDataMain strXDataSub entXData)
     (setq i 0 iLoopLim (length listOffset)
           fTextSize (nth 3 (nth 2 listAllParam))
           fDepthBelowHorAxis (* 1.0 fLabelRowHt)
           fYOrd (- (cadr OriginPoint) (+ fDepthBelowHorAxis (* 0.5 fLabelRowHt)))
     )
     (while (< i iLoopLim)
		(setq fCurOff (nth i listOffset)
		      ptMidOfText (FindDwgCoOrd OriginPoint fCurOff 0.0 listAllParam listMinMaxOffset)
			  ptMidOfText (list (car ptMidOfText) (- (cadr OriginPoint) (+ fDepthBelowHorAxis (* 0.5 fLabelRowHt))))
		)
		(TEXTMID ptMidOfText (rtos fCurOff) 90.0 fTextSize strLyrName)
		;;Extended data .........
		(setq strXDataMain "CSDData" strXDataSub "OFFSET" entXData (entlast))
		(AttatchXData entXData strXDataMain strXDataSub)

		;For Ticks
		(LINE (list (car ptMidOfText) (- fYOrd (* 0.5 fLabelRowHt))) (list (car ptMidOfText) (+ (- fYOrd (* 0.5 fLabelRowHt)) (* 1.0 fTextSize))) strLyrName)
		(LINE (list (car ptMidOfText) (+ fYOrd (* 0.5 fLabelRowHt))) (list (car ptMidOfText) (- (+ fYOrd (* 0.5 fLabelRowHt)) (* 0.5 fTextSize))) strLyrName)
        
        (setq i (+ i 1))
     )
	)
	;LocVar:UPD
	(defun AnnotateLevel (OriginPoint ptVertexList listElevattion strLyrName listAllParam fLabelRowHt / fTextsize fDepthBelowHorAxis fYOrd i iLoopLim ptMidOfText strXDataMain strXDataSub entXData)
	  (setq fTextsize (nth 3 (nth 2 listAllParam))
            fDepthBelowHorAxis (* 0.0 fLabelRowHt)
            fYOrd (- (cadr OriginPoint) (+ fDepthBelowHorAxis (* 0.5 fLabelRowHt)))
      )
	  (setq i 0 iLoopLim (length ptVertexList))
	  (while (< i iLoopLim)
		(setq ptMidOfText (list (car (nth i ptVertexList)) fYOrd))
		(TEXTMID ptMidOfText (rtos (nth i listElevattion)) 90.0 fTextSize strLyrName)
		
		;;Extended data .........
		(setq strXDataMain "CSDData" strXDataSub "ELEV" entXData (entlast))
		(AttatchXData entXData strXDataMain strXDataSub)
		
		;For Ticks
		(LINE (list (car ptMidOfText) (- fYOrd (* 0.5 fLabelRowHt))) (list (car ptMidOfText) (+ (- fYOrd (* 0.5 fLabelRowHt)) (* 0.5 fTextSize))) strLyrName)
		(LINE (list (car ptMidOfText) (+ fYOrd (* 0.5 fLabelRowHt))) (list (car ptMidOfText) (- (+ fYOrd (* 0.5 fLabelRowHt)) (* 1.0 fTextSize))) strLyrName)
		(setq i (+ i 1))
	  )
	)
	;LocVar:UPD
	(defun AnnotateStationTitle (OriginPoint fChainage listAllParam listMinMaxOffset strTitleSuffix fLabelRowHt strLyrName / fCSLabelTextSize fYOrd ptMidPt ptMidPt list_01 list_02 list_03 fHorzScale fMinOff fMaxOff iMaxLen fLabelTextSize fMaxLenOfText fTmp strChainage strLevelCaption)
	  (setq list_01 (nth 0 listAllParam) list_02 (nth 1 listAllParam)
			list_03 (nth 2 listAllParam)
	  )
	  (setq fHorzScale (nth 3 list_01))
	  (setq fMinOff (abs (nth 0 listMinMaxOffset)) fMaxOff (abs (nth 1 listMinMaxOffset)))
	  (setq fLabelTextSize (nth 4 list_03))
	  (setq strChainage (nth 0 list_03)
            strLevelCaption  (nth 1 list_03)
	  )

	  ;; Extra Annotation for Designline/Xisting line........................
      (setq iMaxLen (max (strlen strChainage) (strlen strLevelCaption)))
	  (setq iMaxLen (* iMaxLen 1.2) ; 20% hike
		   fMaxLenOfText (* (* iMaxLen fLabelTextSize) 0.9)
		   fTmp (* 0.5 (+ (* (+ fMinOff fMaxOff) fHorzScale) fMaxLenOfText))
		   ptMidPt (list (+ fTmp (- (car OriginPoint) fMaxLenOfText)) (cadr OriginPoint))
	  )
	  ;;;;;;;;;;;;
	  (setq fCSLabelTextSize (nth 5 (nth 2 listAllParam))
            fYOrd (- (cadr OriginPoint) (* 2.0 fLabelRowHt) (* 1.5 fCSLabelTextSize))
			ptMidPt (list (car ptMidPt) fYOrd)
	  )
	  (TEXTMID ptMidPt (strcat strTitleSuffix (rtos fChainage) " M.") 0.0 (* fCSLabelTextSize 1.5) strLyrName)
	  (princ (strcat "\rDrawing Cross Section at Chainage: " (rtos fChainage)))
	)
	;;; SubR End ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (= StnDataList nil) (progn (alert "Error occured while drawing 'DrawStation'\nAbout to Exit......") (exit)))
	
	;; Proposed Terrain Data
	(if (= StnDataList nil)
	  (setq fChainageProp nil PropCodeList nil PropOffList nil PropEleList nil PropTxtList nil PropBlkList nil)
	  (setq fChainageProp (nth 0 StnDataList) PropCodeList (nth 1 StnDataList)
			PropOffList (nth 2 StnDataList) PropEleList (nth 3 StnDataList)
			PropTxtList (nth 4 StnDataList) PropBlkList (nth 5 StnDataList)
	  )
	)
	(setq listMinMaxOffset (list (FindMinFrList PropOffList) (FindMaxFrList PropOffList)))
	(setq retList (GetGraphInfo listAllParam listMinMaxOffset));; Return Statement
	(setq OriginPoint (list (+ (car OriginPoint) (car (nth 1 retList))) (+ (cadr OriginPoint) (cadr (nth 1 retList)))))

	(setq fLabelRowHt (* (nth 4 (nth 2 listAllParam)) 4.0)
		  fLabelRowHt (nth 6 (nth 2 listAllParam))
	)

	;; LayerSetting...............
	(setq strCSGrphLyr "SURVEY-C-SECN" strGenCSecnLyr "SURVEY-C-SECN-GEN")
	(CreateLayer (list strCSGrphLyr strGenCSecnLyr))
	;;for drawing Axis synchronizing with offset Min-Max data
	(DrawAxis OriginPoint listAllParam listMinMaxOffset fLabelRowHt strGenCSecnLyr)

	(setvar "luprec" 3)
	(AnnotateChainage OriginPoint listAllParam listMinMaxOffset fLabelRowHt PropOffList strGenCSecnLyr)
	(if (= StnDataList nil)
	  (setq ptVertexList nil)
	  (progn
		 (setq ptVertexList (DrawAllOrds OriginPoint PropCodeList PropOffList PropEleList PropTxtList PropBlkList listAllParam listMinMaxOffset strCSGrphLyr))
		 (AnnotateAllVertex PropCodeList ptVertexList PropTxtList PropBlkList strCSGrphLyr (nth 3 (nth 2 listAllParam)))
		 (setvar "luprec" 3)
		 (AnnotateLevel OriginPoint ptVertexList PropEleList strCSGrphLyr listAllParam fLabelRowHt)
		 (setq fChainage fChainageProp)
	  )
	)
	(setq strTitleSuffix (nth 2 (nth 2 listAllParam)))

	(setvar "luprec" 3)
	(AnnotateStationTitle OriginPoint fChainage listAllParam listMinMaxOffset strTitleSuffix fLabelRowHt strGenCSecnLyr)
	(setq retList (GetGraphInfo listAllParam listMinMaxOffset));; Return Statement
)
(PrintLoadMsg 25)


;;
;; DrawCS02.LSP
;; For C/S Data Generator from DCL & File Read
;;

;; DCL Driver
;; Return
;; (list strAlgnName (list bConvProp bDrawOrdProp bDrawCLProp) (list bConvXist bDrawOrdXist bDrawCLXist))
(defun GetCSDataThruDCL (/
						ModifyDataList EliminateNILVals EnableAllTiles SaveAllData RestoreAllData TileInit OnOk OnAlgnEBox OnAnnTextButt OnGraphPosnButt OnFileDlgButton
						returnList dcl_id listAllkey listKeyMainDlg listKeyAnnTextDlg listKeyGrphPosnDlg listDataMainDlg listDataAnnTextDlg listDataGrphPosnDlg iDoneDialogFlag
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
   (defun EnableAllTiles (bOnOffFlag / i iLoopLim bFlag)
       (setq i 1 iLoopLim (length listAllkey))
       (if (= bOnOffFlag 0) (setq bFlag 1) (setq bFlag 0))
       (while  (< i iLoopLim) (mode_tile (nth i listAllkey) bFlag) (setq i (+ i 1)))
   )
   (defun SaveAllData () (setq listDataMainDlg (EliminateNILVals (mapcar 'get_tile listKeyMainDlg))))
   (defun RestoreAllData () (mapcar 'set_tile listKeyMainDlg listDataMainDlg))
   (defun TileInit (iDoneDialogFlag / strAlgn bOnOffFlag)
     (setq strAlgn (car listDataMainDlg))
     (if (= (findfile strAlgn) nil)
	    (setq bOnOffFlag 0) (setq bOnOffFlag 1)
     )
     (EnableAllTiles bOnOffFlag) (RestoreAllData)
   )
   (defun OnOk (listKey / retList bError)
     ;Return Statement
	  (setq retList (EliminateNILVals (mapcar 'get_tile listKey)) bError 0)
	  (setq strAlgnName (nth 0 retList))

      (if (or (= (strlen strAlgnName) 0) (= (findfile strAlgnName) nil)) ;;"AlgnName"
    	 (progn (set_tile "err" "ERROR: Invalid C/S Data file") (mode_tile (nth 0 listKey) 2) (setq bError 1))
	  )
	  (if (and (<= (atof (nth 2 retList)) 0.0) (= bError 0)) ;;"HorSca"
	    (progn (set_tile "err" "ERROR: Bad Horz. Scale!") (mode_tile (nth 2 listKey) 2) (setq bError 1))
	  )
	  (if (and (<= (atof (nth 3 retList)) 0.0) (= bError 0)) ;;"VerSca"
	    (progn (set_tile "err" "ERROR: Bad Vert. Scale!") (mode_tile (nth 3 listKey) 2) (setq bError 1));progn
	  )
	  (if (= bError 0) (done_dialog 0))
	  (setq retList retList)
   )
   (defun OnAlgnEBox ($key $value $reason / strFile bErr)
    (setq bErr 0 strFile (findfile $value))
    (if (not (= strFile nil))
      (progn
	   (setq bErr 0) (EnableAllTiles 1) (set_tile "err" "")
	  )
      (progn ;Disable
	   (setq bErr 1)
	   (EnableAllTiles 0) (set_tile "err" "Cross Section data not found!")
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
			  ((and (<= (atof (nth 6 retList)) 0.0) (= bError 0));;"RowHeight"
			   (set_tile "err" "ERROR: Invalid Row Height")
			   (mode_tile (nth 6 listKey) 2) (setq bError 1)
			  )
			)
	        (if (= bError 0) (done_dialog 0))
			(setq retList retList)
	   )
	   (defun OnCancel()  (done_dialog 1) (setq retList listData))
	   ;====================================================================================
	   (new_dialog "GetCSDataAnnotation" dcl_id)
	   (mapcar 'set_tile listKey listData)
	   (action_tile "accept" "(setq retList (OnOk listKey))")
	   (action_tile "cancel" "(setq retList (OnCancel))")
	   (start_dialog)
       (setq retList retList);; Return
   )
   (defun OnGraphPosnButt ($key $value $reason dcl_id listKey listData / OnOk OnCancel retList)
	   ;====================================================================================
	   (defun OnOk(listKey / retList)
	     (setq retList (list (atof (get_tile "HorGrphNo")) (atof (get_tile "VerGrphNo"))))
	     (if (or (<= (nth 0 retList) 0) (<= (nth 1 retList) 0))
			(set_tile "err" "Invalid Input Data.. Please Check!") 
	        (progn (setq retList (EliminateNILVals (mapcar 'get_tile listKey))) (done_dialog 0))
		 )
		 (setq retList retList);return
	   )
	   (defun OnCancel()  (done_dialog 1) (setq retList listData))
	   ;====================================================================================
	   (new_dialog "GetCSDataGraph" dcl_id)
	   (mapcar 'set_tile listKey listData)
	   (action_tile "accept" "(setq retList (OnOk listKey))")
	   (action_tile "cancel" "(setq retList (OnCancel))")
	   (start_dialog)
       (setq retList retList);; Return
   )
   (defun OnFileDlgButton (strTileFileEBox / strLabel strFname)
      (setq strLabel "Select C/Secn Data File" strFiletype "CSD")
      (setq strFname (getfiled strLabel "" strFiletype 2))
      (if (/= strFname nil) (set_tile strTileFileEBox strFname))
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (setq returnList nil dcl_id (load_dialog (strcat gstrSrvElevPath "SurvPlan")))

   (setq listAllkey (list "AlgnName" "DatumLevel" "HorSca" "VerSca" "FromStn" "UptoStn" "OrdX" "OrdY" "OrdZ" "PointButt" "AnnTextButt" "GraphPosnButt")
		 listKeyMainDlg (list "AlgnName" "DatumLevel" "HorSca" "VerSca" "FromStn" "UptoStn" "OrdX" "OrdY" "OrdZ")
         listKeyAnnTextDlg (list "NumAnnSize" "LabelAnnSize" "StnAnnSize" "LabelChain" "LabelLevel" "LabelStation" "RowHeight")
         listKeyGrphPosnDlg (list "HorGrphNo" "VerGrphNo")
   )
   (setq listDataMainDlg (list (strcat (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 3)) "CSD") "0.0" "1" "10" "0.000" "1.0" "0.0" "0.0" "0.0")
         listDataAnnTextDlg (list "1.0" "3.0" "3.0" "Distance in Metre" "Reduced Level in Metre" "Cross Section at CH :" "9.0")
         listDataGrphPosnDlg (list "1" "1")
   )
   (setq iDoneDialogFlag 10)
   (while (> iDoneDialogFlag 1)
	   (new_dialog "GetCSData" dcl_id)
	   (TileInit iDoneDialogFlag)
	   (action_tile "AlgnName" "(OnAlgnEBox $key $value $reason)")
   	   (action_tile "AnnTextButt" "(setq listDataAnnTextDlg (OnAnnTextButt $key $value $reason dcl_id listKeyAnnTextDlg listDataAnnTextDlg))")
   	   (action_tile "GraphPosnButt" "(setq listDataGrphPosnDlg (OnGraphPosnButt $key $value $reason dcl_id listKeyGrphPosnDlg listDataGrphPosnDlg))")
   	   (action_tile "PointButt" "(progn (SaveAllData)(done_dialog 5))")
	   (action_tile "accept" "(setq returnList T listDataMainDlg (OnOk listKeyMainDlg))")
	   (action_tile "AlgnNameButton" "(progn (OnFileDlgButton (nth 0 listKeyMainDlg))(SaveAllData)(TileInit iDoneDialogFlag))")
	   (action_tile "cancel" "(progn (setq returnList nil)(done_dialog 1))")
	   (setq iDoneDialogFlag (start_dialog))
	   (cond 
	     ((= iDoneDialogFlag 5)
		  (initget 1) (setq point (getpoint "\nPick Cross Section Plot Origin:"))
		  (setq listDataMainDlg (ModifyDataList listDataMainDlg point)) 
		 )
	   )
   )
   (unload_dialog dcl_id)
   (if (= returnList nil)
	   (setq returnList nil)
	   (setq returnList (list listDataMainDlg listDataAnnTextDlg listDataGrphPosnDlg))
   )
   (setq returnList returnList)
)
(PrintLoadMsg 43)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawingParam
             ( /
                returnListFrDCL listDataMainDlg listDataAnnTextDlg listDataGrphPosnDlg
                strAlgnName fHorzScale fVertScale fStartStn fEndStn listPoint fDatumLevel
                iRow iCol strChainage strElevation strCSLabelSuffix fAnnTextSize fLabelTextSize fStnMarkTextSize list_01 list_02 list_03 listReturn 
                fLabelRowHeight hSrcFile listTemp 
             )
  (setq returnListFrDCL (GetCSDataThruDCL)) ;; Dialog Driver
  (if (not (= returnListFrDCL nil))
    (progn
	    (setq listDataMainDlg (nth 0 returnListFrDCL)
	          listDataAnnTextDlg (nth 1 returnListFrDCL)
	          listDataGrphPosnDlg (nth 2 returnListFrDCL)
	    )
		;;"AlgnName" "DatumLevel" "HorSca" "VerSca" "FromStn" "UptoStn" "OrdX" "OrdY" "OrdZ"
		(setq strAlgnName (nth 0 listDataMainDlg)
	          fDatumLevel (atof (nth 1 listDataMainDlg))
		      fHorzScale (atof (nth 2 listDataMainDlg))
		      fVertScale (atof (nth 3 listDataMainDlg))
		      fStartStn (atof (nth 4 listDataMainDlg))
		      fEndStn (atof (nth 5 listDataMainDlg))
              listPoint (list (atof (nth 6 listDataMainDlg)) (atof (nth 7 listDataMainDlg)) (atof (nth 8 listDataMainDlg)))
 	    )
        ;;"HorGrphNo" "VerGrphNo"
		(setq iRow (atoi (nth 0 listDataGrphPosnDlg))
		      iCol (atoi (nth 1 listDataGrphPosnDlg))
 	    )
        ;;"NumAnnSize" "LabelAnnSize" "StnAnnSize" "LabelChain" "LabelLevel" "LabelStation" "RowHeight"
	    (setq strChainage (nth 3 listDataAnnTextDlg)
			  strElevation (nth 4 listDataAnnTextDlg)
			  strCSLabelSuffix (nth 5 listDataAnnTextDlg)
              fAnnTextSize (atof (nth 0 listDataAnnTextDlg))
              fLabelTextSize (atof (nth 1 listDataAnnTextDlg))
	          fStnMarkTextSize (atof (nth 2 listDataAnnTextDlg))
              fLabelRowHeight (atof (nth 6 listDataAnnTextDlg))
	    )
		;;Read Min & Max Elevs from data file..............................
        (setq hSrcFile (open strAlgnName "r"))
        (setq listTemp (ReadDwgParamFrInputFile hSrcFile))
        (setq fMinElev fDatumLevel fMaxElev (nth 1 (nth 1 listTemp)))
        (close hSrcFile)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setq list_01 (list strAlgnName fMinElev fMaxElev fHorzScale fVertScale fStartStn fEndStn listPoint)
              list_02 (list iRow iCol)
              list_03 (list strChainage strElevation strCSLabelSuffix fAnnTextSize fLabelTextSize fStnMarkTextSize fLabelRowHeight)
        )
        (setq listReturn (list list_01 list_02 list_03))
    );progn
  );if
  (setq listReturn listReturn);Return Statement
)
(PrintLoadMsg 50)


;;
;;For Reading of Data Only
;;
; Bit Code Specification for each Line
; 0 = No Annotation
; 1 = Ordinate Line
; 2 = Annotation Text
; 4 = Annotation with Blk
; 9 = Start of of Station Data (for specification of chainage)
;10 = End of Station Data

;;Reads Next Station's Data and returns the list having
;;(member 1> Station Chainage (real);  member 2... n> Off&Elev Sequentially
;;Status : Process
(defun ReadNextStnData (hSrcFile / ReadChainage ReadOffEleData fChainage listRet)
  ;; Read Chainage Data Start ==========================
  ;; Param  : 1> Src File Handle
  ;; Return : Chainage of Station [PASS]
  ;; Return : nil [FAILED]
  (defun ReadChainage (hSrcFile / bContinueRead strLine iChainageCode fChainage chfirstChar)
    (setq bContinueRead 1)
    (while (= bContinueRead 1)
       (setq strLine (read-line hSrcFile))
       (if (= strLine nil)
          (setq bContinueRead -1) ;; End of File
          (progn
               (setq chfirstChar (substr (GetFirstWord strLine) 1 1))
               (if (and (or (= (strlen strLine) 0) (= chfirstChar "*")) (not (= bContinueRead -1)))
                  (setq bContinueRead 1)
                  (setq bContinueRead 0)
               )
          )
       ) 
    )
    (if (not (< bContinueRead 0))
      (progn
        (setq wordList (XtractAllWords strLine))
        (if (< (length wordList) 2)
           (progn (alert (strcat "Error: Chainage Data Not found!\nPlease check the following line in Input File\n" strLine)) (setq bContinueRead -1))
           (setq iChainageCode (atoi (car wordList)) fChainage (atof (cadr wordList)))
        )
        (if (not (= iChainageCode 9))
          (progn 
            (progn (alert (strcat "Error: Invalid Chainage Code found!\nPlease check the following line in Input File\n" strLine)) (setq bContinueRead -1))
            (setq fChainage nil)
          )
          (setq fChainage fChainage)
        )
      );progn
    );if
    (if (= bContinueRead -1) (setq fChainage nil)) ;; End of File
    (setq fChainage fChainage) ;Return
  )
  ;; ========================== Read Chainage Data End
  ;; Read Off/Elev Data Start ==========================
  ;; Param  : 1> Chainage 2> SrcFile Handle
  ;; Return : (list Chainage (Offsets List...) (Elev List...) (Text List...) (BlkName List...)) [PASS]
  ;; Return : nil [FAILED]
  (defun ReadOffEleData (fChainage hSrcFile / 
           bContinueRead strLine iCode fChainage  tempAtom
           listOfCode listOfOff listOfElev listOfText listOfBlkName listTemp chfirstChar 
           )
    (setq listOfCode (list ())
          listOfOff (list ()) listOfElev (list ())
          listOfText (list ()) listOfBlkName (list ())
          listTemp (list ())
    )
    (setq bContinueRead 1)
    (while (= bContinueRead 1)
       (setq strLine (read-line hSrcFile))
       (if (= strLine nil)
          (setq bContinueRead -1) ;; End of File Check
          (setq strFirstWord (GetFirstWord strLine) iCode (atoi strFirstWord))
       )
       (setq chfirstChar (substr strFirstWord 1 1))
       (if (and (or (= (strlen strLine) 0) (= chfirstChar "*")) (= bContinueRead 1))
          (setq bContinueRead 1)
          (progn
             (if (> iCode 7)
                (progn
                   ;; Break the Loop Normally/Abnormally
                   (if (= iCode 10)
                      (setq bContinueRead 0)
                      (progn
                        (progn (alert (strcat "ERROR: Bad Code found\nPlease Check the following line in Input file\n" strLine)) (setq bContinueRead -1))
                        (setq bContinueRead -2)
                      )
                   )
                )
                (progn
                    ;; Create & Update List
                    (setq listTemp (XtractAllWords strLine))
                    (setq listOfCode (append listOfCode (list (atoi (nth 0 listTemp))))
                          listOfOff (append listOfOff (list (atof (nth 1 listTemp))))
                          listOfElev (append listOfElev (list (atof (nth 2 listTemp))))
                    )
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
                            (setq listOfText (append listOfText (list (nth 3 listTemp))))
                            (progn
								(alert (strcat "ERROR: Annotation Text Not Found at Chainage : " (rtos fChainage) " Offset: " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
								(setq bContinueRead -1)
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
										(setq listOfBlkName (append listOfBlkName (list (nth 3 listTemp))))
									)
									(progn
										(if (>= (length listTemp) 5)
											(progn
												(setq listOfBlkName (append listOfBlkName (list (nth 4 listTemp))))
											)
											(progn
												(alert (strcat "ERROR: Block Name Not Found at Chainage : " (rtos fChainage) " Offset : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
												(close hSrcFile)(exit)
											)
										)
									)
								)
							)
                            (progn
								(alert (strcat "ERROR: Block Name Not Found at Chainage : " (rtos fChainage) " Offset : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
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
								(alert (strcat "ERROR: Block Name Not Found at Chainage : " (rtos fChainage) " Offset : " (nth 1 listTemp) "\nPlease Check the Input File and try again..."))
								(close hSrcFile)(exit)
                            )
                         );if
                       )
                    );if
                )
             )
          )
        );if 
    )
    (setq listOfCode (cdr listOfCode)
          listOfOff (cdr listOfOff) listOfElev (cdr listOfElev)
          listOfText (cdr listOfText) listOfBlkName (cdr listOfBlkName)
    )
    (if (= bContinueRead -2)
       (progn
			(alert (strcat "ERROR: Bad Code Found At Chainage : " (rtos fChainage) " Code = " (itoa iCode) "\nPlease Check the Input File and try again..."))
			(close hSrcFile)(exit)
       )
    )
    (if (or (= bContinueRead -2) (= bContinueRead -1))
       (progn (setq listTemp nil))
       (progn (setq listTemp (list fChainage listOfCode listOfOff listOfElev listOfText listOfBlkName)))
    )
    (setq listTemp listTemp);;Return Statement
  )
  ;; ========================== Read Off/Elev Data End
  (setq fChainage (ReadChainage hSrcFile))
  (if (= fChainage nil)
     (setq listRet nil)
     (setq listRet (ReadOffEleData fChainage hSrcFile))
  )
  (setq listRet listRet);; Return Statement
)

(defun ReadDwgParamFrInputFile (hSrcFile / stnDataList i listOffset listElev fTempMin fTempMax fMinOff fMaxOff retList )
  (setq stnDataList T i 0)
  (princ "\nPlease wait.. Checking C/S Data..\n")
  (while (/= stnDataList nil)
    (setq stnDataList (ReadNextStnData hSrcFile))
    (if (/= stnDataList nil)
        (progn
            (princ (strcat "\rPlease wait.. Verifying C/S Data at Section '" (rtos (nth 0 stnDataList)) "'"))
            (setq listOffset (nth 2 stnDataList) listElev (nth 3 stnDataList))
            ;Elevation.........................
            (if (= i 0) (setq fMinElev (FindMinFrList listElev) fMaxElev (FindMaxFrList listElev)))
            (setq fTempMin (FindMinFrList listElev) fTempMax (FindMaxFrList listElev))
            (if (< fTempMin fMinElev) (setq fMinElev  fTempMin))
            (if (> fTempMax fMaxElev) (setq fMaxElev  fTempMax))
            ;Offset .........................
            (if (= i 0) (setq fMinOff (FindMinFrList listOffset) fMaxOff (FindMaxFrList listOffset)))
            (setq fTempMin (FindMinFrList listOffset) fTempMax (FindMaxFrList listOffset))
            (if (< fTempMin fMinOff) (setq fMinOff  fTempMin))
            (if (> fTempMax fMaxOff) (setq fMaxOff  fTempMax))
        )
    )
    (setq i (+ i 1))
  )
  (setq fMinElev (fix fMinElev)
        fMaxElev (fix fMaxElev)
        fMinOff (fix (- fMinOff (* fMinOff 0.25)))
        fMaxOff (fix (+ fMaxOff (* fMaxOff 0.25)))
  )
  (setq retList (list (list fMinOff fMaxOff) (list fMinElev fMaxElev)))
)
(PrintLoadMsg 70)

;;SurvElev.lsp
;;C/S Data Read Related Routines <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;
;;C/S Data Read/Write Related Routines Ends Here<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;
;;
;;General Routines common for both data sets Starts Here>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;
;; Status OK
(defun MakeOffsetElevList (OffElevDataStr / listTemp)
  (setq listTemp (XtractAllWords OffElevDataStr))
  ;; Remove First two columns having only 0's & 3rd having Chainage
  (setq listTemp (cdr (cdr (cdr listTemp))))
  (setq listTemp (WordListToRealList listTemp))
)
(PrintLoadMsg 75)
;;
;; Status OK
;; Returns index of Elev corresponding to MaxOffset
;; Returns -1 if MaxOffset not found
;;
(defun GetMaxOffsetIndex (OffElevList / i iLoopLim iIndex fPreOffset fPostOffset)
  (setq i 2 iLoopLim (length OffElevList) iIndex -1)
  (while (< i iLoopLim)
     (setq fPreOffset (nth (- i 2) OffElevList) fPostOffset (nth i OffElevList))
     (if (> fPreOffset fPostOffset)
		(setq iIndex i iLoopLim -100);Break
	 )
	 (setq i (+ i 2))
  );while
  (if (or (not (= iLoopLim -100)) (not (= iIndex -1)))
    (progn
        (if (not (= iLoopLim -100))
            (setq iIndex (- (length OffElevList) 2))
            (setq iIndex (- iIndex 2))
        )
    )
  )
  (setq iIndex iIndex)
)
;;
;; Status OK
;; Eliminates all Bad offsets & Elevs & returns the modified list
;;
(defun EliminateBadOffsetElev (OffElevList / iIndexOfBadOff listReturn i  iLoopLim)
  (setq iIndexOfBadOff (GetMaxOffsetIndex OffElevList))
  (if (= iIndexOfBadOff -1)
	  (progn (getstring "\nBadStr not found")(setq listReturn OffElevList));; Return
	  (progn
		  (setq listReturn (list ())
				i 0 iLoopLim (+ iIndexOfBadOff 1)
		  )
		  (while (<= i iLoopLim)
		      (setq listReturn (append listReturn (list (nth i OffElevList))))
		      (setq i (+ i 1))
		  );while
		  (setq listReturn (cdr listReturn));; Return
	  )
  );if
  (setq listReturn listReturn);; Return
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUtil.lsp
;; General utility file for String Handling
;;

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
(PrintLoadMsg 90)

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
(defun IsStrStation (strArg / i iLoopLim strPre strPost bGotThePlusSign chChar ret)
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
(PrintLoadMsg 94)
(defun ConvStnToChainage (strArg fInterval /  retList retChainage)
	(setq retList (IsStrStation strArg))
	(setq retChainage nil)
	(if (= retList nil)
	   (alert (strcat "ERROR: Bad Param In SubR FindChainage = " strArg))
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
;;
;;AUtil02.Lsp
;;Graphic Utility File
;;

(defun D2R (fAng) (* fAng (/ pi 180.0)))
(defun R2D (fAng) (* (/ 180.0  pi) fAng))
(defun POINT (pt strLyr) (entmake (list (cons 0 "POINT") (cons 10 pt) (cons 8 strLyr))))
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
(defun TEXTLEFT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str)  (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun TEXTMID (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle")) (cons 1 str) (cons 71 0) (cons 72 4) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(defun TEXTRIGHT (ptStart str fAngDeg fSize strLyr) (entmake (list (cons 0 "TEXT") (cons 7 (getvar "textstyle"))  (cons 1 str) (cons 71 0) (cons 72 2) (cons 73 0) (cons 11 ptStart) (cons 10 ptStart) (cons 67 0) (cons 50 (D2R fAngDeg)) (cons 40 fSize) (cons 8 strLyr))))
(PrintLoadMsg 98)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;XData Related General func;;;;;;;;;;;;;;;;;;;;;;;START;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gStrAppNameMain "ATANU_BANIK" gStrAppNameSub "ATANU_BANIK_DATA")
(defun XDataSetup()
	(if (= (tblsearch "appid" gStrAppNameMain) nil)
	  (if (=  (regapp gStrAppNameMain) nil)
		  (princ "\nInternal Error: failed to register XD");
	  )
	)
	(if (= (tblsearch "appid" gStrAppNameSub) nil)
	  (if (=  (regapp gStrAppNameSub) nil)
		  (princ "\nInternal Error: failed to register XD");
	  )
	)
)
(defun AttatchXData(EName strDataMain strDataSub / listXData listEntData)
	(if (= (tblsearch "appid" gStrAppNameMain) nil)
		(XDataSetup)
	)
	(setq listEntData (entget EName)
		  listXData (list (list -3 (list gStrAppNameMain (cons 1000 strDataMain))))
		  listEntData (append listEntData listXData)
	)
	(entmod listEntData)
	(setq listEntData (entget EName)
		  listXData (list (list -3 (list gStrAppNameSub (cons 1000 strDataSub))))
		  listEntData (append listEntData listXData)
	)
	(entmod listEntData)
)
(defun FetchXData(EName / listEntData listToRet i iLim)
	(setq listEntData (entget EName (list gStrAppNameMain gStrAppNameSub))
		  listEntData (assoc -3 listEntData)
		  listToRet (list nil)
	)
	(if(/= listEntData nil)
		(progn
			(setq listEntData (cdr listEntData) i 0 iLim (length listEntData))
			(while (< i iLim)
				(if (or (= (strcase gStrAppNameMain) (car (nth i listEntData))) (= (strcase gStrAppNameSub) (car (nth i listEntData))))
					(setq listToRet (append listToRet (list (cdr (cadr (nth i listEntData))))))
				)
				(setq i (+ i 1))
			)
		)
	)
	(setq listToRet (reverse (cdr listToRet)));Return
)
(defun UpdAnnTxt(iAnnTypeCode fValueToAdd / GetNativeEntitySS UpdateEntities strXDataMain strXDataSub)
	(defun GetNativeEntitySS(strPrompt strEntType strXDataMain strXDataSub / GetEntType i iLim iFilteredOut ssFiltered EName listXData)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(defun GetEntType(EName) (cdr (assoc 0 (entget EName))))
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(princ strPrompt)
		(setq sset01 (ssget)) 
		(setq ssFiltered (ssadd)) 
		(if(/= sset01 nil) 
			(progn
				(setq i 0 iLim (sslength sset01) iFilteredOut 0)
				(while (< i iLim)
					(setq EName (ssname sset01 i))
					(if (= (strcase (GetEntType EName)) strEntType)
						(progn
							;Chk for XData presence..if true then add to sset 
							(setq listXData (FetchXData EName))
							(if (/= listXData nil)
								(progn
									(if (and (= (length listXData) 2) (= strXDataMain (nth 0 listXData)) (wcmatch (nth 1 listXData) strXDataSub)) ;;;;;;;;;(= strXDataSub (nth 1 listXData))
										(setq ssFiltered (ssadd EName ssFiltered));;Add to new SS
										(setq iFilteredOut (+ iFilteredOut 1))
									)
								)
							)
						)
						(progn
							(setq iFilteredOut (+ iFilteredOut 1))
						)
					)
					(setq i (+ i 1))
				);while
				(princ (strcat "\n" (itoa iFilteredOut)  " No of entities filtered out...\nTotal " (itoa (sslength ssFiltered)) " No of entities found"))
			)
		)
		(if(> (sslength ssFiltered) 0)
			(setq ssFiltered ssFiltered);Return
			(setq ssFiltered nil);Return
		)
	)
	(defun UpdateEntities(strXDataMain strXDataSub ValueToAdd / sset i iLim listEnt strTxt)
		(setq sset (GetNativeEntitySS "\nSelect entities to modify :" "TEXT" strXDataMain strXDataSub))
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
	(setq strXDataMain "CSDData");;Extended data .........
	(cond 
		((= iAnnTypeCode 0) (setq strXDataSub "ELEV"))
		(T)
	)
	(if(/= iAnnTypeCode 0)
		(princ "ERROR: Abnormal parameter passed!!! Contact developer\n")
		(UpdateEntities strXDataMain strXDataSub fValueToAdd)
	)
	(princ)
)
(defun C:XD(/ EName) (setq EName (car (entsel))) (FetchXData EName))
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;XData Related General func;;;;;;;;;;;;;;;;;;;;;;;END  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun C:Test (/ hSrcFile)
;  (setq hSrcFile (open "c:/acadwin/a.csd" "r")) 
;  (princ (ReadDwgParamFrInputFile hSrcFile))
;  (close hSrcFile)
;  (princ)
;)
;(DrawCSStarter "C:/DEVELOPMENT/FieldSurvey/SURVEY/PLAN/")
;C:\DEVELOPMENT\FieldSurvey\SURVEY\PLAN\DATA\CSECN\TEST.CSD

(PrintLoadMsg 100)
(princ)